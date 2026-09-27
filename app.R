# =============================================================================
# AssuRance: Bayesian assurance vs. frequentist power for two-arm trials with
# continuous, baseline-adjusted (ANCOVA), binary or time-to-event outcomes.
#
# Run locally:   shiny::runApp()        (from this directory)
#
# Files (Shiny sources everything in R/ automatically)
#   app.R               server logic (this file)
#   R/calculations.R    outcome-agnostic engine; documents the mapping of the
#                       inputs to bayesassurance's parameters
#   R/models.R          one "model" per outcome type (variances, simulators,
#                       power, labels)
#   R/ui_sidebar.R      sidebar inputs
#   R/ui_tabs.R         main panel and tabs
#   R/summary_text.R    plain-language summary
#   R/plots.R           plot builders
#   R/prompt_generator.R  LLM prompt helper
#   R/methods_ui.R      "Methods & help" tab
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(DT)
library(bayesassurance)


# ============================================================================
# UI
# ============================================================================
ui <- function(request) {
  page_sidebar(
    title = "AssuRance: Bayesian assurance vs. frequentist power",
    theme = bs_theme(version = 5, primary = "#1f5fa8"),
    fillable = FALSE,
    sidebar = sidebar_ui(),
    main_ui()
  )
}


# ============================================================================
# Server
# ============================================================================

# Every input that feeds the calculation.
MODEL_INPUTS <- c(
  "otype", "measure", "sigma", "rho", "p_control", "surv_median", "time_unit",
  "accrual", "followup", "loss_pct",
  "design_mean", "design_sd", "analysis_mean", "analysis_sd",
  "rd_design_mean", "rd_design_sd", "rd_analysis_mean", "rd_analysis_sd",
  "ratio_design_lo", "ratio_design_hi", "ratio_analysis_lo", "ratio_analysis_hi",
  "same_prior", "n_min", "n_max", "n_step", "ratio", "dropout", "alt",
  "threshold", "rd_threshold", "ratio_threshold", "alpha", "target",
  "engine", "mc_iter", "seed", "show_exact")

# The inputs that matter for a given outcome type (used to decide whether the
# shown results are out of date).
relevant_inputs <- function(r) {
  fam <- scale_family(r$otype, r$measure)
  common <- c("otype", "same_prior", "n_min", "n_max", "n_step", "ratio",
              "alt", "alpha", "target", "engine", "mc_iter", "seed", "show_exact")
  type <- switch(r$otype,
    cont   = "sigma",
    ancova = c("sigma", "rho"),
    binary = c("measure", "p_control", "dropout"),
    surv   = c("surv_median", "time_unit", "accrual", "followup", "loss_pct"))
  if (r$otype %in% c("cont", "ancova")) type <- c(type, "dropout")
  priors <- switch(fam,
    additive = c("design_mean", "design_sd", "analysis_mean", "analysis_sd", "threshold"),
    rd       = c("rd_design_mean", "rd_design_sd", "rd_analysis_mean", "rd_analysis_sd", "rd_threshold"),
    ratio    = c("ratio_design_lo", "ratio_design_hi", "ratio_analysis_lo",
                 "ratio_analysis_hi", "ratio_threshold"))
  r[c(common, type, priors)]
}

server <- function(input, output, session) {

  setBookmarkExclude(c(
    "go", "stop", "save_scenario", "remove_last", "clear_scenarios", "scenario_label",
    # LLM prompt helper: free text would bloat the bookmark URL
    "pg_title", "pg_condition", "pg_intervention", "pg_comparator",
    "pg_outcome", "pg_timepoint", "pg_phase", "pg_direction", "pg_setting",
    "pg_mcid", "pg_known", "pg_constraints", "pg_web", "pg_current",
    "pg_language", "pg_answer", "pg_apply",
    paste0("results_table_", c("rows_current", "rows_all", "rows_selected",
                               "search", "state", "cell_clicked",
                               "cells_selected", "columns_selected"))))

  raw_inputs <- reactive(lapply(setNames(MODEL_INPUTS, MODEL_INPUTS),
                                function(id) input[[id]]))

  st <- new.env()          # non-reactive state
  st$job <- NULL           # the running simulation
  st$prev_otype <- NULL
  st$skip_n_defaults <- FALSE

  # ---- Collect and validate inputs --------------------------------------
  # Returns list(p = validated inputs, m = model from R/models.R).
  config <- reactive({
    r <- raw_inputs()
    req(r$otype)
    is_num   <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
    is_whole <- function(x) is_num(x) && abs(x - round(x)) < 1e-8
    pos      <- function(x) is_num(x) && x > 0
    sim      <- identical(r$engine, "sim")
    same     <- isTRUE(r$same_prior)
    one_sided <- !identical(r$alt, "two.sided")
    fam      <- scale_family(r$otype, r$measure)

    validate(
      need(r$otype %in% c("cont", "ancova", "binary", "surv"), "Choose an outcome type."),
      # outcome-specific
      need(!(r$otype %in% c("cont", "ancova")) || pos(r$sigma),
           "Outcome standard deviation must be a positive number."),
      need(r$otype != "ancova" || (is_num(r$rho) && r$rho >= 0 && r$rho <= 0.95),
           "The baseline-outcome correlation must be between 0 and 0.95."),
      need(r$otype != "binary" || (is_num(r$p_control) && r$p_control > 0 && r$p_control < 100),
           "The control-group event rate must be between 0 and 100%."),
      need(r$otype != "surv" || pos(r$surv_median),
           "The control-group median time must be a positive number."),
      need(r$otype != "surv" || (is_num(r$accrual) && r$accrual >= 0 &&
                                 is_num(r$followup) && r$followup >= 0 &&
                                 r$accrual + r$followup > 0),
           "Recruitment and follow-up must be zero or positive, and not both zero."),
      need(r$otype != "surv" || (is_num(r$loss_pct) && r$loss_pct >= 0 && r$loss_pct < 90),
           "Loss to follow-up must be between 0 and 90%."),
      # priors
      need(fam != "additive" || is_num(r$design_mean), "Expected effect (design prior mean) must be a number."),
      need(fam != "additive" || pos(r$design_sd), "Design prior SD must be a positive number."),
      need(fam != "additive" || same || is_num(r$analysis_mean), "Analysis prior mean must be a number."),
      need(fam != "additive" || same || pos(r$analysis_sd), "Analysis prior SD must be a positive number."),
      need(fam != "rd" || (is_num(r$rd_design_mean) && abs(r$rd_design_mean) < 100),
           "Expected risk difference must be between -100 and 100 percentage points."),
      need(fam != "rd" || pos(r$rd_design_sd), "Risk difference SD must be a positive number."),
      need(fam != "rd" || same || is_num(r$rd_analysis_mean), "Analysis prior mean must be a number."),
      need(fam != "rd" || same || pos(r$rd_analysis_sd), "Analysis prior SD must be a positive number."),
      need(fam != "ratio" || (pos(r$ratio_design_lo) && is_num(r$ratio_design_hi) &&
                              r$ratio_design_hi > r$ratio_design_lo),
           "The plausible range for the ratio needs 0 < From < To."),
      need(fam != "ratio" || same || (pos(r$ratio_analysis_lo) && is_num(r$ratio_analysis_hi) &&
                                      r$ratio_analysis_hi > r$ratio_analysis_lo),
           "The analysis prior range for the ratio needs 0 < From < To."),
      # success criterion
      need(!one_sided || fam != "additive" || is_num(r$threshold), "The success threshold must be a number."),
      need(!one_sided || fam != "rd" || (is_num(r$rd_threshold) && abs(r$rd_threshold) < 100),
           "The success threshold must be between -100 and 100 percentage points."),
      need(!one_sided || fam != "ratio" || pos(r$ratio_threshold),
           "The success threshold for a ratio must be positive (1 = no effect)."),
      need(is_num(r$alpha) && r$alpha > 0 && r$alpha < 1,
           "Significance threshold must be between 0 and 1."),
      # design
      need(is_whole(r$n_min) && r$n_min >= 2, "Minimum sample size must be a whole number of at least 2."),
      need(is_whole(r$n_max), "Maximum sample size must be a whole number."),
      need(is_whole(r$n_step) && r$n_step >= 1, "Step must be a whole number of at least 1."),
      need(is_num(r$n_min) && is_num(r$n_max) && r$n_max > r$n_min,
           "Maximum sample size must exceed the minimum."),
      need(!is_num(r$n_max) || r$n_max <= 20000, "Maximum sample size is limited to 20,000 per group."),
      need(is_num(r$ratio) && r$ratio >= 0.1 && r$ratio <= 10,
           "Allocation ratio must be between 0.1 and 10."),
      need(r$otype == "surv" || (is_num(r$dropout) && r$dropout >= 0 && r$dropout < 90),
           "Dropout must be between 0 and 90%."),
      # computation
      need(!sim || (is_whole(r$mc_iter) && r$mc_iter >= 100),
           "Number of simulated trials must be a whole number of at least 100."),
      need(!sim || is_num(r$seed), "Random seed must be a number.")
    )

    p <- r
    p$same_prior <- same
    p$show_exact <- isTRUE(r$show_exact)
    p$p_control  <- if (is_num(r$p_control)) r$p_control / 100 else NA
    p$loss       <- if (is_num(r$loss_pct)) r$loss_pct / 100 else 0
    p$dropout    <- if (r$otype == "surv") 0 else r$dropout / 100
    p$time_unit  <- trimws(r$time_unit %||% "")

    n_c <- seq(r$n_min, r$n_max, by = r$n_step)
    if (utils::tail(n_c, 1) != r$n_max) n_c <- c(n_c, r$n_max)
    validate(need(length(n_c) <= 200,
      "That range gives more than 200 sample sizes; please use a larger step."))
    p$n_c <- as.integer(n_c)
    p$n_t <- as.integer(treatment_n(n_c, r$ratio))

    m <- build_model(p)
    if (sim) {
      est <- p$mc_iter * sum(m$per_iter(p$n_t, p$n_c))
      validate(need(est <= 900, paste0(
        "This simulation would take roughly ", round(est / 60),
        " minutes. Reduce the maximum sample size, the number of sample ",
        "sizes, or the simulations per sample size, or use the exact engine.")))
    }
    list(p = p, m = m)
  })

  # When the outcome type changes and the sample-size range is still the
  # previous type's default, switch to the new type's default range.
  observeEvent(input$otype, {
    prev <- st$prev_otype
    st$prev_otype <- input$otype
    if (isTRUE(st$skip_n_defaults)) { st$skip_n_defaults <- FALSE; return() }
    if (is.null(prev) || prev == input$otype) return()
    now <- c(input$n_min, input$n_max, input$n_step)
    if (length(now) == 3 && isTRUE(all(now == N_DEFAULTS[[prev]]))) {
      d <- N_DEFAULTS[[input$otype]]
      updateNumericInput(session, "n_min", value = d[1])
      updateNumericInput(session, "n_max", value = d[2])
      updateNumericInput(session, "n_step", value = d[3])
    }
  })

  # ---- Live hints in the sidebar -----------------------------------------
  live_model <- reactive(tryCatch(config()$m, error = function(e) NULL))

  output$engine_note <- renderUI({
    m <- live_model(); req(m)
    tags$div(class = "form-text", paste0("Simulation engine: ", m$engine_label,
                                         ". Frequentist comparison: ", m$power_label, "."))
  })

  output$effect_hint <- renderUI({
    m <- live_model(); req(m)
    z <- qnorm(0.975)
    lo <- m$m_d - z * m$s_d; hi <- m$m_d + z * m$s_d
    txt <- switch(m$family,
      additive = paste0("Effect = ", m$labels$short, " (treatment minus control). ",
                        "Your design prior's 95% range: ", fmt_num(signif(lo, 3)), " to ",
                        fmt_num(signif(hi, 3)), "."),
      rd = paste0("Effect = treatment minus control event rate (percentage points; ",
                  "negative = fewer events with treatment). Implied treatment-group ",
                  "event rate: about ", fmt_pct(m$p_t(m$m_d), 0), " (95% range ",
                  fmt_pct(m$p_t(lo), 0), " to ", fmt_pct(m$p_t(hi), 0), ")."),
      ratio = paste0(
        if (grepl("^[aeiou]", m$labels$short)) "An " else "A ", m$labels$short, " below 1 means ",
        if (m$otype == "surv") "a lower event rate (longer time to event)" else "fewer events",
        " with treatment. Your range implies a best guess of ", m$fmt_eff(m$m_d),
        " (SD of the log ", m$labels$short, " ", fmt_num(signif(m$s_d, 2)), ") and a ",
        fmt_pct(pnorm(-m$m_d / m$s_d), 0), " chance that it is below 1.",
        if (m$otype == "binary") paste0(" Implied treatment-group event rate: about ",
                                        fmt_pct(m$p_t(m$m_d), 0), ".") else ""))
    tagList(
      div(class = "alert alert-light border py-2 px-2 small", txt),
      if (isTRUE(m$prior_only_success))
        div(class = "alert alert-danger py-2 px-2 small",
            "Warning: with these priors the analysis would declare success even",
            "without data. See the Results tab, or untick 'Analyse with the same",
            "prior' and use a sceptical analysis prior."))
  })

  output$threshold_hint <- renderUI({
    m <- live_model(); req(m, m$alt != "two.sided")
    kind <- if (abs(m$C) < 1e-12) "ordinary superiority"
            else if ((m$alt == "greater" && m$C < 0) || (m$alt == "less" && m$C > 0))
              "a non-inferiority design"
            else "superiority by a clinically meaningful margin"
    tags$div(class = "form-text mb-2",
             paste0("Success = showing the ", m$labels$short, " is ",
                    if (m$alt == "greater") "above " else "below ",
                    m$fmt_eff(m$C), " (", kind, ")."))
  })

  output$time_estimate <- renderUI({
    cfg <- tryCatch(config(), error = function(e) NULL)
    if (is.null(cfg)) return(NULL)
    p <- cfg$p
    txt <- if (p$engine == "sim") {
      s <- p$mc_iter * sum(cfg$m$per_iter(p$n_t, p$n_c))
      paste0("Estimated run time: ~", format_secs(s), " (", length(p$n_c),
             " sample sizes). Hosted servers may be slower.")
    } else {
      paste0(length(p$n_c), " sample sizes; exact results are instant.")
    }
    tags$div(class = "form-text", txt)
  })

  # ---- Main computation (runs on "Calculate", and once at start-up) -----
  # Why the simulation runs in chunks: R does one thing at a time, so a
  # single long simulation call would block this app completely. No button
  # (not even a Stop button) could respond until it finished, and closing the
  # progress box would only hide it. Instead the simulation is split into
  # small chunks (see sim_chunk_plan) and a few are run per turn of Shiny's
  # event loop. Between turns the app handles clicks, so "Stop run" works,
  # other tabs stay usable, and the run ends if the browser tab is closed.
  results_rv <- reactiveVal(NULL)   # last completed run
  run_error  <- reactiveVal(NULL)   # validation message from the last click
  running    <- reactiveVal(FALSE)
  progress   <- reactiveVal(list(frac = 0, detail = ""))

  results <- reactive({
    if (!is.null(run_error())) validate(need(FALSE, run_error()))
    req(results_rv())
  })

  # Everything except the simulation itself: exact assurance, power,
  # sample-size finder and ceiling. `assur` is NULL for the exact engine.
  finish_run <- function(cfg, inputs_snapshot, assur, seconds) {
    p <- cfg$p; m <- cfg$m
    exact <- model_exact(m, p$n_t, p$n_c)
    if (is.null(assur)) {
      assur <- exact
      se <- NA_real_
    } else {
      se <- mc_se(assur, p$mc_iter)
    }
    power <- model_power(m, p$n_t, p$n_c)
    events <- if (is.null(m$events)) NA_real_ else m$events(m$m_d, p$n_t, p$n_c)

    # Sample size needed for the target (exact formulas, no noise)
    k <- length(p$n_c)
    parts <- model_exact(m, p$n_t[k], p$n_c[k], parts = TRUE)
    metrics <- list(
      n_assur = smallest_n(function(nt, nc) model_exact(m, nt, nc), p$ratio, p$target),
      n_power = smallest_n(function(nt, nc) model_power(m, nt, nc), p$ratio, p$target),
      ceiling = model_ceiling(m),
      # two-sided: share of success in the direction opposite to the expected one
      neg_share = if (m$m_d >= 0) parts$negative else parts$positive
    )

    results_rv(list(
      p = p, model = m, inputs = inputs_snapshot, metrics = metrics,
      seconds = seconds,
      table = data.frame(n_t = p$n_t, n_c = p$n_c, assurance = assur,
                         mc_se = se, exact = exact, power = power,
                         events = events)))
  }

  # Start (or restart) a run. ignoreNULL = FALSE also runs it at start-up.
  observeEvent(input$go, {
    # Results on tabs that don't show them would appear to "do nothing".
    if (!is.null(input$go) && input$go > 0 &&
        isTRUE(input$tabs %in% c("LLM prompt helper", "Compare scenarios", "Methods & help"))) {
      nav_select("tabs", "Results")
    }
    st$job <- NULL                       # cancel any run in progress
    cfg <- tryCatch(config(), error = function(e) e)
    if (inherits(cfg, "error")) {
      running(FALSE)
      run_error(conditionMessage(cfg))
      return()
    }
    run_error(NULL)
    snapshot <- relevant_inputs(raw_inputs())
    p <- cfg$p; m <- cfg$m

    if (p$engine == "exact") {
      running(FALSE)
      finish_run(cfg, snapshot, NULL, 0)
      return()
    }

    per_iter <- m$per_iter(p$n_t, p$n_c)
    plan <- sim_chunk_plan(per_iter, p$mc_iter, max_chunk = m$max_chunk(p$n_t, p$n_c))
    plan$cost <- plan$iters * per_iter[plan$i]

    # The run gets its own random-number stream, saved between chunks, so
    # nothing else that uses random numbers in the meantime (e.g. drawing a
    # plot on another tab) can change the results for a given seed.
    st$job <- list(cfg = cfg, inputs = snapshot, plan = plan, next_chunk = 1L,
                   successes = numeric(length(p$n_c)),
                   rng = with_seed_state(NULL, function() { set.seed(p$seed); NULL })$state,
                   t0 = Sys.time())
    progress(list(frac = 0, detail = "Starting..."))
    running(TRUE)
  }, ignoreNULL = FALSE)

  # Run chunks for about 0.3 s, then yield to the event loop.
  observe({
    req(running())
    j <- st$job
    if (is.null(j)) { running(FALSE); return() }
    p <- j$cfg$p; m <- j$cfg$m
    tick_start <- Sys.time()

    step <- tryCatch(with_seed_state(j$rng, function() {
      repeat {
        ch <- j$plan[j$next_chunk, ]
        est <- m$sim(m, p$n_t[ch$i], p$n_c[ch$i], ch$iters)
        j$successes[ch$i] <- j$successes[ch$i] + est * ch$iters
        j$next_chunk <- j$next_chunk + 1L
        if (j$next_chunk > nrow(j$plan) ||
            difftime(Sys.time(), tick_start, units = "secs") > 0.3) break
      }
      j
    }), error = function(e) e)

    if (inherits(step, "error")) {
      st$job <- NULL
      running(FALSE)
      showNotification(paste("The simulation failed:", conditionMessage(step)),
                       type = "error", duration = NULL)
      return()
    }
    j <- step$value
    j$rng <- step$state

    # A new Calculate click or Stop may have replaced/cleared the job; only
    # keep going if this is still the current one.
    if (!identical(st$job$t0, j$t0)) return()

    elapsed <- as.numeric(difftime(Sys.time(), j$t0, units = "secs"))
    if (j$next_chunk > nrow(j$plan)) {
      st$job <- NULL
      running(FALSE)
      finish_run(j$cfg, j$inputs, j$successes / p$mc_iter, elapsed)
    } else {
      st$job <- j
      done <- j$next_chunk - 1L
      frac <- sum(j$plan$cost[seq_len(done)]) / sum(j$plan$cost)
      left <- if (frac > 0.02) elapsed / frac * (1 - frac) else NA
      progress(list(frac = frac, detail = paste0(
        "Sample size ", j$plan$i[j$next_chunk], " of ", length(p$n_c),
        if (!is.na(left)) paste0(" \u00B7 about ", format_secs(left), " left") else "")))
      invalidateLater(10)
    }
  })

  observeEvent(input$stop, {
    if (!is.null(st$job)) {
      st$job <- NULL
      running(FALSE)
      showNotification(paste0("Run stopped. ", if (is.null(results_rv()))
        "Press Calculate to start again."
        else "The results shown are from your previous run."), type = "warning")
    }
  })

  output$running <- reactive(running())
  outputOptions(output, "running", suspendWhenHidden = FALSE)
  output$run_progress <- renderUI({
    pr <- progress()
    pct <- round(100 * pr$frac)
    tagList(
      div(class = "progress my-2", style = "height: 18px;",
          div(class = "progress-bar progress-bar-striped progress-bar-animated",
              role = "progressbar", style = paste0("width: ", max(pct, 2), "%;"),
              "")),
      tags$small(class = "text-muted", paste0(pct, "% done \u00B7 ", pr$detail)))
  })

  # Keep the per-tab sample-size selectors in step with the latest run.
  observeEvent(results(), {
    n <- max(results()$p$n_c)
    updateNumericInput(session, "cond_n", value = n)
    updateNumericInput(session, "sens_n", value = n)
  })

  output$stale <- renderUI({
    res <- tryCatch(results(), error = function(e) NULL)
    if (running() || is.null(res)) return(NULL)
    now <- tryCatch(relevant_inputs(raw_inputs()), error = function(e) NULL)
    if (identical(now, res$inputs)) return(NULL)
    div(class = "alert alert-warning py-2 px-3 my-2 small",
        "Inputs have changed. Press Calculate to update the results.")
  })

  # ---- Value boxes --------------------------------------------------------
  last_row <- reactive({ t <- results()$table; t[nrow(t), ] })
  n_text <- function(nt, nc) if (nt == nc) paste0("n = ", nc, " per group")
                             else paste0("n = ", nt, " / ", nc, " (T / C)")
  events_note <- function(m, n_c, ratio) {
    if (is.null(m$events) || is.na(n_c)) return("")
    paste0(" (~", fmt_num(round(m$events(m$m_d, treatment_n(n_c, ratio), n_c))), " events)")
  }

  output$vb_assur_title <- renderText(paste0("Bayesian assurance at ",
    n_text(last_row()$n_t, last_row()$n_c)))
  output$vb_assur <- renderText(fmt_pct(last_row()$assurance))
  output$vb_power_title <- renderText(paste0("Frequentist power at ",
    n_text(last_row()$n_t, last_row()$n_c)))
  output$vb_power <- renderText(fmt_pct(last_row()$power))
  output$vb_n_title <- renderText(paste0("n for ", fmt_pct(results()$p$target, 0),
    " assurance", if (results()$p$ratio == 1) " (per group)" else " (control)"))
  output$vb_n <- renderText({
    n <- results()$metrics$n_assur
    if (is.na(n)) "Not reachable" else format(n, big.mark = ",")
  })
  output$vb_n_sub <- renderText({
    res <- results(); mt <- res$metrics
    ev <- if (!is.na(mt$n_assur)) trimws(events_note(res$model, mt$n_assur, res$p$ratio)) else ""
    paste0(if (nzchar(ev)) paste0(gsub("[()]", "", ev), "; ") else "",
           "For ", fmt_pct(res$p$target, 0), " power: ",
           if (is.na(mt$n_power)) "not reachable"
           else paste0(format(mt$n_power, big.mark = ","),
                       events_note(res$model, mt$n_power, res$p$ratio)))
  })
  output$vb_ceiling <- renderText(fmt_pct(results()$metrics$ceiling))

  output$model_warnings <- renderUI({
    w <- results()$model$warnings
    if (length(w)) div(class = "alert alert-warning py-2 small", lapply(w, tags$div))
  })

  # ---- Main plot, summary, table -------------------------------------------
  output$curve <- renderPlotly(plot_curve_plotly(results()))
  output$curve_note <- renderText({
    res <- results(); m <- res$model
    if (res$p$engine == "sim") {
      paste0("Assurance simulated with ", m$engine_label, " (",
             format(res$p$mc_iter, big.mark = ","), " trials per sample size, seed ",
             res$p$seed, ", ", round(res$seconds, 1), " s). Shaded band: 95% ",
             "Monte Carlo interval. Power: ", m$power_label, ".")
    } else {
      paste0("Assurance from the exact formula", if (!m$const_var)
        " (numerical integration over the design prior)" else "",
        ". Power: ", m$power_label, ".")
    }
  })

  output$summary <- renderUI({
    tagList(lapply(build_summary(results()), tags$p))
  })

  display_table <- reactive({
    res <- results(); t <- res$table; p <- res$p
    d <- if (p$ratio == 1) {
      data.frame("n per group" = t$n_c, check.names = FALSE)
    } else {
      data.frame("n treatment" = t$n_t, "n control" = t$n_c, check.names = FALSE)
    }
    d[["Total n"]] <- t$n_t + t$n_c
    if (p$dropout > 0) {
      d[["Total to enrol"]] <- enrolled_n(t$n_t, p$dropout) + enrolled_n(t$n_c, p$dropout)
    }
    if (!all(is.na(t$events))) d[["Expected events"]] <- round(t$events)
    d[["Bayesian assurance"]] <- t$assurance
    if (p$engine == "sim") {
      d[["MC error (95%)"]] <- 1.96 * t$mc_se
      d[["Exact assurance"]] <- t$exact
    }
    d[["Frequentist power"]] <- t$power
    d
  })

  output$results_table <- renderDT({
    d <- display_table()
    pct_cols <- intersect(names(d), c("Bayesian assurance", "MC error (95%)",
                                      "Exact assurance", "Frequentist power"))
    datatable(d, rownames = FALSE, class = "compact stripe hover",
              options = list(dom = "t", paging = FALSE, scrollY = "360px",
                             scrollCollapse = TRUE,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all")))) |>
      formatPercentage(pct_cols, digits = 1)
  })

  # ---- Downloads -------------------------------------------------------------
  stamp <- function() format(Sys.time(), "%Y%m%d-%H%M")

  output$dl_csv <- downloadHandler(
    filename = function() paste0("assurance-results-", stamp(), ".csv"),
    content = function(file) {
      t <- results()$table
      names(t) <- c("n_treatment", "n_control", "assurance", "assurance_mc_se",
                    "assurance_exact", "frequentist_power", "expected_events")
      utils::write.csv(t, file, row.names = FALSE)
    })

  output$dl_png <- downloadHandler(
    filename = function() paste0("assurance-curve-", stamp(), ".png"),
    content = function(file) {
      ggsave(file, plot_curve_gg(results()), width = 8, height = 5, dpi = 150,
             bg = "white")
    })

  output$dl_report <- downloadHandler(
    filename = function() paste0("assurance-report-", stamp(), ".txt"),
    content = function(file) {
      res <- results()
      lines <- c(
        "AssuRance report",
        paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
        "",
        "INPUTS",
        describe_inputs(res$p, res$model),
        "",
        if (length(res$model$warnings))
          c("WARNINGS", strwrap(res$model$warnings, width = 78, prefix = "  ", initial = "  "), ""),
        "SUMMARY",
        strwrap(build_summary(res), width = 78, prefix = "  ", initial = "  "),
        "",
        "RESULTS",
        utils::capture.output(print(
          # round the probability columns for a readable text table
          rapply(display_table(), function(x) round(x, 3), how = "replace"),
          row.names = FALSE))
      )
      writeLines(lines, file)
    })

  # ---- Priors tab -------------------------------------------------------------
  output$prior_plot <- renderPlotly(plot_priors(results()$model))
  output$prior_text <- renderUI({
    res <- results(); m <- res$model; p <- res$p
    z <- qnorm(0.975)
    up <- 1 - pnorm(-m$m_d / m$s_d)
    items <- list(
      tags$li(paste0("Under your design prior there is a ", fmt_pct(up),
                     " chance that treatment increases ", m$labels$thing,
                     " and a ", fmt_pct(1 - up), " chance that it reduces it.")),
      tags$li(paste0("95% of the effects you consider plausible lie between ",
                     m$fmt_eff(m$m_d - z * m$s_d), " and ",
                     m$fmt_eff(m$m_d + z * m$s_d), " (", m$labels$short, ").")))
    if (p$otype == "binary") {
      items <- c(items, list(tags$li(paste0(
        "With a control-group event rate of ", fmt_pct(p$p_control, 0),
        ", the design prior's centre corresponds to a treatment-group rate of about ",
        fmt_pct(m$p_t(m$m_d)), "."))))
    }
    if (p$otype == "surv") {
      items <- c(items, list(tags$li(paste0(
        "About ", fmt_pct(m$pev_c, 0), " of control-group participants are ",
        "expected to have an event by the analysis (", fmt_num(p$accrual), " ",
        m$time_unit, " of recruitment plus ", fmt_num(p$followup), " of follow-up), ",
        "and about ", fmt_pct(m$pev(log(2) / p$surv_median * exp(m$m_d)), 0),
        " of treatment-group participants at the design prior's centre."))))
    }
    if (m$alt != "two.sided") {
      items <- c(items, list(tags$li(paste0(
        "The shaded area (", fmt_pct(res$metrics$ceiling), ") is the ",
        "probability that the true effect is beyond the success threshold. ",
        "This is also the assurance ceiling."))))
    }
    if (!p$same_prior) {
      items <- c(items, list(tags$li(paste0(
        "The analysis prior (", describe_prior(m, "analysis"), ") will be ",
        "combined with the trial data. ",
        if (m$s_a > 5 * m$s_d) "It is much wider than your design prior, so the data will dominate."
        else "It is informative, so it will pull results towards its centre."))))
    }
    tags$ul(class = "mt-3", items)
  })

  # ---- Conditional success tab ----------------------------------------------
  focus_n <- function(x) {
    validate(need(is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 2,
                  "Enter a sample size of at least 2."))
    as.integer(round(x))
  }
  output$cond_plot <- renderPlotly({
    res <- results()
    plot_conditional(res$model, focus_n(input$cond_n), res$p$ratio)
  })
  output$cond_text <- renderUI({
    res <- results(); m <- res$model
    n_c <- focus_n(input$cond_n); n_t <- treatment_n(n_c, res$p$ratio)
    a  <- model_exact(m, n_t, n_c)
    pw <- model_power(m, n_t, n_c)
    p(strong(paste0("At ", n_text(n_t, n_c), events_note(m, n_c, res$p$ratio),
                    ": exact assurance ", fmt_pct(a), ", frequentist power ",
                    fmt_pct(pw), ".")))
  })

  # ---- Sensitivity tab ---------------------------------------------------------
  output$sens_heat <- renderPlotly({
    res <- results()
    plot_sensitivity_heat(res$model, focus_n(input$sens_n), res$p$ratio,
                          res$p$same_prior,
                          len = if (res$model$const_var) 41 else 25)
  })
  output$sens_asd <- renderPlotly({
    res <- results()
    plot_sensitivity_analysis_sd(res$model, focus_n(input$sens_n), res$p$ratio)
  })
  output$sens_nuis_title <- renderText(paste0("Assurance across values of: ",
                                              results()$model$nuisance$label))
  output$sens_nuis <- renderPlotly({
    res <- results()
    plot_sensitivity_nuisance(res$p, res$model, focus_n(input$sens_n))
  })

  # ---- Scenario comparison -----------------------------------------------------
  scenarios <- reactiveVal(list())

  observeEvent(input$save_scenario, {
    res <- results()
    s <- scenarios()
    if (length(s) >= 8) {
      showNotification("You can save up to 8 scenarios. Remove one first.",
                       type = "warning")
      return()
    }
    label <- trimws(input$scenario_label)
    if (!nzchar(label)) label <- paste("Scenario", length(s) + 1)
    s[[length(s) + 1]] <- list(label = label, p = res$p, model = res$model,
                               table = res$table, metrics = res$metrics)
    scenarios(s)
    updateTextInput(session, "scenario_label", value = "")
    showNotification(paste0("Saved \"", label, "\". See the Compare scenarios tab."),
                     type = "message")
  })
  observeEvent(input$remove_last, {
    s <- scenarios(); if (length(s)) scenarios(s[-length(s)])
  })
  observeEvent(input$clear_scenarios, scenarios(list()))

  output$cmp_empty <- renderUI({
    if (length(scenarios()) == 0)
      div(class = "alert alert-info",
          "No scenarios saved yet. Run a calculation, then use \"Save scenario\"",
          "on the Results tab. Save a few with different assumptions (or even",
          "different outcome types) to compare them here.")
  })
  output$cmp_plot <- renderPlotly({
    req(length(scenarios()) > 0)
    plot_scenarios(scenarios(), input$cmp_power, input$cmp_total)
  })
  output$cmp_table <- renderTable({
    s <- scenarios(); req(length(s) > 0)
    do.call(rbind, lapply(s, function(x) {
      p <- x$p; m <- x$model; last <- x$table[nrow(x$table), ]
      data.frame(
        Scenario = x$label,
        Outcome = m$describe,
        "Design prior" = describe_prior(m, "design"),
        "Analysis prior" = if (p$same_prior) "same" else describe_prior(m, "analysis"),
        Test = describe_test(m),
        Ratio = p$ratio,
        "Largest n (T / C)" = paste0(last$n_t, " / ", last$n_c),
        "Assurance there" = fmt_pct(last$assurance),
        "Power there" = fmt_pct(last$power),
        "n for target assurance" = if (is.na(x$metrics$n_assur)) "not reachable"
                                   else paste0(x$metrics$n_assur, " (", fmt_pct(p$target, 0), ")"),
        Ceiling = fmt_pct(x$metrics$ceiling),
        check.names = FALSE)
    }))
  }, striped = TRUE, spacing = "s")

  # ---- LLM prompt helper --------------------------------------------------
  output$pg_type_note <- renderUI({
    div(class = "alert alert-info py-2 small",
        "The prompt is written for the outcome type selected in the sidebar: ",
        strong(outcome_type_label(input$otype, input$measure)),
        ". Change it there first if needed.")
  })

  # Current app inputs, renamed to the JSON field names used in the prompt.
  current_for_llm <- function() {
    r <- raw_inputs()
    keys <- llm_keys_for(input$otype, input$measure)
    v <- r[intersect(keys, names(r))]
    if ("dropout_percent" %in% keys) v$dropout_percent <- r$dropout
    v[vapply(v, function(x) length(x) == 1 && !is.na(x), logical(1))]
  }

  llm_prompt <- reactive({
    info <- list(
      title = input$pg_title, condition = input$pg_condition,
      intervention = input$pg_intervention, comparator = input$pg_comparator,
      outcome = input$pg_outcome, timepoint = input$pg_timepoint,
      phase = input$pg_phase, direction = input$pg_direction,
      setting = input$pg_setting, mcid = input$pg_mcid,
      known = input$pg_known, constraints = input$pg_constraints,
      web = input$pg_web, language = input$pg_language)
    build_llm_prompt(info, if (isTRUE(input$pg_current)) current_for_llm(),
                     otype = input$otype, measure = input$measure)
  }) |> debounce(400)

  output$pg_prompt <- renderText(llm_prompt())
  output$dl_prompt <- downloadHandler(
    filename = function() paste0("assurance-llm-prompt-", stamp(), ".txt"),
    content = function(file) writeLines(llm_prompt(), file))

  observeEvent(input$pg_apply, {
    parsed <- parse_llm_values(input$pg_answer)
    v <- parsed$values
    if (!is.null(v$otype) && v$otype != input$otype) st$skip_n_defaults <- TRUE
    for (k in names(v)) {
      val <- v[[k]]
      switch(k,
        otype           = updateSelectInput(session, "otype", selected = val),
        measure         = updateRadioButtons(session, "measure", selected = val),
        time_unit       = updateTextInput(session, "time_unit", value = val),
        same_prior      = updateCheckboxInput(session, "same_prior", value = val),
        alt             = updateRadioButtons(session, "alt", selected = val),
        target          = updateSliderInput(session, "target", value = val),
        dropout_percent = updateNumericInput(session, "dropout", value = val),
        updateNumericInput(session, k, value = val))
    }
    output$pg_apply_result <- renderUI({
      tagList(
        if (length(v)) {
          tagList(
            div(class = "alert alert-success py-2 mt-3 small",
                paste0("Applied ", length(v), " value(s). Check them in the sidebar, ",
                       "then press Calculate.")),
            tags$table(class = "table table-sm small",
              tags$thead(tags$tr(tags$th("Input"), tags$th("Value"))),
              tags$tbody(lapply(names(v), function(k)
                tags$tr(tags$td(LLM_FIELDS[[k]]$label), tags$td(format(v[[k]]))))))
          )
        },
        if (length(parsed$messages))
          div(class = "alert alert-warning py-2 mt-3 small",
              tags$ul(class = "mb-0", lapply(parsed$messages, tags$li)))
      )
    })
    if (length(v)) showNotification("Values applied. Press Calculate to update the results.",
                                    type = "message")
  })
}

shinyApp(ui, server, enableBookmarking = "url")
