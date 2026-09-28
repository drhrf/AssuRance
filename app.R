# =============================================================================
# AssuRance: Bayesian assurance vs. frequentist power for two-arm trials with
# continuous, baseline-adjusted (ANCOVA), binary or time-to-event outcomes.
# Interface in English and Brazilian Portuguese.
#
# Run locally:   shiny::runApp()        (from this directory)
#
# Files (Shiny sources everything in R/ automatically)
#   app.R               server logic (this file)
#   R/calculations.R    outcome-agnostic engine; documents the mapping of the
#                       inputs to bayesassurance's parameters
#   R/models.R          one "model" per outcome type (variances, simulators,
#                       power, labels)
#   R/i18n.R            English / Portuguese helpers and the language toggle
#   R/ui_sidebar.R      sidebar inputs
#   R/ui_tabs.R         main panel and tabs
#   R/summary_text.R    plain-language summary
#   R/plots.R           plot builders
#   R/detectability.R   detection if real and Shannon information
#   R/complex_design.R  rough extrapolation to complex designs
#   R/prompt_generator.R  LLM prompt helper
#   R/methods_ui.R      "Methods & help" tab
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(DT)
library(bayesassurance)

# Portuguese text needs a UTF-8 locale; some servers start R in the plain
# "C" locale, which would garble accented characters.
if (!isTRUE(l10n_info()$`UTF-8`)) {
  for (loc in c("C.UTF-8", "en_US.UTF-8", "pt_BR.UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
  }
}


# ============================================================================
# UI
# ============================================================================
ui <- function(request) {
  page_sidebar(
    title = h1(class = "bslib-page-title d-flex align-items-center gap-3",
               tags$span("AssuRance: ", tt("Bayesian assurance vs. frequentist power",
                                           "assurance bayesiana vs. poder frequentista")),
               lang_toggle_button()),
    window_title = "AssuRance",
    theme = bs_theme(version = 5, primary = "#1f5fa8"),
    fillable = FALSE,
    sidebar = sidebar_ui(),
    lang_head(),
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
    "go", "stop", "save_scenario", "remove_last", "clear_scenarios", "scenario_label", "lang",
    # LLM prompt helper: free text would bloat the bookmark URL
    "pg_title", "pg_condition", "pg_intervention", "pg_comparator",
    "pg_outcome", "pg_timepoint", "pg_phase", "pg_direction", "pg_setting",
    "pg_mcid", "pg_known", "pg_constraints", "pg_web", "pg_current",
    "pg_language", "pg_answer", "pg_apply", "cx_wf_crit",
    paste0("results_table_", c("rows_current", "rows_all", "rows_selected",
                               "search", "state", "cell_clicked",
                               "cells_selected", "columns_selected"))))

  # ---- Language ------------------------------------------------------------
  # input$lang is set by the toggle in the page header (see R/i18n.R). Every
  # server-generated text is produced inside W(), which makes L(), fmt_pct()
  # and friends use the current language.
  lang <- reactive(if (identical(input$lang, "pt")) "pt" else "en")
  W <- function(expr) with_lang(lang(), expr)

  # Default the AI assistant's answer language to the app's language.
  observeEvent(lang(), {
    if (isTRUE(input$pg_language %in% c("English", "Portuguese (Brazil)"))) {
      updateSelectInput(session, "pg_language",
                        selected = if (lang() == "pt") "Portuguese (Brazil)" else "English")
    }
    # the default time unit follows the language too (other values are the user's own)
    if (isTRUE(input$time_unit %in% c("months", "meses"))) {
      updateTextInput(session, "time_unit", value = if (lang() == "pt") "meses" else "months")
    }
  }, ignoreInit = TRUE)

  raw_inputs <- reactive(lapply(setNames(MODEL_INPUTS, MODEL_INPUTS),
                                function(id) input[[id]]))

  st <- new.env()          # non-reactive state
  st$job <- NULL           # the running simulation
  st$prev_otype <- NULL
  st$skip_n_defaults <- FALSE

  # ---- Collect and validate inputs --------------------------------------
  # Returns list(p = validated inputs, m = model from R/models.R).
  config <- reactive(W({
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
      need(r$otype %in% c("cont", "ancova", "binary", "surv"),
           L("Choose an outcome type.", "Escolha um tipo de desfecho.")),
      # outcome-specific
      need(!(r$otype %in% c("cont", "ancova")) || pos(r$sigma),
           L("Outcome standard deviation must be a positive number.",
             "O desvio padr\u00E3o do desfecho deve ser um n\u00FAmero positivo.")),
      need(r$otype != "ancova" || (is_num(r$rho) && r$rho >= 0 && r$rho <= 0.95),
           L("The baseline-outcome correlation must be between 0 and 0.95.",
             "A correla\u00E7\u00E3o basal-desfecho deve estar entre 0 e 0,95.")),
      need(r$otype != "binary" || (is_num(r$p_control) && r$p_control > 0 && r$p_control < 100),
           L("The control-group event rate must be between 0 and 100%.",
             "A taxa de eventos no grupo controle deve estar entre 0 e 100%.")),
      need(r$otype != "surv" || pos(r$surv_median),
           L("The control-group median time must be a positive number.",
             "A mediana de tempo no grupo controle deve ser um n\u00FAmero positivo.")),
      need(r$otype != "surv" || (is_num(r$accrual) && r$accrual >= 0 &&
                                 is_num(r$followup) && r$followup >= 0 &&
                                 r$accrual + r$followup > 0),
           L("Recruitment and follow-up must be zero or positive, and not both zero.",
             "Recrutamento e seguimento devem ser zero ou positivos, e n\u00E3o ambos zero.")),
      need(r$otype != "surv" || (is_num(r$loss_pct) && r$loss_pct >= 0 && r$loss_pct < 90),
           L("Loss to follow-up must be between 0 and 90%.",
             "A perda de seguimento deve estar entre 0 e 90%.")),
      # priors
      need(fam != "additive" || is_num(r$design_mean),
           L("Expected effect (design prior mean) must be a number.",
             "O efeito esperado (m\u00E9dia da priori de planejamento) deve ser um n\u00FAmero.")),
      need(fam != "additive" || pos(r$design_sd),
           L("Design prior SD must be a positive number.",
             "O DP da priori de planejamento deve ser um n\u00FAmero positivo.")),
      need(fam != "additive" || same || is_num(r$analysis_mean),
           L("Analysis prior mean must be a number.", "A m\u00E9dia da priori de an\u00E1lise deve ser um n\u00FAmero.")),
      need(fam != "additive" || same || pos(r$analysis_sd),
           L("Analysis prior SD must be a positive number.", "O DP da priori de an\u00E1lise deve ser um n\u00FAmero positivo.")),
      need(fam != "rd" || (is_num(r$rd_design_mean) && abs(r$rd_design_mean) < 100),
           L("Expected risk difference must be between -100 and 100 percentage points.",
             "A diferen\u00E7a de riscos esperada deve estar entre -100 e 100 pontos percentuais.")),
      need(fam != "rd" || pos(r$rd_design_sd),
           L("Risk difference SD must be a positive number.",
             "O DP da diferen\u00E7a de riscos deve ser um n\u00FAmero positivo.")),
      need(fam != "rd" || same || is_num(r$rd_analysis_mean),
           L("Analysis prior mean must be a number.", "A m\u00E9dia da priori de an\u00E1lise deve ser um n\u00FAmero.")),
      need(fam != "rd" || same || pos(r$rd_analysis_sd),
           L("Analysis prior SD must be a positive number.", "O DP da priori de an\u00E1lise deve ser um n\u00FAmero positivo.")),
      need(fam != "ratio" || (pos(r$ratio_design_lo) && is_num(r$ratio_design_hi) &&
                              r$ratio_design_hi > r$ratio_design_lo),
           L("The plausible range for the ratio needs 0 < From < To.",
             "O intervalo plaus\u00EDvel da raz\u00E3o precisa de 0 < De < At\u00E9.")),
      need(fam != "ratio" || same || (pos(r$ratio_analysis_lo) && is_num(r$ratio_analysis_hi) &&
                                      r$ratio_analysis_hi > r$ratio_analysis_lo),
           L("The analysis prior range for the ratio needs 0 < From < To.",
             "O intervalo da priori de an\u00E1lise para a raz\u00E3o precisa de 0 < De < At\u00E9.")),
      # success criterion
      need(!one_sided || fam != "additive" || is_num(r$threshold),
           L("The success threshold must be a number.", "O limiar de sucesso deve ser um n\u00FAmero.")),
      need(!one_sided || fam != "rd" || (is_num(r$rd_threshold) && abs(r$rd_threshold) < 100),
           L("The success threshold must be between -100 and 100 percentage points.",
             "O limiar de sucesso deve estar entre -100 e 100 pontos percentuais.")),
      need(!one_sided || fam != "ratio" || pos(r$ratio_threshold),
           L("The success threshold for a ratio must be positive (1 = no effect).",
             "O limiar de sucesso para uma raz\u00E3o deve ser positivo (1 = sem efeito).")),
      need(is_num(r$alpha) && r$alpha > 0 && r$alpha < 1,
           L("Significance threshold must be between 0 and 1.",
             "O n\u00EDvel de signific\u00E2ncia deve estar entre 0 e 1.")),
      # design
      need(is_whole(r$n_min) && r$n_min >= 2,
           L("Minimum sample size must be a whole number of at least 2.",
             "O tamanho amostral m\u00EDnimo deve ser um n\u00FAmero inteiro de pelo menos 2.")),
      need(is_whole(r$n_max),
           L("Maximum sample size must be a whole number.", "O tamanho amostral m\u00E1ximo deve ser um n\u00FAmero inteiro.")),
      need(is_whole(r$n_step) && r$n_step >= 1,
           L("Step must be a whole number of at least 1.", "O passo deve ser um n\u00FAmero inteiro de pelo menos 1.")),
      need(is_num(r$n_min) && is_num(r$n_max) && r$n_max > r$n_min,
           L("Maximum sample size must exceed the minimum.",
             "O tamanho amostral m\u00E1ximo deve ser maior que o m\u00EDnimo.")),
      need(!is_num(r$n_max) || r$n_max <= 20000,
           L("Maximum sample size is limited to 20,000 per group.",
             "O tamanho amostral m\u00E1ximo \u00E9 limitado a 20.000 por grupo.")),
      need(is_num(r$ratio) && r$ratio >= 0.1 && r$ratio <= 10,
           L("Allocation ratio must be between 0.1 and 10.",
             "A raz\u00E3o de aloca\u00E7\u00E3o deve estar entre 0,1 e 10.")),
      need(r$otype == "surv" || (is_num(r$dropout) && r$dropout >= 0 && r$dropout < 90),
           L("Dropout must be between 0 and 90%.", "O abandono deve estar entre 0 e 90%.")),
      # computation
      need(!sim || (is_whole(r$mc_iter) && r$mc_iter >= 100),
           L("Number of simulated trials must be a whole number of at least 100.",
             "O n\u00FAmero de ensaios simulados deve ser um n\u00FAmero inteiro de pelo menos 100.")),
      need(!sim || is_num(r$seed), L("Random seed must be a number.", "A semente aleat\u00F3ria deve ser um n\u00FAmero."))
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
      L("That range gives more than 200 sample sizes; please use a larger step.",
        "Essa faixa gera mais de 200 tamanhos amostrais; use um passo maior.")))
    p$n_c <- as.integer(n_c)
    p$n_t <- as.integer(treatment_n(n_c, r$ratio))

    m <- build_model(p)
    if (sim) {
      est <- p$mc_iter * sum(m$per_iter(p$n_t, p$n_c))
      validate(need(est <= 900, L(
        paste0("This simulation would take roughly ", round(est / 60),
               " minutes. Reduce the maximum sample size, the number of sample ",
               "sizes, or the simulations per sample size, or use the exact engine."),
        paste0("Esta simula\u00E7\u00E3o levaria cerca de ", round(est / 60),
               " minutos. Reduza o tamanho amostral m\u00E1ximo, o n\u00FAmero de tamanhos ",
               "amostrais ou as simula\u00E7\u00F5es por tamanho, ou use o c\u00E1lculo exato."))))
    }
    list(p = p, m = m)
  }))

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

  output$engine_note <- renderUI(W({
    m <- live_model(); req(m)
    tags$div(class = "form-text", paste0(
      L("Simulation engine: ", "Motor de simula\u00E7\u00E3o: "), tx(m$engine_label),
      L(". Frequentist comparison: ", ". Compara\u00E7\u00E3o frequentista: "), tx(m$power_label), "."))
  }))

  output$effect_hint <- renderUI(W({
    m <- live_model(); req(m)
    z <- qnorm(0.975)
    lo <- m$m_d - z * m$s_d; hi <- m$m_d + z * m$s_d
    short <- tx(m$labels$short)
    txt <- switch(m$family,
      additive = L(paste0("Effect = ", short, " (treatment minus control). Your design prior's 95% range: ",
                          fmt_num(signif(lo, 3)), " to ", fmt_num(signif(hi, 3)), "."),
                   paste0("Efeito = ", short, " (tratamento menos controle). Intervalo de 95% da sua priori ",
                          "de planejamento: ", fmt_num(signif(lo, 3)), " a ", fmt_num(signif(hi, 3)), ".")),
      rd = L(paste0("Effect = treatment minus control event rate (percentage points; negative = fewer events ",
                    "with treatment). Implied treatment-group event rate: about ", fmt_pct(m$p_t(m$m_d), 0),
                    " (95% range ", fmt_pct(m$p_t(lo), 0), " to ", fmt_pct(m$p_t(hi), 0), ")."),
             paste0("Efeito = taxa de eventos no tratamento menos no controle (pontos percentuais; negativo = ",
                    "menos eventos com o tratamento). Taxa impl\u00EDcita no grupo tratamento: cerca de ",
                    fmt_pct(m$p_t(m$m_d), 0), " (intervalo de 95%: ", fmt_pct(m$p_t(lo), 0), " a ",
                    fmt_pct(m$p_t(hi), 0), ").")),
      ratio = L(
        paste0(if (grepl("^[aeiou]", short)) "An " else "A ", short, " below 1 means ",
               if (m$otype == "surv") "a lower event rate (longer time to event)" else "fewer events",
               " with treatment. Your range implies a best guess of ", m$fmt_eff(m$m_d),
               " (SD of the log ", short, " ", fmt_num(signif(m$s_d, 2)), ") and a ",
               fmt_pct(pnorm(-m$m_d / m$s_d), 0), " chance that it is below 1.",
               if (m$otype == "binary") paste0(" Implied treatment-group event rate: about ",
                                               fmt_pct(m$p_t(m$m_d), 0), ".") else ""),
        paste0("Um valor de ", short, " abaixo de 1 significa ",
               if (m$otype == "surv") "uma taxa de eventos menor (mais tempo at\u00E9 o evento)" else "menos eventos",
               " com o tratamento. O seu intervalo implica um melhor palpite de ", m$fmt_eff(m$m_d),
               " (DP do log: ", fmt_num(signif(m$s_d, 2)), ") e ", fmt_pct(pnorm(-m$m_d / m$s_d), 0),
               " de chance de ficar abaixo de 1.",
               if (m$otype == "binary") paste0(" Taxa impl\u00EDcita no grupo tratamento: cerca de ",
                                               fmt_pct(m$p_t(m$m_d), 0), ".") else "")))
    tagList(
      div(class = "alert alert-light border py-2 px-2 small", txt),
      if (isTRUE(m$prior_only_success))
        div(class = "alert alert-danger py-2 px-2 small",
            L("Warning: with these priors the analysis would declare success even without data. See the Results tab, or untick 'Analyse with the same prior' and use a sceptical analysis prior.",
              "Aten\u00E7\u00E3o: com estas prioris a an\u00E1lise declararia sucesso mesmo sem dados. Veja a aba Resultados, ou desmarque 'Analisar com a mesma priori' e use uma priori de an\u00E1lise c\u00E9tica.")))
  }))

  output$threshold_hint <- renderUI(W({
    m <- live_model(); req(m, m$alt != "two.sided")
    kind <- if (abs(m$C) < 1e-12) L("ordinary superiority", "superioridade comum")
            else if ((m$alt == "greater" && m$C < 0) || (m$alt == "less" && m$C > 0))
              L("a non-inferiority design", "um desenho de n\u00E3o inferioridade")
            else L("superiority by a clinically meaningful margin", "superioridade por uma margem clinicamente relevante")
    short <- tx(m$labels$short)
    tags$div(class = "form-text mb-2", L(
      paste0("Success = showing the ", short, " is ", if (m$alt == "greater") "above " else "below ",
             m$fmt_eff(m$C), " (", kind, ")."),
      paste0("Sucesso = mostrar que o valor de ", short, " est\u00E1 ",
             if (m$alt == "greater") "acima de " else "abaixo de ", m$fmt_eff(m$C), " (", kind, ").")))
  }))

  output$time_estimate <- renderUI(W({
    cfg <- tryCatch(config(), error = function(e) NULL)
    if (is.null(cfg)) return(NULL)
    p <- cfg$p
    txt <- if (p$engine == "sim") {
      s <- p$mc_iter * sum(cfg$m$per_iter(p$n_t, p$n_c))
      L(paste0("Estimated run time: ~", format_secs(s), " (", length(p$n_c),
               " sample sizes). Hosted servers may be slower."),
        paste0("Tempo estimado: ~", format_secs(s), " (", length(p$n_c),
               " tamanhos amostrais). Servidores na nuvem podem ser mais lentos."))
    } else {
      L(paste0(length(p$n_c), " sample sizes; exact results are instant."),
        paste0(length(p$n_c), " tamanhos amostrais; resultados exatos s\u00E3o instant\u00E2neos."))
    }
    tags$div(class = "form-text", txt)
  }))

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
  progress   <- reactiveVal(list(frac = 0, i = NA, k = NA, left = NA))

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
    det <- detectability(m, p$n_t, p$n_c)
    metrics <- list(
      n_detect = smallest_n(function(nt, nc) detect_rate(m, nt, nc), p$ratio, p$target),
      n_assur = smallest_n(function(nt, nc) model_exact(m, nt, nc), p$ratio, p$target),
      n_power = smallest_n(function(nt, nc) model_power(m, nt, nc), p$ratio, p$target),
      ceiling = model_ceiling(m),
      # two-sided: share of success in the direction opposite to the expected one
      neg_share = if (m$m_d >= 0) parts$negative else parts$positive
    )

    results_rv(list(
      p = p, model = m, inputs = inputs_snapshot, metrics = metrics,
      seconds = seconds, det = det,
      table = data.frame(n_t = p$n_t, n_c = p$n_c, assurance = assur,
                         mc_se = se, exact = exact, power = power,
                         events = events, detect = det$detect)))
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
    progress(list(frac = 0, i = NA, k = length(p$n_c), left = NA))
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
      W(showNotification(paste(L("The simulation failed:", "A simula\u00E7\u00E3o falhou:"),
                               conditionMessage(step)), type = "error", duration = NULL))
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
      progress(list(frac = frac, i = j$plan$i[j$next_chunk], k = length(p$n_c),
                    left = if (frac > 0.02) elapsed / frac * (1 - frac) else NA))
      invalidateLater(10)
    }
  })

  observeEvent(input$stop, {
    if (!is.null(st$job)) {
      st$job <- NULL
      running(FALSE)
      W(showNotification(paste0(
        L("Run stopped. ", "Execu\u00E7\u00E3o interrompida. "),
        if (is.null(results_rv())) L("Press Calculate to start again.", "Aperte Calcular para come\u00E7ar de novo.")
        else L("The results shown are from your previous run.", "Os resultados exibidos s\u00E3o da execu\u00E7\u00E3o anterior.")),
        type = "warning"))
    }
  })

  output$running <- reactive(running())
  outputOptions(output, "running", suspendWhenHidden = FALSE)
  output$run_progress <- renderUI(W({
    pr <- progress()
    pct <- round(100 * pr$frac)
    detail <- if (is.na(pr$i)) L("Starting...", "Iniciando...") else paste0(
      L("Sample size ", "Tamanho amostral "), pr$i, L(" of ", " de "), pr$k,
      if (!is.na(pr$left)) paste0(" \u00B7 ", L("about ", "cerca de "), format_secs(pr$left),
                                  L(" left", " restantes")) else "")
    tagList(
      div(class = "progress my-2", style = "height: 18px;",
          div(class = "progress-bar progress-bar-striped progress-bar-animated",
              role = "progressbar", style = paste0("width: ", max(pct, 2), "%;"),
              "")),
      tags$small(class = "text-muted", paste0(pct, L("% done \u00B7 ", "% conclu\u00EDdo \u00B7 "), detail)))
  }))

  # Keep the per-tab sample-size selectors in step with the latest run.
  observeEvent(results(), {
    n <- max(results()$p$n_c)
    updateNumericInput(session, "cond_n", value = n)
    updateNumericInput(session, "sens_n", value = n)
    updateNumericInput(session, "det_n", value = n)
  })

  output$stale <- renderUI(W({
    res <- tryCatch(results(), error = function(e) NULL)
    if (running() || is.null(res)) return(NULL)
    now <- tryCatch(relevant_inputs(raw_inputs()), error = function(e) NULL)
    if (identical(now, res$inputs)) return(NULL)
    div(class = "alert alert-warning py-2 px-3 my-2 small",
        L("Inputs have changed. Press Calculate to update the results.",
          "As entradas mudaram. Aperte Calcular para atualizar os resultados."))
  }))

  # ---- Value boxes --------------------------------------------------------
  last_row <- reactive({ t <- results()$table; t[nrow(t), ] })
  n_text <- function(nt, nc) {
    if (nt == nc) paste0("n = ", fmt_num(nc), L(" per group", " por grupo"))
    else paste0("n = ", fmt_num(nt), " / ", fmt_num(nc), " (T / C)")
  }
  events_note <- function(m, n_c, ratio) {
    if (is.null(m$events) || is.na(n_c)) return("")
    paste0(" (~", fmt_num(round(m$events(m$m_d, treatment_n(n_c, ratio), n_c))), L(" events)", " eventos)"))
  }

  output$vb_assur_title <- renderText(W(paste0(L("Bayesian assurance at ", "Assurance bayesiana com "),
    n_text(last_row()$n_t, last_row()$n_c))))
  output$vb_assur <- renderText(W(fmt_pct(last_row()$assurance)))
  output$vb_power_title <- renderText(W(paste0(L("Frequentist power at ", "Poder frequentista com "),
    n_text(last_row()$n_t, last_row()$n_c))))
  output$vb_power <- renderText(W(fmt_pct(last_row()$power)))
  output$vb_n_title <- renderText(W(paste0(
    L("n for ", "n para "), fmt_pct(results()$p$target, 0), L(" assurance", " de assurance"),
    if (results()$p$ratio == 1) L(" (per group)", " (por grupo)") else L(" (control)", " (controle)"))))
  output$vb_n <- renderText(W({
    n <- results()$metrics$n_assur
    if (is.na(n)) L("Not reachable", "Inating\u00EDvel") else fmt_num(n)
  }))
  output$vb_n_sub <- renderText(W({
    res <- results(); mt <- res$metrics
    ev <- if (!is.na(mt$n_assur)) trimws(events_note(res$model, mt$n_assur, res$p$ratio)) else ""
    paste0(if (nzchar(ev)) paste0(gsub("[()]", "", ev), "; ") else "",
           L("For ", "Para "), fmt_pct(res$p$target, 0), L(" power: ", " de poder: "),
           if (is.na(mt$n_power)) L("not reachable", "inating\u00EDvel")
           else paste0(fmt_num(mt$n_power), events_note(res$model, mt$n_power, res$p$ratio)))
  }))
  output$vb_ceiling <- renderText(W(fmt_pct(results()$metrics$ceiling)))

  output$model_warnings <- renderUI(W({
    w <- results()$model$warnings
    if (length(w)) div(class = "alert alert-warning py-2 small", lapply(w, function(x) tags$div(tx(x))))
  }))

  # ---- Main plot, summary, table -------------------------------------------
  output$curve <- renderPlotly(W(plot_curve_plotly(results())))
  output$curve_note <- renderText(W({
    res <- results(); m <- res$model
    if (res$p$engine == "sim") {
      L(paste0("Assurance simulated with ", tx(m$engine_label), " (",
               fmt_num(res$p$mc_iter), " trials per sample size, seed ",
               res$p$seed, ", ", fmt_dec(res$seconds, 1), " s). Shaded band: 95% ",
               "Monte Carlo interval. Power: ", tx(m$power_label), "."),
        paste0("Assurance simulada com ", tx(m$engine_label), " (",
               fmt_num(res$p$mc_iter), " ensaios por tamanho amostral, semente ",
               res$p$seed, ", ", fmt_dec(res$seconds, 1), " s). Faixa sombreada: intervalo de ",
               "Monte Carlo de 95%. Poder: ", tx(m$power_label), "."))
    } else {
      L(paste0("Assurance from the exact formula", if (!m$const_var)
          " (numerical integration over the design prior)" else "",
          ". Power: ", tx(m$power_label), "."),
        paste0("Assurance pela f\u00F3rmula exata", if (!m$const_var)
          " (integra\u00E7\u00E3o num\u00E9rica sobre a priori de planejamento)" else "",
          ". Poder: ", tx(m$power_label), "."))
    }
  }))

  output$summary <- renderUI(W({
    tagList(lapply(build_summary(results()), tags$p))
  }))

  # Column names of the results table in the current language.
  col_names <- function() list(
    per_group = L("n per group", "n por grupo"), n_t = L("n treatment", "n tratamento"),
    n_c = L("n control", "n controle"), total = L("Total n", "n total"),
    enrol = L("Total to enrol", "Total a recrutar"), events = L("Expected events", "Eventos esperados"),
    assur = L("Bayesian assurance", "Assurance bayesiana"), mc = L("MC error (95%)", "Erro de MC (95%)"),
    exact = L("Exact assurance", "Assurance exata"), power = L("Frequentist power", "Poder frequentista"),
    detect = L("Detection if real", "Detec\u00e7\u00e3o se real"))

  display_table <- function() {
    res <- results(); t <- res$table; p <- res$p; cn <- col_names()
    d <- if (p$ratio == 1) {
      setNames(data.frame(t$n_c), cn$per_group)
    } else {
      setNames(data.frame(t$n_t, t$n_c), c(cn$n_t, cn$n_c))
    }
    d[[cn$total]] <- t$n_t + t$n_c
    if (p$dropout > 0) {
      d[[cn$enrol]] <- enrolled_n(t$n_t, p$dropout) + enrolled_n(t$n_c, p$dropout)
    }
    if (!all(is.na(t$events))) d[[cn$events]] <- round(t$events)
    d[[cn$assur]] <- t$assurance
    if (p$engine == "sim") {
      d[[cn$mc]] <- 1.96 * t$mc_se
      d[[cn$exact]] <- t$exact
    }
    d[[cn$power]] <- t$power
    d[[cn$detect]] <- t$detect
    d
  }

  output$results_table <- renderDT(W({
    d <- display_table(); cn <- col_names()
    pt <- current_lang() == "pt"
    pct_cols <- intersect(names(d), c(cn$assur, cn$mc, cn$exact, cn$power, cn$detect))
    datatable(d, rownames = FALSE, class = "compact stripe hover",
              options = list(dom = "t", paging = FALSE, scrollY = "360px",
                             scrollCollapse = TRUE,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all")))) |>
      formatPercentage(pct_cols, digits = 1, dec.mark = if (pt) "," else ".") |>
      formatRound(setdiff(names(d), pct_cols), digits = 0, mark = if (pt) "." else ",")
  }))

  # ---- Downloads -------------------------------------------------------------
  stamp <- function() format(Sys.time(), "%Y%m%d-%H%M")

  output$dl_csv <- downloadHandler(
    filename = function() paste0("assurance-results-", stamp(), ".csv"),
    content = function(file) {
      # machine-readable: English column names and decimal points
      t <- results()$table
      names(t) <- c("n_treatment", "n_control", "assurance", "assurance_mc_se",
                    "assurance_exact", "frequentist_power", "expected_events",
                    "detection_if_real")
      utils::write.csv(t, file, row.names = FALSE)
    })

  output$dl_png <- downloadHandler(
    filename = function() paste0("assurance-curve-", stamp(), ".png"),
    content = function(file) W({
      ggsave(file, plot_curve_gg(results()), width = 8, height = 5, dpi = 150,
             bg = "white")
    }))

  output$dl_report <- downloadHandler(
    filename = function() paste0("assurance-report-", stamp(), ".txt"),
    content = function(file) W({
      res <- results()
      tab <- display_table()
      for (k in names(tab)) if (is.numeric(tab[[k]])) tab[[k]] <- round(tab[[k]], 3)
      lines <- c(
        L("AssuRance report", "Relat\u00F3rio do AssuRance"),
        paste(L("Generated:", "Gerado em:"), format(Sys.time(), "%Y-%m-%d %H:%M")),
        "",
        L("INPUTS", "ENTRADAS"),
        describe_inputs(res$p, res$model),
        "",
        if (length(res$model$warnings))
          c(L("WARNINGS", "ALERTAS"),
            strwrap(vapply(res$model$warnings, tx, ""), width = 78, prefix = "  ", initial = "  "), ""),
        L("SUMMARY", "RESUMO"),
        strwrap(build_summary(res), width = 78, prefix = "  ", initial = "  "),
        "",
        L("DETECTABILITY (largest sample size)", "DETECTABILIDADE (maior tamanho amostral)"),
        describe_detectability(res$det[nrow(res$det), ], res$model),
        "",
        L("RESULTS", "RESULTADOS"),
        utils::capture.output(print(tab, row.names = FALSE)),
        if (isTRUE(input$cx_on)) {
          r <- tryCatch(cx_result(), error = function(e) NULL)
          if (!is.null(r))
            c("", L("COMPLEX DESIGN (rough extrapolation)", "DESENHO COMPLEXO (extrapola\u00E7\u00E3o grosseira)"),
              cx_report_lines(r, res$p, cx_base(res)))
        }
      )
      writeLines(enc2utf8(lines), file, useBytes = TRUE)
    }))

  # ---- Priors tab -------------------------------------------------------------
  output$prior_plot <- renderPlotly(W(plot_priors(results()$model)))
  output$prior_text <- renderUI(W({
    res <- results(); m <- res$model; p <- res$p
    z <- qnorm(0.975)
    up <- 1 - pnorm(-m$m_d / m$s_d)
    thing <- tx(m$labels$thing); short <- tx(m$labels$short)
    items <- list(
      tags$li(L(paste0("Under your design prior there is a ", fmt_pct(up),
                       " chance that treatment increases ", thing,
                       " and a ", fmt_pct(1 - up), " chance that it reduces it."),
                paste0("Segundo a sua priori de planejamento, h\u00E1 ", fmt_pct(up),
                       " de chance de o tratamento aumentar ", thing,
                       " e ", fmt_pct(1 - up), " de chance de reduzir."))),
      tags$li(L(paste0("95% of the effects you consider plausible lie between ",
                       m$fmt_eff(m$m_d - z * m$s_d), " and ", m$fmt_eff(m$m_d + z * m$s_d), " (", short, ")."),
                paste0("95% dos efeitos que voc\u00EA considera plaus\u00EDveis ficam entre ",
                       m$fmt_eff(m$m_d - z * m$s_d), " e ", m$fmt_eff(m$m_d + z * m$s_d), " (", short, ")."))))
    if (p$otype == "binary") {
      items <- c(items, list(tags$li(L(
        paste0("With a control-group event rate of ", fmt_pct(p$p_control, 0),
               ", the design prior's centre corresponds to a treatment-group rate of about ",
               fmt_pct(m$p_t(m$m_d)), "."),
        paste0("Com uma taxa de eventos no controle de ", fmt_pct(p$p_control, 0),
               ", o centro da priori de planejamento corresponde a uma taxa no tratamento de cerca de ",
               fmt_pct(m$p_t(m$m_d)), ".")))))
    }
    if (p$otype == "surv") {
      pt_t <- fmt_pct(m$pev(log(2) / p$surv_median * exp(m$m_d)), 0)
      items <- c(items, list(tags$li(L(
        paste0("About ", fmt_pct(m$pev_c, 0), " of control-group participants are ",
               "expected to have an event by the analysis (", fmt_num(p$accrual), " ",
               m$time_unit, " of recruitment plus ", fmt_num(p$followup), " of follow-up), ",
               "and about ", pt_t, " of treatment-group participants at the design prior's centre."),
        paste0("Espera-se que cerca de ", fmt_pct(m$pev_c, 0), " dos participantes do controle ",
               "tenham um evento at\u00E9 a an\u00E1lise (", fmt_num(p$accrual), " ", m$time_unit,
               " de recrutamento mais ", fmt_num(p$followup), " de seguimento), e cerca de ", pt_t,
               " dos participantes do tratamento no centro da priori de planejamento.")))))
    }
    if (m$alt != "two.sided") {
      items <- c(items, list(tags$li(L(
        paste0("The shaded area (", fmt_pct(res$metrics$ceiling), ") is the probability that the true ",
               "effect is beyond the success threshold. This is also the assurance ceiling."),
        paste0("A \u00E1rea sombreada (", fmt_pct(res$metrics$ceiling), ") \u00E9 a probabilidade de o efeito ",
               "verdadeiro estar al\u00E9m do limiar de sucesso. Esse tamb\u00E9m \u00E9 o teto da assurance.")))))
    }
    if (!p$same_prior) {
      wide <- m$s_a > 5 * m$s_d
      items <- c(items, list(tags$li(L(
        paste0("The analysis prior (", describe_prior(m, "analysis"), ") will be combined with the trial data. ",
               if (wide) "It is much wider than your design prior, so the data will dominate."
               else "It is informative, so it will pull results towards its centre."),
        paste0("A priori de an\u00E1lise (", describe_prior(m, "analysis"), ") ser\u00E1 combinada com os dados do ensaio. ",
               if (wide) "Ela \u00E9 bem mais larga que a sua priori de planejamento, ent\u00E3o os dados v\u00E3o predominar."
               else "Ela \u00E9 informativa, ent\u00E3o vai puxar os resultados para o seu centro.")))))
    }
    tags$ul(class = "mt-3", items)
  }))

  # ---- Conditional success tab ----------------------------------------------
  focus_n <- function(x) {
    validate(need(is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 2,
                  L("Enter a sample size of at least 2.", "Informe um tamanho amostral de pelo menos 2.")))
    as.integer(round(x))
  }
  output$cond_plot <- renderPlotly(W({
    res <- results()
    plot_conditional(res$model, focus_n(input$cond_n), res$p$ratio)
  }))
  output$cond_text <- renderUI(W({
    res <- results(); m <- res$model
    n_c <- focus_n(input$cond_n); n_t <- treatment_n(n_c, res$p$ratio)
    a  <- model_exact(m, n_t, n_c)
    pw <- model_power(m, n_t, n_c)
    p(strong(paste0(L("At ", "Com "), n_text(n_t, n_c), events_note(m, n_c, res$p$ratio),
                    L(": exact assurance ", ": assurance exata "), fmt_pct(a),
                    L(", frequentist power ", ", poder frequentista "), fmt_pct(pw), ".")))
  }))

  # ---- Detectability tab ---------------------------------------------------------
  det_focus <- reactive({
    res <- results()
    n_c <- focus_n(input$det_n); n_t <- treatment_n(n_c, res$p$ratio)
    c(list(n_t = n_t, n_c = n_c), detectability_one(res$model, n_t, n_c))
  })
  output$det_vb_detect <- renderText(W(fmt_pct(det_focus()$detect)))
  output$det_vb_detect_sub <- renderText(W({
    res <- results(); n <- res$metrics$n_detect
    paste0(L("For ", "Para "), fmt_pct(res$p$target, 0), ": ",
           if (is.na(n)) L("not reachable", "inating\u00edvel") else paste0("n = ", fmt_num(n)))
  }))
  output$det_vb_missed <- renderText(W(fmt_pct(det_focus()$missed)))
  output$det_vb_false_title <- renderText(W(
    if (results()$model$alt == "two.sided") L("Success in the wrong direction", "Sucesso na dire\u00e7\u00e3o errada")
    else L("Success without a real effect", "Sucesso sem efeito real")))
  output$det_vb_false <- renderText(W({
    d <- det_focus(); fmt_pct(if (results()$model$alt == "two.sided") d$wrong_dir else d$false_success, 2)
  }))
  output$det_vb_false_sub <- renderText(W({
    d <- det_focus()
    if (is.na(d$ppv)) "" else paste0(L("A success means a real effect ", "Um sucesso significa efeito real "),
                                     if (d$ppv > 0.999) paste0("> ", fmt_pct(0.999)) else fmt_pct(d$ppv, 1),
                                     L(" of the time", " das vezes"))
  }))
  output$det_vb_entropy <- renderText(W(paste(fmt_dec(det_focus()$h_outcome, 2), "bits")))
  output$det_text <- renderUI(W({
    res <- results(); d <- det_focus()
    tagList(lapply(detect_paragraphs(d, res$model, res$p, res$metrics$n_detect), tags$p))
  }))
  output$det_bar <- renderPlotly(W(plot_outcome_bar(det_focus(), results()$model$alt == "two.sided")))
  output$det_curves <- renderPlotly(W({
    res <- results(); plot_detect_curves(res$det, res$p$ratio, res$p$target, res$model$alt == "two.sided")
  }))
  output$det_entropy <- renderPlotly(W({
    res <- results(); plot_entropy_curves(res$det, res$p$ratio, res$model$alt == "two.sided")
  }))

  # ---- Complex design (extrapolation) card on the Results tab --------------------
  # A rough extrapolation from the last run's simple design (R/complex_design.R).
  # It uses the exact engine, so it updates live as the settings change.
  cx_inputs <- reactive({
    list(features = input$cx_features %||% character(0),
         target = suppressWarnings(as.numeric(input$cx_target %||% NA)),
         alpha = if (is.null(input$cx_alpha) || identical(input$cx_alpha, "same")) NA_real_
                 else as.numeric(input$cx_alpha),
         m = input$cx_m, icc = input$cx_icc, cv = input$cx_cv,
         k = input$cx_k, rep_rho = input$cx_rep_rho, x_rho = input$cx_x_rho,
         arms = input$cx_arms, mult = input$cx_mult %||% "bonferroni",
         J = input$cx_J, jmode = input$cx_jmode %||% "any",
         looks = input$cx_looks, bound = input$cx_bound %||% "obf",
         nonadh = input$cx_nonadh, contam = input$cx_contam)
  }) |> debounce(500)
  cx_base <- function(res) list(power = res$metrics$n_power, assurance = res$metrics$n_assur,
                                detect = res$metrics$n_detect)
  # Not wrapped in W(): the numbers do not depend on the language.
  cx_result <- reactive({
    req(isTRUE(input$cx_on))
    res <- results(); cx <- cx_inputs()
    req(is.null(with_lang("en", cx_check(cx))))
    cx_extrapolate(res$p, res$model, cx, cx_base(res))
  })

  output$cx_headline <- renderUI(W({
    req(isTRUE(input$cx_on))
    res <- results()
    msg <- cx_check(cx_inputs())
    if (!is.null(msg)) return(div(class = "alert alert-warning py-2 small", msg))
    r <- cx_result()
    tagList(
      lapply(cx_headline(r, res$p, cx_base(res)), tags$p),
      if (!length(r$used) && nrow(r$crit$power$steps) == 1)
        div(class = "alert alert-info py-2 small",
            L("Tick design features or choose a stricter target or alpha in the sidebar (Complex design panel).",
              "Marque caracter\u00EDsticas do desenho ou escolha uma meta ou um alfa mais exigente na barra lateral (painel Desenho complexo).")))
  }))
  output$cx_table <- renderTable(W({
    res <- results(); cx_result_table(cx_result(), res$p, cx_base(res))
  }), striped = TRUE, spacing = "s", align = "l")
  output$cx_waterfall <- renderPlotly(W(
    plot_cx_waterfall(cx_result(), input$cx_wf_crit %||% "power", results()$p$ratio)))
  output$cx_factors <- renderUI(W({
    d <- cx_factor_table(cx_result(), results()$p)
    if (is.null(d)) return(p(class = "text-muted", L("No adjustments selected.", "Nenhum ajuste selecionado.")))
    tagList(
      tags$ul(class = "list-unstyled mb-1",
        lapply(seq_len(nrow(d)), function(i) tags$li(class = "mb-2",
          tags$strong(d[i, 1]), " ", tags$span(class = "badge text-bg-light border", d[i, 4]), tags$br(),
          d[i, 2], tags$br(),
          tags$span(class = "text-muted", d[i, 3], if (nzchar(d[i, 5])) paste0(" (", d[i, 5], ")"))))),
      tags$small(class = "text-muted", L("Multipliers are for the frequentist power sample size.",
                                         "Os multiplicadores s\u00E3o para o tamanho amostral do poder frequentista.")))
  }))
  output$cx_notes <- renderUI(W(tags$ul(lapply(cx_notes(cx_result(), results()$p), tags$li))))

  # ---- Sensitivity tab ---------------------------------------------------------
  output$sens_heat <- renderPlotly(W({
    res <- results()
    plot_sensitivity_heat(res$model, focus_n(input$sens_n), res$p$ratio,
                          res$p$same_prior,
                          len = if (res$model$const_var) 41 else 25)
  }))
  output$sens_asd <- renderPlotly(W({
    res <- results()
    plot_sensitivity_analysis_sd(res$model, focus_n(input$sens_n), res$p$ratio)
  }))
  output$sens_nuis_title <- renderText(W(paste0(
    L("Assurance across values of: ", "Assurance para diferentes valores de: "),
    tx(results()$model$nuisance$label))))
  output$sens_nuis <- renderPlotly(W({
    res <- results()
    plot_sensitivity_nuisance(res$p, res$model, focus_n(input$sens_n))
  }))

  # ---- Scenario comparison -----------------------------------------------------
  scenarios <- reactiveVal(list())

  observeEvent(input$save_scenario, W({
    res <- results()
    s <- scenarios()
    if (length(s) >= 8) {
      showNotification(L("You can save up to 8 scenarios. Remove one first.",
                         "Voc\u00EA pode salvar at\u00E9 8 cen\u00E1rios. Remova um primeiro."),
                       type = "warning")
      return()
    }
    label <- trimws(input$scenario_label)
    if (!nzchar(label)) label <- paste(L("Scenario", "Cen\u00E1rio"), length(s) + 1)
    s[[length(s) + 1]] <- list(label = label, p = res$p, model = res$model,
                               table = res$table, metrics = res$metrics)
    scenarios(s)
    updateTextInput(session, "scenario_label", value = "")
    showNotification(L(paste0("Saved \"", label, "\". See the Compare scenarios tab."),
                       paste0("\"", label, "\" salvo. Veja a aba Comparar cen\u00E1rios.")),
                     type = "message")
  }))
  observeEvent(input$remove_last, {
    s <- scenarios(); if (length(s)) scenarios(s[-length(s)])
  })
  observeEvent(input$clear_scenarios, scenarios(list()))

  output$cmp_empty <- renderUI(W({
    if (length(scenarios()) == 0)
      div(class = "alert alert-info",
          L("No scenarios saved yet. Run a calculation, then use \"Save scenario\" on the Results tab. Save a few with different assumptions (or even different outcome types) to compare them here.",
            "Nenhum cen\u00E1rio salvo ainda. Fa\u00E7a um c\u00E1lculo e use \"Salvar cen\u00E1rio\" na aba Resultados. Salve alguns com suposi\u00E7\u00F5es diferentes (ou at\u00E9 tipos de desfecho diferentes) para compar\u00E1-los aqui."))
  }))
  output$cmp_plot <- renderPlotly(W({
    req(length(scenarios()) > 0)
    plot_scenarios(scenarios(), input$cmp_power, input$cmp_total)
  }))
  output$cmp_table <- renderTable(W({
    s <- scenarios(); req(length(s) > 0)
    do.call(rbind, lapply(s, function(x) {
      p <- x$p; m <- x$model; last <- x$table[nrow(x$table), ]
      row <- list(
        x$label, tx(m$describe), describe_prior(m, "design"),
        if (p$same_prior) L("same", "igual") else describe_prior(m, "analysis"),
        describe_test(m), fmt_num(p$ratio), paste0(last$n_t, " / ", last$n_c),
        fmt_pct(last$assurance), fmt_pct(last$power), fmt_pct(last$detect),
        if (is.na(x$metrics$n_assur)) L("not reachable", "inating\u00EDvel")
        else paste0(fmt_num(x$metrics$n_assur), " (", fmt_pct(p$target, 0), ")"),
        fmt_pct(x$metrics$ceiling))
      names(row) <- c(L("Scenario", "Cen\u00E1rio"), L("Outcome", "Desfecho"),
                      L("Design prior", "Priori de planejamento"), L("Analysis prior", "Priori de an\u00E1lise"),
                      L("Test", "Teste"), L("Ratio", "Raz\u00E3o"), L("Largest n (T / C)", "Maior n (T / C)"),
                      L("Assurance there", "Assurance nele"), L("Power there", "Poder nele"),
                      L("Detection if real there", "Detec\u00e7\u00e3o se real nele"),
                      L("n for target assurance", "n para a assurance desejada"), L("Ceiling", "Teto"))
      as.data.frame(row, check.names = FALSE)
    }))
  }), striped = TRUE, spacing = "s")

  # ---- LLM prompt helper --------------------------------------------------
  output$pg_type_note <- renderUI(W({
    div(class = "alert alert-info py-2 small",
        L("The prompt is written for the outcome type selected in the sidebar: ",
          "O prompt \u00E9 escrito para o tipo de desfecho selecionado na barra lateral: "),
        strong(outcome_type_label(input$otype, input$measure)),
        L(". Change it there first if needed.", ". Mude-o l\u00E1 primeiro, se necess\u00E1rio."),
        if (length(llm_cx())) L(" It also asks for the values of the complex-design features ticked in the sidebar.",
                                " Ele tamb\u00E9m pede os valores das caracter\u00EDsticas de desenho complexo marcadas na barra lateral."))
  }))

  # Current app inputs, renamed to the JSON field names used in the prompt.
  llm_cx <- function() if (isTRUE(input$cx_on)) input$cx_features %||% character(0) else character(0)
  current_for_llm <- function() {
    keys <- llm_keys_for(input$otype, input$measure, llm_cx())
    r <- c(raw_inputs(), lapply(setNames(grep("^cx_", keys, value = TRUE), grep("^cx_", keys, value = TRUE)),
                                function(id) input[[id]]))
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
                     otype = input$otype, measure = input$measure, cx = llm_cx())
  }) |> debounce(400)

  output$pg_prompt <- renderText(llm_prompt())
  output$dl_prompt <- downloadHandler(
    filename = function() paste0("assurance-llm-prompt-", stamp(), ".txt"),
    content = function(file) writeLines(llm_prompt(), file))

  observeEvent(input$pg_apply, W({
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
    output$pg_apply_result <- renderUI(W({
      tagList(
        if (length(v)) {
          tagList(
            div(class = "alert alert-success py-2 mt-3 small",
                L(paste0("Applied ", length(v), " value(s). Check them in the sidebar, then press Calculate."),
                  paste0(length(v), " valor(es) aplicado(s). Confira na barra lateral e aperte Calcular."))),
            tags$table(class = "table table-sm small",
              tags$thead(tags$tr(tags$th(L("Input", "Entrada")), tags$th(L("Value", "Valor")))),
              tags$tbody(lapply(names(v), function(k)
                tags$tr(tags$td(L(LLM_FIELDS[[k]]$label, LLM_FIELDS[[k]]$label_pt %||% LLM_FIELDS[[k]]$label)),
                        tags$td(format(v[[k]]))))))
          )
        },
        if (length(parsed$messages))
          div(class = "alert alert-warning py-2 mt-3 small",
              tags$ul(class = "mb-0", lapply(parsed$messages, tags$li)))
      )
    }))
    if (length(v)) showNotification(L("Values applied. Press Calculate to update the results.",
                                      "Valores aplicados. Aperte Calcular para atualizar os resultados."),
                                    type = "message")
  }))
}

shinyApp(ui, server, enableBookmarking = "url")
