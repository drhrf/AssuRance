# =============================================================================
# AssuRance: Bayesian assurance vs. frequentist power for a two-arm trial
# with a continuous (normal) outcome and known SD.
#
# Run locally:   shiny::runApp()        (from this directory)
#
# Files
#   app.R               UI and server (this file)
#   R/calculations.R    statistical engine; documents the mapping from these
#                       inputs to bayesassurance's parameters
#   R/summary_text.R    plain-language summary
#   R/plots.R           plot builders
#   R/methods_ui.R      "Methods & help" tab
# Shiny sources everything in R/ automatically.
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(DT)
library(bayesassurance)

# Input label with an (i) icon that shows a plain-language tooltip.
lab <- function(text, tip) {
  tags$span(text, " ",
            tooltip(tags$span("\u24D8", class = "text-primary",
                              style = "cursor: help;"),
                    tip, placement = "right"))
}

# Small grey help line under an input.
hint <- function(...) tags$div(class = "form-text mb-3", style = "margin-top:-0.6rem;", ...)


# ============================================================================
# UI
# ============================================================================
ui <- function(request) {
  page_sidebar(
    title = "AssuRance: Bayesian assurance vs. frequentist power",
    theme = bs_theme(version = 5, primary = "#1f5fa8"),
    fillable = FALSE,

    sidebar = sidebar(
      width = 370,
      accordion(
        multiple = TRUE,
        open = c("Effect and priors", "Trial design"),

        # ---- Priors ------------------------------------------------------
        accordion_panel(
          "Effect and priors",
          numericInput("design_mean",
            lab("Expected effect (design prior mean)",
                "Your best guess of the true difference in means (treatment minus control), in the outcome's units."),
            value = 5),
          numericInput("design_sd",
            lab("Uncertainty about the effect (design prior SD)",
                "How unsure you are. About 95% of the effects you consider plausible lie within mean \u00B1 2 SD. Must be > 0."),
            value = 3, min = 0),
          hint("The design prior describes what you believe; it is used only to plan the study."),
          checkboxInput("same_prior", "Analyse with the same prior", value = TRUE),
          hint("Simple, but it builds your optimism into the final analysis. Untick to use a sceptical or vague analysis prior."),
          conditionalPanel(
            "!input.same_prior",
            numericInput("analysis_mean",
              lab("Analysis prior mean",
                  "Centre of the prior used when the trial is analysed. 0 gives a 'sceptical' prior centred on no effect."),
              value = 0),
            numericInput("analysis_sd",
              lab("Analysis prior SD",
                  "Spread of the analysis prior. A very large value (e.g. 1000) lets the data speak for themselves, which is close to a frequentist analysis."),
              value = 10, min = 0)
          )
        ),

        # ---- Design ------------------------------------------------------
        accordion_panel(
          "Trial design",
          numericInput("sigma",
            lab("Outcome standard deviation",
                "Person-to-person variability of the outcome within each group (pooled), assumed known. Must be > 0."),
            value = 10, min = 0),
          tags$label(class = "form-label",
                     lab("Sample sizes to evaluate (per group)",
                         "Analysable participants in each group. With unequal allocation these are control-group sizes. Every value from minimum to maximum, in the given steps, is evaluated.")),
          layout_columns(
            col_widths = c(4, 4, 4), gap = "0.5rem",
            numericInput("n_min", "Min", value = 20, min = 2, step = 1),
            numericInput("n_max", "Max", value = 200, min = 3, step = 1),
            numericInput("n_step", "Step", value = 10, min = 1, step = 1)
          ),
          numericInput("ratio",
            lab("Allocation ratio (treatment : control)",
                "1 = equal groups. 2 = two treated participants for every control. The treatment-group size is ratio x control size."),
            value = 1, min = 0.1, max = 10, step = 0.5),
          numericInput("dropout",
            lab("Expected dropout (%)",
                "Share of enrolled participants you expect to lose. It does not change the probabilities; it only inflates the number to enrol."),
            value = 0, min = 0, max = 90, step = 5)
        ),

        # ---- Success -----------------------------------------------------
        accordion_panel(
          "What counts as success",
          radioButtons("alt",
            lab("Test direction",
                "Two-sided counts a clear difference in either direction. One-sided only counts a difference in the stated direction."),
            choices = c("Two-sided" = "two.sided",
                        "One-sided: treatment > control" = "greater",
                        "One-sided: treatment < control" = "less"),
            selected = "two.sided"),
          conditionalPanel(
            "input.alt != 'two.sided'",
            numericInput("margin",
              lab("Clinically meaningful difference (margin)",
                  "Success requires showing the effect is larger than this, not merely above zero. Use 0 for an ordinary test."),
              value = 0, min = 0)
          ),
          numericInput("alpha",
            lab("Significance threshold (alpha)",
                "Frequentist: the p-value cut-off. Bayesian: success if the posterior probability of an effect in the tested direction exceeds 1 - alpha (1 - alpha/2 per direction for two-sided)."),
            value = 0.05, min = 0.0001, max = 0.5, step = 0.005),
          sliderInput("target",
            lab("Target probability of success",
                "The level you are aiming for, commonly 80% or 90%. Shown as a reference line; the app finds the sample size that reaches it."),
            min = 0.5, max = 0.99, value = 0.8, step = 0.01)
        ),

        # ---- Computation -------------------------------------------------
        accordion_panel(
          "Computation",
          radioButtons("engine",
            lab("Assurance engine",
                "Simulation uses the bayesassurance package (slower, with a little random error). Exact uses the closed-form formula (instant); the two agree up to simulation error."),
            choices = c("Simulation: bayesassurance" = "sim",
                        "Exact formula (instant)" = "exact"),
            selected = "sim"),
          conditionalPanel(
            "input.engine == 'sim'",
            numericInput("mc_iter",
              lab("Simulated trials per sample size",
                  "More simulations give a smoother, more precise curve but take longer. The error is at most about 1/sqrt(number) (95% interval)."),
              value = 2000, min = 100, max = 50000, step = 500),
            numericInput("seed", "Random seed (reproducibility)", value = 2024),
            checkboxInput("show_exact", "Overlay exact curve as a check", value = TRUE)
          ),
          uiOutput("time_estimate")
        )
      ),
      actionButton("go", "Calculate", class = "btn-primary btn-lg w-100"),
      uiOutput("stale"),
      bookmarkButton(label = "Share / bookmark these inputs",
                     class = "btn-outline-secondary btn-sm w-100")
    ),

    navset_card_underline(
      id = "tabs",

      # ---- Results -------------------------------------------------------
      nav_panel(
        "Results",
        layout_column_wrap(
          width = 1 / 4, fill = FALSE,
          value_box(title = textOutput("vb_assur_title", inline = TRUE),
                    value = textOutput("vb_assur", inline = TRUE),
                    theme = "primary"),
          value_box(title = textOutput("vb_power_title", inline = TRUE),
                    value = textOutput("vb_power", inline = TRUE),
                    theme = value_box_theme(bg = "#d1661a", fg = "white")),
          value_box(title = textOutput("vb_n_title", inline = TRUE),
                    value = textOutput("vb_n", inline = TRUE),
                    textOutput("vb_n_sub", inline = TRUE),
                    theme = "light"),
          value_box(title = "Assurance ceiling (unlimited n)",
                    value = textOutput("vb_ceiling", inline = TRUE),
                    tags$small("Highest assurance any sample size can reach"),
                    theme = "light")
        ),
        card(
          card_header("Probability of success by sample size"),
          plotlyOutput("curve", height = "440px"),
          card_footer(tags$small(textOutput("curve_note", inline = TRUE)))
        ),
        card(
          card_header("Summary in plain language"),
          uiOutput("summary")
        ),
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            "Results table",
            div(
              downloadButton("dl_csv", "CSV", class = "btn-sm btn-outline-primary"),
              downloadButton("dl_png", "Plot (PNG)", class = "btn-sm btn-outline-primary"),
              downloadButton("dl_report", "Report (.txt)", class = "btn-sm btn-outline-primary")
            )
          ),
          DTOutput("results_table")
        ),
        card(
          card_header("Save this run as a scenario to compare"),
          layout_columns(
            col_widths = c(8, 4),
            textInput("scenario_label", NULL, placeholder = "Scenario name (optional)"),
            actionButton("save_scenario", "Save scenario", class = "btn-outline-primary w-100")
          )
        )
      ),

      # ---- Priors --------------------------------------------------------
      nav_panel(
        "Priors",
        card(
          card_header("Your design prior (and analysis prior, if different)"),
          plotlyOutput("prior_plot", height = "400px"),
          uiOutput("prior_text")
        )
      ),

      # ---- Conditional success -------------------------------------------
      nav_panel(
        "Success vs true effect",
        card(
          card_header("How likely is success if the true effect were exactly x?"),
          numericInput("cond_n", "Sample size (per group / control group)",
                       value = 200, min = 2, step = 1, width = "260px"),
          plotlyOutput("cond_plot", height = "420px"),
          p(class = "mt-2",
            "The solid blue curve is the chance that the Bayesian analysis",
            "declares success if the true effect were exactly the value on",
            "the x-axis; the dashed orange curve is the same for the t-test.",
            "Frequentist power reads the orange curve at a single point (the",
            "design mean). Assurance is the blue curve averaged over the grey",
            "design prior, so effects you consider plausible but small pull it",
            "down."),
          uiOutput("cond_text")
        )
      ),

      # ---- Sensitivity ---------------------------------------------------
      nav_panel(
        "Sensitivity",
        card(
          card_header("How much do the results depend on your assumptions?"),
          numericInput("sens_n", "Sample size (per group / control group)",
                       value = 200, min = 2, step = 1, width = "260px"),
          p("Values here use the exact formula, so they update instantly and",
            "carry no simulation noise."),
          layout_columns(
            col_widths = c(6, 6),
            div(h6("Assurance across design priors"),
                plotlyOutput("sens_heat", height = "400px"),
                tags$small("The orange x marks your inputs. Contours show",
                           "assurance; moving right (a larger expected",
                           "effect) or down (more certainty) usually helps.")),
            div(h6("Assurance across analysis priors"),
                plotlyOutput("sens_asd", height = "400px"),
                tags$small("Far right = vague analysis prior (data-driven",
                           "analysis). The dashed vertical line marks your",
                           "current analysis prior SD."))
          )
        )
      ),

      # ---- Scenario comparison ---------------------------------------------
      nav_panel(
        "Compare scenarios",
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            "Saved scenarios",
            div(
              actionButton("remove_last", "Remove last", class = "btn-sm btn-outline-secondary"),
              actionButton("clear_scenarios", "Clear all", class = "btn-sm btn-outline-danger")
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            checkboxInput("cmp_power", "Also show frequentist power (dashed)", FALSE),
            checkboxInput("cmp_total", "x-axis: total sample size (both groups)", FALSE)
          ),
          uiOutput("cmp_empty"),
          plotlyOutput("cmp_plot", height = "420px"),
          tableOutput("cmp_table")
        )
      ),

      # ---- LLM prompt helper ----------------------------------------------
      nav_panel(
        "LLM prompt helper",
        p("Not sure what to enter? Describe your project below. The app writes",
          "a prompt you can paste into an AI assistant (ideally one that can",
          "search the web) to research evidence-based values for every input.",
          "Then paste the assistant's answer back in step 3 to fill in the app."),
        div(class = "alert alert-warning py-2 small",
            strong("Check before you trust:"), "AI assistants can make mistakes",
            "or cite sources that do not exist. Verify the key numbers and",
            "references, and don't paste confidential details into a tool your",
            "institution has not approved."),
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header("1. Describe your project"),
            textInput("pg_title", "Study title or working name", width = "100%"),
            textInput("pg_condition", "Condition and population",
                      placeholder = "e.g. adults with uncontrolled hypertension", width = "100%"),
            layout_columns(
              col_widths = c(6, 6),
              textInput("pg_intervention", "Intervention", placeholder = "e.g. drug X 10 mg daily"),
              textInput("pg_comparator", "Comparator", placeholder = "e.g. placebo, usual care")
            ),
            textInput("pg_outcome", "Primary outcome and units",
                      placeholder = "e.g. systolic blood pressure (mmHg)", width = "100%"),
            layout_columns(
              col_widths = c(6, 6),
              textInput("pg_timepoint", "Time point", placeholder = "e.g. 12 weeks"),
              selectInput("pg_phase", "Study type",
                          c("Pilot / feasibility", "Phase II", "Phase III / confirmatory",
                            "Pragmatic / effectiveness", "Other / not sure"),
                          selected = "Phase III / confirmatory")
            ),
            radioButtons("pg_direction", "Which outcome values are better?",
                         c("Higher is better" = "higher", "Lower is better" = "lower",
                           "Not sure" = "unsure"), selected = "unsure", inline = TRUE),
            layout_columns(
              col_widths = c(6, 6),
              textInput("pg_setting", "Setting / country", placeholder = "e.g. primary care, Brazil"),
              textInput("pg_mcid", "Meaningful difference (if known)", placeholder = "e.g. 5 mmHg")
            ),
            textAreaInput("pg_known", "Evidence you already know about (optional)", rows = 3,
                          placeholder = "Pilot results, key trials, meta-analyses, DOIs...",
                          width = "100%"),
            textAreaInput("pg_constraints", "Practical constraints (optional)", rows = 2,
                          placeholder = "e.g. can recruit at most 150 per arm in 2 years",
                          width = "100%"),
            layout_columns(
              col_widths = c(6, 6),
              div(checkboxInput("pg_web", "The AI assistant can search the web", TRUE),
                  checkboxInput("pg_current", "Include my current app inputs", FALSE)),
              selectInput("pg_language", "Answer language",
                          c("English", "Portuguese (Brazil)", "Spanish", "French", "German"))
            )
          ),
          div(
            card(
              card_header(
                class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                "2. Copy this prompt into your AI assistant",
                div(
                  tags$button(
                    id = "pg_copy", type = "button", class = "btn btn-sm btn-primary",
                    onclick = paste0(
                      "var b=this;navigator.clipboard.writeText(",
                      "document.getElementById('pg_prompt').innerText).then(function(){",
                      "b.textContent='Copied!';setTimeout(function(){b.textContent='Copy prompt';},1500);",
                      "},function(){b.textContent='Select the text and copy it manually';});"),
                    "Copy prompt"),
                  downloadButton("dl_prompt", "Download (.txt)", class = "btn-sm btn-outline-primary")
                )
              ),
              tags$style("#pg_prompt { white-space: pre-wrap; max-height: 420px; overflow-y: auto; font-size: 0.8rem; }"),
              verbatimTextOutput("pg_prompt")
            ),
            card(
              card_header("3. Paste the assistant's answer to fill in the app"),
              p(class = "small mb-1",
                "Paste the whole answer, or just its JSON block. Only recognised,",
                "valid values are applied; you can review them in the sidebar",
                "before pressing Calculate."),
              textAreaInput("pg_answer", NULL, rows = 6, width = "100%",
                            placeholder = "{ \"design_mean\": 5, \"design_sd\": 3, \"sigma\": 10, ... }"),
              actionButton("pg_apply", "Apply values to the app", class = "btn-primary"),
              uiOutput("pg_apply_result")
            )
          )
        )
      ),

      # ---- Methods --------------------------------------------------------
      nav_panel("Methods & help", card(methods_ui()))
    )
  )
}


# ============================================================================
# Server
# ============================================================================
server <- function(input, output, session) {

  setBookmarkExclude(c(
    "go", "save_scenario", "remove_last", "clear_scenarios", "scenario_label",
    # LLM prompt helper: free text would bloat the bookmark URL
    "pg_title", "pg_condition", "pg_intervention", "pg_comparator",
    "pg_outcome", "pg_timepoint", "pg_phase", "pg_direction", "pg_setting",
    "pg_mcid", "pg_known", "pg_constraints", "pg_web", "pg_current",
    "pg_language", "pg_answer", "pg_apply",
    paste0("results_table_", c("rows_current", "rows_all", "rows_selected",
                               "search", "state", "cell_clicked",
                               "cells_selected", "columns_selected"))))

  input_ids <- c("design_mean", "design_sd", "same_prior", "analysis_mean",
                 "analysis_sd", "sigma", "n_min", "n_max", "n_step", "ratio",
                 "dropout", "alt", "margin", "alpha", "target", "engine",
                 "mc_iter", "seed", "show_exact")
  raw_inputs <- reactive(lapply(setNames(input_ids, input_ids),
                                function(id) input[[id]]))

  # ---- Collect and validate inputs --------------------------------------
  params <- reactive({
    r <- raw_inputs()
    is_num   <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
    is_whole <- function(x) is_num(x) && abs(x - round(x)) < 1e-8
    sim <- identical(r$engine, "sim")

    validate(
      need(is_num(r$design_mean), "Expected effect (design prior mean) must be a number."),
      need(is_num(r$design_sd) && r$design_sd > 0, "Design prior SD must be a positive number."),
      need(isTRUE(r$same_prior) || is_num(r$analysis_mean), "Analysis prior mean must be a number."),
      need(isTRUE(r$same_prior) || (is_num(r$analysis_sd) && r$analysis_sd > 0),
           "Analysis prior SD must be a positive number."),
      need(is_num(r$sigma) && r$sigma > 0, "Outcome standard deviation must be a positive number."),
      need(is_whole(r$n_min) && r$n_min >= 2, "Minimum sample size must be a whole number of at least 2."),
      need(is_whole(r$n_max), "Maximum sample size must be a whole number."),
      need(is_whole(r$n_step) && r$n_step >= 1, "Step must be a whole number of at least 1."),
      need(is_num(r$n_min) && is_num(r$n_max) && r$n_max > r$n_min,
           "Maximum sample size must exceed the minimum."),
      need(!is_num(r$n_max) || r$n_max <= 20000, "Maximum sample size is limited to 20,000 per group."),
      need(is_num(r$ratio) && r$ratio >= 0.1 && r$ratio <= 10,
           "Allocation ratio must be between 0.1 and 10."),
      need(is_num(r$dropout) && r$dropout >= 0 && r$dropout < 90,
           "Dropout must be between 0 and 90%."),
      need(r$alt == "two.sided" || (is_num(r$margin) && r$margin >= 0),
           "The clinically meaningful difference must be zero or positive."),
      need(is_num(r$alpha) && r$alpha > 0 && r$alpha < 1,
           "Significance threshold must be between 0 and 1."),
      need(!sim || (is_whole(r$mc_iter) && r$mc_iter >= 100),
           "Number of simulated trials must be a whole number of at least 100."),
      need(!sim || is_num(r$seed), "Random seed must be a number.")
    )

    p <- r
    p$same_prior <- isTRUE(r$same_prior)
    # When the checkbox is ticked the analysis prior is an exact copy of the
    # design prior; otherwise the separate inputs are used.
    if (p$same_prior) {
      p$analysis_mean <- r$design_mean
      p$analysis_sd   <- r$design_sd
    }
    p$margin  <- if (r$alt == "two.sided") 0 else r$margin
    p$dropout <- r$dropout / 100
    p$show_exact <- isTRUE(r$show_exact)

    n_c <- seq(r$n_min, r$n_max, by = r$n_step)
    if (utils::tail(n_c, 1) != r$n_max) n_c <- c(n_c, r$n_max)
    validate(need(length(n_c) <= 200,
      "That range gives more than 200 sample sizes; please use a larger step."))
    p$n_c <- as.integer(n_c)
    p$n_t <- as.integer(treatment_n(n_c, r$ratio))

    if (sim) {
      est <- estimate_seconds(p$n_t, p$n_c, r$mc_iter)
      validate(need(est <= 900, paste0(
        "This simulation would take roughly ", round(est / 60),
        " minutes. Reduce the maximum sample size, the number of sample ",
        "sizes, or the simulations per sample size, or use the exact engine.")))
    }
    p
  })

  output$time_estimate <- renderUI({
    p <- tryCatch(params(), error = function(e) NULL)
    if (is.null(p)) return(NULL)
    txt <- if (p$engine == "sim") {
      s <- estimate_seconds(p$n_t, p$n_c, p$mc_iter)
      paste0("Estimated run time: ", if (s < 60) paste0("~", max(1, round(s)), " s")
             else paste0("~", round(s / 60, 1), " min"),
             " (", length(p$n_c), " sample sizes). Hosted servers may be slower.")
    } else {
      paste0(length(p$n_c), " sample sizes; exact results are instant.")
    }
    tags$div(class = "form-text", txt)
  })

  # ---- Main computation (runs on "Calculate", and once at start-up) -----
  results <- eventReactive(input$go, {
    p <- params()
    inputs_snapshot <- raw_inputs()
    t0 <- Sys.time()

    exact <- exact_assurance(p$n_t, p$n_c, p$design_mean, p$design_sd,
                             p$analysis_mean, p$analysis_sd, p$sigma,
                             p$alpha, p$alt, p$margin)
    if (p$engine == "sim") {
      set.seed(p$seed)
      assur <- withProgress(
        message = "Simulating trials with bayesassurance", value = 0, {
          sim_assurance(
            p$n_t, p$n_c, p$design_mean, p$design_sd, p$analysis_mean,
            p$analysis_sd, p$sigma, p$alpha, p$alt, p$margin, p$mc_iter,
            progress = function(i, k) setProgress(
              i / k, detail = paste0("sample size ", i, " of ", k)))
        })
      se <- mc_se(assur, p$mc_iter)
    } else {
      assur <- exact
      se <- NA_real_
    }
    power <- freq_power(p$n_t, p$n_c, p$design_mean, p$sigma, p$alpha,
                        p$alt, p$margin)

    # Sample size needed for the target (exact formulas, no noise)
    f_assur <- function(nt, nc) exact_assurance(nt, nc, p$design_mean,
      p$design_sd, p$analysis_mean, p$analysis_sd, p$sigma, p$alpha, p$alt,
      p$margin)
    f_power <- function(nt, nc) freq_power(nt, nc, p$design_mean, p$sigma,
                                           p$alpha, p$alt, p$margin)
    k <- length(p$n_c)
    parts <- exact_assurance(p$n_t[k], p$n_c[k], p$design_mean, p$design_sd,
                             p$analysis_mean, p$analysis_sd, p$sigma, p$alpha,
                             p$alt, p$margin, parts = TRUE)
    metrics <- list(
      n_assur = smallest_n(f_assur, p$ratio, p$target),
      n_power = smallest_n(f_power, p$ratio, p$target),
      ceiling = assurance_ceiling(p$design_mean, p$design_sd, p$alt, p$margin),
      # two-sided: share of success in the direction opposite to the expected one
      neg_share = if (p$design_mean >= 0) parts$negative else parts$positive
    )

    list(p = p, inputs = inputs_snapshot, metrics = metrics,
         seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
         table = data.frame(n_t = p$n_t, n_c = p$n_c, assurance = assur,
                            mc_se = se, exact = exact, power = power))
  }, ignoreNULL = FALSE)

  # Keep the per-tab sample-size selectors in step with the latest run.
  observeEvent(results(), {
    n <- max(results()$p$n_c)
    updateNumericInput(session, "cond_n", value = n)
    updateNumericInput(session, "sens_n", value = n)
  })

  output$stale <- renderUI({
    res <- tryCatch(results(), error = function(e) NULL)
    if (is.null(res) || identical(raw_inputs(), res$inputs)) return(NULL)
    div(class = "alert alert-warning py-2 px-3 my-2 small",
        "Inputs have changed. Press Calculate to update the results.")
  })

  # ---- Value boxes --------------------------------------------------------
  last_row <- reactive({ t <- results()$table; t[nrow(t), ] })
  n_text <- function(nt, nc) if (nt == nc) paste0("n = ", nc, " per group")
                             else paste0("n = ", nt, " / ", nc, " (T / C)")

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
    res <- results(); n <- res$metrics$n_power
    paste0("For ", fmt_pct(res$p$target, 0), " power: ",
           if (is.na(n)) "not reachable" else format(n, big.mark = ","))
  })
  output$vb_ceiling <- renderText(fmt_pct(results()$metrics$ceiling))

  # ---- Main plot, summary, table -------------------------------------------
  output$curve <- renderPlotly(plot_curve_plotly(results()))
  output$curve_note <- renderText({
    res <- results()
    if (res$p$engine == "sim") {
      paste0("Assurance simulated with bayesassurance::bayes_sim_unbalanced (",
             format(res$p$mc_iter, big.mark = ","), " trials per sample size, seed ",
             res$p$seed, ", ", round(res$seconds, 1), " s). Shaded band: 95% ",
             "Monte Carlo interval. Power: two-sample t-test.")
    } else {
      "Assurance from the exact closed-form formula. Power: two-sample t-test."
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
    d[["Bayesian assurance"]] <- t$assurance
    if (p$engine == "sim") {
      d[["\u00B1 95% MC error"]] <- 1.96 * t$mc_se
      d[["Exact assurance"]] <- t$exact
    }
    d[["Frequentist power"]] <- t$power
    d
  })

  output$results_table <- renderDT({
    d <- display_table()
    pct_cols <- intersect(names(d), c("Bayesian assurance", "\u00B1 95% MC error",
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
                    "assurance_exact", "frequentist_power")
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
      res <- results(); p <- res$p
      alt_txt <- c(two.sided = "two-sided", greater = "one-sided, treatment > control",
                   less = "one-sided, treatment < control")[[p$alt]]
      lines <- c(
        "AssuRance report",
        paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
        "",
        "INPUTS",
        sprintf("  Design prior:    N(mean = %s, SD = %s)", p$design_mean, p$design_sd),
        sprintf("  Analysis prior:  N(mean = %s, SD = %s)%s", p$analysis_mean,
                p$analysis_sd, if (p$same_prior) "  [same as design prior]" else ""),
        sprintf("  Outcome SD:      %s", p$sigma),
        sprintf("  Test:            %s, alpha = %s%s", alt_txt, p$alpha,
                if (p$margin > 0) paste0(", margin = ", p$margin) else ""),
        sprintf("  Allocation:      %s : 1 (treatment : control)", p$ratio),
        sprintf("  Dropout:         %s%%", 100 * p$dropout),
        sprintf("  Target:          %s", fmt_pct(p$target, 0)),
        sprintf("  Engine:          %s", if (p$engine == "sim")
          paste0("bayesassurance simulation, ", p$mc_iter, " trials per n, seed ", p$seed)
          else "exact closed-form formula"),
        "",
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
  output$prior_plot <- renderPlotly(plot_priors(results()$p))
  output$prior_text <- renderUI({
    p <- results()$p
    pos <- 1 - pnorm(0, p$design_mean, p$design_sd)
    items <- list(
      tags$li(paste0("Under your design prior there is a ", fmt_pct(pos),
                     " chance that treatment is truly better than control ",
                     "(effect above 0), and a ", fmt_pct(1 - pos),
                     " chance that it is not.")),
      tags$li(paste0("95% of the effects you consider plausible lie between ",
                     fmt_num(p$design_mean - 1.96 * p$design_sd), " and ",
                     fmt_num(p$design_mean + 1.96 * p$design_sd), ".")))
    if (p$alt != "two.sided") {
      items <- c(items, list(tags$li(paste0(
        "The shaded area (", fmt_pct(results()$metrics$ceiling), ") is the ",
        "probability that the true effect is large enough to count as a ",
        "success. This is also the assurance ceiling."))))
    }
    if (!p$same_prior) {
      items <- c(items, list(tags$li(paste0(
        "The analysis prior N(", fmt_num(p$analysis_mean), ", ",
        fmt_num(p$analysis_sd), "\u00B2) will be combined with the trial data. ",
        if (p$analysis_sd > 5 * p$sigma) "It is vague, so the data will dominate."
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
  output$cond_plot <- renderPlotly(plot_conditional(results()$p, focus_n(input$cond_n)))
  output$cond_text <- renderUI({
    p <- results()$p; n_c <- focus_n(input$cond_n); n_t <- treatment_n(n_c, p$ratio)
    a <- exact_assurance(n_t, n_c, p$design_mean, p$design_sd, p$analysis_mean,
                         p$analysis_sd, p$sigma, p$alpha, p$alt, p$margin)
    pw <- freq_power(n_t, n_c, p$design_mean, p$sigma, p$alpha, p$alt, p$margin)
    p(strong(paste0("At ", n_text(n_t, n_c), ": exact assurance ", fmt_pct(a),
                    ", frequentist power ", fmt_pct(pw), ".")))
  })

  # ---- Sensitivity tab ---------------------------------------------------------
  output$sens_heat <- renderPlotly(plot_sensitivity_heat(results()$p, focus_n(input$sens_n)))
  output$sens_asd  <- renderPlotly(plot_sensitivity_analysis_sd(results()$p, focus_n(input$sens_n)))

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
    s[[length(s) + 1]] <- list(label = label, p = res$p, table = res$table,
                               metrics = res$metrics)
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
          "on the Results tab. Save a few with different assumptions to compare them here.")
  })
  output$cmp_plot <- renderPlotly({
    req(length(scenarios()) > 0)
    plot_scenarios(scenarios(), input$cmp_power, input$cmp_total)
  })
  output$cmp_table <- renderTable({
    s <- scenarios(); req(length(s) > 0)
    do.call(rbind, lapply(s, function(x) {
      p <- x$p; last <- x$table[nrow(x$table), ]
      data.frame(
        Scenario = x$label,
        "Design prior" = sprintf("mean %s, SD %s", fmt_num(p$design_mean), fmt_num(p$design_sd)),
        "Analysis prior" = if (p$same_prior) "same" else
          sprintf("mean %s, SD %s", fmt_num(p$analysis_mean), fmt_num(p$analysis_sd)),
        "Outcome SD" = fmt_num(p$sigma),
        Test = paste0(c(two.sided = "2-sided", greater = "T > C", less = "T < C")[[p$alt]],
                      if (p$margin > 0) paste0(", margin ", fmt_num(p$margin)) else "",
                      ", alpha = ", p$alpha),
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
  # Current app inputs, renamed to the JSON field names used in the prompt.
  current_for_llm <- function() {
    r <- raw_inputs()
    v <- r[intersect(names(r), names(LLM_FIELDS))]
    v$dropout_percent <- r$dropout
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
    build_llm_prompt(info, if (isTRUE(input$pg_current)) current_for_llm())
  }) |> debounce(400)

  output$pg_prompt <- renderText(llm_prompt())
  output$dl_prompt <- downloadHandler(
    filename = function() paste0("assurance-llm-prompt-", stamp(), ".txt"),
    content = function(file) writeLines(llm_prompt(), file))

  observeEvent(input$pg_apply, {
    parsed <- parse_llm_values(input$pg_answer)
    v <- parsed$values
    labels <- c(design_mean = "Design prior mean", design_sd = "Design prior SD",
                same_prior = "Analyse with the same prior",
                analysis_mean = "Analysis prior mean", analysis_sd = "Analysis prior SD",
                sigma = "Outcome SD", n_min = "Min n", n_max = "Max n", n_step = "Step",
                ratio = "Allocation ratio", dropout_percent = "Dropout (%)",
                alt = "Test direction", margin = "Margin", alpha = "Alpha",
                target = "Target")
    for (k in names(v)) {
      val <- v[[k]]
      switch(k,
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
                tags$tr(tags$td(labels[[k]]), tags$td(format(v[[k]]))))))
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
