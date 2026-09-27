# =============================================================================
# AssuRance: Bayesian assurance vs. frequentist power for a two-arm trial
# with a continuous (normally distributed) outcome and known SD.
#
# Run with:  shiny::runApp()   (from this directory)
#
# The statistical work lives in R/calculations.R, which Shiny sources
# automatically. That file documents the mapping from the inputs below
# (design prior, analysis prior, outcome SD) to the arguments of
# bayesassurance::bayes_sim().
# =============================================================================

library(shiny)
library(ggplot2)
library(bayesassurance)

# Input label with an (i) hover tooltip for a longer explanation.
label_help <- function(label, tip) {
  tags$span(label, " ",
            tags$span("ⓘ", title = tip,
                      style = "cursor: help; color: #3a7bd5;"))
}

ui <- fluidPage(
  titlePanel("Bayesian assurance vs. frequentist power (two-arm trial)"),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # --- Design prior ------------------------------------------------------
      h4("Design prior (your belief about the true effect)"),
      numericInput("design_mean",
        label_help("Expected effect (mean)",
          "Your best guess of the true difference in means (treatment minus control), in the outcome's units."),
        value = 5),
      helpText("Best guess of the true difference, treatment minus control."),
      numericInput("design_sd",
        label_help("Uncertainty about the effect (SD)",
          "How unsure you are about the effect. Roughly 95% of the effects you consider plausible lie within mean +/- 2 SD."),
        value = 3, min = 0),
      helpText("How unsure you are. Larger = less certain. Must be > 0."),

      # --- Analysis prior ----------------------------------------------------
      h4("Analysis prior (used when analysing the trial)"),
      checkboxInput("same_prior", "Use the same prior as the design prior",
                    value = TRUE),
      helpText("If ticked, your design belief is also built into the final analysis,",
               "which boosts assurance at small sample sizes. Untick to use a",
               "sceptical (mean 0) or vague (very large SD) analysis prior instead."),
      conditionalPanel(
        "!input.same_prior",
        numericInput("analysis_mean",
          label_help("Analysis prior mean",
            "The prior centre used in the final Bayesian analysis. 0 gives a 'sceptical' prior centred on no effect."),
          value = 0),
        helpText("Centre of the prior used in the final analysis (0 = sceptical)."),
        numericInput("analysis_sd",
          label_help("Analysis prior SD",
            "Spread of the analysis prior. A very large value (e.g. 1000) makes the analysis let the data speak for themselves, close to a frequentist analysis."),
          value = 10, min = 0),
        helpText("A very large SD means the analysis relies almost only on the data. Must be > 0.")
      ),

      # --- Outcome -----------------------------------------------------------
      h4("Outcome"),
      numericInput("sigma",
        label_help("Outcome standard deviation",
          "The natural person-to-person variability of the measurement within each group (pooled), assumed known."),
        value = 10, min = 0),
      helpText("Person-to-person variability of the outcome, assumed known. Must be > 0."),

      # --- Sample sizes ------------------------------------------------------
      h4("Sample sizes per group"),
      fluidRow(
        column(4, numericInput("n_min", "Minimum", value = 20, min = 2, step = 1)),
        column(4, numericInput("n_max", "Maximum", value = 200, min = 3, step = 1)),
        column(4, numericInput("n_step", "Step", value = 10, min = 1, step = 1))
      ),
      helpText("Number of participants in EACH arm. Assurance and power are computed at every value from minimum to maximum in the given steps."),

      # --- Success criterion -------------------------------------------------
      h4("What counts as success"),
      numericInput("alpha",
        label_help("Significance threshold (alpha)",
          "Frequentist: the p-value cut-off. Bayesian: success if the posterior probability of an effect in the tested direction exceeds 1 - alpha (1 - alpha/2 per direction for two-sided)."),
        value = 0.05, min = 0.0001, max = 0.5, step = 0.005),
      helpText("Conventional value is 0.05."),
      radioButtons("alt",
        label_help("Test direction",
          "Two-sided detects a difference in either direction. One-sided only counts a difference in the stated direction."),
        choices = c("Two-sided" = "two.sided",
                    "One-sided: treatment > control" = "greater",
                    "One-sided: treatment < control" = "less"),
        selected = "two.sided"),

      # --- Simulation settings -----------------------------------------------
      h4("Simulation settings"),
      numericInput("mc_iter",
        label_help("Simulated trials per sample size",
          "Assurance is estimated by simulating this many trials at each sample size. More = smoother curve but slower. Monte Carlo error is at most about 0.5/sqrt(number)."),
        value = 2000, min = 100, max = 50000, step = 500),
      helpText("More simulations give more precise estimates but take longer (2000 gives about +/- 1 to 2 percentage points)."),
      numericInput("seed", "Random seed (for reproducible results)", value = 2024),

      actionButton("go", "Calculate", class = "btn-primary", width = "100%")
    ),

    mainPanel(
      width = 8,
      plotOutput("curve", height = "420px"),
      h4("Results"),
      tableOutput("table"),
      h4("Summary"),
      uiOutput("summary")
    )
  )
)

server <- function(input, output, session) {

  # Collect and validate inputs; returns a list of clean parameters.
  params <- reactive({
    same <- isTRUE(input$same_prior)
    p <- list(
      design_mean   = input$design_mean,
      design_sd     = input$design_sd,
      # When the checkbox is ticked, the analysis prior is an exact copy of
      # the design prior; otherwise the separate inputs are used.
      analysis_mean = if (same) input$design_mean else input$analysis_mean,
      analysis_sd   = if (same) input$design_sd   else input$analysis_sd,
      sigma   = input$sigma,
      n_min   = input$n_min,
      n_max   = input$n_max,
      n_step  = input$n_step,
      alpha   = input$alpha,
      alt     = input$alt,
      mc_iter = input$mc_iter,
      seed    = input$seed
    )

    is_num  <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
    is_whole <- function(x) is_num(x) && abs(x - round(x)) < 1e-8

    validate(
      need(is_num(p$design_mean), "Design prior mean must be a number."),
      need(is_num(p$design_sd) && p$design_sd > 0,
           "Design prior SD must be a positive number."),
      need(is_num(p$analysis_mean), "Analysis prior mean must be a number."),
      need(is_num(p$analysis_sd) && p$analysis_sd > 0,
           "Analysis prior SD must be a positive number."),
      need(is_num(p$sigma) && p$sigma > 0,
           "Outcome standard deviation must be a positive number."),
      need(is_whole(p$n_min) && p$n_min >= 2,
           "Minimum sample size must be a whole number of at least 2."),
      need(is_whole(p$n_max), "Maximum sample size must be a whole number."),
      need(is_whole(p$n_step) && p$n_step >= 1,
           "Step must be a whole number of at least 1."),
      need(is_num(p$n_max) && is_num(p$n_min) && p$n_max > p$n_min,
           "Maximum sample size must exceed the minimum."),
      need(is_num(p$alpha) && p$alpha > 0 && p$alpha < 1,
           "Significance threshold must be between 0 and 1."),
      need(is_whole(p$mc_iter) && p$mc_iter >= 100,
           "Number of simulated trials must be a whole number of at least 100."),
      need(is_num(p$seed), "Random seed must be a number.")
    )

    p$n <- seq(p$n_min, p$n_max, by = p$n_step)
    # Always include the maximum, so the summary refers to the value typed in.
    if (utils::tail(p$n, 1) != p$n_max) p$n <- c(p$n, p$n_max)
    validate(need(length(p$n) <= 100,
      "That range gives more than 100 sample sizes; please use a larger step."))
    p
  })

  # Heavy computation: only re-run when "Calculate" is pressed.
  results <- eventReactive(input$go, {
    p <- params()
    withProgress(message = "Simulating trials with bayesassurance...",
                 detail = paste(length(p$n), "sample sizes x", p$mc_iter,
                                "simulations; this can take a little while."),
                 value = 0.2, {
      set.seed(p$seed)
      assur <- compute_bayes_assurance(
        n = p$n,
        design_mean = p$design_mean, design_sd = p$design_sd,
        analysis_mean = p$analysis_mean, analysis_sd = p$analysis_sd,
        sigma = p$sigma, alpha = p$alpha, alt = p$alt, mc_iter = p$mc_iter)
      incProgress(0.7)
      pwr <- compute_freq_power(n = p$n, design_mean = p$design_mean,
                                sigma = p$sigma, alpha = p$alpha, alt = p$alt)
    })
    list(p = p,
         table = data.frame(n = p$n, assurance = assur, power = pwr))
  }, ignoreNULL = FALSE)  # also compute once at start-up with the defaults

  output$curve <- renderPlot({
    r <- results()$table
    long <- rbind(
      data.frame(n = r$n, prob = r$assurance, method = "Bayesian assurance"),
      data.frame(n = r$n, prob = r$power,     method = "Frequentist power")
    )
    ggplot(long, aes(n, prob, colour = method, linetype = method)) +
      geom_hline(yintercept = 0.8, colour = "grey40", linetype = "dotted") +
      annotate("text", x = min(r$n), y = 0.8, label = "80%", vjust = -0.5,
               hjust = 0, colour = "grey30", size = 3.5) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.8) +
      scale_colour_manual(values = c("Bayesian assurance" = "#1f5fa8",
                                     "Frequentist power"  = "#d1661a")) +
      scale_linetype_manual(values = c("Bayesian assurance" = "solid",
                                       "Frequentist power"  = "dashed")) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                         labels = function(x) paste0(100 * x, "%")) +
      labs(x = "Sample size per group", y = "Probability of success",
           colour = NULL, linetype = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })

  output$table <- renderTable({
    r <- results()$table
    data.frame(
      "Sample size per group" = as.integer(r$n),
      "Bayesian assurance"    = sprintf("%.3f", r$assurance),
      "Frequentist power"     = sprintf("%.3f", r$power),
      check.names = FALSE
    )
  }, align = "c")

  output$summary <- renderUI({
    res <- results()
    paras <- build_summary(res$table, res$p$design_mean, res$p$alt)
    tagList(
      lapply(paras, tags$p),
      tags$p(tags$small(tags$em(paste0(
        "Assurance values are Monte Carlo estimates from ", res$p$mc_iter,
        " simulated trials per sample size (bayesassurance::bayes_sim), so ",
        "they carry a small random error; frequentist power is from ",
        "stats::power.t.test (two-sample t-test).")))))
  })
}

shinyApp(ui, server)
