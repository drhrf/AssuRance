# =============================================================================
# Sidebar inputs. Inputs that only apply to some outcome types sit inside
# conditionalPanel()s; the JavaScript conditions below decide which "scale
# family" is active:
#   additive : continuous outcomes (difference in means, ANCOVA)
#   rd       : binary outcome, risk difference
#   ratio    : binary outcome with odds ratio / risk ratio, and time to event
# =============================================================================

JS_ADDITIVE <- "input.otype == 'cont' || input.otype == 'ancova'"
JS_RD       <- "input.otype == 'binary' && input.measure == 'rd'"
JS_RATIO    <- "(input.otype == 'binary' && input.measure != 'rd') || input.otype == 'surv'"

# Default sample-size ranges per outcome type (control group / per group).
N_DEFAULTS <- list(cont = c(20, 200, 10), ancova = c(20, 200, 10),
                   binary = c(50, 600, 25), surv = c(50, 500, 25))

# Input label with an (i) icon that shows a plain-language tooltip.
lab <- function(text, tip) {
  tags$span(text, " ",
            tooltip(tags$span("\u24D8", class = "text-primary",
                              style = "cursor: help;"),
                    tip, placement = "right"))
}

# Small grey help line under an input.
hint <- function(...) tags$div(class = "form-text mb-3", style = "margin-top:-0.6rem;", ...)

sidebar_ui <- function() {
  sidebar(
    width = 380,
    accordion(
      multiple = TRUE,
      open = c("Outcome", "Effect and priors", "Trial design"),

      # ---- Outcome type --------------------------------------------------
      accordion_panel(
        "Outcome",
        selectInput("otype",
          lab("Type of primary outcome",
              "Continuous: a measurement such as blood pressure. ANCOVA: the same, analysed adjusting for its baseline value. Binary: an event that happens or not (e.g. response, death by 1 year). Time to event: how long until an event, allowing for censoring."),
          c("Continuous: difference in means" = "cont",
            "Continuous, adjusted for baseline (ANCOVA)" = "ancova",
            "Binary (event yes / no)" = "binary",
            "Time to event (survival)" = "surv"),
          selectize = FALSE),

        # continuous
        conditionalPanel(
          JS_ADDITIVE,
          numericInput("sigma",
            lab("Outcome standard deviation",
                "Person-to-person variability of the outcome within each group (pooled), assumed known. Must be > 0."),
            value = 10, min = 0)
        ),
        conditionalPanel(
          "input.otype == 'ancova'",
          numericInput("rho",
            lab("Correlation between baseline and outcome",
                "How strongly the baseline measurement predicts the final one (0 to 0.95). Adjusting for baseline shrinks the residual SD by sqrt(1 - rho^2). Typical values are 0.4 to 0.7."),
            value = 0.5, min = 0, max = 0.95, step = 0.05)
        ),

        # binary
        conditionalPanel(
          "input.otype == 'binary'",
          radioButtons("measure",
            lab("Effect measure",
                "Odds ratio: the usual choice for logistic regression. Risk ratio: relative change in risk. Risk difference: absolute change in percentage points."),
            c("Odds ratio" = "or", "Risk ratio" = "rr", "Risk difference" = "rd"),
            inline = TRUE),
          numericInput("p_control",
            lab("Event rate in the control group (%)",
                "The percentage of control-group participants expected to have the event."),
            value = 30, min = 0.1, max = 99.9, step = 1)
        ),

        # survival
        conditionalPanel(
          "input.otype == 'surv'",
          layout_columns(
            col_widths = c(7, 5), gap = "0.5rem",
            numericInput("surv_median",
              lab("Control-group median time to event",
                  "Half of control-group participants have had the event by this time (exponential survival is assumed)."),
              value = 12, min = 0),
            textInput("time_unit", "Time unit", value = "months")
          ),
          layout_columns(
            col_widths = c(6, 6), gap = "0.5rem",
            numericInput("accrual",
              lab("Recruitment period",
                  "How long recruitment lasts; participants are assumed to join evenly over this period."),
              value = 12, min = 0),
            numericInput("followup",
              lab("Extra follow-up",
                  "Follow-up after the last participant joins. The analysis happens at recruitment period + extra follow-up."),
              value = 12, min = 0)
          ),
          numericInput("loss_pct",
            lab("Lost to follow-up by the end (%)",
                "Share of participants expected to drop out (and be censored) before having the event, over an average follow-up. They still count as enrolled."),
            value = 5, min = 0, max = 90, step = 1)
        ),
        uiOutput("engine_note")
      ),

      # ---- Priors ------------------------------------------------------
      accordion_panel(
        "Effect and priors",
        uiOutput("effect_hint"),

        # additive (continuous)
        conditionalPanel(
          JS_ADDITIVE,
          numericInput("design_mean",
            lab("Expected effect (design prior mean)",
                "Your best guess of the true difference in means (treatment minus control), in the outcome's units."),
            value = 5),
          numericInput("design_sd",
            lab("Uncertainty about the effect (design prior SD)",
                "How unsure you are. About 95% of the effects you consider plausible lie within mean \u00B1 2 SD. Must be > 0."),
            value = 3, min = 0)
        ),
        # risk difference
        conditionalPanel(
          JS_RD,
          numericInput("rd_design_mean",
            lab("Expected risk difference (percentage points)",
                "Treatment-group event rate minus control-group event rate, in percentage points. -10 means 10 points fewer events with treatment."),
            value = -10),
          numericInput("rd_design_sd",
            lab("Uncertainty (SD, percentage points)",
                "About 95% of the differences you consider plausible lie within mean \u00B1 2 SD."),
            value = 5, min = 0)
        ),
        # ratios
        conditionalPanel(
          JS_RATIO,
          tags$label(class = "form-label",
            lab("Plausible range for the true ratio (95%)",
                "The range you are 95% sure contains the true ratio (treatment vs control). Below 1 = fewer events / lower hazard with treatment. The best guess is the geometric midpoint.")),
          layout_columns(
            col_widths = c(6, 6), gap = "0.5rem",
            numericInput("ratio_design_lo", "From", value = 0.5, min = 0, step = 0.05),
            numericInput("ratio_design_hi", "To", value = 1.0, min = 0, step = 0.05)
          )
        ),
        hint("The design prior describes what you believe; it is used only to plan the study."),

        checkboxInput("same_prior", "Analyse with the same prior", value = TRUE),
        hint("Simple, but it builds your optimism into the final analysis. Untick to use a sceptical or vague analysis prior."),
        conditionalPanel(
          "!input.same_prior",
          conditionalPanel(
            JS_ADDITIVE,
            numericInput("analysis_mean",
              lab("Analysis prior mean",
                  "Centre of the prior used when the trial is analysed. 0 gives a 'sceptical' prior centred on no effect."),
              value = 0),
            numericInput("analysis_sd",
              lab("Analysis prior SD",
                  "Spread of the analysis prior. A very large value (e.g. 1000) lets the data speak for themselves, which is close to a frequentist analysis."),
              value = 10, min = 0)
          ),
          conditionalPanel(
            JS_RD,
            numericInput("rd_analysis_mean",
              lab("Analysis prior mean (percentage points)", "0 = sceptical, centred on no difference."),
              value = 0),
            numericInput("rd_analysis_sd",
              lab("Analysis prior SD (percentage points)", "Large (e.g. 50) = vague."),
              value = 20, min = 0)
          ),
          conditionalPanel(
            JS_RATIO,
            tags$label(class = "form-label",
              lab("Analysis prior: 95% range for the ratio",
                  "A range centred on 1 (e.g. 0.5 to 2) is a sceptical prior; a very wide one (e.g. 0.01 to 100) is vague.")),
            layout_columns(
              col_widths = c(6, 6), gap = "0.5rem",
              numericInput("ratio_analysis_lo", "From", value = 0.5, min = 0, step = 0.05),
              numericInput("ratio_analysis_hi", "To", value = 2.0, min = 0, step = 0.05)
            )
          )
        )
      ),

      # ---- Design ------------------------------------------------------
      accordion_panel(
        "Trial design",
        tags$label(class = "form-label",
                   lab("Sample sizes to evaluate (per group)",
                       "Participants in each group (analysable ones, except for time-to-event outcomes, where everyone enrolled is analysed). With unequal allocation these are control-group sizes. Every value from minimum to maximum, in the given steps, is evaluated.")),
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
        conditionalPanel(
          "input.otype != 'surv'",
          numericInput("dropout",
            lab("Expected dropout (%)",
                "Share of enrolled participants you expect to lose. It does not change the probabilities; it only inflates the number to enrol."),
            value = 0, min = 0, max = 90, step = 5)
        )
      ),

      # ---- Success -----------------------------------------------------
      accordion_panel(
        "What counts as success",
        radioButtons("alt",
          lab("Test direction",
              "Two-sided counts a clear difference in either direction. One-sided only counts a result in the stated direction (for ratios: '<' means a ratio below the threshold, e.g. a hazard ratio below 1)."),
          choices = c("Two-sided" = "two.sided",
                      "One-sided: treatment > control" = "greater",
                      "One-sided: treatment < control" = "less"),
          selected = "two.sided"),
        conditionalPanel(
          "input.alt != 'two.sided'",
          conditionalPanel(
            JS_ADDITIVE,
            numericInput("threshold",
              lab("Success threshold",
                  "0 = ordinary superiority. A value beyond 0 in the tested direction demands a clinically meaningful effect; a value on the other side gives a non-inferiority design (the non-inferiority margin)."),
              value = 0)
          ),
          conditionalPanel(
            JS_RD,
            numericInput("rd_threshold",
              lab("Success threshold (percentage points)",
                  "0 = ordinary superiority. E.g. -5 with 'treatment < control' requires showing at least 5 points fewer events; +5 would be a non-inferiority margin."),
              value = 0)
          ),
          conditionalPanel(
            JS_RATIO,
            numericInput("ratio_threshold",
              lab("Success threshold (ratio)",
                  "1 = ordinary superiority. E.g. 0.8 with 'treatment < control' requires showing the ratio is below 0.8; 1.3 would be a non-inferiority margin."),
              value = 1, min = 0, step = 0.05)
          ),
          uiOutput("threshold_hint")
        ),
        numericInput("alpha",
          lab("Significance threshold (alpha)",
              "Frequentist: the p-value cut-off. Bayesian: success if the posterior probability of an effect beyond the threshold in the tested direction exceeds 1 - alpha (1 - alpha/2 per direction for two-sided)."),
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
              "Simulation generates thousands of virtual trials (slower, with a little random error). Exact uses closed-form formulas or numerical integration (instant). They agree closely except with very few events."),
          choices = c("Simulation of virtual trials" = "sim",
                      "Exact formula (instant)" = "exact"),
          selected = "sim"),
        conditionalPanel(
          "input.engine == 'sim'",
          numericInput("mc_iter",
            lab("Simulated trials per sample size",
                "More simulations give a smoother, more precise curve but take longer. The error is at most about 1/sqrt(number) (95% interval)."),
            value = 2000, min = 100, max = 100000, step = 500),
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
  )
}
