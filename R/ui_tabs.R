# =============================================================================
# Main panel: floating progress box and the tabs.
# =============================================================================

progress_box_ui <- function() {
  # Floating progress box with a Stop button, shown while a simulation runs.
  # It lives outside the sidebar because the sidebar's CSS would trap it.
  conditionalPanel(
    "output.running",
    div(class = "card shadow",
        style = "position: fixed; bottom: 20px; right: 20px; width: 340px; z-index: 2000;",
        div(class = "card-body",
            strong("Simulating virtual trials"),
            uiOutput("run_progress"),
            actionButton("stop", "Stop run", class = "btn-danger btn-sm w-100 mt-2"),
            tags$small(class = "d-block text-muted mt-2",
                       "You can keep using the other tabs meanwhile. Pressing",
                       "Calculate again restarts with the current inputs.")))
  )
}

results_tab <- function() {
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
    uiOutput("model_warnings"),
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
  )
}

priors_tab <- function() {
  nav_panel(
    "Priors",
    card(
      card_header("Your design prior (and analysis prior, if different)"),
      plotlyOutput("prior_plot", height = "400px"),
      uiOutput("prior_text")
    )
  )
}

conditional_tab <- function() {
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
        "the x-axis; the dashed orange curve is the same for the frequentist",
        "test. Frequentist power reads the orange curve at a single point (the",
        "design prior's centre). Assurance is the blue curve averaged over the",
        "grey design prior, so effects you consider plausible but small pull",
        "it down."),
      uiOutput("cond_text")
    )
  )
}

sensitivity_tab <- function() {
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
                       "assurance. A larger expected effect in the tested",
                       "direction, or more certainty (lower on the chart),",
                       "usually helps.")),
        div(h6("Assurance across analysis priors"),
            plotlyOutput("sens_asd", height = "400px"),
            tags$small("Far right = vague analysis prior (data-driven",
                       "analysis). The dashed vertical line marks your",
                       "current analysis prior SD."))
      ),
      hr(),
      h6(textOutput("sens_nuis_title", inline = TRUE)),
      plotlyOutput("sens_nuis", height = "360px"),
      tags$small("How assurance (solid) and power (dashed) change if this",
                 "assumption is wrong. The dashed vertical line marks your",
                 "current value.")
    )
  )
}

compare_tab <- function() {
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
      div(style = "overflow-x: auto;", tableOutput("cmp_table"))
    )
  )
}

prompt_tab <- function() {
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
        uiOutput("pg_type_note"),
        textInput("pg_title", "Study title or working name", width = "100%"),
        textInput("pg_condition", "Condition and population",
                  placeholder = "e.g. adults with uncontrolled hypertension", width = "100%"),
        layout_columns(
          col_widths = c(6, 6),
          textInput("pg_intervention", "Intervention", placeholder = "e.g. drug X 10 mg daily"),
          textInput("pg_comparator", "Comparator", placeholder = "e.g. placebo, usual care")
        ),
        textInput("pg_outcome", "Primary outcome (and units or event definition)",
                  placeholder = "e.g. systolic BP (mmHg); death from any cause", width = "100%"),
        layout_columns(
          col_widths = c(6, 6),
          textInput("pg_timepoint", "Time point / follow-up", placeholder = "e.g. 12 weeks"),
          selectInput("pg_phase", "Study type",
                      c("Pilot / feasibility", "Phase II", "Phase III / confirmatory",
                        "Pragmatic / effectiveness", "Non-inferiority", "Other / not sure"),
                      selected = "Phase III / confirmatory")
        ),
        radioButtons("pg_direction", "Which outcome values are better?",
                     c("Higher is better" = "higher", "Lower is better" = "lower",
                       "Not sure" = "unsure"), selected = "unsure", inline = TRUE),
        layout_columns(
          col_widths = c(6, 6),
          textInput("pg_setting", "Setting / country", placeholder = "e.g. primary care, Brazil"),
          textInput("pg_mcid", "Meaningful difference (if known)", placeholder = "e.g. 5 mmHg; HR 0.8")
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
                        placeholder = "{ \"otype\": \"cont\", \"design_mean\": 5, \"design_sd\": 3, ... }"),
          actionButton("pg_apply", "Apply values to the app", class = "btn-primary"),
          uiOutput("pg_apply_result")
        )
      )
    )
  )
}

main_ui <- function() {
  tagList(
    progress_box_ui(),
    navset_card_underline(
      id = "tabs",
      results_tab(),
      priors_tab(),
      conditional_tab(),
      sensitivity_tab(),
      compare_tab(),
      prompt_tab(),
      nav_panel("Methods & help", card(methods_ui()))
    )
  )
}
