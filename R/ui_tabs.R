# =============================================================================
# Main panel: floating progress box and the tabs (bilingual: see R/i18n.R).
# Tab values stay in English (the server refers to them); only the visible
# titles are translated.
# =============================================================================

progress_box_ui <- function() {
  # Floating progress box with a Stop button, shown while a simulation runs.
  # It lives outside the sidebar because the sidebar's CSS would trap it.
  conditionalPanel(
    "output.running",
    div(class = "card shadow",
        style = "position: fixed; bottom: 20px; right: 20px; width: 340px; z-index: 2000;",
        div(class = "card-body",
            strong(tt("Simulating virtual trials", "Simulando ensaios virtuais")),
            uiOutput("run_progress"),
            actionButton("stop", tt("Stop run", "Parar"), class = "btn-danger btn-sm w-100 mt-2"),
            tags$small(class = "d-block text-muted mt-2",
                       tt("You can keep using the other tabs meanwhile. Pressing Calculate again restarts with the current inputs.",
                          "Voc\u00EA pode continuar usando as outras abas. Apertar Calcular de novo reinicia com as entradas atuais."))))
  )
}

results_tab <- function() {
  nav_panel(
    tt("Results", "Resultados"), value = "Results",
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
      value_box(title = tt("Assurance ceiling (unlimited n)", "Teto da assurance (n ilimitado)"),
                value = textOutput("vb_ceiling", inline = TRUE),
                tags$small(tt("Highest assurance any sample size can reach",
                              "A maior assurance que algum tamanho amostral pode atingir")),
                theme = "light")
    ),
    uiOutput("model_warnings"),
    card(
      card_header(tt("Probability of success by sample size", "Probabilidade de sucesso por tamanho amostral")),
      plotlyOutput("curve", height = "440px"),
      card_footer(tags$small(textOutput("curve_note", inline = TRUE)))
    ),
    card(
      card_header(tt("Summary in plain language", "Resumo em linguagem simples")),
      uiOutput("summary")
    ),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        tt("Results table", "Tabela de resultados"),
        div(
          downloadButton("dl_csv", "CSV", class = "btn-sm btn-outline-primary"),
          downloadButton("dl_png", tt("Plot (PNG)", "Gr\u00E1fico (PNG)"), class = "btn-sm btn-outline-primary"),
          downloadButton("dl_report", tt("Report (.txt)", "Relat\u00F3rio (.txt)"), class = "btn-sm btn-outline-primary")
        )
      ),
      DTOutput("results_table")
    ),
    card(
      card_header(tt("Save this run as a scenario to compare", "Salvar esta execu\u00E7\u00E3o como cen\u00E1rio para comparar")),
      layout_columns(
        col_widths = c(8, 4),
        textInput("scenario_label", NULL, placeholder = "Scenario name (optional)"),
        actionButton("save_scenario", tt("Save scenario", "Salvar cen\u00E1rio"), class = "btn-outline-primary w-100")
      )
    )
  )
}

priors_tab <- function() {
  nav_panel(
    tt("Priors", "Prioris"), value = "Priors",
    card(
      card_header(tt("Your design prior (and analysis prior, if different)",
                     "Sua priori de planejamento (e a de an\u00E1lise, se diferente)")),
      plotlyOutput("prior_plot", height = "400px"),
      uiOutput("prior_text")
    )
  )
}

conditional_tab <- function() {
  nav_panel(
    tt("Success vs true effect", "Sucesso vs efeito verdadeiro"), value = "Success vs true effect",
    card(
      card_header(tt("How likely is success if the true effect were exactly x?",
                     "Qual a chance de sucesso se o efeito verdadeiro fosse exatamente x?")),
      numericInput("cond_n", tt("Sample size (per group / control group)", "Tamanho amostral (por grupo / grupo controle)"),
                   value = 200, min = 2, step = 1, width = "260px"),
      plotlyOutput("cond_plot", height = "420px"),
      tp(paste("The solid blue curve is the chance that the Bayesian analysis declares success if the true",
               "effect were exactly the value on the x-axis; the dashed orange curve is the same for the",
               "frequentist test. Frequentist power reads the orange curve at a single point (the design",
               "prior's centre). Assurance is the blue curve averaged over the grey design prior, so effects",
               "you consider plausible but small pull it down."),
         paste("A curva azul cont\u00EDnua \u00E9 a chance de a an\u00E1lise bayesiana declarar sucesso se o efeito",
               "verdadeiro fosse exatamente o valor no eixo x; a curva laranja tracejada \u00E9 o mesmo para o",
               "teste frequentista. O poder frequentista l\u00EA a curva laranja em um \u00FAnico ponto (o centro da",
               "priori de planejamento). A assurance \u00E9 a m\u00E9dia da curva azul sobre a priori de planejamento",
               "(em cinza), por isso efeitos plaus\u00EDveis por\u00E9m pequenos a puxam para baixo."),
         class = "mt-2"),
      uiOutput("cond_text")
    )
  )
}

sensitivity_tab <- function() {
  nav_panel(
    tt("Sensitivity", "Sensibilidade"), value = "Sensitivity",
    card(
      card_header(tt("How much do the results depend on your assumptions?",
                     "Quanto os resultados dependem das suas suposi\u00E7\u00F5es?")),
      numericInput("sens_n", tt("Sample size (per group / control group)", "Tamanho amostral (por grupo / grupo controle)"),
                   value = 200, min = 2, step = 1, width = "260px"),
      tp("Values here use the exact formula, so they update instantly and carry no simulation noise.",
         "Os valores aqui usam a f\u00F3rmula exata, por isso se atualizam na hora e n\u00E3o t\u00EAm ru\u00EDdo de simula\u00E7\u00E3o."),
      layout_columns(
        col_widths = c(6, 6),
        div(h6(tt("Assurance across design priors", "Assurance para outras prioris de planejamento")),
            plotlyOutput("sens_heat", height = "400px"),
            tags$small(tt("The orange x marks your inputs. Contours show assurance. A larger expected effect in the tested direction, or more certainty (lower on the chart), usually helps.",
                          "O x laranja marca as suas entradas. As curvas de n\u00EDvel mostram a assurance. Um efeito esperado maior na dire\u00E7\u00E3o testada, ou mais certeza (mais abaixo no gr\u00E1fico), geralmente ajuda."))),
        div(h6(tt("Assurance across analysis priors", "Assurance para outras prioris de an\u00E1lise")),
            plotlyOutput("sens_asd", height = "400px"),
            tags$small(tt("Far right = vague analysis prior (data-driven analysis). The dashed vertical line marks your current analysis prior SD.",
                          "Extremo direito = priori de an\u00E1lise vaga (an\u00E1lise guiada pelos dados). A linha vertical tracejada marca o DP atual da sua priori de an\u00E1lise.")))
      ),
      hr(),
      h6(textOutput("sens_nuis_title", inline = TRUE)),
      plotlyOutput("sens_nuis", height = "360px"),
      tags$small(tt("How assurance (solid) and power (dashed) change if this assumption is wrong. The dashed vertical line marks your current value.",
                    "Como a assurance (cont\u00EDnua) e o poder (tracejado) mudam se esta suposi\u00E7\u00E3o estiver errada. A linha vertical tracejada marca o seu valor atual."))
    )
  )
}

compare_tab <- function() {
  nav_panel(
    tt("Compare scenarios", "Comparar cen\u00E1rios"), value = "Compare scenarios",
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        tt("Saved scenarios", "Cen\u00E1rios salvos"),
        div(
          actionButton("remove_last", tt("Remove last", "Remover o \u00FAltimo"), class = "btn-sm btn-outline-secondary"),
          actionButton("clear_scenarios", tt("Clear all", "Limpar tudo"), class = "btn-sm btn-outline-danger")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        checkboxInput("cmp_power", tt("Also show frequentist power (dashed)", "Mostrar tamb\u00E9m o poder frequentista (tracejado)"), FALSE),
        checkboxInput("cmp_total", tt("x-axis: total sample size (both groups)", "Eixo x: tamanho amostral total (dois grupos)"), FALSE)
      ),
      uiOutput("cmp_empty"),
      plotlyOutput("cmp_plot", height = "420px"),
      div(style = "overflow-x: auto;", tableOutput("cmp_table"))
    )
  )
}

prompt_tab <- function() {
  nav_panel(
    tt("LLM prompt helper", "Assistente de prompt (IA)"), value = "LLM prompt helper",
    tp(paste("Not sure what to enter? Describe your project below. The app writes a prompt you can paste into an AI",
             "assistant (ideally one that can search the web) to research evidence-based values for every input.",
             "Then paste the assistant's answer back in step 3 to fill in the app."),
       paste("N\u00E3o sabe o que informar? Descreva seu projeto abaixo. O aplicativo escreve um prompt para voc\u00EA colar em",
             "um assistente de IA (de prefer\u00EAncia um que pesquise na web) para buscar valores baseados em evid\u00EAncias",
             "para cada entrada. Depois cole a resposta do assistente no passo 3 para preencher o aplicativo.",
             "O prompt fica em ingl\u00EAs, o que costuma funcionar melhor com assistentes de IA; escolha abaixo o idioma da resposta.")),
    div(class = "alert alert-warning py-2 small",
        tt(tagList(strong("Check before you trust:"), " AI assistants can make mistakes or cite sources that do not exist. Verify the key numbers and references, and don't paste confidential details into a tool your institution has not approved."),
           tagList(strong("Confira antes de confiar:"), " assistentes de IA podem errar ou citar fontes que n\u00E3o existem. Verifique os n\u00FAmeros e as refer\u00EAncias principais, e n\u00E3o cole informa\u00E7\u00F5es confidenciais em ferramentas n\u00E3o aprovadas pela sua institui\u00E7\u00E3o."))),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header(tt("1. Describe your project", "1. Descreva seu projeto")),
        uiOutput("pg_type_note"),
        textInput("pg_title", tt("Study title or working name", "T\u00EDtulo do estudo ou nome provis\u00F3rio"), width = "100%"),
        textInput("pg_condition", tt("Condition and population", "Condi\u00E7\u00E3o e popula\u00E7\u00E3o"),
                  placeholder = "e.g. adults with uncontrolled hypertension", width = "100%"),
        layout_columns(
          col_widths = c(6, 6),
          textInput("pg_intervention", tt("Intervention", "Interven\u00E7\u00E3o"), placeholder = "e.g. drug X 10 mg daily"),
          textInput("pg_comparator", tt("Comparator", "Comparador"), placeholder = "e.g. placebo, usual care")
        ),
        textInput("pg_outcome", tt("Primary outcome (and units or event definition)", "Desfecho prim\u00E1rio (e unidades ou defini\u00E7\u00E3o do evento)"),
                  placeholder = "e.g. systolic BP (mmHg); death from any cause", width = "100%"),
        layout_columns(
          col_widths = c(6, 6),
          textInput("pg_timepoint", tt("Time point / follow-up", "Momento / seguimento"), placeholder = "e.g. 12 weeks"),
          selectInput("pg_phase", tt("Study type", "Tipo de estudo"),
                      c("Pilot / feasibility", "Phase II", "Phase III / confirmatory",
                        "Pragmatic / effectiveness", "Non-inferiority", "Other / not sure"),
                      selected = "Phase III / confirmatory", selectize = FALSE)
        ),
        radioButtons("pg_direction", tt("Which outcome values are better?", "Quais valores do desfecho s\u00E3o melhores?"),
                     choiceNames = list(tt("Higher is better", "Maior \u00E9 melhor"), tt("Lower is better", "Menor \u00E9 melhor"),
                                        tt("Not sure", "N\u00E3o sei")),
                     choiceValues = c("higher", "lower", "unsure"), selected = "unsure", inline = TRUE),
        layout_columns(
          col_widths = c(6, 6),
          textInput("pg_setting", tt("Setting / country", "Cen\u00E1rio / pa\u00EDs"), placeholder = "e.g. primary care, Brazil"),
          textInput("pg_mcid", tt("Meaningful difference (if known)", "Diferen\u00E7a relevante (se souber)"), placeholder = "e.g. 5 mmHg; HR 0.8")
        ),
        textAreaInput("pg_known", tt("Evidence you already know about (optional)", "Evid\u00EAncias que voc\u00EA j\u00E1 conhece (opcional)"), rows = 3,
                      placeholder = "Pilot results, key trials, meta-analyses, DOIs...",
                      width = "100%"),
        textAreaInput("pg_constraints", tt("Practical constraints (optional)", "Restri\u00E7\u00F5es pr\u00E1ticas (opcional)"), rows = 2,
                      placeholder = "e.g. can recruit at most 150 per arm in 2 years",
                      width = "100%"),
        layout_columns(
          col_widths = c(6, 6),
          div(checkboxInput("pg_web", tt("The AI assistant can search the web", "O assistente de IA pode pesquisar na web"), TRUE),
              checkboxInput("pg_current", tt("Include my current app inputs", "Incluir minhas entradas atuais"), FALSE)),
          selectInput("pg_language", tt("Answer language", "Idioma da resposta"),
                      c("English", "Portuguese (Brazil)", "Spanish", "French", "German"), selectize = FALSE)
        )
      ),
      div(
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            tt("2. Copy this prompt into your AI assistant", "2. Copie este prompt para o seu assistente de IA"),
            div(
              tags$button(
                id = "pg_copy", type = "button", class = "btn btn-sm btn-primary",
                onclick = paste0(
                  "var b=this,pt=document.documentElement.getAttribute('data-lang')==='pt',old=b.innerHTML;",
                  "navigator.clipboard.writeText(document.getElementById('pg_prompt').innerText).then(function(){",
                  "b.textContent=pt?'Copiado!':'Copied!';setTimeout(function(){b.innerHTML=old;},1500);",
                  "},function(){b.textContent=pt?'Selecione o texto e copie manualmente':'Select the text and copy it manually';});"),
                tt("Copy prompt", "Copiar prompt")),
              downloadButton("dl_prompt", tt("Download (.txt)", "Baixar (.txt)"), class = "btn-sm btn-outline-primary")
            )
          ),
          tags$style("#pg_prompt { white-space: pre-wrap; max-height: 420px; overflow-y: auto; font-size: 0.8rem; }"),
          verbatimTextOutput("pg_prompt")
        ),
        card(
          card_header(tt("3. Paste the assistant's answer to fill in the app",
                         "3. Cole a resposta do assistente para preencher o aplicativo")),
          p(class = "small mb-1",
            tt("Paste the whole answer, or just its JSON block. Only recognised, valid values are applied; you can review them in the sidebar before pressing Calculate.",
               "Cole a resposta inteira, ou s\u00F3 o bloco JSON. Apenas valores reconhecidos e v\u00E1lidos s\u00E3o aplicados; voc\u00EA pode revis\u00E1-los na barra lateral antes de apertar Calcular.")),
          textAreaInput("pg_answer", NULL, rows = 6, width = "100%",
                        placeholder = "{ \"otype\": \"cont\", \"design_mean\": 5, \"design_sd\": 3, ... }"),
          actionButton("pg_apply", tt("Apply values to the app", "Aplicar valores no aplicativo"), class = "btn-primary"),
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
      nav_panel(tt("Methods & help", "M\u00E9todos e ajuda"), value = "Methods & help", card(methods_ui()))
    )
  )
}
