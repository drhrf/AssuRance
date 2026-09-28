# =============================================================================
# Sidebar inputs (bilingual: see R/i18n.R). Inputs that only apply to some
# outcome types sit inside conditionalPanel()s; the JavaScript conditions
# below decide which "scale family" is active:
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
# Bilingual label + tooltip.
lab2 <- function(en, pt, tip_en, tip_pt) lab(tt(en, pt), tt(tip_en, tip_pt))

# Small grey help line under an input.
hint <- function(...) tags$div(class = "form-text mb-3", style = "margin-top:-0.6rem;", ...)
hint2 <- function(en, pt) hint(tt(en, pt))

sidebar_ui <- function() {
  sidebar(
    width = 380,
    accordion(
      multiple = TRUE,
      open = c("outcome", "priors", "design"),

      # ---- Outcome type --------------------------------------------------
      accordion_panel(
        tt("Outcome", "Desfecho"), value = "outcome",
        selectInput("otype",
          lab2("Type of primary outcome", "Tipo de desfecho prim\u00E1rio",
               "Continuous: a measurement such as blood pressure. ANCOVA: the same, analysed adjusting for its baseline value. Binary: an event that happens or not (e.g. response, death by 1 year). Time to event: how long until an event, allowing for censoring.",
               "Cont\u00EDnuo: uma medida como a press\u00E3o arterial. ANCOVA: o mesmo, analisado com ajuste pelo valor basal. Bin\u00E1rio: um evento que ocorre ou n\u00E3o (ex.: resposta, \u00F3bito em 1 ano). Tempo at\u00E9 o evento: quanto tempo at\u00E9 um evento, considerando a censura."),
          c("Continuous: difference in means" = "cont",
            "Continuous, adjusted for baseline (ANCOVA)" = "ancova",
            "Binary (event yes / no)" = "binary",
            "Time to event (survival)" = "surv"),
          selectize = FALSE),

        # continuous
        conditionalPanel(
          JS_ADDITIVE,
          numericInput("sigma",
            lab2("Outcome standard deviation", "Desvio padr\u00E3o do desfecho",
                 "Person-to-person variability of the outcome within each group (pooled), assumed known. Must be > 0.",
                 "Variabilidade do desfecho entre pessoas dentro de cada grupo (combinada), considerada conhecida. Deve ser > 0."),
            value = 10, min = 0)
        ),
        conditionalPanel(
          "input.otype == 'ancova'",
          numericInput("rho",
            lab2("Correlation between baseline and outcome", "Correla\u00E7\u00E3o entre basal e desfecho",
                 "How strongly the baseline measurement predicts the final one (0 to 0.95). Adjusting for baseline shrinks the residual SD by sqrt(1 - rho^2). Typical values are 0.4 to 0.7.",
                 "O quanto a medida basal prev\u00EA a final (0 a 0,95). Ajustar pelo basal reduz o DP residual por sqrt(1 - rho^2). Valores t\u00EDpicos: 0,4 a 0,7."),
            value = 0.5, min = 0, max = 0.95, step = 0.05)
        ),

        # binary
        conditionalPanel(
          "input.otype == 'binary'",
          radioButtons("measure",
            lab2("Effect measure", "Medida de efeito",
                 "Odds ratio: the usual choice for logistic regression. Risk ratio: relative change in risk. Risk difference: absolute change in percentage points.",
                 "Raz\u00E3o de chances: a escolha usual na regress\u00E3o log\u00EDstica. Risco relativo: mudan\u00E7a relativa no risco. Diferen\u00E7a de riscos: mudan\u00E7a absoluta em pontos percentuais."),
            choiceNames = list(tt("Odds ratio", "Raz\u00E3o de chances"), tt("Risk ratio", "Risco relativo"),
                               tt("Risk difference", "Diferen\u00E7a de riscos")),
            choiceValues = c("or", "rr", "rd"), inline = TRUE),
          numericInput("p_control",
            lab2("Event rate in the control group (%)", "Taxa de eventos no grupo controle (%)",
                 "The percentage of control-group participants expected to have the event.",
                 "A porcentagem de participantes do grupo controle que devem ter o evento."),
            value = 30, min = 0.1, max = 99.9, step = 1)
        ),

        # survival
        conditionalPanel(
          "input.otype == 'surv'",
          layout_columns(
            col_widths = c(7, 5), gap = "0.5rem",
            numericInput("surv_median",
              lab2("Control-group median time to event", "Mediana do tempo at\u00E9 o evento no controle",
                   "Half of control-group participants have had the event by this time (exponential survival is assumed).",
                   "Metade dos participantes do grupo controle teve o evento at\u00E9 esse tempo (sup\u00F5e-se sobrevida exponencial)."),
              value = 12, min = 0),
            textInput("time_unit", tt("Time unit", "Unidade de tempo"), value = "months")
          ),
          layout_columns(
            col_widths = c(6, 6), gap = "0.5rem",
            numericInput("accrual",
              lab2("Recruitment period", "Per\u00EDodo de recrutamento",
                   "How long recruitment lasts; participants are assumed to join evenly over this period.",
                   "Quanto dura o recrutamento; sup\u00F5e-se que os participantes entram de forma uniforme nesse per\u00EDodo."),
              value = 12, min = 0),
            numericInput("followup",
              lab2("Extra follow-up", "Seguimento adicional",
                   "Follow-up after the last participant joins. The analysis happens at recruitment period + extra follow-up.",
                   "Seguimento ap\u00F3s a entrada do \u00FAltimo participante. A an\u00E1lise acontece em recrutamento + seguimento adicional."),
              value = 12, min = 0)
          ),
          numericInput("loss_pct",
            lab2("Lost to follow-up by the end (%)", "Perda de seguimento at\u00E9 o fim (%)",
                 "Share of participants expected to drop out (and be censored) before having the event, over an average follow-up. They still count as enrolled.",
                 "Propor\u00E7\u00E3o de participantes que devem abandonar (e ser censurados) antes de ter o evento, em um seguimento m\u00E9dio. Eles continuam contando como recrutados."),
            value = 5, min = 0, max = 90, step = 1)
        ),
        uiOutput("engine_note")
      ),

      # ---- Priors ------------------------------------------------------
      accordion_panel(
        tt("Effect and priors", "Efeito e prioris"), value = "priors",
        uiOutput("effect_hint"),

        # additive (continuous)
        conditionalPanel(
          JS_ADDITIVE,
          numericInput("design_mean",
            lab2("Expected effect (design prior mean)", "Efeito esperado (m\u00E9dia da priori de planejamento)",
                 "Your best guess of the true difference in means (treatment minus control), in the outcome's units.",
                 "Seu melhor palpite para a verdadeira diferen\u00E7a de m\u00E9dias (tratamento menos controle), nas unidades do desfecho."),
            value = 5),
          numericInput("design_sd",
            lab2("Uncertainty about the effect (design prior SD)", "Incerteza sobre o efeito (DP da priori de planejamento)",
                 "How unsure you are. About 95% of the effects you consider plausible lie within mean \u00B1 2 SD. Must be > 0.",
                 "O quanto voc\u00EA est\u00E1 incerto. Cerca de 95% dos efeitos que voc\u00EA considera plaus\u00EDveis ficam em m\u00E9dia \u00B1 2 DP. Deve ser > 0."),
            value = 3, min = 0)
        ),
        # risk difference
        conditionalPanel(
          JS_RD,
          numericInput("rd_design_mean",
            lab2("Expected risk difference (percentage points)", "Diferen\u00E7a de riscos esperada (pontos percentuais)",
                 "Treatment-group event rate minus control-group event rate, in percentage points. -10 means 10 points fewer events with treatment.",
                 "Taxa de eventos no tratamento menos taxa no controle, em pontos percentuais. -10 significa 10 pontos a menos de eventos com o tratamento."),
            value = -10),
          numericInput("rd_design_sd",
            lab2("Uncertainty (SD, percentage points)", "Incerteza (DP, pontos percentuais)",
                 "About 95% of the differences you consider plausible lie within mean \u00B1 2 SD.",
                 "Cerca de 95% das diferen\u00E7as que voc\u00EA considera plaus\u00EDveis ficam em m\u00E9dia \u00B1 2 DP."),
            value = 5, min = 0)
        ),
        # ratios
        conditionalPanel(
          JS_RATIO,
          tags$label(class = "form-label",
            lab2("Plausible range for the true ratio (95%)", "Intervalo plaus\u00EDvel para a raz\u00E3o verdadeira (95%)",
                 "The range you are 95% sure contains the true ratio (treatment vs control). Below 1 = fewer events / lower hazard with treatment. The best guess is the geometric midpoint.",
                 "O intervalo que voc\u00EA tem 95% de certeza de conter a raz\u00E3o verdadeira (tratamento vs controle). Abaixo de 1 = menos eventos / menor risco com o tratamento. O melhor palpite \u00E9 o ponto m\u00E9dio geom\u00E9trico.")),
          layout_columns(
            col_widths = c(6, 6), gap = "0.5rem",
            numericInput("ratio_design_lo", tt("From", "De"), value = 0.5, min = 0, step = 0.05),
            numericInput("ratio_design_hi", tt("To", "At\u00E9"), value = 1.0, min = 0, step = 0.05)
          )
        ),
        hint2("The design prior describes what you believe; it is used only to plan the study.",
              "A priori de planejamento descreve aquilo em que voc\u00EA acredita; ela serve apenas para planejar o estudo."),

        checkboxInput("same_prior", tt("Analyse with the same prior", "Analisar com a mesma priori"), value = TRUE),
        hint2("Simple, but it builds your optimism into the final analysis. Untick to use a sceptical or vague analysis prior.",
              "Simples, mas incorpora o seu otimismo na an\u00E1lise final. Desmarque para usar uma priori de an\u00E1lise c\u00E9tica ou vaga."),
        conditionalPanel(
          "!input.same_prior",
          conditionalPanel(
            JS_ADDITIVE,
            numericInput("analysis_mean",
              lab2("Analysis prior mean", "M\u00E9dia da priori de an\u00E1lise",
                   "Centre of the prior used when the trial is analysed. 0 gives a 'sceptical' prior centred on no effect.",
                   "Centro da priori usada na an\u00E1lise do ensaio. 0 d\u00E1 uma priori 'c\u00E9tica', centrada no efeito nulo."),
              value = 0),
            numericInput("analysis_sd",
              lab2("Analysis prior SD", "DP da priori de an\u00E1lise",
                   "Spread of the analysis prior. A very large value (e.g. 1000) lets the data speak for themselves, which is close to a frequentist analysis.",
                   "Dispers\u00E3o da priori de an\u00E1lise. Um valor muito grande (ex.: 1000) deixa os dados falarem por si, o que se aproxima de uma an\u00E1lise frequentista."),
              value = 10, min = 0)
          ),
          conditionalPanel(
            JS_RD,
            numericInput("rd_analysis_mean",
              lab2("Analysis prior mean (percentage points)", "M\u00E9dia da priori de an\u00E1lise (pontos percentuais)",
                   "0 = sceptical, centred on no difference.", "0 = c\u00E9tica, centrada em nenhuma diferen\u00E7a."),
              value = 0),
            numericInput("rd_analysis_sd",
              lab2("Analysis prior SD (percentage points)", "DP da priori de an\u00E1lise (pontos percentuais)",
                   "Large (e.g. 50) = vague.", "Grande (ex.: 50) = vaga."),
              value = 20, min = 0)
          ),
          conditionalPanel(
            JS_RATIO,
            tags$label(class = "form-label",
              lab2("Analysis prior: 95% range for the ratio", "Priori de an\u00E1lise: intervalo de 95% para a raz\u00E3o",
                   "A range centred on 1 (e.g. 0.5 to 2) is a sceptical prior; a very wide one (e.g. 0.01 to 100) is vague.",
                   "Um intervalo centrado em 1 (ex.: 0,5 a 2) \u00E9 uma priori c\u00E9tica; um muito largo (ex.: 0,01 a 100) \u00E9 vago.")),
            layout_columns(
              col_widths = c(6, 6), gap = "0.5rem",
              numericInput("ratio_analysis_lo", tt("From", "De"), value = 0.5, min = 0, step = 0.05),
              numericInput("ratio_analysis_hi", tt("To", "At\u00E9"), value = 2.0, min = 0, step = 0.05)
            )
          )
        )
      ),

      # ---- Design ------------------------------------------------------
      accordion_panel(
        tt("Trial design", "Desenho do ensaio"), value = "design",
        tags$label(class = "form-label",
                   lab2("Sample sizes to evaluate (per group)", "Tamanhos amostrais a avaliar (por grupo)",
                        "Participants in each group (analysable ones, except for time-to-event outcomes, where everyone enrolled is analysed). With unequal allocation these are control-group sizes. Every value from minimum to maximum, in the given steps, is evaluated.",
                        "Participantes em cada grupo (os analis\u00E1veis, exceto em desfechos de tempo at\u00E9 o evento, em que todos os recrutados s\u00E3o analisados). Com aloca\u00E7\u00E3o desigual, s\u00E3o os tamanhos do grupo controle. Todos os valores do m\u00EDnimo ao m\u00E1ximo, no passo indicado, s\u00E3o avaliados.")),
        layout_columns(
          col_widths = c(4, 4, 4), gap = "0.5rem",
          numericInput("n_min", tt("Min", "M\u00EDn."), value = 20, min = 2, step = 1),
          numericInput("n_max", tt("Max", "M\u00E1x."), value = 200, min = 3, step = 1),
          numericInput("n_step", tt("Step", "Passo"), value = 10, min = 1, step = 1)
        ),
        numericInput("ratio",
          lab2("Allocation ratio (treatment : control)", "Raz\u00E3o de aloca\u00E7\u00E3o (tratamento : controle)",
               "1 = equal groups. 2 = two treated participants for every control. The treatment-group size is ratio x control size.",
               "1 = grupos iguais. 2 = dois participantes tratados para cada controle. O tamanho do grupo tratamento \u00E9 raz\u00E3o x tamanho do controle."),
          value = 1, min = 0.1, max = 10, step = 0.5),
        conditionalPanel(
          "input.otype != 'surv'",
          numericInput("dropout",
            lab2("Expected dropout (%)", "Abandono esperado (%)",
                 "Share of enrolled participants you expect to lose. It does not change the probabilities; it only inflates the number to enrol.",
                 "Propor\u00E7\u00E3o de participantes recrutados que voc\u00EA espera perder. N\u00E3o muda as probabilidades; apenas aumenta o n\u00FAmero a recrutar."),
            value = 0, min = 0, max = 90, step = 5)
        )
      ),

      # ---- Success -----------------------------------------------------
      accordion_panel(
        tt("What counts as success", "O que conta como sucesso"), value = "success",
        radioButtons("alt",
          lab2("Test direction", "Dire\u00E7\u00E3o do teste",
               "Two-sided counts a clear difference in either direction. One-sided only counts a result in the stated direction (for ratios: '<' means a ratio below the threshold, e.g. a hazard ratio below 1).",
               "Bilateral conta uma diferen\u00E7a clara em qualquer dire\u00E7\u00E3o. Unilateral s\u00F3 conta um resultado na dire\u00E7\u00E3o indicada (para raz\u00F5es: '<' significa uma raz\u00E3o abaixo do limiar, ex.: HR abaixo de 1)."),
          choiceNames = list(tt("Two-sided", "Bilateral"),
                             tt("One-sided: treatment > control", "Unilateral: tratamento > controle"),
                             tt("One-sided: treatment < control", "Unilateral: tratamento < controle")),
          choiceValues = c("two.sided", "greater", "less"),
          selected = "two.sided"),
        conditionalPanel(
          "input.alt != 'two.sided'",
          conditionalPanel(
            JS_ADDITIVE,
            numericInput("threshold",
              lab2("Success threshold", "Limiar de sucesso",
                   "0 = ordinary superiority. A value beyond 0 in the tested direction demands a clinically meaningful effect; a value on the other side gives a non-inferiority design (the non-inferiority margin).",
                   "0 = superioridade comum. Um valor al\u00E9m de 0 na dire\u00E7\u00E3o testada exige um efeito clinicamente relevante; um valor do outro lado gera um desenho de n\u00E3o inferioridade (a margem de n\u00E3o inferioridade)."),
              value = 0)
          ),
          conditionalPanel(
            JS_RD,
            numericInput("rd_threshold",
              lab2("Success threshold (percentage points)", "Limiar de sucesso (pontos percentuais)",
                   "0 = ordinary superiority. E.g. -5 with 'treatment < control' requires showing at least 5 points fewer events; +5 would be a non-inferiority margin.",
                   "0 = superioridade comum. Ex.: -5 com 'tratamento < controle' exige mostrar pelo menos 5 pontos a menos de eventos; +5 seria uma margem de n\u00E3o inferioridade."),
              value = 0)
          ),
          conditionalPanel(
            JS_RATIO,
            numericInput("ratio_threshold",
              lab2("Success threshold (ratio)", "Limiar de sucesso (raz\u00E3o)",
                   "1 = ordinary superiority. E.g. 0.8 with 'treatment < control' requires showing the ratio is below 0.8; 1.3 would be a non-inferiority margin.",
                   "1 = superioridade comum. Ex.: 0,8 com 'tratamento < controle' exige mostrar que a raz\u00E3o est\u00E1 abaixo de 0,8; 1,3 seria uma margem de n\u00E3o inferioridade."),
              value = 1, min = 0, step = 0.05)
          ),
          uiOutput("threshold_hint")
        ),
        numericInput("alpha",
          lab2("Significance threshold (alpha)", "N\u00EDvel de signific\u00E2ncia (alfa)",
               "Frequentist: the p-value cut-off. Bayesian: success if the posterior probability of an effect beyond the threshold in the tested direction exceeds 1 - alpha (1 - alpha/2 per direction for two-sided).",
               "Frequentista: o ponto de corte do valor-p. Bayesiana: sucesso se a probabilidade posteriori de um efeito al\u00E9m do limiar, na dire\u00E7\u00E3o testada, passar de 1 - alfa (1 - alfa/2 por dire\u00E7\u00E3o no teste bilateral)."),
          value = 0.05, min = 0.0001, max = 0.5, step = 0.005),
        sliderInput("target",
          lab2("Target probability of success", "Probabilidade de sucesso desejada",
               "The level you are aiming for, commonly 80% or 90%. Shown as a reference line; the app finds the sample size that reaches it.",
               "O n\u00EDvel que voc\u00EA busca, geralmente 80% ou 90%. Aparece como linha de refer\u00EAncia; o aplicativo encontra o tamanho amostral que o atinge."),
          min = 0.5, max = 0.99, value = 0.8, step = 0.01)
      ),

      # ---- Complex design (extrapolation) ------------------------------
      accordion_panel(
        tt("Complex design (extrapolation)", "Desenho complexo (extrapola\u00E7\u00E3o)"), value = "complex",
        checkboxInput("cx_on",
          lab2("Extrapolate to a more complex or stricter design",
               "Extrapolar para um desenho mais complexo ou exigente",
               "Shows a card on the Results tab with a rough estimate of how much bigger the trial must be with the complications and stricter requirements chosen here. It does not change the main results.",
               "Mostra um cart\u00E3o na aba Resultados com uma estimativa grosseira de quanto o ensaio precisa crescer com as complica\u00E7\u00F5es e exig\u00EAncias escolhidas aqui. N\u00E3o muda os resultados principais."),
          value = FALSE),
        conditionalPanel(
          "input.cx_on",
          checkboxGroupInput("cx_features",
            lab2("Design features", "Caracter\u00EDsticas do desenho",
                 "Tick every complication your real design has. Each one is turned into a textbook adjustment of the simple two-arm sample size.",
                 "Marque cada complica\u00E7\u00E3o que o seu desenho real tem. Cada uma vira um ajuste cl\u00E1ssico do tamanho amostral do desenho simples de dois bra\u00E7os."),
            choiceNames = list(
              tt("Cluster randomisation", "Randomiza\u00E7\u00E3o por clusters"),
              tt("Repeated measures (continuous)", "Medidas repetidas (cont\u00EDnuo)"),
              tt("2x2 crossover (difference in means)", "Crossover 2x2 (diferen\u00E7a de m\u00E9dias)"),
              tt("Several arms, shared control", "V\u00E1rios bra\u00E7os, controle comum"),
              tt("Several primary endpoints", "V\u00E1rios desfechos prim\u00E1rios"),
              tt("Interim analyses (group sequential)", "An\u00E1lises interinas (sequencial em grupos)"),
              tt("Non-adherence / contamination", "N\u00E3o ades\u00E3o / contamina\u00E7\u00E3o")),
            choiceValues = CX_FEATURES),
          conditionalPanel(
            "input.cx_features && input.cx_features.indexOf('cluster') > -1",
            tags$strong(class = "small", tt("Cluster randomisation", "Randomiza\u00E7\u00E3o por clusters")),
            layout_columns(
              col_widths = c(4, 4, 4), gap = "0.5rem",
              numericInput("cx_m", lab2("Cluster size", "Tamanho do cluster",
                "Average number of participants per cluster (clinic, school, village...).",
                "N\u00FAmero m\u00E9dio de participantes por cluster (cl\u00EDnica, escola, comunidade...)."),
                value = 20, min = 1, step = 1),
              numericInput("cx_icc", lab2("ICC", "ICC",
                "Intracluster correlation: how alike participants in the same cluster are. Typically 0.01-0.05 for clinical outcomes, higher for process measures.",
                "Correla\u00E7\u00E3o intracluster: o quanto participantes do mesmo cluster se parecem. Tipicamente 0,01-0,05 para desfechos cl\u00EDnicos, maior para medidas de processo."),
                value = 0.05, min = 0, max = 0.99, step = 0.01),
              numericInput("cx_cv", lab2("Size CV", "CV do tamanho",
                "Coefficient of variation of cluster sizes (SD / mean). 0 = all clusters the same size; 0.4-0.7 is common.",
                "Coeficiente de varia\u00E7\u00E3o dos tamanhos dos clusters (DP / m\u00E9dia). 0 = todos do mesmo tamanho; 0,4-0,7 \u00E9 comum."),
                value = 0, min = 0, max = 3, step = 0.1))
          ),
          conditionalPanel(
            "input.cx_features && input.cx_features.indexOf('repeated') > -1",
            tags$strong(class = "small", tt("Repeated measures", "Medidas repetidas")),
            layout_columns(
              col_widths = c(6, 6), gap = "0.5rem",
              numericInput("cx_k", lab2("Measurements", "Medidas",
                "Number of follow-up measurements whose mean is analysed.",
                "N\u00FAmero de medidas de seguimento cuja m\u00E9dia \u00E9 analisada."),
                value = 3, min = 1, max = 50, step = 1),
              numericInput("cx_rep_rho", lab2("Correlation", "Correla\u00E7\u00E3o",
                "Correlation between two measurements of the same participant.",
                "Correla\u00E7\u00E3o entre duas medidas do mesmo participante."),
                value = 0.5, min = 0, max = 1, step = 0.05))
          ),
          conditionalPanel(
            "input.cx_features && input.cx_features.indexOf('crossover') > -1",
            numericInput("cx_x_rho",
              lab2("Crossover: within-person correlation", "Crossover: correla\u00E7\u00E3o intraindividual",
                   "Correlation between a participant's outcomes in the two periods. Higher means a crossover saves more participants.",
                   "Correla\u00E7\u00E3o entre os desfechos de um participante nos dois per\u00EDodos. Quanto maior, mais participantes o crossover economiza."),
              value = 0.6, min = 0, max = 0.99, step = 0.05)
          ),
          conditionalPanel(
            "input.cx_features && input.cx_features.indexOf('multiarm') > -1",
            numericInput("cx_arms",
              lab2("Number of arms (including control)", "N\u00FAmero de bra\u00E7os (incluindo o controle)",
                   "Each treatment arm is compared with the shared control; alpha is split over the comparisons.",
                   "Cada bra\u00E7o de tratamento \u00E9 comparado ao controle comum; o alfa \u00E9 dividido entre as compara\u00E7\u00F5es."),
              value = 3, min = 3, max = 10, step = 1),
            radioButtons("cx_mult", NULL, inline = TRUE,
              choiceNames = list(tt("Bonferroni", "Bonferroni"), tt("\u0160id\u00E1k", "\u0160id\u00E1k")),
              choiceValues = c("bonferroni", "sidak"))
          ),
          conditionalPanel(
            "input.cx_features && input.cx_features.indexOf('endpoints') > -1",
            numericInput("cx_J",
              lab2("Number of primary endpoints", "N\u00FAmero de desfechos prim\u00E1rios",
                   "Assumes each endpoint needs about the same sample size as the one described above.",
                   "Sup\u00F5e que cada desfecho precise de um tamanho amostral parecido com o descrito acima."),
              value = 2, min = 2, max = 10, step = 1),
            radioButtons("cx_jmode", NULL,
              choiceNames = list(tt("Success if any one succeeds (alpha is split)", "Sucesso se qualquer um tiver sucesso (alfa dividido)"),
                                 tt("All must succeed (co-primary)", "Todos devem ter sucesso (coprim\u00E1rios)")),
              choiceValues = c("any", "all"))
          ),
          conditionalPanel(
            "input.cx_features && input.cx_features.indexOf('interim') > -1",
            numericInput("cx_looks",
              lab2("Number of analyses (including the final one)", "N\u00FAmero de an\u00E1lises (incluindo a final)",
                   "Equally spaced analyses that may stop the trial early for efficacy.",
                   "An\u00E1lises igualmente espa\u00E7adas que podem encerrar o ensaio cedo por efic\u00E1cia."),
              value = 3, min = 2, max = 5, step = 1),
            radioButtons("cx_bound", NULL, inline = TRUE,
              choiceNames = list(tt("O'Brien-Fleming", "O'Brien-Fleming"), tt("Pocock", "Pocock")),
              choiceValues = c("obf", "pocock"))
          ),
          conditionalPanel(
            "input.cx_features && input.cx_features.indexOf('adherence') > -1",
            layout_columns(
              col_widths = c(6, 6), gap = "0.5rem",
              numericInput("cx_nonadh", lab2("Not taking treatment (%)", "Sem tomar o tratamento (%)",
                "Share of the treatment group expected not to receive or take the treatment.",
                "Propor\u00E7\u00E3o do grupo tratamento que se espera n\u00E3o receber ou n\u00E3o tomar o tratamento."),
                value = 10, min = 0, max = 89, step = 5),
              numericInput("cx_contam", lab2("Controls treated (%)", "Controles tratados (%)",
                "Share of the control group expected to get the treatment anyway (contamination).",
                "Propor\u00E7\u00E3o do grupo controle que se espera receber o tratamento mesmo assim (contamina\u00E7\u00E3o)."),
                value = 5, min = 0, max = 89, step = 5))
          ),
          tags$hr(class = "my-2"),
          tags$strong(class = "small", tt("Stricter requirements", "Exig\u00EAncias mais r\u00EDgidas")),
          layout_columns(
            col_widths = c(6, 6), gap = "0.5rem",
            selectInput("cx_target",
              lab2("Target", "Meta",
                   "Probability of success the complex design should reach (for power, assurance and detection if real).",
                   "Probabilidade de sucesso que o desenho complexo deve atingir (para poder, assurance e detec\u00E7\u00E3o se real)."),
              choices = c("80%" = "0.8", "85%" = "0.85", "90%" = "0.9", "95%" = "0.95"),
              selected = "0.9", selectize = FALSE),
            selectInput("cx_alpha",
              lab2("Alpha", "Alfa",
                   "Keep the alpha set above, or pick a stricter one. 0.00125 (one-sided) is sometimes asked of a single pivotal trial in place of two trials at 0.025.",
                   "Mantenha o alfa definido acima ou escolha um mais exigente. 0,00125 (unilateral) \u00E0s vezes \u00E9 exigido de um \u00FAnico ensaio pivotal no lugar de dois ensaios a 0,025."),
              choices = c("Same as above" = "same", "0.05" = "0.05", "0.025" = "0.025", "0.01" = "0.01",
                          "0.005" = "0.005", "0.00125" = "0.00125"),
              selected = "same", selectize = FALSE)
          )
        )
      ),

      # ---- Computation -------------------------------------------------
      accordion_panel(
        tt("Computation", "C\u00E1lculo"), value = "computation",
        radioButtons("engine",
          lab2("Assurance engine", "Motor de c\u00E1lculo da assurance",
               "Simulation generates thousands of virtual trials (slower, with a little random error). Exact uses closed-form formulas or numerical integration (instant). They agree closely except with very few events.",
               "A simula\u00E7\u00E3o gera milhares de ensaios virtuais (mais lenta, com um pequeno erro aleat\u00F3rio). O exato usa f\u00F3rmulas fechadas ou integra\u00E7\u00E3o num\u00E9rica (instant\u00E2neo). Os dois concordam de perto, exceto com pouqu\u00EDssimos eventos."),
          choiceNames = list(tt("Simulation of virtual trials", "Simula\u00E7\u00E3o de ensaios virtuais"),
                             tt("Exact formula (instant)", "F\u00F3rmula exata (instant\u00E2nea)")),
          choiceValues = c("sim", "exact"),
          selected = "sim"),
        conditionalPanel(
          "input.engine == 'sim'",
          numericInput("mc_iter",
            lab2("Simulated trials per sample size", "Ensaios simulados por tamanho amostral",
                 "More simulations give a smoother, more precise curve but take longer. The error is at most about 1/sqrt(number) (95% interval).",
                 "Mais simula\u00E7\u00F5es d\u00E3o uma curva mais suave e precisa, mas demoram mais. O erro \u00E9 no m\u00E1ximo cerca de 1/sqrt(n\u00FAmero) (intervalo de 95%)."),
            value = 2000, min = 100, max = 100000, step = 500),
          numericInput("seed", tt("Random seed (reproducibility)", "Semente aleat\u00F3ria (reprodutibilidade)"), value = 2024),
          checkboxInput("show_exact", tt("Overlay exact curve as a check", "Sobrepor a curva exata como verifica\u00E7\u00E3o"), value = TRUE)
        ),
        uiOutput("time_estimate")
      )
    ),
    actionButton("go", tt("Calculate", "Calcular"), class = "btn-primary btn-lg w-100"),
    uiOutput("stale"),
    bookmarkButton(label = tt("Share / bookmark these inputs", "Compartilhar / salvar estas entradas"),
                   class = "btn-outline-secondary btn-sm w-100")
  )
}
