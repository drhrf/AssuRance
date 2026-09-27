# =============================================================================
# Static content for the "Methods & help" tab (bilingual: see R/i18n.R).
# =============================================================================

# Bilingual list item and definition-list entries.
li2 <- function(en, pt) tags$li(tt(en, pt))
h3_2 <- function(en, pt) h3(tt(en, pt))
h5_2 <- function(en, pt) h5(tt(en, pt))

methods_ui <- function() {
  withMathJax(div(
    style = "max-width: 920px;",

    h3_2("What this app does", "O que este aplicativo faz"),
    tp("It helps you choose a sample size for a two-arm randomised trial (treatment vs control) by comparing two ways of answering \"how likely is this study to succeed?\":",
       "Ele ajuda a escolher o tamanho amostral de um ensaio randomizado com dois bra\u00e7os (tratamento vs controle), comparando duas formas de responder \"qual a chance de este estudo ter sucesso?\":"),
    tags$ul(
      tags$li(tt(tagList(strong("Frequentist power"), " assumes the true effect is known exactly (your best guess) and asks how often the usual test would be significant."),
                 tagList(strong("O poder frequentista"), " sup\u00f5e que o efeito verdadeiro \u00e9 conhecido exatamente (seu melhor palpite) e pergunta com que frequ\u00eancia o teste usual seria significativo."))),
      tags$li(tt(tagList(strong("Bayesian assurance"), " (also called the probability of success) admits that the true effect is uncertain. It averages the chance of success over every effect size you consider plausible, weighted by how plausible you think it is."),
                 tagList(strong("A assurance bayesiana"), " (tamb\u00e9m chamada de probabilidade de sucesso) admite que o efeito verdadeiro \u00e9 incerto. Ela faz a m\u00e9dia da chance de sucesso sobre todos os tamanhos de efeito que voc\u00ea considera plaus\u00edveis, ponderada pelo quanto voc\u00ea os acha plaus\u00edveis.")))),
    tp("Assurance is usually lower than power when you are optimistic about the effect, because it also counts the scenarios in which the treatment works less well than hoped, or not at all.",
       "A assurance costuma ser menor que o poder quando voc\u00ea \u00e9 otimista sobre o efeito, porque ela tamb\u00e9m conta os cen\u00e1rios em que o tratamento funciona pior do que o esperado, ou n\u00e3o funciona."),

    h3_2("Outcome types", "Tipos de desfecho"),
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th(tt("Outcome", "Desfecho")), tags$th(tt("Effect (working scale)", "Efeito (escala de trabalho)")),
                         tags$th(tt("Extra inputs", "Entradas adicionais")), tags$th(tt("Simulation engine", "Motor de simula\u00e7\u00e3o")))),
      tags$tbody(
        tags$tr(tags$td(tt("Continuous", "Cont\u00ednuo")), tags$td(tt("difference in means", "diferen\u00e7a de m\u00e9dias")),
                tags$td(tt("outcome SD", "DP do desfecho")), tags$td(code("bayesassurance::bayes_sim_unbalanced"))),
        tags$tr(tags$td(tt("Continuous, baseline-adjusted (ANCOVA)", "Cont\u00ednuo, ajustado pelo basal (ANCOVA)")),
                tags$td(tt("adjusted difference in means", "diferen\u00e7a de m\u00e9dias ajustada")),
                tags$td(tt("outcome SD, baseline-outcome correlation \\(\\rho\\)", "DP do desfecho, correla\u00e7\u00e3o basal-desfecho \\(\\rho\\)")),
                tags$td(code("bayes_sim_unbalanced"), tt(" with a simulated baseline covariate", " com uma covari\u00e1vel basal simulada"))),
        tags$tr(tags$td(tt("Binary", "Bin\u00e1rio")), tags$td(tt("log odds ratio, log risk ratio, or risk difference", "log da raz\u00e3o de chances, log do risco relativo ou diferen\u00e7a de riscos")),
                tags$td(tt("control-group event rate", "taxa de eventos no grupo controle")), tags$td(tt("built-in binomial simulation", "simula\u00e7\u00e3o binomial pr\u00f3pria"))),
        tags$tr(tags$td(tt("Time to event", "Tempo at\u00e9 o evento")), tags$td(tt("log hazard ratio", "log da raz\u00e3o de riscos")),
                tags$td(tt("control median, recruitment, follow-up, loss to follow-up", "mediana no controle, recrutamento, seguimento, perda de seguimento")),
                tags$td(tt("built-in patient-level survival simulation", "simula\u00e7\u00e3o pr\u00f3pria de sobrevida por paciente"))))),

    h3_2("Quick guide to the inputs", "Guia r\u00e1pido das entradas"),
    tags$dl(
      tags$dt(tt("Design prior", "Priori de planejamento")),
      tags$dd(tt("Your honest belief about the true effect before the study. For continuous outcomes and risk differences, enter a mean (best guess) and SD (about 95% of plausible effects lie within mean \u00B1 2 SD). For odds, risk and hazard ratios, enter the range you are 95% sure contains the true ratio; the app treats the ratio as log-normal with its best guess at the geometric midpoint. The design prior is only used to plan the study.",
                 "Sua cren\u00e7a honesta sobre o efeito verdadeiro antes do estudo. Para desfechos cont\u00ednuos e diferen\u00e7as de riscos, informe uma m\u00e9dia (melhor palpite) e um DP (cerca de 95% dos efeitos plaus\u00edveis ficam em m\u00e9dia \u00B1 2 DP). Para raz\u00f5es de chances, riscos relativos e raz\u00f5es de riscos, informe o intervalo que voc\u00ea tem 95% de certeza de conter a raz\u00e3o verdadeira; o aplicativo trata a raz\u00e3o como log-normal, com o melhor palpite no ponto m\u00e9dio geom\u00e9trico. A priori de planejamento serve apenas para planejar o estudo.")),
      tags$dt(tt("Analysis prior", "Priori de an\u00e1lise")),
      tags$dd(tt("The prior that will actually be used when the trial data are analysed. Regulators and reviewers often prefer a sceptical prior (centred on no effect) or a vague one (very wide, which lets the data speak). Using the design prior here is simpler, but it builds your optimism into the final analysis.",
                 "A priori que ser\u00e1 de fato usada quando os dados do ensaio forem analisados. Ag\u00eancias reguladoras e revisores costumam preferir uma priori c\u00e9tica (centrada no efeito nulo) ou vaga (muito larga, deixando os dados falarem). Usar aqui a priori de planejamento \u00e9 mais simples, mas incorpora o seu otimismo na an\u00e1lise final.")),
      tags$dt(tt("Success threshold", "Limiar de sucesso")),
      tags$dd(tt("For one-sided tests. The no-effect value (0, or 1 for ratios) gives ordinary superiority. A value beyond it in the tested direction demands a clinically meaningful effect. A value on the other side gives a non-inferiority design, e.g. 'treatment < control' with a hazard-ratio threshold of 1.3.",
                 "Para testes unilaterais. O valor de efeito nulo (0, ou 1 para raz\u00f5es) d\u00e1 a superioridade comum. Um valor al\u00e9m dele, na dire\u00e7\u00e3o testada, exige um efeito clinicamente relevante. Um valor do outro lado gera um desenho de n\u00e3o inferioridade, ex.: 'tratamento < controle' com limiar de raz\u00e3o de riscos 1,3.")),
      tags$dt(tt("Allocation ratio and dropout", "Raz\u00e3o de aloca\u00e7\u00e3o e abandono")),
      tags$dd(tt("The ratio sets the treatment-group size relative to the control group. Dropout (continuous and binary outcomes) does not change the calculations, which refer to analysable participants; it only inflates the number to enrol. For time-to-event outcomes loss to follow-up is modelled directly: those participants are censored and contribute fewer events.",
                 "A raz\u00e3o define o tamanho do grupo tratamento em rela\u00e7\u00e3o ao controle. O abandono (desfechos cont\u00ednuos e bin\u00e1rios) n\u00e3o muda os c\u00e1lculos, que se referem a participantes analis\u00e1veis; apenas aumenta o n\u00famero a recrutar. Em desfechos de tempo at\u00e9 o evento, a perda de seguimento \u00e9 modelada diretamente: esses participantes s\u00e3o censurados e contribuem com menos eventos.")),
      tags$dt(tt("Target", "Meta")),
      tags$dd(tt("The probability of success you are aiming for (commonly 80% or 90%). The app finds the smallest sample size that reaches it.",
                 "A probabilidade de sucesso que voc\u00ea busca (geralmente 80% ou 90%). O aplicativo encontra o menor tamanho amostral que a atinge."))),

    h3_2("The common Bayesian framework", "O arcabou\u00e7o bayesiano comum"),
    tp("Every outcome type is reduced to an effect \\(\\theta\\) on a working scale on which its estimate \\(\\hat\\theta\\) is approximately normal: \\(\\hat\\theta \\sim N(\\theta, v)\\), where the variance \\(v\\) depends on the sample sizes and, for binary and survival outcomes, on \\(\\theta\\) itself.",
       "Cada tipo de desfecho \u00e9 reduzido a um efeito \\(\\theta\\) em uma escala de trabalho na qual sua estimativa \\(\\hat\\theta\\) \u00e9 aproximadamente normal: \\(\\hat\\theta \\sim N(\\theta, v)\\), em que a vari\u00e2ncia \\(v\\) depende dos tamanhos amostrais e, para desfechos bin\u00e1rios e de sobrevida, do pr\u00f3prio \\(\\theta\\)."),
    tp("Design prior \\(\\theta \\sim N(m_d, s_d^2)\\); analysis prior \\(\\theta \\sim N(m_a, s_a^2)\\). The posterior after the trial is normal with precision \\(1/s_a^2 + 1/v\\). With threshold \\(C\\) and level \\(\\alpha\\), the trial is a success when",
       "Priori de planejamento \\(\\theta \\sim N(m_d, s_d^2)\\); priori de an\u00e1lise \\(\\theta \\sim N(m_a, s_a^2)\\). A posteriori ap\u00f3s o ensaio \u00e9 normal com precis\u00e3o \\(1/s_a^2 + 1/v\\). Com limiar \\(C\\) e n\u00edvel \\(\\alpha\\), o ensaio \u00e9 um sucesso quando"),
    tags$ul(
      li2("one-sided, '>': \\(P(\\theta > C \\mid \\text{data}) > 1-\\alpha\\)", "unilateral, '>': \\(P(\\theta > C \\mid \\text{dados}) > 1-\\alpha\\)"),
      li2("one-sided, '<': \\(P(\\theta < C \\mid \\text{data}) > 1-\\alpha\\)", "unilateral, '<': \\(P(\\theta < C \\mid \\text{dados}) > 1-\\alpha\\)"),
      li2("two-sided: \\(P(\\theta > 0 \\mid \\text{data}) > 1-\\alpha/2\\) or \\(P(\\theta < 0 \\mid \\text{data}) > 1-\\alpha/2\\). This counts a convincing effect in either direction, including the unexpected one; the summary says how much of the assurance comes from that.",
          "bilateral: \\(P(\\theta > 0 \\mid \\text{dados}) > 1-\\alpha/2\\) ou \\(P(\\theta < 0 \\mid \\text{dados}) > 1-\\alpha/2\\). Isso conta um efeito convincente em qualquer dire\u00e7\u00e3o, inclusive na inesperada; o resumo informa quanto da assurance vem disso.")),
    tp("Assurance is $$\\text{Assurance}(n) = \\int P(\\text{success} \\mid \\theta, n)\\, \\pi_d(\\theta)\\, d\\theta,$$ the probability of success averaged over the design prior \\(\\pi_d\\). The \"Success vs true effect\" tab shows \\(P(\\text{success} \\mid \\theta, n)\\) together with \\(\\pi_d\\).",
       "A assurance \u00e9 $$\\text{Assurance}(n) = \\int P(\\text{sucesso} \\mid \\theta, n)\\, \\pi_d(\\theta)\\, d\\theta,$$ a probabilidade de sucesso em m\u00e9dia sobre a priori de planejamento \\(\\pi_d\\). A aba \"Sucesso vs efeito verdadeiro\" mostra \\(P(\\text{sucesso} \\mid \\theta, n)\\) junto com \\(\\pi_d\\)."),

    h3_2("Outcome-specific details", "Detalhes por tipo de desfecho"),
    h5_2("Continuous outcome", "Desfecho cont\u00ednuo"),
    tp("\\(v = \\sigma^2 (1/n_T + 1/n_C)\\) with the outcome SD \\(\\sigma\\) known. Everything is exactly normal, so assurance has a closed form: marginally \\(\\hat\\theta \\sim N(m_d, s_d^2 + v)\\).",
       "\\(v = \\sigma^2 (1/n_T + 1/n_C)\\), com o DP do desfecho \\(\\sigma\\) conhecido. Tudo \u00e9 exatamente normal, ent\u00e3o a assurance tem forma fechada: marginalmente \\(\\hat\\theta \\sim N(m_d, s_d^2 + v)\\)."),
    h5_2("Continuous outcome adjusted for baseline (ANCOVA)", "Desfecho cont\u00ednuo ajustado pelo basal (ANCOVA)"),
    tp("Adjusting for a baseline value correlated \\(\\rho\\) with the outcome reduces the residual variance to \\(\\sigma^2(1-\\rho^2)\\). Because the slope is estimated and the arms' baseline means differ by chance, \\(v = \\sigma^2(1-\\rho^2)(1/n_T + 1/n_C)\\,(1 + 1/(N-4))\\), where \\(N = n_T + n_C\\) (Borm et al., 2007). The simulation generates baseline values and fits the full regression model with bayes_sim_unbalanced (design matrix: two group columns plus the baseline; flat analysis prior on the slope). Frequentist power uses the ANCOVA t-test with \\(N-3\\) degrees of freedom.",
       "Ajustar por um valor basal com correla\u00e7\u00e3o \\(\\rho\\) com o desfecho reduz a vari\u00e2ncia residual para \\(\\sigma^2(1-\\rho^2)\\). Como a inclina\u00e7\u00e3o \u00e9 estimada e as m\u00e9dias basais dos bra\u00e7os diferem por acaso, \\(v = \\sigma^2(1-\\rho^2)(1/n_T + 1/n_C)\\,(1 + 1/(N-4))\\), em que \\(N = n_T + n_C\\) (Borm et al., 2007). A simula\u00e7\u00e3o gera valores basais e ajusta o modelo de regress\u00e3o completo com bayes_sim_unbalanced (matriz de desenho: duas colunas de grupo mais o basal; priori de an\u00e1lise plana na inclina\u00e7\u00e3o). O poder frequentista usa o teste t da ANCOVA com \\(N-3\\) graus de liberdade."),
    h5_2("Binary outcome", "Desfecho bin\u00e1rio"),
    tp("Given the control-group event rate \\(p_C\\), the effect determines the treatment-group rate \\(p_T\\). Delta-method variances: log odds ratio \\(1/(n_T p_T q_T) + 1/(n_C p_C q_C)\\); log risk ratio \\(q_T/(n_T p_T) + q_C/(n_C p_C)\\); risk difference \\(p_T q_T/n_T + p_C q_C/n_C\\) (\\(q = 1-p\\)). As \\(v\\) depends on \\(\\theta\\), exact assurance is computed by numerical integration over the design prior (a fine grid over \\(\\pm 8\\) SD). The simulation draws a true effect from the design prior for every virtual trial, simulates binomial counts, and analyses them on the working scale (with a 0.5 continuity correction when a cell is empty). For risk ratios and differences, prior values implying event rates outside 0-100% are truncated, and the app warns when that matters.",
       "Dada a taxa de eventos no controle \\(p_C\\), o efeito determina a taxa no tratamento \\(p_T\\). Vari\u00e2ncias pelo m\u00e9todo delta: log da raz\u00e3o de chances \\(1/(n_T p_T q_T) + 1/(n_C p_C q_C)\\); log do risco relativo \\(q_T/(n_T p_T) + q_C/(n_C p_C)\\); diferen\u00e7a de riscos \\(p_T q_T/n_T + p_C q_C/n_C\\) (\\(q = 1-p\\)). Como \\(v\\) depende de \\(\\theta\\), a assurance exata \u00e9 calculada por integra\u00e7\u00e3o num\u00e9rica sobre a priori de planejamento (uma grade fina em \\(\\pm 8\\) DP). A simula\u00e7\u00e3o sorteia um efeito verdadeiro da priori de planejamento para cada ensaio virtual, simula contagens binomiais e as analisa na escala de trabalho (com corre\u00e7\u00e3o de continuidade de 0,5 quando uma c\u00e9lula \u00e9 zero). Para riscos relativos e diferen\u00e7as de riscos, valores da priori que implicam taxas fora de 0-100% s\u00e3o truncados, e o aplicativo avisa quando isso importa."),
    h5_2("Time to event", "Tempo at\u00e9 o evento"),
    tp("Exponential survival with control median \\(M\\), so the control hazard is \\(\\lambda_C = \\log 2 / M\\) and \\(\\lambda_T = \\text{HR} \\times \\lambda_C\\). Participants join uniformly over the recruitment period \\(A\\), the analysis happens at \\(A + F\\), and loss to follow-up is a competing exponential hazard \\(\\eta\\) calibrated to the stated percentage over the average follow-up \\(F + A/2\\). The probability that a participant has an observed event is $$P_e = \\frac{\\lambda}{\\lambda+\\eta}\\left(1 - \\frac{e^{-(\\lambda+\\eta)F} - e^{-(\\lambda+\\eta)(A+F)}}{(\\lambda+\\eta)A}\\right),$$ and the log hazard ratio has variance \\(v \\approx 1/D_T + 1/D_C\\), with expected events \\(D = n P_e\\) per arm. This is close to Schoenfeld's \\(4/D\\) for equal allocation and a hazard ratio near 1. What drives precision is the number of events, so the app reports expected events alongside sample sizes. The simulation generates entry times, event times and dropout times for every participant, applies administrative censoring, and estimates the log hazard ratio from the ratio of event rates (events / person-time) in the two arms.",
       "Sobrevida exponencial com mediana no controle \\(M\\), de modo que o risco no controle \u00e9 \\(\\lambda_C = \\log 2 / M\\) e \\(\\lambda_T = \\text{HR} \\times \\lambda_C\\). Os participantes entram de modo uniforme durante o recrutamento \\(A\\), a an\u00e1lise ocorre em \\(A + F\\), e a perda de seguimento \u00e9 um risco exponencial competitivo \\(\\eta\\), calibrado para a porcentagem informada no seguimento m\u00e9dio \\(F + A/2\\). A probabilidade de um participante ter um evento observado \u00e9 $$P_e = \\frac{\\lambda}{\\lambda+\\eta}\\left(1 - \\frac{e^{-(\\lambda+\\eta)F} - e^{-(\\lambda+\\eta)(A+F)}}{(\\lambda+\\eta)A}\\right),$$ e o log da raz\u00e3o de riscos tem vari\u00e2ncia \\(v \\approx 1/D_T + 1/D_C\\), com eventos esperados \\(D = n P_e\\) por bra\u00e7o. Isso \u00e9 pr\u00f3ximo do \\(4/D\\) de Schoenfeld para aloca\u00e7\u00e3o igual e raz\u00e3o de riscos perto de 1. O que determina a precis\u00e3o \u00e9 o n\u00famero de eventos, por isso o aplicativo mostra os eventos esperados junto dos tamanhos amostrais. A simula\u00e7\u00e3o gera tempos de entrada, de evento e de abandono para cada participante, aplica a censura administrativa e estima o log da raz\u00e3o de riscos pela raz\u00e3o das taxas de eventos (eventos / pessoa-tempo) nos dois bra\u00e7os."),

    h3_2("How it is computed", "Como \u00e9 calculado"),
    tags$ul(
      tags$li(tt(tagList(strong("Simulated assurance"), " (the default). Continuous and ANCOVA outcomes use ", code("bayesassurance::bayes_sim_unbalanced()"), " (Pan & Banerjee). In its parameterisation prior variances are relative to the outcome variance, so a prior SD \\(s\\) enters as \\(s^2/\\sigma^2\\); the full mapping is documented in ", code("R/calculations.R"), ". Binary and time-to-event outcomes use the app's own simulators described above. In every case each virtual trial draws its own true effect from the design prior, simulates data, analyses them with the analysis prior and checks the success rule. The shaded band is the 95% Monte Carlo interval."),
                 tagList(strong("Assurance simulada"), " (padr\u00e3o). Desfechos cont\u00ednuos e ANCOVA usam ", code("bayesassurance::bayes_sim_unbalanced()"), " (Pan & Banerjee). Na parametriza\u00e7\u00e3o do pacote, as vari\u00e2ncias da priori s\u00e3o relativas \u00e0 vari\u00e2ncia do desfecho, ent\u00e3o um DP \\(s\\) da priori entra como \\(s^2/\\sigma^2\\); o mapeamento completo est\u00e1 documentado em ", code("R/calculations.R"), ". Desfechos bin\u00e1rios e de tempo at\u00e9 o evento usam os simuladores pr\u00f3prios descritos acima. Em todos os casos, cada ensaio virtual sorteia seu pr\u00f3prio efeito verdadeiro da priori de planejamento, simula dados, analisa-os com a priori de an\u00e1lise e verifica a regra de sucesso. A faixa sombreada \u00e9 o intervalo de Monte Carlo de 95%."))),
      tags$li(tt(tagList(strong("Exact assurance"), " uses the closed form (continuous outcomes) or numerical integration (binary and survival), with the variance evaluated at each true effect. It drives the dotted check line, the sample-size finder, the sensitivity plots and the success-vs-effect curve. It agrees with the simulation to within Monte Carlo error except when very few events are expected, where the simulation (which analyses the actual counts) is the more accurate of the two."),
                 tagList(strong("A assurance exata"), " usa a forma fechada (desfechos cont\u00ednuos) ou integra\u00e7\u00e3o num\u00e9rica (bin\u00e1rios e sobrevida), com a vari\u00e2ncia avaliada em cada efeito verdadeiro. Ela alimenta a linha pontilhada de verifica\u00e7\u00e3o, o c\u00e1lculo do tamanho amostral, os gr\u00e1ficos de sensibilidade e a curva de sucesso vs efeito. Ela concorda com a simula\u00e7\u00e3o dentro do erro de Monte Carlo, exceto com pouqu\u00edssimos eventos esperados, caso em que a simula\u00e7\u00e3o (que analisa as contagens reais) \u00e9 a mais precisa das duas."))),
      tags$li(tt(tagList(strong("Frequentist power"), " uses the design prior's centre as the true effect: the two-sample t-test (identical to ", code("stats::power.t.test()"), " for equal groups) or the ANCOVA t-test for continuous outcomes, and a Wald z-test on the working scale for binary and survival outcomes, with the standard error evaluated at the assumed effect."),
                 tagList(strong("O poder frequentista"), " usa o centro da priori de planejamento como efeito verdadeiro: o teste t para duas amostras (id\u00eantico ao ", code("stats::power.t.test()"), " para grupos iguais) ou o teste t da ANCOVA para desfechos cont\u00ednuos, e um teste z de Wald na escala de trabalho para desfechos bin\u00e1rios e de sobrevida, com o erro padr\u00e3o avaliado no efeito suposto."))),
      tags$li(tt(tagList(strong("Assurance ceiling."), " As the sample size grows, the data overwhelm the analysis prior and the trial succeeds exactly when the true effect lies beyond the success threshold. Assurance therefore cannot exceed the design-prior probability of that event. If your target is above this ceiling, no sample size can reach it."),
                 tagList(strong("Teto da assurance."), " \u00c0 medida que o tamanho amostral cresce, os dados superam a priori de an\u00e1lise e o ensaio tem sucesso exatamente quando o efeito verdadeiro est\u00e1 al\u00e9m do limiar de sucesso. Por isso a assurance n\u00e3o pode passar da probabilidade desse evento segundo a priori de planejamento. Se a sua meta estiver acima desse teto, nenhum tamanho amostral a atinge.")))),

    h3_2("About the bayesassurance package", "Sobre o pacote bayesassurance"),
    tp("The app uses the package's general linear-model simulator bayes_sim_unbalanced() for continuous outcomes. Several other functions were reviewed and deliberately not used:",
       "O aplicativo usa o simulador de modelo linear geral do pacote, bayes_sim_unbalanced(), para desfechos cont\u00ednuos. Outras fun\u00e7\u00f5es foram analisadas e deliberadamente n\u00e3o usadas:"),
    tags$ul(
      li2("assurance_nd_na() (closed form) ties the analysis prior mean to the null value and does not match the exact posterior-probability assurance when the analysis prior is informative.",
          "assurance_nd_na() (forma fechada) amarra a m\u00e9dia da priori de an\u00e1lise ao valor nulo e n\u00e3o coincide com a assurance exata baseada na probabilidade posteriori quando a priori de an\u00e1lise \u00e9 informativa."),
      li2("bayes_sim_betabin() (binary outcomes) draws the true proportions only once per call rather than once per simulated trial, so it does not average over the design prior. It also uses the same beta prior for design and analysis.",
          "bayes_sim_betabin() (desfechos bin\u00e1rios) sorteia as propor\u00e7\u00f5es verdadeiras apenas uma vez por chamada, e n\u00e3o uma vez por ensaio simulado, ent\u00e3o n\u00e3o faz a m\u00e9dia sobre a priori de planejamento. Al\u00e9m disso, usa a mesma priori beta para planejamento e an\u00e1lise."),
      li2("bayes_sim_unknownvar() (unknown variance) builds a default covariance matrix whose size does not match its own two-group design matrix and is very slow. Uncertainty about the outcome SD is instead explored on the Sensitivity tab.",
          "bayes_sim_unknownvar() (vari\u00e2ncia desconhecida) monta uma matriz de covari\u00e2ncia padr\u00e3o cujo tamanho n\u00e3o corresponde \u00e0 sua pr\u00f3pria matriz de desenho com dois grupos, e \u00e9 muito lenta. A incerteza sobre o DP do desfecho \u00e9 explorada na aba Sensibilidade."),
      li2("The package has no functions for time-to-event outcomes.", "O pacote n\u00e3o tem fun\u00e7\u00f5es para desfechos de tempo at\u00e9 o evento.")),

    h3_2("Limitations", "Limita\u00e7\u00f5es"),
    tags$ul(
      li2("Normal approximations on the working scale underlie the exact engine and the frequentist power for binary and survival outcomes; they are rough with very few events.",
          "Aproxima\u00e7\u00f5es normais na escala de trabalho fundamentam o c\u00e1lculo exato e o poder frequentista para desfechos bin\u00e1rios e de sobrevida; elas s\u00e3o grosseiras com pouqu\u00edssimos eventos."),
      li2("The survival model assumes exponential event times and proportional hazards, and non-informative censoring. Non-constant hazards or delayed treatment effects need dedicated software.",
          "O modelo de sobrevida sup\u00f5e tempos exponenciais, riscos proporcionais e censura n\u00e3o informativa. Riscos n\u00e3o constantes ou efeitos tardios do tratamento exigem software espec\u00edfico."),
      li2("Nuisance quantities (outcome SD, correlation, control event rate, control median) are treated as known; the Sensitivity tab shows how much the answer depends on them.",
          "Quantidades de inc\u00f4modo (DP do desfecho, correla\u00e7\u00e3o, taxa de eventos no controle, mediana no controle) s\u00e3o tratadas como conhecidas; a aba Sensibilidade mostra o quanto a resposta depende delas."),
      li2("Clustered, longitudinal, count and multi-arm designs are not covered.",
          "Desenhos por conglomerados, longitudinais, de contagem e com m\u00faltiplos bra\u00e7os n\u00e3o s\u00e3o cobertos.")),

    h3_2("Tips", "Dicas"),
    tags$ul(
      li2("Simulation cost grows with the square of the sample size for continuous outcomes. Use the exact engine for quick exploration, then confirm with the simulation.",
          "Para desfechos cont\u00ednuos, o custo da simula\u00e7\u00e3o cresce com o quadrado do tamanho amostral. Use o c\u00e1lculo exato para explorar rapidamente e depois confirme com a simula\u00e7\u00e3o."),
      li2("Use the bookmark button to get a link that restores all your inputs.",
          "Use o bot\u00e3o de marcador para obter um link que restaura todas as suas entradas."),
      li2("Save scenarios on the Results tab to compare assumptions, or outcome types, side by side.",
          "Salve cen\u00e1rios na aba Resultados para comparar suposi\u00e7\u00f5es, ou tipos de desfecho, lado a lado.")),

    h3_2("References", "Refer\u00eancias"),
    tags$ul(
      tags$li("O'Hagan A, Stevens JW (2001). Bayesian assessment of sample size for clinical trials of cost-effectiveness.",
              em("Medical Decision Making"), "21(3):219-230."),
      tags$li("O'Hagan A, Stevens JW, Campbell MJ (2005). Assurance in clinical trial design.", em("Pharmaceutical Statistics"),
              "4(3):187-201."),
      tags$li("Ren S, Oakley JE (2014). Assurance calculations for planning clinical trials with time-to-event outcomes.",
              em("Statistics in Medicine"), "33(1):31-45."),
      tags$li("Schoenfeld DA (1983). Sample-size formula for the proportional-hazards regression model.", em("Biometrics"),
              "39(2):499-503."),
      tags$li("Borm GF, Fransen J, Lemmens WA (2007). A simple sample size formula for analysis of covariance in randomized clinical trials.",
              em("Journal of Clinical Epidemiology"), "60(12):1234-1238."),
      tags$li("Wang F, Gelfand AE (2002). A simulation-based approach to Bayesian sample size determination for performance under a given model and for separating models.",
              em("Statistical Science"), "17(2):193-208."),
      tags$li("Pan J, Banerjee S (2023). bayesassurance: An R package for calculating sample size and Bayesian assurance.",
              em("The R Journal"),
              tags$a(href = "https://journal.r-project.org/articles/RJ-2023-066/", target = "_blank", "RJ-2023-066")))
  ))
}
