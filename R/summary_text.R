# =============================================================================
# Plain-language summary for non-statisticians, and short text descriptions
# of the inputs (used by the report and the scenario table).
# All text follows the current language (see R/i18n.R).
# =============================================================================

# Numbers in the current language: Portuguese uses a decimal comma and a
# dot for thousands.
fmt_dec <- function(x, digits) {
  s <- formatC(x, format = "f", digits = digits)
  if (current_lang() == "pt") chartr(".", ",", s) else s
}

fmt_pct <- function(p, digits = 1) paste0(fmt_dec(100 * p, digits), "%")

fmt_num <- function(x) {
  pt <- current_lang() == "pt"
  format(signif(x, 4), big.mark = if (pt) "." else ",",
         decimal.mark = if (pt) "," else ".", scientific = FALSE, trim = TRUE)
}

# Describe the arm sizes, e.g. "100 participants per group (200 in total)".
describe_arms <- function(n_t, n_c) {
  if (n_t == n_c) {
    L(paste0(fmt_num(n_c), " participants per group (", fmt_num(2 * n_c), " in total)"),
      paste0(fmt_num(n_c), " participantes por grupo (", fmt_num(2 * n_c), " no total)"))
  } else {
    L(paste0(fmt_num(n_t), " participants in the treatment group and ", fmt_num(n_c),
             " in the control group (", fmt_num(n_t + n_c), " in total)"),
      paste0(fmt_num(n_t), " participantes no grupo tratamento e ", fmt_num(n_c),
             " no grupo controle (", fmt_num(n_t + n_c), " no total)"))
  }
}

# Expected number of events at the design prior's centre (binary, survival).
expected_events <- function(m, n_t, n_c) {
  if (is.null(m$events)) NA_real_ else m$events(m$m_d, n_t, n_c)
}

# Prior as text on the natural scale.
describe_prior <- function(m, which = c("design", "analysis")) {
  which <- match.arg(which)
  mu <- if (which == "design") m$m_d else m$m_a
  s  <- if (which == "design") m$s_d else m$s_a
  z <- qnorm(0.975)
  switch(m$family,
    additive = L(paste0("mean ", fmt_num(mu), ", SD ", fmt_num(s)),
                 paste0("m\u00E9dia ", fmt_num(mu), ", DP ", fmt_num(s))),
    rd       = L(paste0("mean ", fmt_num(100 * mu), " points, SD ", fmt_num(100 * s), " points"),
                 paste0("m\u00E9dia ", fmt_num(100 * mu), " pontos, DP ", fmt_num(100 * s), " pontos")),
    ratio    = paste0(tx(m$labels$short), " ", fmt_dec(exp(mu), 2),
                      L(" (95% range ", " (intervalo de 95%: "), fmt_dec(exp(mu - z * s), 2),
                      L(" to ", " a "), fmt_dec(exp(mu + z * s), 2), ")"))
}

describe_test <- function(m) {
  base <- switch(m$alt,
    two.sided = L("two-sided", "bilateral"),
    greater   = L("one-sided, treatment > control", "unilateral, tratamento > controle"),
    less      = L("one-sided, treatment < control", "unilateral, tratamento < controle"))
  if (m$alt != "two.sided" && abs(m$C) > 1e-12) {
    base <- paste0(base, L(", threshold ", ", limiar "), m$fmt_eff(m$C))
  }
  paste0(base, ", alpha = ", fmt_num(m$alpha))
}

# res : list produced by the server's finish_run() (see app.R).
# Returns a character vector of paragraphs.
build_summary <- function(res) {
  p   <- res$p
  m   <- res$model
  tab <- res$table
  last <- tab[nrow(tab), ]
  assur <- last$assurance
  pwr   <- last$power
  goal  <- model_goal(m)
  short <- tx(m$labels$short)
  ev_txt <- function(n_t, n_c) {
    ev <- expected_events(m, n_t, n_c)
    if (is.na(ev)) "" else L(paste0(", with about ", fmt_num(round(ev)), " events expected"),
                             paste0(", com cerca de ", fmt_num(round(ev)), " eventos esperados"))
  }

  out <- character(0)

  # 1. Bayesian assurance at the largest sample size ------------------------
  out <- c(out, L(
    paste0("With ", describe_arms(last$n_t, last$n_c), ev_txt(last$n_t, last$n_c),
           ", and given your assumptions, there is an estimated ", fmt_pct(assur),
           " probability that this study design will successfully show ", goal,
           ", accounting for your uncertainty about the true effect size. This is ",
           "the Bayesian assurance: it averages the chance of success over the whole ",
           "range of effect sizes you consider plausible, rather than betting on one value."),
    paste0("Com ", describe_arms(last$n_t, last$n_c), ev_txt(last$n_t, last$n_c),
           ", e dadas as suas suposi\u00E7\u00F5es, h\u00E1 uma probabilidade estimada de ",
           fmt_pct(assur), " de que este desenho de estudo consiga mostrar ", goal,
           ", levando em conta a sua incerteza sobre o verdadeiro tamanho do efeito. ",
           "Esta \u00E9 a assurance bayesiana: ela faz a m\u00E9dia da chance de sucesso ",
           "sobre toda a faixa de tamanhos de efeito que voc\u00EA considera plaus\u00EDveis, ",
           "em vez de apostar em um \u00FAnico valor.")))

  # 2. Frequentist power at the same sample size ----------------------------
  out <- c(out, L(
    paste0("By contrast, under a traditional frequentist framework that assumes the ",
           short, " is known exactly to be ", m$fmt_eff(m$m_d),
           ", the study has ", fmt_pct(pwr), " power to detect this effect at the ",
           "same sample size (", tx(m$power_label), "). That figure takes no account of ",
           "the possibility that the true effect is smaller (or larger) than assumed."),
    paste0("Em contraste, em uma abordagem frequentista tradicional, que sup\u00F5e que a ",
           "medida de efeito (", short, ") \u00E9 conhecida e vale exatamente ", m$fmt_eff(m$m_d),
           ", o estudo tem ", fmt_pct(pwr), " de poder para detectar esse efeito com o ",
           "mesmo tamanho amostral (", tx(m$power_label), "). Esse n\u00FAmero n\u00E3o considera ",
           "a possibilidade de o efeito verdadeiro ser menor (ou maior) do que o suposto.")))

  # 3. Flag a large gap -----------------------------------------------------
  gap <- pwr - assur
  if (abs(gap) > 0.15) {
    out <- c(out, L(
      paste0("Note: the two numbers differ by ", round(100 * abs(gap)),
             " percentage points. The difference reflects the extra uncertainty ",
             "about the true effect size that the Bayesian approach takes into account",
             if (!p$same_prior) " (and the influence of the analysis prior you chose)" else "",
             ". This gap is worth considering when judging whether the study is feasible: ",
             if (gap > 0) "the traditional power calculation may be painting an optimistic picture."
             else "here the traditional power calculation is the more pessimistic of the two."),
      paste0("Aten\u00E7\u00E3o: os dois n\u00FAmeros diferem em ", round(100 * abs(gap)),
             " pontos percentuais. A diferen\u00E7a reflete a incerteza adicional sobre o ",
             "verdadeiro tamanho do efeito que a abordagem bayesiana leva em conta",
             if (!p$same_prior) " (e a influ\u00EAncia da priori de an\u00E1lise escolhida)" else "",
             ". Vale considerar essa diferen\u00E7a ao julgar se o estudo \u00E9 vi\u00E1vel: ",
             if (gap > 0) "o c\u00E1lculo tradicional de poder pode estar pintando um quadro otimista demais."
             else "aqui o c\u00E1lculo tradicional de poder \u00E9 o mais pessimista dos dois.")))
  }

  # 4. Sample size needed for the target ------------------------------------
  mt <- res$metrics
  tgt <- fmt_pct(p$target, 0)
  if (is.na(mt$n_assur)) {
    if (mt$ceiling < p$target) {
      out <- c(out, L(
        paste0("Your target of ", tgt, " assurance cannot be reached at any sample ",
               "size. Even with unlimited participants, assurance levels off at ",
               "about ", fmt_pct(mt$ceiling), ", because your design prior gives a ",
               fmt_pct(1 - mt$ceiling), " chance that the true ", short,
               " is not beyond the success threshold (", m$fmt_eff(m$C),
               "). No trial can reliably show an effect that is not there; more ",
               "participants will not fix this."),
        paste0("A sua meta de ", tgt, " de assurance n\u00E3o pode ser atingida com nenhum ",
               "tamanho amostral. Mesmo com participantes ilimitados, a assurance se ",
               "estabiliza em cerca de ", fmt_pct(mt$ceiling), ", porque a sua priori de ",
               "planejamento d\u00E1 ", fmt_pct(1 - mt$ceiling), " de chance de que o valor ",
               "verdadeiro (", short, ") n\u00E3o esteja al\u00E9m do limiar de sucesso (",
               m$fmt_eff(m$C), "). Nenhum ensaio consegue mostrar de forma confi\u00E1vel ",
               "um efeito que n\u00E3o existe; mais participantes n\u00E3o resolvem isso.")))
    } else {
      out <- c(out, L(
        paste0("Your target of ", tgt, " assurance is not reached below 20,000 ",
               "participants per group."),
        paste0("A sua meta de ", tgt, " de assurance n\u00E3o \u00E9 atingida abaixo de ",
               fmt_num(20000), " participantes por grupo.")))
    }
  } else {
    n_t <- treatment_n(mt$n_assur, p$ratio)
    txt <- L(paste0("To reach ", tgt, " assurance you would need about ",
                    describe_arms(n_t, mt$n_assur), ev_txt(n_t, mt$n_assur)),
             paste0("Para atingir ", tgt, " de assurance, voc\u00EA precisaria de cerca de ",
                    describe_arms(n_t, mt$n_assur), ev_txt(n_t, mt$n_assur)))
    if (p$dropout > 0) {
      enrol <- fmt_num(enrolled_n(n_t, p$dropout) + enrolled_n(mt$n_assur, p$dropout))
      txt <- paste0(txt, L(paste0("; allowing for ", fmt_pct(p$dropout, 0),
                                  " dropout, plan to enrol about ", enrol, " in total"),
                           paste0("; considerando ", fmt_pct(p$dropout, 0),
                                  " de abandono, planeje recrutar cerca de ", enrol, " no total")))
    }
    txt <- paste0(txt, ".")
    if (!is.na(mt$n_power)) {
      nt_p <- treatment_n(mt$n_power, p$ratio)
      comma <- if (is.null(m$events)) "" else ","
      txt <- paste0(txt, L(
        paste0(" A traditional calculation would instead ask for ",
               describe_arms(nt_p, mt$n_power), ev_txt(nt_p, mt$n_power), comma,
               " to reach ", tgt, " power."),
        paste0(" Um c\u00E1lculo tradicional pediria, em vez disso, ",
               describe_arms(nt_p, mt$n_power), ev_txt(nt_p, mt$n_power), comma,
               " para atingir ", tgt, " de poder.")))
    }
    out <- c(out, paste0(txt, L(
      " (These sample sizes use the exact formula, so they are free of simulation noise.)",
      " (Esses tamanhos amostrais usam a f\u00F3rmula exata, portanto n\u00E3o t\u00EAm ru\u00EDdo de simula\u00E7\u00E3o.)")))
  }

  # 5. One-sided ceiling, when it is informative ------------------------------
  if (m$alt != "two.sided" && mt$ceiling < 0.99 && !is.na(mt$n_assur)) {
    out <- c(out, L(
      paste0("Keep in mind that assurance can never exceed about ",
             fmt_pct(mt$ceiling), " for this design, however large the trial: that ",
             "is the probability, under your design prior, that the true effect is ",
             "beyond the success threshold at all."),
      paste0("Lembre-se de que a assurance nunca pode passar de cerca de ",
             fmt_pct(mt$ceiling), " neste desenho, por maior que seja o ensaio: essa ",
             "\u00E9 a probabilidade, segundo a sua priori de planejamento, de que o efeito ",
             "verdadeiro esteja al\u00E9m do limiar de sucesso.")))
  }

  # 6. Two-sided: success in the unexpected direction -------------------------
  if (m$alt == "two.sided" && mt$neg_share > 0.01) {
    out <- c(out, L(
      paste0("With a two-sided test, 'success' also includes finding a clear effect ",
             "in the opposite direction to the one you expect. About ",
             fmt_pct(mt$neg_share), " of the ", fmt_pct(assur), " assurance comes ",
             "from such findings, which you may not regard as a success."),
      paste0("Com um teste bilateral, \"sucesso\" tamb\u00E9m inclui encontrar um efeito claro ",
             "na dire\u00E7\u00E3o oposta \u00E0 esperada. Cerca de ", fmt_pct(mt$neg_share),
             " dos ", fmt_pct(assur), " de assurance v\u00EAm desses achados, que talvez ",
             "voc\u00EA n\u00E3o considere um sucesso.")))
  }

  # 7. Few events: approximations are less reliable -----------------------------
  if (!is.null(m$events)) {
    ev_min <- expected_events(m, tab$n_t[1], tab$n_c[1])
    if (!is.na(ev_min) && ev_min < 30) {
      out <- c(out, L(
        paste0("At the smallest sample sizes only about ", fmt_num(round(ev_min)),
               " events are expected. With so few events the normal approximations ",
               "behind the exact formula and the frequentist power are rough; the ",
               "simulation engine, which analyses the simulated counts directly, is ",
               "more trustworthy there."),
        paste0("Nos menores tamanhos amostrais s\u00E3o esperados apenas cerca de ",
               fmt_num(round(ev_min)), " eventos. Com t\u00E3o poucos eventos, as aproxima\u00E7\u00F5es ",
               "normais por tr\u00E1s da f\u00F3rmula exata e do poder frequentista s\u00E3o ",
               "grosseiras; a simula\u00E7\u00E3o, que analisa diretamente as contagens simuladas, ",
               "\u00E9 mais confi\u00E1vel nesses casos.")))
    }
  }

  out
}

# Plain-text description of every input, for the downloadable report.
describe_inputs <- function(p, m) {
  pad <- function(label) formatC(paste0(label, ":"), width = -25)
  lines <- c(paste0("  ", pad(L("Outcome", "Desfecho")), tx(m$describe)))
  if (p$otype == "surv") {
    lines <- c(lines, paste0("  ", pad(L("Survival design", "Desenho (sobrevida)")),
      L(sprintf("recruitment %s, extra follow-up %s %s, %s%% lost to follow-up",
                fmt_num(p$accrual), fmt_num(p$followup), m$time_unit, fmt_num(100 * p$loss)),
        sprintf("recrutamento %s, seguimento adicional %s %s, %s%% de perda de seguimento",
                fmt_num(p$accrual), fmt_num(p$followup), m$time_unit, fmt_num(100 * p$loss)))))
  }
  c(lines,
    paste0("  ", pad(L("Design prior", "Priori de planejamento")), describe_prior(m, "design")),
    paste0("  ", pad(L("Analysis prior", "Priori de an\u00E1lise")), describe_prior(m, "analysis"),
           if (p$same_prior) L("  [same as design prior]", "  [igual \u00E0 priori de planejamento]") else ""),
    paste0("  ", pad(L("Test", "Teste")), describe_test(m)),
    paste0("  ", pad(L("Allocation", "Aloca\u00E7\u00E3o")), fmt_num(p$ratio),
           L(" : 1 (treatment : control)", " : 1 (tratamento : controle)")),
    if (p$otype != "surv") paste0("  ", pad(L("Dropout", "Abandono")), fmt_num(100 * p$dropout), "%"),
    paste0("  ", pad(L("Target", "Meta")), fmt_pct(p$target, 0)),
    paste0("  ", pad(L("Engine", "Motor")), if (p$engine == "sim")
      paste0(tx(m$engine_label), ", ", p$mc_iter, L(" trials per n, seed ", " ensaios por n, semente "), p$seed)
      else L("exact formula", "f\u00F3rmula exata")),
    paste0("  ", pad(L("Power", "Poder")), tx(m$power_label)))
}
