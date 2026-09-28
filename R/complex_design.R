# =============================================================================
# Complex designs: a rough extrapolation from the simple two-arm trial.
#
# The app's calculations are for a simple, individually randomised two-arm
# trial analysed once. Real protocols often add complications. This file
# extrapolates the sample size found for the simple design to such a design,
# with textbook adjustments. It is meant for early planning ("roughly how much
# bigger?"), not as the final sample-size calculation for the complex design.
#
# Three kinds of adjustment, applied in this order:
#   1. Changes to the success rule, which need a recalculation of the smallest
#      sample size (with the exact engine, for all three criteria: power,
#      assurance and detection if real):
#        stricter target           the target probability of success
#        stricter alpha            a preset significance threshold
#        several arms              alpha split over the k - 1 comparisons
#                                  with the shared control (Bonferroni or
#                                  Sidak; Dunnett is slightly less strict)
#        several primary endpoints "any one succeeds": alpha split over J;
#                                  "all must succeed": the target per endpoint
#                                  becomes target^(1/J) (independence, so
#                                  conservative for correlated endpoints)
#        non-adherence / contamination
#                                  the intention-to-treat effect is diluted by
#                                  d = 1 - c_t - c_c; the design prior (and the
#                                  analysis prior when it is the same prior) is
#                                  scaled by d on the working scale. For a
#                                  known effect this is the classic
#                                  n x 1 / d^2 (Lachin 1981).
#   2. Design effects, which multiply the number of participants:
#        cluster randomisation     DE = 1 + ((CV^2 + 1) m - 1) ICC
#                                  (Eldridge, Ashby & Kerry 2006), m = mean
#                                  cluster size, CV = its coefficient of
#                                  variation
#        repeated measures         (1 + (k - 1) rho) / k for the mean of k
#                                  follow-up measurements with exchangeable
#                                  correlation rho (Frison & Pocock 1992)
#        2x2 crossover             (1 - rho) / 2 participants per "arm" (per
#                                  sequence), rho = within-person correlation
#                                  (Senn 2002): each participant gets both
#                                  treatments and is their own control
#        interim analyses          the maximum-sample-size inflation factor of
#                                  a group sequential design (Pocock or
#                                  O'Brien-Fleming boundaries, K equally spaced
#                                  analyses; Jennison & Turnbull 2000). The
#                                  factors below were computed exactly with
#                                  numerical integration and match their
#                                  tables; they depend slightly on alpha and
#                                  the target, and are interpolated.
#   3. Dropout, extra arms and clusters turn the per-group size into totals.
#
# The factors are assumed to act independently, which is itself an
# approximation (e.g. cluster crossover or stepped-wedge designs need
# dedicated methods).
# =============================================================================

CX_FEATURES <- c("cluster", "repeated", "crossover", "multiarm", "endpoints",
                 "interim", "adherence")

# Outcome types each complication is offered for.
CX_APPLIES <- list(
  cluster = c("cont", "ancova", "binary", "surv"),
  repeated = c("cont", "ancova"),
  crossover = "cont",
  multiarm = c("cont", "ancova", "binary", "surv"),
  endpoints = c("cont", "ancova", "binary", "surv"),
  interim = c("cont", "ancova", "binary", "surv"),
  adherence = c("cont", "ancova", "binary", "surv"))

# ---- Design effects -------------------------------------------------------------
cluster_de   <- function(m, icc, cv = 0) 1 + ((cv^2 + 1) * m - 1) * icc
repeated_de  <- function(k, rho) (1 + (k - 1) * rho) / k
crossover_de <- function(rho) (1 - rho) / 2

# Alpha for each of k comparisons.
split_alpha <- function(alpha, k, method = "bonferroni") {
  if (identical(method, "sidak")) 1 - (1 - alpha)^(1 / k) else alpha / k
}

# ---- Group sequential inflation factors ---------------------------------------------
# Maximum sample size of a group sequential design relative to a single
# analysis, for K = 2..5 equally spaced analyses (including the final one).
# Rows: two-sided alpha 0.1, 0.05, 0.01, 0.0025 (4 rows each, K = 2..5).
# Columns: target power 0.7, 0.8, 0.9, 0.95, 0.99.
GS_ALPHA2 <- c(0.1, 0.05, 0.01, 0.0025)
GS_POWER  <- c(0.7, 0.8, 0.9, 0.95, 0.99)
GS_TABLE <- list(
  obf = rbind(
    c(1.017, 1.016, 1.014, 1.013, 1.012), c(1.029, 1.027, 1.025, 1.023, 1.021),
    c(1.037, 1.035, 1.032, 1.030, 1.027), c(1.042, 1.040, 1.037, 1.035, 1.032),
    c(1.008, 1.008, 1.007, 1.007, 1.006), c(1.018, 1.017, 1.016, 1.015, 1.014),
    c(1.025, 1.024, 1.022, 1.021, 1.019), c(1.030, 1.028, 1.026, 1.025, 1.023),
    c(1.002, 1.001, 1.001, 1.001, 1.001), c(1.007, 1.007, 1.006, 1.006, 1.005),
    c(1.012, 1.011, 1.010, 1.010, 1.009), c(1.015, 1.015, 1.014, 1.013, 1.012),
    c(1.000, 1.000, 1.000, 1.000, 1.000), c(1.003, 1.003, 1.003, 1.003, 1.003),
    c(1.007, 1.006, 1.006, 1.006, 1.005), c(1.009, 1.009, 1.008, 1.008, 1.007)),
  pocock = rbind(
    c(1.130, 1.121, 1.109, 1.101, 1.089), c(1.198, 1.183, 1.165, 1.153, 1.134),
    c(1.241, 1.223, 1.201, 1.186, 1.163), c(1.273, 1.252, 1.227, 1.210, 1.184),
    c(1.119, 1.110, 1.100, 1.093, 1.082), c(1.180, 1.166, 1.151, 1.140, 1.123),
    c(1.219, 1.202, 1.183, 1.170, 1.149), c(1.247, 1.228, 1.206, 1.191, 1.168),
    c(1.098, 1.092, 1.084, 1.078, 1.069), c(1.147, 1.137, 1.125, 1.117, 1.103),
    c(1.179, 1.166, 1.152, 1.141, 1.125), c(1.201, 1.187, 1.170, 1.159, 1.141),
    c(1.085, 1.080, 1.073, 1.068, 1.061), c(1.128, 1.119, 1.109, 1.102, 1.091),
    c(1.154, 1.144, 1.132, 1.124, 1.110), c(1.173, 1.162, 1.149, 1.139, 1.124)))

# Inflation factor for K analyses; alpha2 = two-sided alpha (2 x the one-sided
# alpha). Interpolated in the target and in log(alpha); clamped to the table.
gs_inflation <- function(K, bound, alpha2, power) {
  if (K < 2) return(1)
  K <- min(K, 5)
  tab <- GS_TABLE[[bound]]
  pw <- min(max(power, min(GS_POWER)), max(GS_POWER))
  la <- log(GS_ALPHA2)
  x  <- min(max(log(alpha2), min(la)), max(la))
  at_alpha <- vapply(seq_along(GS_ALPHA2), function(i)
    stats::approx(GS_POWER, tab[(i - 1) * 4 + K - 1, ], pw)$y, 0)
  stats::approx(la, at_alpha, x)$y
}

# ---- Input checks ---------------------------------------------------------------------
# `cx` is the list of complex-design inputs (see cx_inputs() in app.R).
# Returns NULL, or a message in the current language.
cx_check <- function(cx) {
  is_num <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  whole  <- function(x, lo, hi) is_num(x) && abs(x - round(x)) < 1e-8 && x >= lo && x <= hi
  has <- function(f) f %in% cx$features
  msgs <- c(
    if (!is_num(cx$target) || cx$target < 0.5 || cx$target > 0.99)
      L("Choose a stricter target between 50% and 99%.", "Escolha uma meta mais exigente entre 50% e 99%."),
    if (!is.na(cx$alpha) && (!is_num(cx$alpha) || cx$alpha <= 0 || cx$alpha >= 1))
      L("The stricter alpha must be between 0 and 1.", "O alfa mais exigente deve estar entre 0 e 1."),
    if (has("cluster") && !(is_num(cx$m) && cx$m >= 1 && cx$m <= 10000))
      L("Mean cluster size must be at least 1.", "O tamanho m\u00E9dio do cluster deve ser pelo menos 1."),
    if (has("cluster") && !(is_num(cx$icc) && cx$icc >= 0 && cx$icc < 1))
      L("The ICC must be at least 0 and below 1.", "O ICC deve ser pelo menos 0 e menor que 1."),
    if (has("cluster") && !(is_num(cx$cv) && cx$cv >= 0 && cx$cv <= 3))
      L("The cluster-size CV must be between 0 and 3.", "O CV do tamanho dos clusters deve estar entre 0 e 3."),
    if (has("repeated") && !whole(cx$k, 1, 50))
      L("The number of follow-up measurements must be a whole number from 1 to 50.",
        "O n\u00FAmero de medidas de seguimento deve ser um n\u00FAmero inteiro de 1 a 50."),
    if (has("repeated") && !(is_num(cx$rep_rho) && cx$rep_rho >= 0 && cx$rep_rho <= 1))
      L("The correlation between repeated measurements must be between 0 and 1.",
        "A correla\u00E7\u00E3o entre as medidas repetidas deve estar entre 0 e 1."),
    if (has("crossover") && !(is_num(cx$x_rho) && cx$x_rho >= 0 && cx$x_rho <= 0.99))
      L("The within-person correlation must be between 0 and 0.99.",
        "A correla\u00E7\u00E3o intraindividual deve estar entre 0 e 0,99."),
    if (has("multiarm") && !whole(cx$arms, 3, 10))
      L("The number of arms (including control) must be a whole number from 3 to 10.",
        "O n\u00FAmero de bra\u00E7os (incluindo o controle) deve ser um n\u00FAmero inteiro de 3 a 10."),
    if (has("endpoints") && !whole(cx$J, 2, 10))
      L("The number of primary endpoints must be a whole number from 2 to 10.",
        "O n\u00FAmero de desfechos prim\u00E1rios deve ser um n\u00FAmero inteiro de 2 a 10."),
    if (has("interim") && !whole(cx$looks, 2, 5))
      L("The number of analyses must be a whole number from 2 to 5.",
        "O n\u00FAmero de an\u00E1lises deve ser um n\u00FAmero inteiro de 2 a 5."),
    if (has("adherence") && !(is_num(cx$nonadh) && is_num(cx$contam) && cx$nonadh >= 0 &&
                              cx$contam >= 0 && cx$nonadh + cx$contam < 90))
      L("Non-adherence and contamination must be at least 0% and add up to less than 90%.",
        "N\u00E3o ades\u00E3o e contamina\u00E7\u00E3o devem ser pelo menos 0% e somar menos de 90%."))
  if (length(msgs)) msgs[1] else NULL
}

# ---- The extrapolation -------------------------------------------------------------------
# p, m : validated inputs and model of the simple design (from the last run)
# cx   : complex-design inputs (checked with cx_check)
# base : the simple design's smallest control-group sizes at p$target,
#        list(power = , assurance = , detect = ) (NA = not reachable)
cx_extrapolate <- function(p, m, cx, base) {
  used <- intersect(CX_FEATURES, cx$features)
  ok_type <- vapply(used, function(f) p$otype %in% CX_APPLIES[[f]], logical(1))
  ignored <- used[!ok_type]; used <- used[ok_type]
  has <- function(f) f %in% used
  arms <- if (has("multiarm")) as.integer(cx$arms) else 2L
  J    <- if (has("endpoints")) as.integer(cx$J) else 1L

  # 1. Changes to the success rule (each step keeps the earlier ones).
  a0 <- p$alpha; t0 <- p$target
  a1 <- if (is.na(cx$alpha)) a0 else cx$alpha
  a2 <- if (has("multiarm")) split_alpha(a1, arms - 1, cx$mult) else a1
  a3 <- if (has("endpoints") && cx$jmode == "any") split_alpha(a2, J) else a2
  t1 <- cx$target
  t3 <- if (has("endpoints") && cx$jmode == "all") t1^(1 / J) else t1
  dil <- if (has("adherence")) 1 - (cx$nonadh + cx$contam) / 100 else 1

  steps <- list(list(key = "simple", alpha = a0, target = t0, dil = 1))
  add <- function(key, alpha, target, d) steps[[length(steps) + 1]] <<- list(key = key, alpha = alpha, target = target, dil = d)
  if (abs(t1 - t0) > 1e-9) add("target", a0, t1, 1)
  if (abs(a1 - a0) > 1e-12) add("alpha", a1, t1, 1)
  if (has("multiarm")) add("multiarm", a2, t1, 1)
  if (has("endpoints")) add("endpoints", a3, t3, 1)
  if (has("adherence")) add("adherence", a3, t3, dil)

  models <- list()
  model_for <- function(alpha, d) {
    key <- paste(alpha, d)
    if (is.null(models[[key]])) {
      mx <- if (abs(alpha - p$alpha) < 1e-15) m else build_model(modifyList(p, list(alpha = alpha)))
      if (d != 1) {
        mx <- model_with_priors(mx, m_d = d * mx$m_d, s_d = d * mx$s_d,
                                m_a = if (p$same_prior) d * mx$m_a else mx$m_a,
                                s_a = if (p$same_prior) d * mx$s_a else mx$s_a)
      }
      models[[key]] <<- mx
    }
    models[[key]]
  }
  crit_fun <- list(
    power     = function(mx) function(nt, nc) model_power(mx, nt, nc),
    assurance = function(mx) function(nt, nc) model_exact(mx, nt, nc),
    detect    = function(mx) function(nt, nc) detect_rate(mx, nt, nc))

  # 2. Design effects.
  mult <- list()
  if (has("cluster"))   mult$cluster   <- cluster_de(cx$m, cx$icc, cx$cv)
  if (has("repeated"))  mult$repeated  <- repeated_de(cx$k, cx$rep_rho)
  if (has("crossover")) mult$crossover <- crossover_de(cx$x_rho)
  if (has("interim"))   mult$interim   <- gs_inflation(cx$looks, cx$bound,
                                                       if (p$alt == "two.sided") a3 else 2 * a3, t3)
  de <- if (length(mult)) prod(unlist(mult)) else 1
  final_model <- model_for(a3, dil)

  one_crit <- function(crit) {
    n_rule <- vapply(steps, function(s) {
      if (s$key == "simple") as.numeric(base[[crit]])
      else as.numeric(smallest_n(crit_fun[[crit]](model_for(s$alpha, s$dil)), p$ratio, s$target))
    }, 0)
    n_eff <- n_rule[length(n_rule)]
    n_chain <- c(n_rule, n_eff * cumprod(unlist(mult)))
    keys <- c(vapply(steps, `[[`, "", "key"), names(mult))
    final <- NULL
    if (!is.na(n_eff)) {
      nc <- max(2, ceiling(n_eff * de - 1e-9))
      nt <- max(2, ceiling(treatment_n(n_eff, p$ratio) * de - 1e-9))
      en_c <- enrolled_n(nc, p$dropout); en_t <- enrolled_n(nt, p$dropout)
      final <- list(
        n_c = nc, n_t = nt, total = nc + (arms - 1) * nt,
        enrol = en_c + (arms - 1) * en_t,
        clusters = if (has("cluster")) ceiling(en_c / cx$m) + (arms - 1) * ceiling(en_t / cx$m) else NA,
        events = if (is.null(final_model$events)) NA_real_ else
          final_model$events(final_model$m_d, nt, nc) + (arms - 2) * final_model$events(final_model$m_d, nt, 0))
    }
    list(steps = data.frame(key = keys, n = n_chain, stringsAsFactors = FALSE), final = final)
  }

  list(used = used, ignored = ignored, arms = arms, J = J,
       alpha = c(simple = a0, preset = a1, final = a3),
       target = c(simple = t0, stricter = t1, final = t3),
       dil = dil, mult = mult, de = de, cx = cx,
       ceiling = model_ceiling(final_model),
       crit = lapply(setNames(names(crit_fun), names(crit_fun)), one_crit))
}


# =============================================================================
# Text (current language)
# =============================================================================
fmt_int <- function(x) {
  pt <- current_lang() == "pt"
  format(round(x), big.mark = if (pt) "." else ",", decimal.mark = if (pt) "," else ".",
         scientific = FALSE, trim = TRUE)
}
fmt_alpha <- function(a) fmt_num(signif(a, 3))
fmt_x <- function(f) paste0("\u00D7", vapply(f, function(x) fmt_dec(x, if (x < 1.1 && x > 0.9) 3 else 2), ""))

cx_crit_name <- function(crit) switch(crit,
  power = L("Frequentist power", "Poder frequentista"),
  assurance = L("Bayesian assurance", "Assurance bayesiana"),
  detect = L("Detection if real", "Detec\u00E7\u00E3o se real"))

cx_feature_name <- function(f) switch(f,
  cluster   = L("Cluster randomisation", "Randomiza\u00E7\u00E3o por clusters"),
  repeated  = L("Repeated measures", "Medidas repetidas"),
  crossover = L("2x2 crossover", "Crossover 2x2"),
  multiarm  = L("Several arms", "V\u00E1rios bra\u00E7os"),
  endpoints = L("Several primary endpoints", "V\u00E1rios desfechos prim\u00E1rios"),
  interim   = L("Interim analyses", "An\u00E1lises interinas"),
  adherence = L("Non-adherence / contamination", "N\u00E3o ades\u00E3o / contamina\u00E7\u00E3o"))

# Short labels for the waterfall bars.
cx_step_label <- function(key, r) switch(key,
  simple    = L("Simple design", "Desenho simples"),
  target    = paste0(L("Target ", "Meta "), fmt_pct(r$target[["stricter"]], 0)),
  alpha     = paste0(L("Alpha ", "Alfa "), fmt_alpha(r$alpha[["preset"]])),
  multiarm  = paste0(r$arms, L(" arms", " bra\u00E7os")),
  endpoints = paste0(r$J, L(" endpoints", " desfechos")),
  adherence = L("Non-adherence", "N\u00E3o ades\u00E3o"),
  cluster   = L("Clusters", "Clusters"),
  repeated  = L("Repeated measures", "Medidas repetidas"),
  crossover = L("Crossover", "Crossover"),
  interim   = L("Interim analyses", "An\u00E1lises interinas"),
  final     = L("Complex design", "Desenho complexo"))

# One row per adjustment: what you entered, what it changes, and by how much
# it multiplies the sample size needed for frequentist power.
cx_factor_table <- function(r, p) {
  st <- r$crit$power$steps
  ratio_of <- function(key) {
    i <- match(key, st$key)
    if (is.na(i) || i == 1 || is.na(st$n[i]) || is.na(st$n[i - 1])) return("\u2014")
    fmt_x(st$n[i] / st$n[i - 1])
  }
  cx <- r$cx; rows <- list()
  row <- function(key, name, input, change, src)
    rows[[length(rows) + 1]] <<- c(name, input, change, ratio_of(key), src)
  if ("target" %in% st$key)
    row("target", L("Stricter target", "Meta mais exigente"),
        paste0(fmt_pct(r$target[["simple"]], 0), " \u2192 ", fmt_pct(r$target[["stricter"]], 0)),
        L("Sample size recalculated", "Tamanho amostral recalculado"), "")
  if ("alpha" %in% st$key)
    row("alpha", L("Stricter alpha", "Alfa mais exigente"),
        paste0(fmt_alpha(r$alpha[["simple"]]), " \u2192 ", fmt_alpha(r$alpha[["preset"]])),
        L("Sample size recalculated", "Tamanho amostral recalculado"), "")
  if ("multiarm" %in% r$used)
    row("multiarm", cx_feature_name("multiarm"),
        paste0(r$arms, L(" arms incl. control; ", " bra\u00E7os com o controle; "),
               if (cx$mult == "sidak") "\u0160id\u00E1k" else "Bonferroni"),
        paste0(L("Alpha per comparison ", "Alfa por compara\u00E7\u00E3o "),
               fmt_alpha(split_alpha(r$alpha[["preset"]], r$arms - 1, cx$mult)),
               L("; each extra arm adds its own participants", "; cada bra\u00E7o extra traz os seus participantes")),
        "Bonferroni / \u0160id\u00E1k")
  if ("endpoints" %in% r$used)
    row("endpoints", cx_feature_name("endpoints"),
        paste0(r$J, if (cx$jmode == "all") L(", all must succeed", ", todos devem ter sucesso")
               else L(", any one is enough", ", basta um")),
        if (cx$jmode == "all") paste0(L("Target per endpoint ", "Meta por desfecho "), fmt_pct(r$target[["final"]], 1))
        else paste0(L("Alpha per endpoint ", "Alfa por desfecho "), fmt_alpha(r$alpha[["final"]])),
        "Bonferroni")
  if ("adherence" %in% r$used)
    row("adherence", cx_feature_name("adherence"),
        paste0(fmt_num(cx$nonadh), L("% not taking treatment; ", "% sem tomar o tratamento; "),
               fmt_num(cx$contam), L("% of controls treated", "% dos controles tratados")),
        paste0(L("Intention-to-treat effect ", "Efeito por inten\u00E7\u00E3o de tratar "), fmt_x(r$dil),
               L(" (rule of thumb: n ", " (regra pr\u00E1tica: n "), fmt_x(1 / r$dil^2), ")"),
        "Lachin 1981")
  if ("cluster" %in% r$used)
    row("cluster", cx_feature_name("cluster"),
        paste0("m = ", fmt_num(cx$m), ", ICC = ", fmt_num(cx$icc), ", CV = ", fmt_num(cx$cv)),
        paste0(L("Design effect 1 + ((CV\u00B2 + 1) m \u2212 1) ICC = ", "Efeito de desenho 1 + ((CV\u00B2 + 1) m \u2212 1) ICC = "),
               fmt_dec(r$mult$cluster, 2)),
        "Eldridge et al. 2006")
  if ("repeated" %in% r$used)
    row("repeated", cx_feature_name("repeated"),
        paste0("k = ", cx$k, ", \u03C1 = ", fmt_num(cx$rep_rho)),
        paste0(L("Mean of k measurements: (1 + (k \u2212 1) \u03C1) / k = ", "M\u00E9dia de k medidas: (1 + (k \u2212 1) \u03C1) / k = "),
               fmt_dec(r$mult$repeated, 2)),
        "Frison & Pocock 1992")
  if ("crossover" %in% r$used)
    row("crossover", cx_feature_name("crossover"),
        paste0("\u03C1 = ", fmt_num(cx$x_rho)),
        paste0(L("Each participant is their own control: (1 \u2212 \u03C1) / 2 = ",
                 "Cada participante \u00E9 o seu pr\u00F3prio controle: (1 \u2212 \u03C1) / 2 = "),
               fmt_dec(r$mult$crossover, 2)),
        "Senn 2002")
  if ("interim" %in% r$used)
    row("interim", cx_feature_name("interim"),
        paste0(cx$looks, L(" analyses, ", " an\u00E1lises, "),
               if (cx$bound == "pocock") "Pocock" else "O'Brien-Fleming"),
        paste0(L("Maximum sample size inflation factor ", "Fator de infla\u00E7\u00E3o do tamanho m\u00E1ximo "),
               fmt_dec(r$mult$interim, 3)),
        "Jennison & Turnbull 2000")
  if (!length(rows)) return(NULL)
  d <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(d) <- c(L("Adjustment", "Ajuste"), L("Your input", "Sua entrada"),
                L("What it changes", "O que muda"), L("n multiplier (power)", "Multiplicador do n (poder)"),
                L("Source", "Fonte"))
  d
}

# Per-criterion results table.
cx_result_table <- function(r, p, base) {
  rows <- lapply(names(r$crit), function(crit) {
    f <- r$crit[[crit]]$final; b <- base[[crit]]
    na <- L("not reachable", "inating\u00EDvel")
    row <- c(
      cx_crit_name(crit),
      if (is.na(b)) na else fmt_int(b),
      if (is.null(f)) na else if (f$n_t == f$n_c) fmt_int(f$n_c) else paste0(fmt_int(f$n_t), " / ", fmt_int(f$n_c)),
      if (is.null(f)) "\u2014" else fmt_int(f$enrol))
    if ("cluster" %in% r$used) row <- c(row, if (is.null(f)) "\u2014" else fmt_int(f$clusters))
    if (p$otype %in% c("binary", "surv")) row <- c(row, if (is.null(f)) "\u2014" else fmt_int(f$events))
    row
  })
  per <- if (p$ratio == 1) L(" per group", " por grupo") else L(" (T / C)", " (T / C)")
  cols <- c(L("Criterion", "Crit\u00E9rio"),
            paste0(L("Simple design: n", "Desenho simples: n"), if (p$ratio == 1) per else L(" (control)", " (controle)"),
                   ", ", fmt_pct(p$target, 0)),
            paste0(L("Complex design: n", "Desenho complexo: n"), per, ", ", fmt_pct(r$target[["stricter"]], 0)),
            L("Total to enrol", "Total a recrutar"))
  if ("cluster" %in% r$used) cols <- c(cols, L("Clusters", "Clusters"))
  if (p$otype %in% c("binary", "surv")) cols <- c(cols, L("Expected events", "Eventos esperados"))
  d <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(d) <- cols
  d
}

# Headline paragraphs.
cx_headline <- function(r, p, base) {
  pw <- r$crit$power; as <- r$crit$assurance
  feats <- vapply(r$used, cx_feature_name, "")
  what <- c(feats, if (abs(r$target[["stricter"]] - p$target) > 1e-9)
                     paste0(L("a ", "uma meta de "), fmt_pct(r$target[["stricter"]], 0), L(" target", "")),
            if (abs(r$alpha[["preset"]] - p$alpha) > 1e-12)
              paste0(L("alpha ", "alfa "), fmt_alpha(r$alpha[["preset"]])))
  what_txt <- if (length(what)) paste(tolower(what), collapse = ", ") else L("no changes", "nenhuma mudan\u00E7a")
  n_txt <- function(f) if (f$n_t == f$n_c) paste0(fmt_int(f$n_c), L(" per group", " por grupo"))
                       else paste0(fmt_int(f$n_t), " / ", fmt_int(f$n_c), " (T / C)")
  simple <- if (is.na(base$power)) L("cannot reach the target power", "n\u00E3o atinge o poder desejado")
            else paste0(L("needs n = ", "precisa de n = "), fmt_int(base$power), L(" for ", " para "),
                        fmt_pct(p$target, 0), L(" power", " de poder"))
  intro <- paste0(L("The simple two-arm design ", "O desenho simples de dois bra\u00E7os "), simple,
                  if (!is.na(base$assurance)) paste0(" (", fmt_int(base$assurance), L(" for ", " para "),
                                                     fmt_pct(p$target, 0), L(" assurance)", " de assurance)")) else "",
                  L(". With ", ". Com "), what_txt, ":")
  if (is.null(pw$final)) {
    out <- paste(intro, L("The frequentist power target cannot be reached with these settings.",
                    "A meta de poder frequentista n\u00E3o pode ser atingida com essas configura\u00E7\u00F5es."))
  } else {
    f <- pw$final
    out <- paste(intro, paste0(
      L("a rough extrapolation is about ", "uma extrapola\u00E7\u00E3o grosseira d\u00E1 cerca de "), n_txt(f),
      L(" for ", " para "), fmt_pct(r$target[["stricter"]], 0), L(" power", " de poder"),
      if (!is.null(as$final)) paste0(" (", n_txt(as$final), L(" for assurance)", " para a assurance)"))
      else L(" (the assurance target is not reachable)", " (a meta de assurance \u00E9 inating\u00EDvel)"),
      L(": ", ": "), fmt_int(f$enrol), L(" participants to enrol in total", " participantes a recrutar no total"),
      if (r$arms > 2) paste0(L(" across ", " em "), r$arms, L(" arms", " bra\u00E7os")) else "",
      if (!is.na(f$clusters)) paste0(L(", in about ", ", em cerca de "), fmt_int(f$clusters), L(" clusters", " clusters")) else "",
      if (!is.na(f$events) && p$otype == "surv") paste0(L(", with about ", ", com cerca de "), fmt_int(f$events), L(" events", " eventos")) else "",
      "."))
    st <- pw$steps
    if (nrow(st) > 1 && !is.na(base$power)) {
      ratio <- st$n[-1] / st$n[-nrow(st)]
      i <- which.max(abs(log(ratio)))
      overall <- f$n_c / base$power
      out <- c(out, paste0(
        L("Overall that is ", "No total, isso \u00E9 "), fmt_x(overall), L(" the simple design's size per group. ",
          " o tamanho por grupo do desenho simples. "),
        L("The largest single change comes from ", "A maior mudan\u00E7a isolada vem de "),
        tolower(cx_step_label(st$key[i + 1], r)), " (", fmt_x(ratio[i]), ")."))
    }
  }
  out
}

# Cautions for the adjustments in use.
cx_notes <- function(r, p) {
  cx <- r$cx; f <- r$crit$power$final
  n <- c(
    L("This is an approximate extrapolation for early planning. It treats each complication as an independent multiplier, which real designs rarely are exactly. Confirm the final sample size with methods made for your design, ideally by simulation.",
      "Esta \u00E9 uma extrapola\u00E7\u00E3o aproximada para o planejamento inicial. Ela trata cada complica\u00E7\u00E3o como um multiplicador independente, o que raramente \u00E9 exato em desenhos reais. Confirme o tamanho amostral final com m\u00E9todos feitos para o seu desenho, de prefer\u00EAncia por simula\u00E7\u00E3o."))
  if (length(r$ignored))
    n <- c(n, paste0(L("Not applied to this outcome type: ", "N\u00E3o aplicado a este tipo de desfecho: "),
                     paste(vapply(r$ignored, cx_feature_name, ""), collapse = ", "),
                     L(" (repeated measures: continuous outcomes; crossover: difference in means).",
                       " (medidas repetidas: desfechos cont\u00EDnuos; crossover: diferen\u00E7a de m\u00E9dias).")))
  if ("cluster" %in% r$used) {
    n <- c(n, L("Cluster trials: the ICC is often uncertain, so try a range of values. The design effect assumes the analysis accounts for clustering (e.g. mixed models or GEE).",
                "Ensaios por clusters: o ICC costuma ser incerto, ent\u00E3o teste uma faixa de valores. O efeito de desenho sup\u00F5e que a an\u00E1lise considere os clusters (ex.: modelos mistos ou GEE)."))
    if (!is.null(f) && !is.na(f$clusters) && f$clusters / r$arms < 10)
      n <- c(n, L("Fewer than about 10 clusters per arm: the design effect underestimates the sample size needed, because small-sample corrections cost extra power. Consider adding clusters (a common rule is one extra cluster per arm) and use dedicated software.",
                  "Menos de cerca de 10 clusters por bra\u00E7o: o efeito de desenho subestima o tamanho necess\u00E1rio, porque as corre\u00E7\u00F5es para amostras pequenas custam poder. Considere acrescentar clusters (uma regra comum \u00E9 um cluster a mais por bra\u00E7o) e use software espec\u00EDfico."))
  }
  if ("repeated" %in% r$used)
    n <- c(n, L("Repeated measures: assumes the analysis uses the mean of the follow-up measurements, equal correlation between them, and no missing visits.",
                "Medidas repetidas: sup\u00F5e que a an\u00E1lise use a m\u00E9dia das medidas de seguimento, correla\u00E7\u00E3o igual entre elas e nenhuma visita perdida."))
  if ("crossover" %in% r$used)
    n <- c(n, paste0(L("Crossover: the sizes shown are per sequence (AB and BA); every participant receives both treatments. It assumes a stable condition, an adequate washout and no carry-over.",
                       "Crossover: os tamanhos mostrados s\u00E3o por sequ\u00EAncia (AB e BA); todo participante recebe os dois tratamentos. Sup\u00F5e uma condi\u00E7\u00E3o est\u00E1vel, washout adequado e nenhum efeito residual (carry-over)."),
                     if (p$ratio != 1) L(" Crossover trials normally use equal sequences; your allocation ratio was kept but is unusual here.",
                                         " Ensaios crossover normalmente usam sequ\u00EAncias iguais; a sua raz\u00E3o de aloca\u00E7\u00E3o foi mantida, mas \u00E9 incomum aqui.") else ""))
  if ("multiarm" %in% r$used)
    n <- c(n, L("Several arms: each treatment arm is compared with the shared control. Dunnett's test is slightly less strict than Bonferroni, and giving the control about \u221A(k \u2212 1) times as many participants as each treatment arm is more efficient.",
                "V\u00E1rios bra\u00E7os: cada bra\u00E7o de tratamento \u00E9 comparado ao controle comum. O teste de Dunnett \u00E9 um pouco menos exigente que Bonferroni, e dar ao controle cerca de \u221A(k \u2212 1) vezes o n\u00FAmero de participantes de cada bra\u00E7o de tratamento \u00E9 mais eficiente."))
  if ("endpoints" %in% r$used)
    n <- c(n, if (cx$jmode == "all")
      L("All endpoints must succeed: the target for each is raised as if they were independent, which is conservative when the endpoints are correlated. It assumes each endpoint needs about the same sample size as this one.",
        "Todos os desfechos devem ter sucesso: a meta de cada um \u00E9 elevada como se fossem independentes, o que \u00E9 conservador quando os desfechos s\u00E3o correlacionados. Sup\u00F5e que cada desfecho precise de um tamanho amostral parecido com o deste.")
      else L("Any endpoint is enough: alpha is split equally (Bonferroni); hierarchical (fixed-sequence) testing or Holm's procedure can be less costly.",
             "Basta um desfecho: o alfa \u00E9 dividido igualmente (Bonferroni); testes hier\u00E1rquicos (sequ\u00EAncia fixa) ou o procedimento de Holm podem custar menos."))
  if ("interim" %in% r$used)
    n <- c(n, L("Interim analyses: the number shown is the maximum sample size, with equally spaced analyses that can stop early for efficacy. The expected sample size is usually smaller. For assurance and detection if real the same factor is applied as a rule of thumb.",
                "An\u00E1lises interinas: o n\u00FAmero mostrado \u00E9 o tamanho m\u00E1ximo, com an\u00E1lises igualmente espa\u00E7adas que podem parar cedo por efic\u00E1cia. O tamanho esperado costuma ser menor. Para a assurance e a detec\u00E7\u00E3o se real o mesmo fator \u00E9 aplicado como regra pr\u00E1tica."))
  if ("adherence" %in% r$used)
    n <- c(n, L("Non-adherence: assumes non-adherent participants respond like controls and treated controls respond like the treatment group, for an intention-to-treat analysis. Your analysis prior is left unchanged unless you analyse with the design prior.",
                "N\u00E3o ades\u00E3o: sup\u00F5e que participantes n\u00E3o aderentes respondam como controles e que controles tratados respondam como o grupo tratamento, numa an\u00E1lise por inten\u00E7\u00E3o de tratar. A sua priori de an\u00E1lise n\u00E3o muda, a menos que voc\u00EA analise com a priori de planejamento."))
  st_a <- r$crit$assurance$steps
  if ("target" %in% st_a$key) {
    i <- match("target", st_a$key)
    if (!is.na(st_a$n[i]) && !is.na(st_a$n[i - 1]) && st_a$n[i] > 2 * st_a$n[i - 1])
      n <- c(n, L("Assurance grows slowly as it approaches its ceiling, so a stricter target costs far more for assurance than for power. That is a real feature of your uncertainty about the effect, not an artefact.",
                  "A assurance cresce devagar perto do seu teto, ent\u00E3o uma meta mais exigente custa muito mais para a assurance do que para o poder. Isso reflete de fato a sua incerteza sobre o efeito; n\u00E3o \u00E9 um artefato."))
  }
  if (r$ceiling < r$target[["final"]])
    n <- c(n, paste0(L("The assurance target is above the assurance ceiling (", "A meta de assurance est\u00E1 acima do teto da assurance ("),
                     fmt_pct(r$ceiling), L("), so no sample size reaches it.", "), ent\u00E3o nenhum tamanho amostral a atinge.")))
  c(n, L("Dedicated tools: clusterPower, CRTSize or the Shiny CRT Calculator (cluster trials); swCRTdesign or SWSamp (stepped-wedge); rpact or gsDesign (interim analyses); DunnettTests or MAMS (several arms); longpower (repeated measures); or simulation of the full design.",
         "Ferramentas espec\u00EDficas: clusterPower, CRTSize ou o Shiny CRT Calculator (ensaios por clusters); swCRTdesign ou SWSamp (stepped-wedge); rpact ou gsDesign (an\u00E1lises interinas); DunnettTests ou MAMS (v\u00E1rios bra\u00E7os); longpower (medidas repetidas); ou simula\u00E7\u00E3o do desenho completo."))
}

# Plain-text lines for the downloaded report.
cx_report_lines <- function(r, p, base) {
  wrap <- function(x, first = "  ", rest = "  ")
    unlist(lapply(x, strwrap, width = 78, prefix = rest, initial = first))
  tab <- cx_result_table(r, p, base); fac <- cx_factor_table(r, p)
  c(wrap(cx_headline(r, p, base)),
    "",
    utils::capture.output(print(tab, row.names = FALSE)),
    "",
    if (!is.null(fac)) c(wrap(paste0(fac[[1]], ": ", fac[[2]], ". ", fac[[3]], " (n ", fac[[4]], ")."),
                              "  - ", "    "), ""),
    wrap(cx_notes(r, p), "  * ", "    "))
}
