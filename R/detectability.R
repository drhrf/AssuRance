# =============================================================================
# Detectability: how easily would the trial detect a true effect?
#
# All quantities come from the exact engine: we average over the design prior
# with the quadrature grid used for exact assurance (QUAD_Z / QUAD_W in
# R/calculations.R), so they work for every outcome type and are instant.
#
# "True effect" = the true effect lies beyond the success threshold C:
#     one-sided ">" : theta > C        one-sided "<" : theta < C
#     two-sided     : theta != 0 (always true for a continuous prior); the
#                     trial "detects" it only if it succeeds in the RIGHT
#                     direction. Success in the wrong direction is a
#                     direction error (a "type S" error).
#
# For each sample size the trial ends in one of these outcomes (probabilities
# averaged over the design prior and the data):
#     detected   : effect real and trial succeeds (right direction)
#     missed     : effect real and trial fails
#     false      : effect NOT real but trial succeeds       (one-sided)
#     wrong_dir  : trial succeeds in the wrong direction    (two-sided)
#     correct_no : effect NOT real and trial fails          (one-sided)
#
# Measures
#   detect      P(detected | effect real)  -- the headline "detection if real"
#               (known as "expected power" / "power given a relevant effect";
#               Kunzmann et al. 2021, The American Statistician 75:424-432)
#   ppv         P(effect real | trial succeeds)
#   h_outcome   Shannon entropy of the trial result, in bits:
#               1 bit = a coin flip, 0 = the result is certain
#   mi_truth    Shannon mutual information between "is the effect real (and
#               in which direction)?" and the trial's result, in bits: how
#               much the result tells you about whether the effect is real
#   mi_share    mi_truth as a share of the prior uncertainty about that
#               question (H(truth)); NA when there is almost no such doubt
#   mi_theta    information about the SIZE of the effect, in bits:
#               1/2 log2(1 + s_d^2 / v), the mutual information between the
#               effect and its estimate (normal approximation, with the
#               estimate's variance v averaged over the design prior)
#   var_removed share of your uncertainty about the size of the effect that
#               the trial is expected to remove: s_d^2 / (s_d^2 + v)
# =============================================================================

h2 <- function(q) ifelse(q <= 0 | q >= 1, 0, -q * log2(q) - (1 - q) * log2(1 - q))

# Shannon mutual information (bits) of a joint probability table.
mutual_info <- function(J) {
  J <- J / sum(J)
  pr <- rowSums(J); pc <- colSums(J)
  E <- outer(pr, pc)
  keep <- J > 1e-15
  sum(J[keep] * log2(J[keep] / E[keep]))
}

entropy_bits <- function(p) { p <- p[p > 1e-15]; -sum(p * log2(p)) }

# Detectability measures for one sample size.
detectability_one <- function(m, n_t, n_c) {
  th <- m$m_d + m$s_d * QUAD_Z
  w  <- QUAD_W
  se2 <- rep_len(m$se2(th, n_t, n_c), length(th))
  sp <- success_prob(cutoffs_from_se2(se2, m$m_a, m$s_a, m$alpha, m$alt, m$C), th, sqrt(se2))

  # Share of each grid cell lying beyond a boundary b: 1 or 0 except for the
  # cell containing b, which is split proportionally. This makes P(effect
  # real) accurate even though the grid treats theta as discrete.
  h <- m$s_d * (QUAD_Z[2] - QUAD_Z[1])
  above <- function(b) pmin(1, pmax(0, (th + h / 2 - b) / h))

  if (m$alt == "two.sided") {
    up <- above(0); down <- 1 - up
    right <- up * sp$positive + down * sp$negative
    wrong <- up * sp$negative + down * sp$positive
    fail  <- 1 - sp$total
    detected <- sum(w * right); wrong_dir <- sum(w * wrong); missed <- sum(w * fail)
    false_s <- 0; correct_no <- 0
    p_real <- 1
    # truth = direction of the effect; result = success up / success down / fail
    J <- rbind(up   = c(sum(w * up * sp$positive), sum(w * up * sp$negative), sum(w * up * fail)),
               down = c(sum(w * down * sp$positive), sum(w * down * sp$negative), sum(w * down * fail)))
  } else {
    real <- if (m$alt == "less") 1 - above(m$C) else above(m$C)
    ps <- sp$total
    detected <- sum(w * ps * real); missed <- sum(w * (1 - ps) * real)
    false_s <- sum(w * ps * (1 - real)); correct_no <- sum(w * (1 - ps) * (1 - real))
    wrong_dir <- 0
    p_real <- sum(w * real)
    J <- rbind(real = c(detected, missed), not_real = c(false_s, correct_no))
  }
  succ <- detected + false_s + wrong_dir
  h_truth <- entropy_bits(rowSums(J))
  mi <- mutual_info(J)
  vbar <- sum(w * se2)
  list(
    detect = if (p_real > 1e-12) detected / p_real else NA_real_,
    p_real = p_real, assurance = succ,
    detected = detected, missed = missed, false_success = false_s,
    wrong_dir = wrong_dir, correct_no = correct_no,
    ppv = if (succ > 1e-12) detected / succ else NA_real_,
    h_outcome = entropy_bits(colSums(J)),
    mi_truth = mi, h_truth = h_truth,
    mi_share = if (h_truth > 0.05) mi / h_truth else NA_real_,
    mi_theta = 0.5 * log2(1 + m$s_d^2 / vbar),
    var_removed = m$s_d^2 / (m$s_d^2 + vbar))
}

# Vectorised over sample sizes: a data frame with one row per sample size.
detectability <- function(m, n_t, n_c) {
  rows <- lapply(seq_along(n_c), function(i) as.data.frame(detectability_one(m, n_t[i], n_c[i])))
  cbind(n_t = n_t, n_c = n_c, do.call(rbind, rows))
}

# Detection rate only, vectorised (for the sample-size finder).
detect_rate <- function(m, n_t, n_c) {
  vapply(seq_along(n_c), function(i) detectability_one(m, n_t[i], n_c[i])$detect, 0)
}
