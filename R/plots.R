# =============================================================================
# Plot builders. Interactive plots use plotly; the downloadable PNG of the
# main curve uses ggplot2.
# =============================================================================

COL_ASSUR <- "#1f5fa8"   # Bayesian assurance (blue, solid)
COL_POWER <- "#d1661a"   # frequentist power (orange, dashed)
COL_PRIOR <- "#6b6b6b"   # priors, reference lines
COL_ANALYSIS <- "#2a9d8f"
SCENARIO_COLS <- c("#1f5fa8", "#d1661a", "#2a9d8f", "#9b5de5", "#e63946",
                   "#6b6b6b", "#f4a261", "#264653")

# Finishing touches shared by every plotly chart: number separators for the
# current language (decimal comma in Portuguese) and a cleaner toolbar.
plotly_finish <- function(fig) {
  fig |>
    plotly::layout(separators = L(".,", ",.")) |>
    plotly::config(displaylogo = FALSE)
}

pct_axis <- function(title) {
  list(title = title, range = c(0, 1.02), tickformat = ".0%",
       zeroline = FALSE)
}

hline_shape <- function(y, color, dash = "dot") {
  list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = y, y1 = y,
       line = list(color = color, dash = dash, width = 1.2))
}

vline_shape <- function(x, color, dash = "dot") {
  list(type = "line", yref = "paper", y0 = 0, y1 = 1, x0 = x, x1 = x,
       line = list(color = color, dash = dash, width = 1.2))
}

x_label <- function(ratio) {
  if (ratio == 1) L("Sample size per group", "Tamanho amostral por grupo")
  else L("Control-group sample size", "Tamanho amostral do grupo controle")
}

# ---- Main curve: assurance and power vs sample size -------------------------
plot_curve_plotly <- function(res) {
  tab <- res$table
  p <- res$p
  xl <- x_label(p$ratio)
  hover_n <- paste0(L("n (T / C): ", "n (T / C): "), tab$n_t, " / ", tab$n_c)

  fig <- plotly::plot_ly()
  if (p$engine == "sim") {
    fig <- fig |>
      plotly::add_ribbons(
        x = tab$n_c, ymin = pmax(0, tab$assurance - 1.96 * tab$mc_se),
        ymax = pmin(1, tab$assurance + 1.96 * tab$mc_se),
        name = L("95% simulation band", "Faixa de simula\u00E7\u00E3o de 95%"), fillcolor = "rgba(31,95,168,0.15)",
        line = list(width = 0), hoverinfo = "skip")
  }
  fig <- fig |>
    plotly::add_trace(
      x = tab$n_c, y = tab$assurance, type = "scatter", mode = "lines+markers",
      name = if (p$engine == "sim") L("Bayesian assurance (simulated)", "Assurance bayesiana (simulada)")
             else L("Bayesian assurance (exact)", "Assurance bayesiana (exata)"),
      line = list(color = COL_ASSUR, width = 3),
      marker = list(color = COL_ASSUR, size = 6),
      text = paste0(hover_n, "<br>Assurance: ", fmt_pct(tab$assurance)),
      hoverinfo = "text")
  if (p$engine == "sim" && p$show_exact) {
    fig <- fig |>
      plotly::add_trace(
        x = tab$n_c, y = tab$exact, type = "scatter", mode = "lines",
        name = L("Assurance (exact check)", "Assurance (verifica\u00E7\u00E3o exata)"),
        line = list(color = COL_ASSUR, width = 1.5, dash = "dot"),
        text = paste0(hover_n, L("<br>Exact assurance: ", "<br>Assurance exata: "), fmt_pct(tab$exact)),
        hoverinfo = "text")
  }
  fig <- fig |>
    plotly::add_trace(
      x = tab$n_c, y = tab$power, type = "scatter", mode = "lines+markers",
      name = L("Frequentist power", "Poder frequentista"),
      line = list(color = COL_POWER, width = 3, dash = "dash"),
      marker = list(color = COL_POWER, size = 6, symbol = "square"),
      text = paste0(hover_n, L("<br>Power: ", "<br>Poder: "), fmt_pct(tab$power)),
      hoverinfo = "text")

  shapes <- list(hline_shape(p$target, "#333333"))
  annos <- list(list(x = 0, xref = "paper", y = p$target, yanchor = "bottom",
                     xanchor = "left", showarrow = FALSE,
                     text = paste0(L("Target ", "Meta "), fmt_pct(p$target, 0)),
                     font = list(size = 11, color = "#333333")))
  ceil <- res$metrics$ceiling
  if (ceil < 0.995) {
    shapes <- c(shapes, list(hline_shape(ceil, COL_PRIOR, "dashdot")))
    annos <- c(annos, list(list(
      x = 1, xref = "paper", y = ceil, yanchor = "top", xanchor = "right",
      showarrow = FALSE, text = paste0(L("Assurance ceiling ", "Teto da assurance "), fmt_pct(ceil)),
      font = list(size = 11, color = COL_PRIOR))))
  }

  fig |>
    plotly::layout(
      xaxis = list(title = xl, zeroline = FALSE),
      yaxis = pct_axis(L("Probability of success", "Probabilidade de sucesso")),
      shapes = shapes, annotations = annos,
      legend = list(orientation = "h", y = 1.12),
      hovermode = "x unified") |>
    plotly_finish()
}

plot_curve_gg <- function(res) {
  tab <- res$table
  p <- res$p
  lab_a <- L("Bayesian assurance", "Assurance bayesiana")
  lab_p <- L("Frequentist power", "Poder frequentista")
  long <- rbind(
    data.frame(n = tab$n_c, prob = tab$assurance, what = lab_a),
    data.frame(n = tab$n_c, prob = tab$power, what = lab_p))
  g <- ggplot2::ggplot(long, ggplot2::aes(n, prob, colour = what,
                                          linetype = what))
  if (p$engine == "sim") {
    g <- g + ggplot2::geom_ribbon(
      data = tab, inherit.aes = FALSE, alpha = 0.15, fill = COL_ASSUR,
      ggplot2::aes(x = n_c, ymin = pmax(0, assurance - 1.96 * mc_se),
                   ymax = pmin(1, assurance + 1.96 * mc_se)))
  }
  g + ggplot2::geom_hline(yintercept = p$target, linetype = "dotted",
                          colour = "grey30") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_colour_manual(values = setNames(c(COL_ASSUR, COL_POWER), c(lab_a, lab_p))) +
    ggplot2::scale_linetype_manual(values = setNames(c("solid", "dashed"), c(lab_a, lab_p))) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                                labels = function(x) paste0(100 * x, "%")) +
    ggplot2::labs(x = x_label(p$ratio), y = L("Probability of success", "Probabilidade de sucesso"),
                  colour = NULL, linetype = NULL,
                  caption = paste0(L("Dotted line: target ", "Linha pontilhada: meta "),
                                   fmt_pct(p$target, 0))) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "top")
}

# ---- Effect axes -----------------------------------------------------------------
# Grid of true effects (working scale) covering the priors and thresholds.
effect_grid <- function(m, len = 400, include_analysis = TRUE) {
  lo <- min(m$m_d - 4 * m$s_d, 0, m$C)
  hi <- max(m$m_d + 4 * m$s_d, 0, m$C)
  if (include_analysis && (m$m_a != m$m_d || m$s_a != m$s_d)) {
    # include the analysis prior's bulk, without letting a very vague
    # analysis prior squash the design prior into a spike
    span <- hi - lo
    lo <- max(min(lo, m$m_a - 3 * m$s_a), lo - span)
    hi <- min(max(hi, m$m_a + 3 * m$s_a), hi + span)
  }
  seq(lo, hi, length.out = len)
}

# Plotly x-axis for the effect, on the natural scale (log axis for ratios).
effect_axis <- function(m, title = tx(m$labels$axis)) {
  if (m$log_axis) list(title = title, type = "log", zeroline = FALSE)
  else list(title = title, zeroline = FALSE)
}

success_region <- function(theta, m) {
  switch(m$alt,
    greater   = theta > m$C,
    less      = theta < m$C,
    two.sided = if (m$m_d >= 0) theta > 0 else theta < 0)
}

eff_hover <- function(m) {
  if (m$family == "ratio") "%{x:.3f}" else "%{x:.3g}"
}

# ---- Priors ------------------------------------------------------------------------
plot_priors <- function(m) {
  th <- effect_grid(m)
  x <- m$nat(th)
  # densities on the working scale (log scale for ratios), rescaled so the
  # design prior peaks at 1: only the shapes matter here
  dd <- dnorm(th, m$m_d, m$s_d)
  da <- dnorm(th, m$m_a, m$s_a)
  top <- max(dd)
  dd <- dd / top; da <- da / top
  reg <- success_region(th, m)
  same <- m$m_a == m$m_d && m$s_a == m$s_d

  fig <- plotly::plot_ly() |>
    plotly::add_trace(
      x = x, y = ifelse(reg, dd, 0), type = "scatter", mode = "none",
      fill = "tozeroy", fillcolor = "rgba(31,95,168,0.18)",
      name = L("Effects that count as success", "Efeitos que contam como sucesso"), hoverinfo = "skip") |>
    plotly::add_trace(
      x = x, y = dd, type = "scatter", mode = "lines",
      name = L("Design prior (your belief)", "Priori de planejamento (sua cren\u00E7a)"),
      line = list(color = COL_ASSUR, width = 3),
      hovertemplate = paste0(L("Effect ", "Efeito "), eff_hover(m), "<extra></extra>"))
  if (!same) {
    fig <- fig |>
      plotly::add_trace(
        x = x, y = da, type = "scatter", mode = "lines",
        name = L("Analysis prior", "Priori de an\u00E1lise"),
        line = list(color = COL_ANALYSIS, width = 3, dash = "dash"),
        hovertemplate = paste0(L("Effect ", "Efeito "), eff_hover(m), "<extra></extra>"))
  }
  shapes <- list(vline_shape(m$nat(0), "#333333"))
  if (abs(m$C) > 1e-12) shapes <- c(shapes, list(vline_shape(m$nat(m$C), COL_POWER, "dash")))
  fig |>
    plotly::layout(
      xaxis = effect_axis(m),
      yaxis = list(title = L("Relative plausibility", "Plausibilidade relativa"), rangemode = "tozero",
                   showticklabels = FALSE),
      shapes = shapes, legend = list(orientation = "h", y = 1.12)) |>
    plotly_finish()
}

# ---- Probability of success as a function of the true effect ----------------
plot_conditional <- function(m, n_c, ratio) {
  n_t <- treatment_n(n_c, ratio)
  th <- effect_grid(m, include_analysis = FALSE)
  x <- m$nat(th)
  bayes <- model_cond(m, th, n_t, n_c)
  freq <- rep_len(m$power_at(th, n_t, n_c), length(th))
  dens <- dnorm(th, m$m_d, m$s_d)
  dens <- dens / max(dens)

  plotly::plot_ly() |>
    plotly::add_trace(
      x = x, y = dens, type = "scatter", mode = "lines", fill = "tozeroy",
      fillcolor = "rgba(107,107,107,0.15)",
      line = list(color = "rgba(107,107,107,0.5)", width = 1),
      name = L("Design prior (scaled)", "Priori de planejamento (em escala)"), hoverinfo = "skip") |>
    plotly::add_trace(
      x = x, y = bayes, type = "scatter", mode = "lines",
      name = L("Bayesian analysis succeeds", "An\u00E1lise bayesiana tem sucesso"),
      line = list(color = COL_ASSUR, width = 3),
      hovertemplate = paste0(L("True effect ", "Efeito verdadeiro "), eff_hover(m),
                             L("<br>P(success) %{y:.1%}<extra>Bayesian</extra>",
                               "<br>P(sucesso) %{y:.1%}<extra>Bayesiana</extra>"))) |>
    plotly::add_trace(
      x = x, y = freq, type = "scatter", mode = "lines",
      name = L("Frequentist test significant", "Teste frequentista significativo"),
      line = list(color = COL_POWER, width = 3, dash = "dash"),
      hovertemplate = paste0(L("True effect ", "Efeito verdadeiro "), eff_hover(m),
                             L("<br>Power %{y:.1%}<extra>Frequentist</extra>",
                               "<br>Poder %{y:.1%}<extra>Frequentista</extra>"))) |>
    plotly::layout(
      xaxis = effect_axis(m),
      yaxis = pct_axis(L("Probability of success", "Probabilidade de sucesso")),
      shapes = list(vline_shape(m$nat(m$m_d), COL_PRIOR, "dash")),
      annotations = list(list(
        x = if (m$log_axis) log10(m$nat(m$m_d)) else m$nat(m$m_d),
        y = 1.02, yref = "y", showarrow = FALSE,
        text = L("design prior centre", "centro da priori de planejamento"), font = list(size = 11, color = COL_PRIOR))),
      legend = list(orientation = "h", y = 1.15)) |>
    plotly_finish()
}

# ---- Sensitivity: design prior centre x SD ------------------------------------
sd_label <- function(m, which = c("design", "analysis")) {
  which <- match.arg(which)
  en <- paste(if (which == "design") "Design" else "Analysis", "prior SD")
  pt <- paste("DP da priori de", if (which == "design") "planejamento" else "an\u00e1lise")
  suffix <- switch(m$family,
    additive = c("", ""),
    rd       = c(" (percentage points)", " (pontos percentuais)"),
    ratio    = c(paste0(" (log ", tx(m$labels$short), ")"), paste0(" (log da ", tx(m$labels$short), ")")))
  L(paste0(en, suffix[1]), paste0(pt, suffix[2]))
}
sd_display <- function(m, s) if (m$family == "rd") 100 * s else s

plot_sensitivity_heat <- function(m, n_c, ratio, same_prior, len = 41) {
  n_t <- treatment_n(n_c, ratio)
  span <- max(m$s_d, abs(m$m_d) / 2, sqrt(m$se2(m$m_d, n_t, n_c)) / 2)
  means <- seq(m$m_d - 2 * span, m$m_d + 2 * span, length.out = len)
  sds <- seq(m$s_d / 5, m$s_d * 3, length.out = len)
  z <- outer(sds, means, Vectorize(function(s, mu) {
    # when the analysis prior copies the design prior, it moves with it
    mm <- if (same_prior) model_with_priors(m, mu, s, mu, s)
          else model_with_priors(m, m_d = mu, s_d = s)
    model_exact(mm, n_t, n_c)
  }))
  plotly::plot_ly(
    x = m$nat(means), y = sd_display(m, sds), z = z, type = "contour",
    colorscale = "Blues", reversescale = TRUE, zmin = 0, zmax = 1,
    contours = list(start = 0, end = 1, size = 0.1, showlabels = TRUE,
                    labelfont = list(color = "white")),
    colorbar = list(title = "Assurance", tickformat = ".0%"),
    hovertemplate = paste0(L("Prior centre ", "Centro da priori "), eff_hover(m),
                           L("<br>SD %{y:.3g}", "<br>DP %{y:.3g}"),
                           "<br>Assurance %{z:.1%}<extra></extra>")) |>
    plotly::add_markers(
      x = m$nat(m$m_d), y = sd_display(m, m$s_d), inherit = FALSE,
      marker = list(color = COL_POWER, size = 12, symbol = "x"),
      name = L("Your inputs", "Suas entradas"), hoverinfo = "name") |>
    plotly::layout(xaxis = effect_axis(m, L(paste0("Design prior centre (", tx(m$labels$short), ")"),
                                            paste0("Centro da priori de planejamento (", tx(m$labels$short), ")"))),
                   yaxis = list(title = sd_label(m, "design")),
                   showlegend = FALSE) |>
    plotly_finish()
}

plot_sensitivity_analysis_sd <- function(m, n_c, ratio) {
  n_t <- treatment_n(n_c, ratio)
  ref <- max(m$s_d, sqrt(m$se2(m$m_d, n_t, n_c)))
  sds <- 10^seq(log10(ref / 20), log10(ref * 200), length.out = 120)
  y_cur <- vapply(sds, function(s) model_exact(model_with_priors(m, s_a = s), n_t, n_c), 0)
  fig <- plotly::plot_ly() |>
    plotly::add_trace(
      x = sd_display(m, sds), y = y_cur, type = "scatter", mode = "lines",
      name = paste0(L("Analysis prior centred at ", "Priori de an\u00E1lise centrada em "), m$fmt_eff(m$m_a)),
      line = list(color = COL_ASSUR, width = 3),
      hovertemplate = L("Analysis SD %{x:.3g}<br>Assurance %{y:.1%}<extra></extra>",
                        "DP de an\u00E1lise %{x:.3g}<br>Assurance %{y:.1%}<extra></extra>"))
  if (abs(m$m_a) > 1e-12) {
    y_zero <- vapply(sds, function(s)
      model_exact(model_with_priors(m, m_a = 0, s_a = s), n_t, n_c), 0)
    fig <- fig |>
      plotly::add_trace(
        x = sd_display(m, sds), y = y_zero, type = "scatter", mode = "lines",
        name = L("Sceptical analysis prior (centred on no effect)", "Priori de an\u00E1lise c\u00E9tica (centrada no efeito nulo)"),
        line = list(color = COL_ANALYSIS, width = 3, dash = "dash"),
        hovertemplate = L("Analysis SD %{x:.3g}<br>Assurance %{y:.1%}<extra></extra>",
                        "DP de an\u00E1lise %{x:.3g}<br>Assurance %{y:.1%}<extra></extra>"))
  }
  fig |>
    plotly::layout(
      xaxis = list(title = paste(sd_label(m, "analysis"), L("- log axis; right = vaguer", "- eixo log; \u00E0 direita = mais vaga")),
                   type = "log"),
      yaxis = pct_axis("Assurance"),
      shapes = list(vline_shape(sd_display(m, m$s_a), COL_PRIOR, "dash")),
      legend = list(orientation = "h", y = 1.15)) |>
    plotly_finish()
}

# ---- Sensitivity to the outcome-specific ("nuisance") assumption ------------------
# p : the validated inputs of the run (used to rebuild the model).
plot_sensitivity_nuisance <- function(p, m, n_c) {
  n_t <- treatment_n(n_c, p$ratio)
  nu <- m$nuisance
  vals <- nu$values
  res <- t(vapply(vals, function(v) {
    mm <- build_model(nu$set(p, v))
    c(model_exact(mm, n_t, n_c), model_power(mm, n_t, n_c))
  }, numeric(2)))
  plotly::plot_ly() |>
    plotly::add_trace(
      x = vals, y = res[, 1], type = "scatter", mode = "lines",
      name = L("Bayesian assurance", "Assurance bayesiana"), line = list(color = COL_ASSUR, width = 3),
      hovertemplate = "%{x:.3g}<br>Assurance %{y:.1%}<extra></extra>") |>
    plotly::add_trace(
      x = vals, y = res[, 2], type = "scatter", mode = "lines",
      name = L("Frequentist power", "Poder frequentista"), line = list(color = COL_POWER, width = 3, dash = "dash"),
      hovertemplate = L("%{x:.3g}<br>Power %{y:.1%}<extra></extra>", "%{x:.3g}<br>Poder %{y:.1%}<extra></extra>")) |>
    plotly::layout(
      xaxis = list(title = tx(nu$label), zeroline = FALSE),
      yaxis = pct_axis(L("Probability of success", "Probabilidade de sucesso")),
      shapes = list(vline_shape(nu$current, COL_PRIOR, "dash")),
      legend = list(orientation = "h", y = 1.15)) |>
    plotly_finish()
}

# ---- Scenario comparison ------------------------------------------------------
plot_scenarios <- function(scenarios, show_power, x_total) {
  fig <- plotly::plot_ly()
  for (i in seq_along(scenarios)) {
    s <- scenarios[[i]]
    col <- SCENARIO_COLS[(i - 1) %% length(SCENARIO_COLS) + 1]
    x <- if (x_total) s$table$n_t + s$table$n_c else s$table$n_c
    fig <- fig |>
      plotly::add_trace(
        x = x, y = s$table$assurance, type = "scatter", mode = "lines+markers",
        name = paste0(s$label, ": assurance"),
        line = list(color = col, width = 3), marker = list(color = col),
        hovertemplate = "%{y:.1%}")
    if (show_power) {
      fig <- fig |>
        plotly::add_trace(
          x = x, y = s$table$power, type = "scatter", mode = "lines",
          name = paste0(s$label, L(": power", ": poder")),
          line = list(color = col, width = 2, dash = "dash"),
          hovertemplate = "%{y:.1%}")
    }
  }
  fig |>
    plotly::layout(
      xaxis = list(title = if (x_total) L("Total sample size (both groups)", "Tamanho amostral total (dois grupos)")
                           else L("Control-group (or per-group) sample size", "Tamanho amostral do controle (ou por grupo)")),
      yaxis = pct_axis(L("Probability of success", "Probabilidade de sucesso")),
      legend = list(orientation = "h", y = -0.2),
      hovermode = "x unified") |>
    plotly_finish()
}

# ---- Detectability ------------------------------------------------------------
COL_DETECT <- "#2a9d8f"
COL_INFO   <- "#9b5de5"

# Probabilities against sample size: detection if real, assurance, and the
# share of uncertainty the trial removes.
plot_detect_curves <- function(det, ratio, target, two_sided = FALSE) {
  x <- det$n_c
  fig <- plotly::plot_ly() |>
    plotly::add_trace(x = x, y = det$detect, type = "scatter", mode = "lines+markers",
      name = L("Detection if the effect is real", "Detec\u00E7\u00E3o se o efeito for real"),
      line = list(color = COL_DETECT, width = 3), marker = list(color = COL_DETECT, size = 6),
      hovertemplate = "%{y:.1%}<extra></extra>") |>
    plotly::add_trace(x = x, y = det$assurance, type = "scatter", mode = "lines",
      name = L("Assurance (any success)", "Assurance (qualquer sucesso)"),
      line = list(color = COL_ASSUR, width = 2, dash = "dot"),
      hovertemplate = "%{y:.1%}<extra></extra>") |>
    plotly::add_trace(x = x, y = det$var_removed, type = "scatter", mode = "lines",
      name = L("Uncertainty about the effect's size removed", "Incerteza sobre o tamanho do efeito removida"),
      line = list(color = COL_PRIOR, width = 2, dash = "dash"),
      hovertemplate = "%{y:.0%}<extra></extra>")
  if (!all(is.na(det$mi_share))) {
    fig <- fig |> plotly::add_trace(x = x, y = det$mi_share, type = "scatter", mode = "lines",
      name = if (two_sided) L("Doubt about the effect's direction, resolved", "D\u00FAvida sobre a dire\u00E7\u00E3o do efeito, resolvida")
             else L("Doubt about whether the effect is real, resolved", "D\u00FAvida sobre o efeito ser real, resolvida"),
      line = list(color = COL_INFO, width = 2, dash = "dashdot"),
      hovertemplate = "%{y:.0%}<extra></extra>")
  }
  fig |>
    plotly::layout(
      xaxis = list(title = x_label(ratio), zeroline = FALSE),
      yaxis = pct_axis(L("Probability / share", "Probabilidade / propor\u00E7\u00E3o")),
      shapes = list(hline_shape(target, "#333333")),
      legend = list(orientation = "h", y = -0.25), hovermode = "x unified") |>
    plotly_finish()
}

# Shannon information measures (bits) against sample size.
plot_entropy_curves <- function(det, ratio, two_sided = FALSE) {
  x <- det$n_c
  plotly::plot_ly() |>
    plotly::add_trace(x = x, y = det$h_outcome, type = "scatter", mode = "lines",
      name = L("Outcome entropy (1 bit = coin flip)", "Entropia do resultado (1 bit = cara ou coroa)"),
      line = list(color = COL_POWER, width = 3),
      hovertemplate = "%{y:.2f} bits<extra></extra>") |>
    plotly::add_trace(x = x, y = det$mi_truth, type = "scatter", mode = "lines",
      name = if (two_sided) L("Information about the effect's direction", "Informa\u00E7\u00E3o sobre a dire\u00E7\u00E3o do efeito")
             else L("Information about whether the effect is real", "Informa\u00E7\u00E3o sobre o efeito ser real"),
      line = list(color = COL_INFO, width = 3, dash = "dashdot"),
      hovertemplate = "%{y:.3f} bits<extra></extra>") |>
    plotly::add_trace(x = x, y = det$mi_theta, type = "scatter", mode = "lines",
      name = L("Information about the effect's size", "Informa\u00E7\u00E3o sobre o tamanho do efeito"),
      line = list(color = COL_PRIOR, width = 2, dash = "dash"),
      hovertemplate = "%{y:.2f} bits<extra></extra>") |>
    plotly::layout(
      xaxis = list(title = x_label(ratio), zeroline = FALSE),
      yaxis = list(title = "bits", rangemode = "tozero"),
      legend = list(orientation = "h", y = -0.25), hovermode = "x unified") |>
    plotly_finish()
}

# The possible outcomes of the trial at one sample size, as one stacked bar.
plot_outcome_bar <- function(d, two_sided) {
  parts <- if (two_sided) {
    list(list(d$detected, L("Real effect detected (right direction)", "Efeito real detectado (dire\u00E7\u00E3o certa)"), COL_DETECT),
         list(d$missed, L("Real effect missed", "Efeito real n\u00E3o detectado"), "#f4a261"),
         list(d$wrong_dir, L("Success in the wrong direction", "Sucesso na dire\u00E7\u00E3o errada"), "#e63946"))
  } else {
    list(list(d$detected, L("Real effect detected", "Efeito real detectado"), COL_DETECT),
         list(d$missed, L("Real effect missed", "Efeito real n\u00E3o detectado"), "#f4a261"),
         list(d$false_success, L("Success without a real effect", "Sucesso sem efeito real"), "#e63946"),
         list(d$correct_no, L("No real effect, trial negative", "Sem efeito real, ensaio negativo"), "#adb5bd"))
  }
  fig <- plotly::plot_ly()
  for (pt in parts) {
    fig <- fig |> plotly::add_trace(
      x = pt[[1]], y = "", type = "bar", orientation = "h", name = pt[[2]],
      marker = list(color = pt[[3]]), text = if (pt[[1]] >= 0.04) fmt_pct(pt[[1]], 0) else "",
      textposition = "inside", insidetextanchor = "middle",
      hovertemplate = paste0(pt[[2]], ": %{x:.1%}<extra></extra>"))
  }
  fig |>
    plotly::layout(barmode = "stack",
                   xaxis = list(range = c(0, 1), tickformat = ".0%", title = ""),
                   yaxis = list(showticklabels = FALSE),
                   legend = list(orientation = "h", y = -0.6, traceorder = "normal"),
                   margin = list(t = 10, b = 10)) |>
    plotly_finish() |>
    plotly::config(displayModeBar = FALSE)
}
