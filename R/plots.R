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
  if (ratio == 1) "Sample size per group" else "Control-group sample size"
}

# ---- Main curve: assurance and power vs sample size -------------------------
plot_curve_plotly <- function(res) {
  tab <- res$table
  p <- res$p
  xl <- x_label(p$ratio)
  hover_n <- paste0("n (T / C): ", tab$n_t, " / ", tab$n_c)

  fig <- plotly::plot_ly()
  if (p$engine == "sim") {
    fig <- fig |>
      plotly::add_ribbons(
        x = tab$n_c, ymin = pmax(0, tab$assurance - 1.96 * tab$mc_se),
        ymax = pmin(1, tab$assurance + 1.96 * tab$mc_se),
        name = "95% simulation band", fillcolor = "rgba(31,95,168,0.15)",
        line = list(width = 0), hoverinfo = "skip")
  }
  fig <- fig |>
    plotly::add_trace(
      x = tab$n_c, y = tab$assurance, type = "scatter", mode = "lines+markers",
      name = if (p$engine == "sim") "Bayesian assurance (simulated)"
             else "Bayesian assurance (exact)",
      line = list(color = COL_ASSUR, width = 3),
      marker = list(color = COL_ASSUR, size = 6),
      text = paste0(hover_n, "<br>Assurance: ", fmt_pct(tab$assurance)),
      hoverinfo = "text")
  if (p$engine == "sim" && p$show_exact) {
    fig <- fig |>
      plotly::add_trace(
        x = tab$n_c, y = tab$exact, type = "scatter", mode = "lines",
        name = "Assurance (exact check)",
        line = list(color = COL_ASSUR, width = 1.5, dash = "dot"),
        text = paste0(hover_n, "<br>Exact assurance: ", fmt_pct(tab$exact)),
        hoverinfo = "text")
  }
  fig <- fig |>
    plotly::add_trace(
      x = tab$n_c, y = tab$power, type = "scatter", mode = "lines+markers",
      name = "Frequentist power",
      line = list(color = COL_POWER, width = 3, dash = "dash"),
      marker = list(color = COL_POWER, size = 6, symbol = "square"),
      text = paste0(hover_n, "<br>Power: ", fmt_pct(tab$power)),
      hoverinfo = "text")

  shapes <- list(hline_shape(p$target, "#333333"))
  annos <- list(list(x = 0, xref = "paper", y = p$target, yanchor = "bottom",
                     xanchor = "left", showarrow = FALSE,
                     text = paste0("Target ", fmt_pct(p$target, 0)),
                     font = list(size = 11, color = "#333333")))
  ceil <- res$metrics$ceiling
  if (ceil < 0.995) {
    shapes <- c(shapes, list(hline_shape(ceil, COL_PRIOR, "dashdot")))
    annos <- c(annos, list(list(
      x = 1, xref = "paper", y = ceil, yanchor = "top", xanchor = "right",
      showarrow = FALSE, text = paste0("Assurance ceiling ", fmt_pct(ceil)),
      font = list(size = 11, color = COL_PRIOR))))
  }

  fig |>
    plotly::layout(
      xaxis = list(title = xl, zeroline = FALSE),
      yaxis = pct_axis("Probability of success"),
      shapes = shapes, annotations = annos,
      legend = list(orientation = "h", y = 1.12),
      hovermode = "x unified") |>
    plotly::config(displaylogo = FALSE)
}

plot_curve_gg <- function(res) {
  tab <- res$table
  p <- res$p
  long <- rbind(
    data.frame(n = tab$n_c, prob = tab$assurance, what = "Bayesian assurance"),
    data.frame(n = tab$n_c, prob = tab$power, what = "Frequentist power"))
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
    ggplot2::scale_colour_manual(values = c("Bayesian assurance" = COL_ASSUR,
                                            "Frequentist power" = COL_POWER)) +
    ggplot2::scale_linetype_manual(values = c("Bayesian assurance" = "solid",
                                              "Frequentist power" = "dashed")) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                                labels = function(x) paste0(100 * x, "%")) +
    ggplot2::labs(x = x_label(p$ratio), y = "Probability of success",
                  colour = NULL, linetype = NULL,
                  caption = paste0("Dotted line: target ",
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
effect_axis <- function(m, title = m$labels$axis) {
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
      name = "Effects that count as success", hoverinfo = "skip") |>
    plotly::add_trace(
      x = x, y = dd, type = "scatter", mode = "lines",
      name = "Design prior (your belief)",
      line = list(color = COL_ASSUR, width = 3),
      hovertemplate = paste0("Effect ", eff_hover(m), "<extra></extra>"))
  if (!same) {
    fig <- fig |>
      plotly::add_trace(
        x = x, y = da, type = "scatter", mode = "lines",
        name = "Analysis prior",
        line = list(color = COL_ANALYSIS, width = 3, dash = "dash"),
        hovertemplate = paste0("Effect ", eff_hover(m), "<extra></extra>"))
  }
  shapes <- list(vline_shape(m$nat(0), "#333333"))
  if (abs(m$C) > 1e-12) shapes <- c(shapes, list(vline_shape(m$nat(m$C), COL_POWER, "dash")))
  fig |>
    plotly::layout(
      xaxis = effect_axis(m),
      yaxis = list(title = "Relative plausibility", rangemode = "tozero",
                   showticklabels = FALSE),
      shapes = shapes, legend = list(orientation = "h", y = 1.12)) |>
    plotly::config(displaylogo = FALSE)
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
      name = "Design prior (scaled)", hoverinfo = "skip") |>
    plotly::add_trace(
      x = x, y = bayes, type = "scatter", mode = "lines",
      name = "Bayesian analysis succeeds",
      line = list(color = COL_ASSUR, width = 3),
      hovertemplate = paste0("True effect ", eff_hover(m),
                             "<br>P(success) %{y:.1%}<extra>Bayesian</extra>")) |>
    plotly::add_trace(
      x = x, y = freq, type = "scatter", mode = "lines",
      name = "Frequentist test significant",
      line = list(color = COL_POWER, width = 3, dash = "dash"),
      hovertemplate = paste0("True effect ", eff_hover(m),
                             "<br>Power %{y:.1%}<extra>Frequentist</extra>")) |>
    plotly::layout(
      xaxis = effect_axis(m),
      yaxis = pct_axis("Probability of success"),
      shapes = list(vline_shape(m$nat(m$m_d), COL_PRIOR, "dash")),
      annotations = list(list(
        x = if (m$log_axis) log10(m$nat(m$m_d)) else m$nat(m$m_d),
        y = 1.02, yref = "y", showarrow = FALSE,
        text = "design prior centre", font = list(size = 11, color = COL_PRIOR))),
      legend = list(orientation = "h", y = 1.15)) |>
    plotly::config(displaylogo = FALSE)
}

# ---- Sensitivity: design prior centre x SD ------------------------------------
sd_label <- function(m, which = "Design") {
  switch(m$family,
    additive = paste(which, "prior SD"),
    rd       = paste(which, "prior SD (percentage points)"),
    ratio    = paste0(which, " prior SD (log ", m$labels$short, ")"))
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
    hovertemplate = paste0("Prior centre ", eff_hover(m), "<br>SD %{y:.3g}",
                           "<br>Assurance %{z:.1%}<extra></extra>")) |>
    plotly::add_markers(
      x = m$nat(m$m_d), y = sd_display(m, m$s_d), inherit = FALSE,
      marker = list(color = COL_POWER, size = 12, symbol = "x"),
      name = "Your inputs", hoverinfo = "name") |>
    plotly::layout(xaxis = effect_axis(m, paste0("Design prior centre (", m$labels$short, ")")),
                   yaxis = list(title = sd_label(m)),
                   showlegend = FALSE) |>
    plotly::config(displaylogo = FALSE)
}

plot_sensitivity_analysis_sd <- function(m, n_c, ratio) {
  n_t <- treatment_n(n_c, ratio)
  ref <- max(m$s_d, sqrt(m$se2(m$m_d, n_t, n_c)))
  sds <- 10^seq(log10(ref / 20), log10(ref * 200), length.out = 120)
  y_cur <- vapply(sds, function(s) model_exact(model_with_priors(m, s_a = s), n_t, n_c), 0)
  fig <- plotly::plot_ly() |>
    plotly::add_trace(
      x = sd_display(m, sds), y = y_cur, type = "scatter", mode = "lines",
      name = paste0("Analysis prior centred at ", m$fmt_eff(m$m_a)),
      line = list(color = COL_ASSUR, width = 3),
      hovertemplate = "Analysis SD %{x:.3g}<br>Assurance %{y:.1%}<extra></extra>")
  if (abs(m$m_a) > 1e-12) {
    y_zero <- vapply(sds, function(s)
      model_exact(model_with_priors(m, m_a = 0, s_a = s), n_t, n_c), 0)
    fig <- fig |>
      plotly::add_trace(
        x = sd_display(m, sds), y = y_zero, type = "scatter", mode = "lines",
        name = "Sceptical analysis prior (centred on no effect)",
        line = list(color = COL_ANALYSIS, width = 3, dash = "dash"),
        hovertemplate = "Analysis SD %{x:.3g}<br>Assurance %{y:.1%}<extra></extra>")
  }
  fig |>
    plotly::layout(
      xaxis = list(title = paste(sd_label(m, "Analysis"), "- log axis; right = vaguer"),
                   type = "log"),
      yaxis = pct_axis("Assurance"),
      shapes = list(vline_shape(sd_display(m, m$s_a), COL_PRIOR, "dash")),
      legend = list(orientation = "h", y = 1.15)) |>
    plotly::config(displaylogo = FALSE)
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
      name = "Bayesian assurance", line = list(color = COL_ASSUR, width = 3),
      hovertemplate = "%{x:.3g}<br>Assurance %{y:.1%}<extra></extra>") |>
    plotly::add_trace(
      x = vals, y = res[, 2], type = "scatter", mode = "lines",
      name = "Frequentist power", line = list(color = COL_POWER, width = 3, dash = "dash"),
      hovertemplate = "%{x:.3g}<br>Power %{y:.1%}<extra></extra>") |>
    plotly::layout(
      xaxis = list(title = nu$label, zeroline = FALSE),
      yaxis = pct_axis("Probability of success"),
      shapes = list(vline_shape(nu$current, COL_PRIOR, "dash")),
      legend = list(orientation = "h", y = 1.15)) |>
    plotly::config(displaylogo = FALSE)
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
          name = paste0(s$label, ": power"),
          line = list(color = col, width = 2, dash = "dash"),
          hovertemplate = "%{y:.1%}")
    }
  }
  fig |>
    plotly::layout(
      xaxis = list(title = if (x_total) "Total sample size (both groups)"
                           else "Control-group (or per-group) sample size"),
      yaxis = pct_axis("Probability of success"),
      legend = list(orientation = "h", y = -0.2),
      hovermode = "x unified") |>
    plotly::config(displaylogo = FALSE)
}
