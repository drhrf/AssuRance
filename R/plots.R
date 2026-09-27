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

# ---- Grid of effect sizes covering the priors -------------------------------
delta_grid <- function(p, len = 400, include_analysis = TRUE) {
  lo <- min(p$design_mean - 4 * p$design_sd, 0, -p$margin)
  hi <- max(p$design_mean + 4 * p$design_sd, 0, p$margin)
  if (include_analysis && !p$same_prior) {
    # include the analysis prior's bulk, without letting a very vague
    # analysis prior squash the design prior into a spike
    span <- hi - lo
    lo <- max(min(lo, p$analysis_mean - 3 * p$analysis_sd), lo - span)
    hi <- min(max(hi, p$analysis_mean + 3 * p$analysis_sd), hi + span)
  }
  seq(lo, hi, length.out = len)
}

success_region <- function(x, p) {
  switch(p$alt,
    greater   = x > p$margin,
    less      = x < -p$margin,
    two.sided = if (p$design_mean >= 0) x > 0 else x < 0)
}

# ---- Priors ------------------------------------------------------------------
plot_priors <- function(p) {
  x <- delta_grid(p)
  dd <- dnorm(x, p$design_mean, p$design_sd)
  da <- dnorm(x, p$analysis_mean, p$analysis_sd)
  reg <- success_region(x, p)

  fig <- plotly::plot_ly() |>
    plotly::add_trace(
      x = x, y = ifelse(reg, dd, 0), type = "scatter", mode = "none",
      fill = "tozeroy", fillcolor = "rgba(31,95,168,0.18)",
      name = "Effects that count as success", hoverinfo = "skip") |>
    plotly::add_trace(
      x = x, y = dd, type = "scatter", mode = "lines",
      name = "Design prior (your belief)",
      line = list(color = COL_ASSUR, width = 3),
      hovertemplate = "Effect %{x:.3g}<br>Density %{y:.3g}<extra></extra>")
  if (!p$same_prior) {
    fig <- fig |>
      plotly::add_trace(
        x = x, y = da, type = "scatter", mode = "lines",
        name = "Analysis prior",
        line = list(color = COL_ANALYSIS, width = 3, dash = "dash"),
        hovertemplate = "Effect %{x:.3g}<br>Density %{y:.3g}<extra></extra>")
  }
  shapes <- list(vline_shape(0, "#333333"))
  if (p$margin > 0) {
    shapes <- c(shapes, list(vline_shape(
      if (p$alt == "less") -p$margin else p$margin, COL_POWER, "dash")))
  }
  fig |>
    plotly::layout(
      xaxis = list(title = "True effect (treatment minus control)",
                   zeroline = FALSE),
      yaxis = list(title = "Density", rangemode = "tozero"),
      shapes = shapes, legend = list(orientation = "h", y = 1.12)) |>
    plotly::config(displaylogo = FALSE)
}

# ---- Probability of success as a function of the true effect ----------------
plot_conditional <- function(p, n_c) {
  n_t <- treatment_n(n_c, p$ratio)
  x <- delta_grid(p, include_analysis = FALSE)
  bayes <- bayes_success_given_delta(x, n_t, n_c, p$analysis_mean,
                                     p$analysis_sd, p$sigma, p$alpha, p$alt,
                                     p$margin)
  freq <- freq_power(n_t, n_c, x, p$sigma, p$alpha, p$alt, p$margin)
  dens <- dnorm(x, p$design_mean, p$design_sd)
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
      hovertemplate = "True effect %{x:.3g}<br>P(success) %{y:.1%}<extra>Bayesian</extra>") |>
    plotly::add_trace(
      x = x, y = freq, type = "scatter", mode = "lines",
      name = "t-test significant",
      line = list(color = COL_POWER, width = 3, dash = "dash"),
      hovertemplate = "True effect %{x:.3g}<br>Power %{y:.1%}<extra>Frequentist</extra>") |>
    plotly::layout(
      xaxis = list(title = "True effect (treatment minus control)",
                   zeroline = FALSE),
      yaxis = pct_axis("Probability of success"),
      shapes = list(vline_shape(p$design_mean, COL_PRIOR, "dash")),
      annotations = list(list(
        x = p$design_mean, y = 1.02, yref = "y", showarrow = FALSE,
        text = "design mean", font = list(size = 11, color = COL_PRIOR))),
      legend = list(orientation = "h", y = 1.15)) |>
    plotly::config(displaylogo = FALSE)
}

# ---- Sensitivity: design prior mean x SD ------------------------------------
sensitivity_grid <- function(p, n_c, len = 45) {
  n_t <- treatment_n(n_c, p$ratio)
  span <- max(p$design_sd, abs(p$design_mean) / 2, p$sigma / 20)
  means <- seq(p$design_mean - 2 * span, p$design_mean + 2 * span,
               length.out = len)
  sds <- seq(p$design_sd / 5, p$design_sd * 3, length.out = len)
  z <- outer(sds, means, function(s, m) {
    # when the analysis prior copies the design prior, it moves with it
    am <- if (p$same_prior) m else p$analysis_mean
    as_ <- if (p$same_prior) s else p$analysis_sd
    exact_assurance(n_t, n_c, m, s, am, as_, p$sigma, p$alpha, p$alt,
                    p$margin)
  })
  list(means = means, sds = sds, z = z)
}

plot_sensitivity_heat <- function(p, n_c) {
  g <- sensitivity_grid(p, n_c)
  plotly::plot_ly(
    x = g$means, y = g$sds, z = g$z, type = "contour",
    colorscale = "Blues", reversescale = TRUE,
    zmin = 0, zmax = 1,
    contours = list(start = 0, end = 1, size = 0.1, showlabels = TRUE,
                    labelfont = list(color = "white")),
    colorbar = list(title = "Assurance", tickformat = ".0%"),
    hovertemplate = paste0("Design mean %{x:.3g}<br>Design SD %{y:.3g}",
                           "<br>Assurance %{z:.1%}<extra></extra>")) |>
    plotly::add_markers(
      x = p$design_mean, y = p$design_sd, inherit = FALSE,
      marker = list(color = COL_POWER, size = 12, symbol = "x"),
      name = "Your inputs", hoverinfo = "name") |>
    plotly::layout(xaxis = list(title = "Design prior mean (expected effect)"),
                   yaxis = list(title = "Design prior SD (uncertainty)"),
                   showlegend = FALSE) |>
    plotly::config(displaylogo = FALSE)
}

plot_sensitivity_analysis_sd <- function(p, n_c) {
  n_t <- treatment_n(n_c, p$ratio)
  sds <- 10^seq(log10(p$sigma / 50), log10(p$sigma * 200), length.out = 120)
  cur_mean <- if (p$same_prior) p$design_mean else p$analysis_mean
  cur_sd <- if (p$same_prior) p$design_sd else p$analysis_sd
  y_cur <- exact_assurance(n_t, n_c, p$design_mean, p$design_sd, cur_mean,
                           sds, p$sigma, p$alpha, p$alt, p$margin)
  y_zero <- exact_assurance(n_t, n_c, p$design_mean, p$design_sd, 0,
                            sds, p$sigma, p$alpha, p$alt, p$margin)
  fig <- plotly::plot_ly() |>
    plotly::add_trace(
      x = sds, y = y_cur, type = "scatter", mode = "lines",
      name = paste0("Analysis prior centred at ", fmt_num(cur_mean)),
      line = list(color = COL_ASSUR, width = 3),
      hovertemplate = "Analysis SD %{x:.3g}<br>Assurance %{y:.1%}<extra></extra>")
  if (cur_mean != 0) {
    fig <- fig |>
      plotly::add_trace(
        x = sds, y = y_zero, type = "scatter", mode = "lines",
        name = "Sceptical analysis prior (centred at 0)",
        line = list(color = COL_ANALYSIS, width = 3, dash = "dash"),
        hovertemplate = "Analysis SD %{x:.3g}<br>Assurance %{y:.1%}<extra></extra>")
  }
  fig |>
    plotly::layout(
      xaxis = list(title = "Analysis prior SD (log scale; right = vaguer)",
                   type = "log"),
      yaxis = pct_axis("Assurance"),
      shapes = list(vline_shape(cur_sd, COL_PRIOR, "dash")),
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
