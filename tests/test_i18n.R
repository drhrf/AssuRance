# Checks for the English / Portuguese interface (R/i18n.R and its users).
# Run from the project root:   Rscript tests/test_i18n.R

suppressMessages({ library(shiny); library(bslib); library(plotly); library(DT) })
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
for (f in list.files("R", full.names = TRUE)) source(f)

ok <- TRUE
check <- function(desc, cond) {
  cat(sprintf("%-70s %s\n", desc, if (isTRUE(cond)) "OK" else "FAIL"))
  ok <<- ok && isTRUE(cond)
}

# --- Number formatting ---------------------------------------------------------
check("percentages use a decimal comma in Portuguese",
      with_lang("pt", fmt_pct(0.882)) == "88,2%" && with_lang("en", fmt_pct(0.882)) == "88.2%")
check("thousands separator is a dot in Portuguese",
      with_lang("pt", fmt_num(20000)) == "20.000" && with_lang("en", fmt_num(20000)) == "20,000")
check("language is restored after with_lang()",
      { with_lang("pt", NULL); current_lang() == "en" })

# --- Every static English text has a Portuguese twin ----------------------------
count <- function(html, l) lengths(regmatches(html, gregexpr(paste0('lang="', l, '"'), html)))
side <- as.character(htmltools::renderTags(sidebar_ui())$html); main <- as.character(htmltools::renderTags(main_ui())$html)
check("sidebar: as many Portuguese as English text elements",
      count(side, "en") > 50 && count(side, "en") == count(side, "pt"))
check("main panel and Methods tab: as many Portuguese as English elements",
      count(main, "en") > 100 && count(main, "en") == count(main, "pt"))
check("no empty translation", !grepl('lang="pt"></span>|lang="pt"></p>', paste(side, main)))
choices <- list(otype = c("cont", "ancova", "binary", "surv"),
                pg_phase = c("Pilot / feasibility", "Phase II", "Phase III / confirmatory",
                             "Pragmatic / effectiveness", "Non-inferiority", "Other / not sure"),
                pg_language = c("English", "Portuguese (Brazil)", "Spanish", "French", "German"))
check("every dropdown option has a translation",
      all(vapply(names(choices), function(id) setequal(names(I18N_OPTIONS[[id]]), choices[[id]]), logical(1))))
check("every translated placeholder belongs to a real input",
      all(vapply(names(I18N_PLACEHOLDERS), function(id) grepl(paste0('id="', id, '"'), main), logical(1))))

# --- Server-generated text ---------------------------------------------------------
base <- list(
  otype = "cont", measure = "or", sigma = 10, rho = 0.5, p_control = 0.3,
  surv_median = 12, time_unit = "months", accrual = 12, followup = 12, loss = 0.05,
  design_mean = 5, design_sd = 3, analysis_mean = 0, analysis_sd = 10,
  rd_design_mean = -10, rd_design_sd = 5, rd_analysis_mean = 0, rd_analysis_sd = 20,
  ratio_design_lo = 0.5, ratio_design_hi = 1.0, ratio_analysis_lo = 0.5, ratio_analysis_hi = 2,
  same_prior = FALSE, alt = "two.sided", alpha = 0.05, threshold = 0,
  rd_threshold = 0, ratio_threshold = 1, ratio = 1, dropout = 0.1, target = 0.8,
  engine = "exact", mc_iter = 2000, seed = 1)
for (a in list(list(otype = "cont"), list(otype = "ancova"), list(otype = "binary", measure = "rd"),
               list(otype = "binary", measure = "or", alt = "less"), list(otype = "surv", alt = "less"))) {
  p <- modifyList(base, a); p$n_c <- c(20L, 400L); p$n_t <- p$n_c
  m <- build_model(p)
  res <- list(p = p, model = m,
              table = data.frame(n_t = p$n_t, n_c = p$n_c,
                                 assurance = model_exact(m, p$n_t, p$n_c),
                                 power = model_power(m, p$n_t, p$n_c)),
              metrics = list(n_assur = smallest_n(function(nt, nc) model_exact(m, nt, nc), 1, 0.8),
                             n_power = smallest_n(function(nt, nc) model_power(m, nt, nc), 1, 0.8),
                             ceiling = model_ceiling(m), neg_share = 0.02))
  pt <- with_lang("pt", c(build_summary(res), describe_inputs(p, m), model_goal(m),
                          describe_prior(m, "design"), describe_test(m)))
  en <- with_lang("en", c(build_summary(res), describe_inputs(p, m)))
  check(paste("Portuguese summary and report text for", a$otype, a$measure %||% ""),
        any(grepl("assurance bayesiana", pt)) && !any(grepl("participants|probability that|Design prior", pt)) &&
          any(grepl("[0-9],[0-9]", pt)) && any(grepl("participants", en)))
}
mw <- build_model(modifyList(base, list(otype = "surv", alt = "less", same_prior = TRUE)))
check("warnings are stored in both languages",
      grepl("^Your analysis prior", with_lang("en", tx(mw$warnings[[1]]))) &&
        grepl("^A sua priori", with_lang("pt", tx(mw$warnings[[1]]))))
check("model labels follow the language",
      with_lang("pt", tx(mw$labels$short)) == "raz\u00E3o de riscos (HR)" && with_lang("en", tx(mw$labels$short)) == "hazard ratio")
check("parser messages follow the language",
      grepl("^Nada", with_lang("pt", parse_llm_values("")$messages)))
check("the AI prompt stays in English when the app is in Portuguese",
      grepl("continuous outcome", with_lang("pt", build_llm_prompt(list(web = TRUE), otype = "cont"))))
pl <- with_lang("pt", plot_curve_gg(list(p = modifyList(base, list(ratio = 1, engine = "exact")),
  table = data.frame(n_t = 1:2, n_c = 1:2, assurance = c(.2, .4), power = c(.3, .5), mc_se = NA))))
check("plot labels follow the language", pl$labels$y == "Probabilidade de sucesso")

# --- Every interactive plot builds in both languages ------------------------------------
pp <- modifyList(base, list(otype = "surv", alt = "less")); pp$n_c <- c(50L, 200L); pp$n_t <- pp$n_c
mm <- build_model(pp)
rr <- list(p = pp, model = mm, metrics = list(ceiling = model_ceiling(mm)),
           table = data.frame(n_t = pp$n_t, n_c = pp$n_c, assurance = c(.3, .6), mc_se = .01,
                              exact = c(.3, .6), power = c(.4, .7)))
builders <- list(
  curve = function() plot_curve_plotly(modifyList(rr, list(p = modifyList(pp, list(engine = "sim", show_exact = TRUE))))),
  priors = function() plot_priors(mm),
  conditional = function() plot_conditional(mm, 200, 1),
  heat = function() plot_sensitivity_heat(mm, 200, 1, FALSE, len = 5),
  analysis_sd = function() plot_sensitivity_analysis_sd(mm, 200, 1),
  nuisance = function() plot_sensitivity_nuisance(pp, mm, 200),
  scenarios = function() plot_scenarios(list(list(label = "A", table = rr$table)), TRUE, TRUE),
  detect_curves = function() plot_detect_curves(detectability(mm, pp$n_t, pp$n_c), 1, 0.8),
  entropy_curves = function() plot_entropy_curves(detectability(mm, pp$n_t, pp$n_c), 1),
  outcome_bar = function() plot_outcome_bar(detectability_one(mm, 200, 200), FALSE),
  outcome_bar_2s = function() plot_outcome_bar(detectability_one(build_model(modifyList(pp, list(alt = "two.sided"))), 200, 200), TRUE))
for (lg in c("en", "pt")) {
  okb <- vapply(builders, function(f) !inherits(tryCatch(with_lang(lg, plotly::plotly_build(f())),
                                                           error = function(e) e), "error"), logical(1))
  check(paste0("all interactive plots build (", lg, ")"), all(okb))
}
check("Portuguese plots use a decimal comma",
      with_lang("pt", plotly::plotly_build(plot_priors(mm)))$x$layout$separators == ",.")

# --- Source files stay ASCII ------------------------------------------------------------
files <- c("app.R", list.files("R", full.names = TRUE), list.files("tests", full.names = TRUE))
check("all R source files are ASCII (accents written as \\u escapes)",
      all(vapply(files, function(f) !any(grepl("[^\\x01-\\x7F]", readLines(f, warn = FALSE), perl = TRUE)), logical(1))))

if (!ok) stop("Some checks failed.")
cat("All checks passed.\n")
