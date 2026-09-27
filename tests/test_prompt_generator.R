# Checks for R/prompt_generator.R. Run from the project root:
#   Rscript tests/test_prompt_generator.R

source("R/i18n.R"); source("R/prompt_generator.R")

ok <- TRUE
check <- function(desc, cond) {
  cat(sprintf("%-66s %s\n", desc, if (isTRUE(cond)) "OK" else "FAIL"))
  ok <<- ok && isTRUE(cond)
}
has <- function(txt, pattern) grepl(pattern, txt, fixed = TRUE)

# --- Prompt ----------------------------------------------------------------
info <- list(title = "BP-LOWER", condition = "adults with hypertension",
             intervention = "drug X", comparator = "placebo",
             outcome = "systolic BP (mmHg)", timepoint = "12 weeks",
             phase = "Phase III / confirmatory", direction = "lower",
             setting = "", mcid = "5 mmHg", known = "", constraints = "",
             web = TRUE, language = "Portuguese (Brazil)")
pr <- build_llm_prompt(info)
check("prompt includes project details", has(pr, "adults with hypertension") && has(pr, "drug X"))
check("empty fields are left out", !has(pr, "Setting / country"))
check("direction 'lower' explained (beneficial treatment lowers the outcome)",
      has(pr, "beneficial treatment LOWERS"))
check("web-search instructions when web = TRUE", has(pr, "Search the web"))
check("no-web instructions when web = FALSE",
      has(build_llm_prompt(modifyList(info, list(web = FALSE))), "may not have live web access"))
check("every JSON key for the outcome type is listed", all(vapply(llm_keys_for("cont"),
      function(k) has(pr, paste0("`", k, "`")), logical(1))))
check("answer language requested", has(pr, "Write your answer in Portuguese (Brazil)"))
check("current values embedded when given",
      has(build_llm_prompt(info, list(sigma = 12, alt = "less")), "\"sigma\": 12"))
check("prompt is pure ASCII (safe on any server locale)",
      !grepl("[^\\x01-\\x7F]", pr, perl = TRUE))

# --- Parser ------------------------------------------------------------------
answer <- paste(
  "Here is my analysis...", "",
  "```json",
  "{",
  "  \"design_mean\": -6, \"design_sd\": 4, \"same_prior\": false,",
  "  \"analysis_mean\": 0, \"analysis_sd\": 100, \"sigma\": 15,",
  "  \"n_min\": 50, \"n_max\": 400, \"n_step\": 25, \"ratio\": 1,",
  "  \"dropout_percent\": 12, \"alt\": \"less\", \"margin\": 2,",
  "  \"alpha\": 0.025, \"target\": 0.9",
  "}",
  "```", "Caveats: ...", sep = "\n")
res <- parse_llm_values(answer)
check("all 15 fields parsed from a fenced block (margin -> threshold)",
      length(res$values) == 15 && !length(res$messages) && res$values$threshold == -2)
check("values have the right types",
      identical(res$values$same_prior, FALSE) && res$values$alt == "less" &&
        res$values$sigma == 15 && res$values$dropout_percent == 12)

bare <- parse_llm_values("{\"sigma\": \"10\", \"alt\": \"two-sided\", \"foo\": 1}")
check("bare JSON, numeric strings and alt synonyms accepted",
      bare$values$sigma == 10 && bare$values$alt == "two.sided")
check("unknown fields reported", any(grepl("foo", bare$messages)))

bad <- parse_llm_values("{\"design_sd\": -1, \"alpha\": 5, \"n_min\": 100, \"n_max\": 50, \"sigma\": 8}")
check("invalid values skipped, valid ones kept",
      is.null(bad$values$design_sd) && is.null(bad$values$alpha) &&
        is.null(bad$values$n_min) && bad$values$sigma == 8)
check("no JSON gives a clear message",
      length(parse_llm_values("no numbers here")$values) == 0)
check("broken JSON gives a clear message",
      grepl("could not be read", parse_llm_values("{sigma: }")$messages[1]))

# --- Outcome types --------------------------------------------------------------
ps <- build_llm_prompt(info, otype = "surv")
check("survival prompt explains hazard ratio and median conversion",
      has(ps, "HAZARD RATIO") && has(ps, "median = t x log(2)"))
check("survival prompt asks for survival keys, not continuous ones",
      has(ps, "`surv_median`") && has(ps, "`ratio_design_lo`") && !has(ps, "`sigma`"))
pb <- build_llm_prompt(info, otype = "binary", measure = "rd")
check("risk-difference prompt uses percentage-point keys",
      has(pb, "`rd_design_mean`") && has(pb, "`p_control`") && !has(pb, "`ratio_design_lo`"))
check("ANCOVA prompt asks for the baseline correlation",
      has(build_llm_prompt(info, otype = "ancova"), "`rho`"))
check("keys for binary OR exclude survival fields",
      !("surv_median" %in% llm_keys_for("binary", "or")) && "p_control" %in% llm_keys_for("binary", "or"))
check("current values are filtered to the outcome type",
      !has(build_llm_prompt(info, list(sigma = 12, surv_median = 9), otype = "surv"), "\"sigma\""))
sv <- parse_llm_values(paste0("```json\n{\"otype\": \"survival\", \"surv_median\": 18, ",
  "\"time_unit\": \"months\", \"ratio_design_lo\": 0.6, \"ratio_design_hi\": 0.95, ",
  "\"ratio_threshold\": 1.3, \"loss_pct\": 5}\n```"))
check("survival answer parsed (otype synonym, text and ratio fields)",
      sv$values$otype == "surv" && sv$values$time_unit == "months" &&
        sv$values$ratio_threshold == 1.3 && length(sv$messages) == 0)
bad_r <- parse_llm_values("{\"ratio_design_lo\": 1.2, \"ratio_design_hi\": 0.8, \"p_control\": 100, \"measure\": \"hr\"}")
check("inverted ratio range, impossible rate and unknown measure rejected",
      length(bad_r$values) == 0 && length(bad_r$messages) == 3)
old <- parse_llm_values("{\"margin\": 2, \"alt\": \"less\"}")
check("old `margin` key converted to a signed threshold", old$values$threshold == -2)

if (!ok) stop("Some checks failed.")
cat("All checks passed.\n")
