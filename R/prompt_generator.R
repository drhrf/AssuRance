# =============================================================================
# LLM prompt helper.
#
# build_llm_prompt() turns basic project information into a prompt that asks
# an LLM (ideally one with web search) to find evidence-based values for every
# input of this app, and to end its answer with a JSON block.
# parse_llm_values() reads that JSON block back so the app can fill its inputs.
#
# Plain R (no Shiny), so both are covered by tests/test_prompt_generator.R.
# =============================================================================

# The JSON fields the LLM is asked to return, in the order the app uses them.
# `type` drives validation in parse_llm_values().
LLM_FIELDS <- list(
  design_mean     = list(type = "number",   desc = "expected true effect (treatment minus control), outcome units"),
  design_sd       = list(type = "positive", desc = "uncertainty about that effect (SD of the design prior)"),
  same_prior      = list(type = "logical",  desc = "true to analyse with the design prior, false to use analysis_mean/analysis_sd"),
  analysis_mean   = list(type = "number",   desc = "analysis prior mean (commonly 0 = sceptical)"),
  analysis_sd     = list(type = "positive", desc = "analysis prior SD (large = vague)"),
  sigma           = list(type = "positive", desc = "outcome standard deviation within a group"),
  n_min           = list(type = "whole",    desc = "smallest sample size per group (or control group) to evaluate"),
  n_max           = list(type = "whole",    desc = "largest sample size per group (or control group) to evaluate"),
  n_step          = list(type = "whole",    desc = "step between sample sizes"),
  ratio           = list(type = "positive", desc = "allocation ratio treatment:control (1 = equal)"),
  dropout_percent = list(type = "percent",  desc = "expected dropout, in percent (0-90)"),
  alt             = list(type = "alt",      desc = "\"two.sided\", \"greater\" (treatment > control) or \"less\" (treatment < control)"),
  margin          = list(type = "nonneg",   desc = "clinically meaningful difference that must be exceeded (one-sided tests only; 0 = none)"),
  alpha           = list(type = "prob",     desc = "significance threshold, e.g. 0.05 (0.025 is common for one-sided confirmatory trials)"),
  target          = list(type = "target",   desc = "target probability of success, 0.5-0.99 (e.g. 0.8 or 0.9)")
)

# ---- Prompt ---------------------------------------------------------------
# info    : named list of character/logical fields from the form (see app.R).
# current : optional list of the app's current input values, or NULL.
build_llm_prompt <- function(info, current = NULL) {
  has <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(trimws(x))
  line <- function(label, x) if (has(x)) paste0("- ", label, ": ", trimws(x)) else NULL

  direction_txt <- switch(info$direction %||% "unsure",
    higher = "Higher values of the outcome are better, so a beneficial effect (treatment minus control) is POSITIVE.",
    lower  = "Lower values of the outcome are better, so a beneficial effect (treatment minus control) is NEGATIVE.",
    "It is not yet clear whether higher or lower outcome values are better; work this out and state it.")

  project <- c(
    line("Study title / working name", info$title),
    line("Condition and population", info$condition),
    line("Intervention (treatment arm)", info$intervention),
    line("Comparator (control arm)", info$comparator),
    line("Primary outcome and units", info$outcome),
    line("Time point of the primary outcome", info$timepoint),
    line("Study type / phase", info$phase),
    line("Setting / country", info$setting),
    line("Clinically meaningful difference (if known)", info$mcid),
    line("Practical constraints (recruitment, budget, maximum feasible size)", info$constraints),
    paste0("- Direction: ", direction_txt))
  known <- if (has(info$known)) c("", "Evidence I already know about (verify it; do not assume it is complete):",
                                  trimws(info$known)) else NULL

  current_block <- NULL
  if (!is.null(current)) {
    current_block <- c(
      "", "## Values currently entered in the app (a starting point only; replace them if the evidence says otherwise)",
      "```json", to_json_block(current), "```")
  }

  search_txt <- if (isTRUE(info$web)) c(
    "Search the web and the literature (PubMed, Cochrane Library, ClinicalTrials.gov, published protocols, core outcome sets, regulatory guidance) for the evidence you need. Prefer, in order: recent systematic reviews/meta-analyses, large randomised trials in a similar population, smaller trials, observational data, then expert opinion.")
  else c(
    "You may not have live web access. Use what you know, but be explicit about which values come from specific published studies you are confident exist and which are your own estimates. Never invent citations; if you are unsure a source exists, say so, and list the searches I should run to verify.")

  field_lines <- vapply(names(LLM_FIELDS), function(k)
    paste0("  - `", k, "`: ", LLM_FIELDS[[k]]$desc), character(1))

  lang <- info$language %||% "English"

  paste(c(
    "# Task",
    "You are an experienced biostatistician and clinical trial methodologist. Help me choose evidence-based input values for a Bayesian assurance (probability of success) calculation for a planned two-arm randomised trial with a continuous primary outcome. The calculation tool (the \"AssuRance\" app) compares Bayesian assurance with frequentist power for a two-sample comparison of means with a known outcome SD.",
    "",
    "## My project",
    project,
    known,
    current_block,
    "",
    "## How to find the evidence",
    search_txt,
    "",
    "## The inputs I need, and how to derive each one",
    "Sign convention: the effect is always the difference in means TREATMENT MINUS CONTROL, in the units of the primary outcome. Keep units consistent across all values (convert if sources use different scales, and say how).",
    "",
    "1. **Outcome standard deviation (`sigma`)**: the within-group SD of the outcome at the primary time point in a population like mine. Pool SDs across comparable studies (weight by sample size) rather than taking a single study. If only standard errors or confidence intervals are reported, convert (SD = SE x sqrt(n); SE = CI width / 3.92). If only a median and IQR are available, SD is roughly IQR / 1.35. State whether the SD is for final values or change from baseline, and match the analysis I will run.",
    "2. **Design prior mean (`design_mean`)**: your best estimate of the TRUE effect of the intervention versus the comparator. Base it on a meta-analytic estimate where possible. Adjust for known optimism: effects from small or early-phase trials tend to shrink in larger confirmatory trials, and published results are affected by publication bias. Explain any adjustment.",
    "3. **Design prior SD (`design_sd`)**: how uncertain we are about the true effect. Use a value that includes between-study heterogeneity, not just the standard error of a pooled estimate: for example, derive it from a 95% prediction interval, SD roughly (upper - lower) / 3.92. With very little evidence, use a wider SD. Say which plausible range of effects (mean +/- 2 SD) this implies, and check it makes clinical sense (for example, does it give a sensible probability that the treatment is actually harmful?).",
    "4. **Analysis prior (`same_prior`, `analysis_mean`, `analysis_sd`)**: the prior that will be used in the final analysis. For a confirmatory or regulatory-facing trial, recommend a sceptical prior (centred at 0) or a vague one (SD much larger than any plausible effect, e.g. 10 x sigma) rather than reusing the optimistic design prior, and explain the trade-off. Set `same_prior` to false unless reusing the design prior is clearly justified.",
    "5. **Success criterion (`alt`, `margin`, `alpha`)**: two-sided vs one-sided (and which direction, given the sign convention above), the significance threshold usual for this kind of trial, and whether success should require exceeding a clinically meaningful difference (`margin`, a positive number of outcome units; only used with one-sided tests). Find the minimal clinically important difference (MCID) for this outcome and population if one is published, and prefer anchor-based estimates.",
    "6. **Allocation ratio (`ratio`) and dropout (`dropout_percent`)**: the planned randomisation ratio (treatment : control), and the dropout / loss-to-follow-up rate at the primary time point in comparable trials.",
    "7. **Sample sizes (`n_min`, `n_max`, `n_step`)**: a range per group that brackets the likely answer. Compute an approximate conventional sample size for 80-90% power at the design mean, then choose a range from well below to well above it (also respecting my constraints), with a step giving about 15-25 points.",
    "8. **Target (`target`)**: the probability of success to aim for (commonly 0.8, or 0.9 for confirmatory trials).",
    "",
    "## What to give me",
    "A. A short summary of the evidence you found (with full references: authors, year, title, journal, DOI or URL).",
    "B. A table with one row per input: value, how it was derived, source(s), and your confidence (high / moderate / low).",
    "C. Two alternative scenarios I can compare in the app: a pessimistic one (smaller effect and/or larger SD) and an optimistic one, each with the inputs that change.",
    "D. Caveats: gaps in the evidence, assumptions I should check with the clinical team, and anything that makes a normal outcome with a known, common SD a poor approximation (skewness, floor/ceiling effects, clustering, repeated measures).",
    "E. Finally, the base-case values as ONE JSON object in a ```json code block with EXACTLY these keys (numbers as plain numbers, no units, no comments):",
    field_lines,
    "",
    "## Rules",
    "- Do not fabricate studies, numbers or DOIs. If you cannot find evidence for a value, say so and give a clearly labelled judgement-based value.",
    "- Show the arithmetic for every conversion.",
    "- Keep the explanation understandable to a clinician who is not a statistician.",
    paste0("- Write your answer in ", lang, " (keep the JSON keys and the values of `alt` exactly as specified).")
  ), collapse = "\n")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Pretty-print a named list of values as JSON (only fields in LLM_FIELDS).
to_json_block <- function(values) {
  keep <- values[intersect(names(LLM_FIELDS), names(values))]
  as.character(jsonlite::toJSON(keep, auto_unbox = TRUE, pretty = TRUE,
                                digits = NA))
}

# ---- Parse the LLM's answer --------------------------------------------------
# text : the LLM's full answer (or just the JSON).
# Returns list(values = <named list of valid values>, messages = <character>).
parse_llm_values <- function(text) {
  msgs <- character(0)
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list(values = list(), messages = "Nothing was pasted."))
  }

  # Prefer the last ```json fenced block; otherwise the outermost { ... }.
  blocks <- regmatches(text, gregexpr("```(json|JSON)?\\s*\\{[\\s\\S]*?\\}\\s*```", text, perl = TRUE))[[1]]
  json <- if (length(blocks)) {
    sub("^```(json|JSON)?\\s*", "", sub("\\s*```$", "", blocks[length(blocks)]))
  } else {
    start <- regexpr("\\{", text); ends <- gregexpr("\\}", text)[[1]]
    if (start < 0 || ends[1] < 0) {
      return(list(values = list(), messages = "No JSON object was found in the pasted text."))
    }
    substr(text, start, max(ends))
  }

  raw <- tryCatch(jsonlite::fromJSON(json, simplifyVector = TRUE),
                  error = function(e) NULL)
  if (!is.list(raw) || is.null(names(raw))) {
    return(list(values = list(),
                messages = "The JSON could not be read. Check that it is a single object like {\"sigma\": 10, ...}."))
  }

  unknown <- setdiff(names(raw), names(LLM_FIELDS))
  if (length(unknown)) msgs <- c(msgs, paste0("Ignored unknown field(s): ", paste(unknown, collapse = ", "), "."))

  out <- list()
  for (k in intersect(names(LLM_FIELDS), names(raw))) {
    v <- raw[[k]]
    type <- LLM_FIELDS[[k]]$type
    if (type == "alt") {
      v <- tolower(trimws(as.character(v)))
      v <- switch(v, "two-sided" = , "two_sided" = , "two.sided" = "two.sided",
                  "greater" = , "one-sided greater" = "greater",
                  "less" = , "one-sided less" = "less", NA)
      ok <- !is.na(v)
    } else if (type == "logical") {
      if (is.character(v)) v <- tolower(trimws(v)) %in% c("true", "yes", "1")
      ok <- is.logical(v) && length(v) == 1 && !is.na(v)
    } else {
      v <- suppressWarnings(as.numeric(v))
      ok <- length(v) == 1 && is.finite(v) && switch(type,
        number   = TRUE,
        positive = v > 0,
        nonneg   = v >= 0,
        whole    = v >= 1 && abs(v - round(v)) < 1e-8,
        percent  = v >= 0 && v < 90,
        prob     = v > 0 && v < 1,
        target   = v >= 0.5 && v <= 0.99)
    }
    if (isTRUE(ok)) out[[k]] <- v
    else msgs <- c(msgs, paste0("Skipped `", k, "`: invalid value."))
  }
  if (!is.null(out$n_min) && !is.null(out$n_max) && out$n_max <= out$n_min) {
    msgs <- c(msgs, "Skipped the sample-size range: n_max must exceed n_min.")
    out$n_min <- NULL; out$n_max <- NULL
  }
  list(values = out, messages = msgs)
}
