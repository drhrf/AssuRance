# =============================================================================
# LLM prompt helper.
#
# build_llm_prompt() turns basic project information into a prompt that asks
# an LLM (ideally one with web search) to find evidence-based values for every
# input of this app that applies to the chosen outcome type, and to end its
# answer with a JSON block. parse_llm_values() reads that JSON block back so
# the app can fill its inputs.
#
# Plain R (no Shiny), so both are covered by tests/test_prompt_generator.R.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# The JSON fields (named after the app's input IDs, except dropout_percent).
#   type : how parse_llm_values() validates the value
#   use  : which outcome types / scale families the field applies to
LLM_FIELDS <- list(
  otype            = list(type = "otype",    use = "all",      label = "Outcome type",
                          desc = "outcome type: keep exactly as given"),
  measure          = list(type = "measure",  use = "binary",   label = "Effect measure",
                          desc = "effect measure: keep exactly as given (\"or\", \"rr\" or \"rd\")"),
  sigma            = list(type = "positive", use = c("cont", "ancova"), label = "Outcome SD",
                          desc = "outcome standard deviation within a group, in outcome units"),
  rho              = list(type = "rho",      use = "ancova",   label = "Baseline-outcome correlation",
                          desc = "correlation between the baseline and follow-up values of the outcome (0-0.95)"),
  p_control        = list(type = "pct_open", use = "binary",   label = "Control event rate (%)",
                          desc = "percentage of control-group participants with the event by the time point (0-100, exclusive)"),
  surv_median      = list(type = "positive", use = "surv",     label = "Control median time",
                          desc = "median time to event in the control group, in `time_unit`"),
  time_unit        = list(type = "text",     use = "surv",     label = "Time unit",
                          desc = "time unit used for all times, e.g. \"months\""),
  accrual          = list(type = "nonneg",   use = "surv",     label = "Recruitment period",
                          desc = "recruitment period, in `time_unit` (recruitment assumed uniform)"),
  followup         = list(type = "nonneg",   use = "surv",     label = "Extra follow-up",
                          desc = "follow-up after the last participant is recruited, in `time_unit`"),
  loss_pct         = list(type = "percent",  use = "surv",     label = "Lost to follow-up (%)",
                          desc = "percentage of participants lost to follow-up before an event over the study (0-90)"),
  design_mean      = list(type = "number",   use = "additive", label = "Design prior mean",
                          desc = "expected true difference in means (treatment minus control), outcome units"),
  design_sd        = list(type = "positive", use = "additive", label = "Design prior SD",
                          desc = "uncertainty about that difference (SD of the design prior)"),
  analysis_mean    = list(type = "number",   use = "additive", label = "Analysis prior mean",
                          desc = "analysis prior mean (commonly 0 = sceptical)"),
  analysis_sd      = list(type = "positive", use = "additive", label = "Analysis prior SD",
                          desc = "analysis prior SD (large = vague)"),
  rd_design_mean   = list(type = "rd",       use = "rd",       label = "Design prior mean (points)",
                          desc = "expected true risk difference, treatment minus control, in PERCENTAGE POINTS (e.g. -10)"),
  rd_design_sd     = list(type = "positive", use = "rd",       label = "Design prior SD (points)",
                          desc = "uncertainty about that risk difference, SD in percentage points"),
  rd_analysis_mean = list(type = "rd",       use = "rd",       label = "Analysis prior mean (points)",
                          desc = "analysis prior mean in percentage points (commonly 0)"),
  rd_analysis_sd   = list(type = "positive", use = "rd",       label = "Analysis prior SD (points)",
                          desc = "analysis prior SD in percentage points (large = vague)"),
  ratio_design_lo  = list(type = "positive", use = "ratio",    label = "Design prior: 95% range from",
                          desc = "lower end of the range you are 95% sure contains the true ratio (treatment vs control)"),
  ratio_design_hi  = list(type = "positive", use = "ratio",    label = "Design prior: 95% range to",
                          desc = "upper end of that range; its geometric midpoint sqrt(lo * hi) is the best guess"),
  ratio_analysis_lo = list(type = "positive", use = "ratio",   label = "Analysis prior: 95% range from",
                           desc = "analysis prior: lower end of its 95% range (e.g. 0.5 for a sceptical prior centred on 1)"),
  ratio_analysis_hi = list(type = "positive", use = "ratio",   label = "Analysis prior: 95% range to",
                           desc = "analysis prior: upper end of its 95% range (e.g. 2.0)"),
  same_prior       = list(type = "logical",  use = "all",      label = "Analyse with the same prior",
                          desc = "true to analyse with the design prior, false to use the separate analysis prior"),
  n_min            = list(type = "whole",    use = "all",      label = "Min n",
                          desc = "smallest sample size per group (or control group) to evaluate"),
  n_max            = list(type = "whole",    use = "all",      label = "Max n",
                          desc = "largest sample size per group (or control group) to evaluate"),
  n_step           = list(type = "whole",    use = "all",      label = "Step",
                          desc = "step between sample sizes"),
  ratio            = list(type = "positive", use = "all",      label = "Allocation ratio",
                          desc = "allocation ratio treatment:control (1 = equal)"),
  dropout_percent  = list(type = "percent",  use = c("cont", "ancova", "binary"), label = "Dropout (%)",
                          desc = "expected dropout before the primary time point, in percent (0-90)"),
  alt              = list(type = "alt",      use = "all",      label = "Test direction",
                          desc = "\"two.sided\", \"greater\" (effect above the threshold) or \"less\" (effect below the threshold)"),
  threshold        = list(type = "number",   use = "additive", label = "Success threshold",
                          desc = "one-sided tests only: 0 = superiority; a clinically meaningful difference, or a non-inferiority margin on the harmful side"),
  rd_threshold     = list(type = "rd",       use = "rd",       label = "Success threshold (points)",
                          desc = "one-sided tests only, in percentage points: 0 = superiority; or a meaningful difference / non-inferiority margin"),
  ratio_threshold  = list(type = "positive", use = "ratio",    label = "Success threshold (ratio)",
                          desc = "one-sided tests only: 1 = superiority; e.g. 0.8 to require a meaningful benefit, or 1.3 as a non-inferiority margin"),
  alpha            = list(type = "prob",     use = "all",      label = "Alpha",
                          desc = "significance threshold, e.g. 0.05 (0.025 is common for one-sided confirmatory trials)"),
  target           = list(type = "target",   use = "all",      label = "Target",
                          desc = "target probability of success, 0.5-0.99 (e.g. 0.8 or 0.9)")
)

# Portuguese labels for the table shown after applying an answer.
LLM_LABELS_PT <- c(
  otype = "Tipo de desfecho", measure = "Medida de efeito", sigma = "DP do desfecho",
  rho = "Correla\u00E7\u00E3o basal-desfecho", p_control = "Taxa de eventos no controle (%)",
  surv_median = "Mediana no controle", time_unit = "Unidade de tempo", accrual = "Per\u00EDodo de recrutamento",
  followup = "Seguimento adicional", loss_pct = "Perda de seguimento (%)",
  design_mean = "M\u00E9dia da priori de planejamento", design_sd = "DP da priori de planejamento",
  analysis_mean = "M\u00E9dia da priori de an\u00E1lise", analysis_sd = "DP da priori de an\u00E1lise",
  rd_design_mean = "M\u00E9dia da priori de planejamento (pontos)", rd_design_sd = "DP da priori de planejamento (pontos)",
  rd_analysis_mean = "M\u00E9dia da priori de an\u00E1lise (pontos)", rd_analysis_sd = "DP da priori de an\u00E1lise (pontos)",
  ratio_design_lo = "Priori de planejamento: intervalo de 95% de", ratio_design_hi = "Priori de planejamento: intervalo de 95% at\u00E9",
  ratio_analysis_lo = "Priori de an\u00E1lise: intervalo de 95% de", ratio_analysis_hi = "Priori de an\u00E1lise: intervalo de 95% at\u00E9",
  same_prior = "Analisar com a mesma priori", n_min = "n m\u00EDnimo", n_max = "n m\u00E1ximo", n_step = "Passo",
  ratio = "Raz\u00E3o de aloca\u00E7\u00E3o", dropout_percent = "Abandono (%)", alt = "Dire\u00E7\u00E3o do teste",
  threshold = "Limiar de sucesso", rd_threshold = "Limiar de sucesso (pontos)", ratio_threshold = "Limiar de sucesso (raz\u00E3o)",
  alpha = "Alfa", target = "Meta")
for (k in names(LLM_LABELS_PT)) LLM_FIELDS[[k]]$label_pt <- LLM_LABELS_PT[[k]]

outcome_type_label <- function(otype, measure = NULL) {
  switch(otype %||% "cont",
    cont   = L("continuous outcome, difference in means", "desfecho cont\u00EDnuo, diferen\u00E7a de m\u00E9dias"),
    ancova = L("continuous outcome analysed with adjustment for its baseline value (ANCOVA)",
               "desfecho cont\u00EDnuo analisado com ajuste pelo valor basal (ANCOVA)"),
    binary = L(paste0("binary outcome, ", c(or = "odds ratio", rr = "risk ratio",
                                            rd = "risk difference")[[measure %||% "or"]]),
               paste0("desfecho bin\u00E1rio, ", c(or = "raz\u00E3o de chances", rr = "risco relativo",
                                                  rd = "diferen\u00E7a de riscos")[[measure %||% "or"]])),
    surv   = L("time-to-event (survival) outcome, hazard ratio",
               "desfecho de tempo at\u00E9 o evento (sobrevida), raz\u00E3o de riscos"))
}

# JSON keys that apply to an outcome type (in LLM_FIELDS order).
llm_keys_for <- function(otype, measure = NULL) {
  otype <- otype %||% "cont"
  fam <- if (otype %in% c("cont", "ancova")) "additive"
         else if (otype == "binary" && identical(measure, "rd")) "rd" else "ratio"
  keep <- vapply(LLM_FIELDS, function(f)
    any(f$use %in% c("all", otype, fam)), logical(1))
  names(LLM_FIELDS)[keep]
}

# Outcome-specific guidance for the prompt: what the effect is, and how to
# derive each outcome-specific input.
type_guidance <- function(otype, measure) {
  switch(otype,
    cont = c(
      "The effect is the difference in means, TREATMENT MINUS CONTROL, in the outcome's units, analysed as a normal outcome with a known common SD.",
      "- **Outcome SD (`sigma`)**: the within-group SD at the primary time point in a population like mine. Pool SDs across comparable studies (weight by sample size) rather than taking one study. Convert if needed: SD = SE x sqrt(n); SE = CI width / 3.92; SD is roughly IQR / 1.35. Say whether it is for final values or change from baseline."),
    ancova = c(
      "The effect is the baseline-adjusted difference in means (ANCOVA), TREATMENT MINUS CONTROL, in the outcome's units.",
      "- **Outcome SD (`sigma`)**: the within-group SD of the FINAL value of the outcome (not the change score), pooled across comparable studies. Convert if needed: SD = SE x sqrt(n); SE = CI width / 3.92; SD is roughly IQR / 1.35.",
      "- **Baseline-outcome correlation (`rho`)**: the correlation between baseline and follow-up measurements of this outcome over a similar interval. Look for reported test-retest or baseline-follow-up correlations, or derive it from the SD of change scores: rho = (SD_base^2 + SD_final^2 - SD_change^2) / (2 SD_base SD_final)."),
    binary = c(
      paste0("The effect is the ", c(or = "ODDS RATIO", rr = "RISK RATIO", rd = "RISK DIFFERENCE")[[measure]],
             " for the event, TREATMENT VERSUS CONTROL",
             if (measure == "rd") ", in percentage points (treatment rate minus control rate)"
             else " (values below 1 mean fewer events with treatment)", "."),
      "- **Control event rate (`p_control`)**: the percentage of participants in a control group like mine who have the event by the primary time point. Prefer recent trials and registries in a similar population and setting; event rates drift over time and differ between settings.",
      if (measure != "rd") "- **Ratios**: if studies report a different measure (e.g. risk ratios when I need odds ratios), convert using the control event rate, and say how." else
        "- **Risk difference**: if studies report ratios, convert to a risk difference using the control event rate, and say how."),
    surv = c(
      "The effect is the HAZARD RATIO, TREATMENT VERSUS CONTROL (below 1 = fewer events / longer time to event with treatment). The calculation assumes exponential survival and proportional hazards.",
      "- **Control median time (`surv_median`)**: the median time to the event in a control group like mine, in `time_unit`. If only a survival percentage at a fixed time t is reported (S(t)), convert with median = t x log(2) / (-log S(t)).",
      "- **Recruitment period (`accrual`) and extra follow-up (`followup`)**: realistic values for this kind of trial (look at comparable trials' designs and my constraints). The analysis happens at recruitment + extra follow-up.",
      "- **Loss to follow-up (`loss_pct`)**: the percentage of participants lost before having the event in comparable trials."))
}

prior_guidance <- function(otype, measure) {
  fam <- if (otype %in% c("cont", "ancova")) "additive"
         else if (otype == "binary" && measure == "rd") "rd" else "ratio"
  switch(fam,
    additive = c(
      "- **Design prior (`design_mean`, `design_sd`)**: `design_mean` is your best estimate of the TRUE effect, ideally from a meta-analysis, adjusted for known optimism (effects from small or early trials tend to shrink in larger confirmatory trials; publication bias). `design_sd` must include between-study heterogeneity, not just the SE of a pooled estimate: e.g. from a 95% prediction interval, SD ~ (upper - lower) / 3.92.",
      "- **Analysis prior (`same_prior`, `analysis_mean`, `analysis_sd`)**: for a confirmatory or regulatory-facing trial, recommend a sceptical prior (mean 0) or a vague one (SD much larger than any plausible effect, e.g. 10 x sigma) rather than reusing the optimistic design prior."),
    rd = c(
      "- **Design prior (`rd_design_mean`, `rd_design_sd`, percentage points)**: best estimate of the TRUE risk difference and its uncertainty, including between-study heterogeneity (e.g. from a 95% prediction interval, SD ~ (upper - lower) / 3.92). Adjust for known optimism of small or early trials.",
      "- **Analysis prior (`same_prior`, `rd_analysis_mean`, `rd_analysis_sd`)**: for a confirmatory trial prefer a sceptical (mean 0) or vague (large SD, e.g. 50 points) prior."),
    ratio = c(
      "- **Design prior (`ratio_design_lo`, `ratio_design_hi`)**: the range you are 95% sure contains the TRUE ratio. Base it on a meta-analysis where possible and use a 95% PREDICTION interval (which includes between-study heterogeneity) rather than a confidence interval; widen it for optimism of small or early trials, publication bias, or differences between the studied populations and mine. The app treats the ratio as log-normal: best guess = sqrt(lo x hi).",
      "- **Analysis prior (`same_prior`, `ratio_analysis_lo`, `ratio_analysis_hi`)**: for a confirmatory or regulatory-facing trial, recommend a sceptical prior centred on 1 (e.g. 0.5 to 2) or a vague one (e.g. 0.01 to 100) rather than reusing the optimistic design prior."))
}

# ---- Prompt ---------------------------------------------------------------
# info    : named list of character/logical fields from the form (see app.R).
# current : optional list of the app's current input values, or NULL.
build_llm_prompt <- function(info, current = NULL, otype = "cont", measure = "or") {
  otype <- otype %||% "cont"; measure <- measure %||% "or"
  has <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(trimws(x))
  line <- function(label, x) if (has(x)) paste0("- ", label, ": ", trimws(x)) else NULL

  direction_txt <- switch(info$direction %||% "unsure",
    higher = "Higher values of the outcome are better (for an event: the event is desirable).",
    lower  = "Lower values of the outcome are better (for an event: the event is harmful), so a beneficial treatment LOWERS the outcome, the event rate or the hazard.",
    "It is not yet clear whether higher or lower values are better; work this out and state it.")

  project <- c(
    line("Study title / working name", info$title),
    line("Condition and population", info$condition),
    line("Intervention (treatment arm)", info$intervention),
    line("Comparator (control arm)", info$comparator),
    line("Primary outcome", info$outcome),
    line("Time point / follow-up", info$timepoint),
    line("Study type / phase", info$phase),
    line("Setting / country", info$setting),
    line("Clinically meaningful difference (if known)", info$mcid),
    line("Practical constraints (recruitment, budget, maximum feasible size)", info$constraints),
    paste0("- Direction: ", direction_txt))
  known <- if (has(info$known)) c("", "Evidence I already know about (verify it; do not assume it is complete):",
                                  trimws(info$known)) else NULL

  keys <- llm_keys_for(otype, measure)
  current_block <- NULL
  if (!is.null(current)) {
    current_block <- c(
      "", "## Values currently entered in the app (a starting point only; replace them if the evidence says otherwise)",
      "```json", to_json_block(current[intersect(keys, names(current))]), "```")
  }

  search_txt <- if (isTRUE(info$web)) c(
    "Search the web and the literature (PubMed, Cochrane Library, ClinicalTrials.gov, published protocols, core outcome sets, regulatory guidance) for the evidence you need. Prefer, in order: recent systematic reviews/meta-analyses, large randomised trials in a similar population, smaller trials, observational data, then expert opinion.")
  else c(
    "You may not have live web access. Use what you know, but be explicit about which values come from specific published studies you are confident exist and which are your own estimates. Never invent citations; if you are unsure a source exists, say so, and list the searches I should run to verify.")

  field_lines <- vapply(keys, function(k)
    paste0("  - `", k, "`: ", LLM_FIELDS[[k]]$desc), character(1))
  fixed <- paste0("`otype` = \"", otype, "\"",
                  if (otype == "binary") paste0(" and `measure` = \"", measure, "\"") else "")

  lang <- info$language %||% "English"

  paste(c(
    "# Task",
    paste0("You are an experienced biostatistician and clinical trial methodologist. Help me choose evidence-based input values for a Bayesian assurance (probability of success) calculation for a planned two-arm randomised trial. The primary outcome is a ",
           with_lang("en", outcome_type_label(otype, measure)),
           ". The calculation tool (the \"AssuRance\" app) compares Bayesian assurance with frequentist power."),
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
    type_guidance(otype, measure),
    prior_guidance(otype, measure),
    "- **Success criterion (`alt`, threshold, `alpha`)**: two-sided vs one-sided (and which direction: `greater` means the effect must be above the threshold, `less` below it), the significance level usual for this kind of trial, and the success threshold for one-sided tests: the no-effect value for ordinary superiority, a value beyond it in the beneficial direction to demand a clinically meaningful effect (find the published minimal clinically important difference, preferring anchor-based estimates), or a value on the harmful side for a non-inferiority design (justify the margin, e.g. from regulatory guidance).",
    "- **Allocation (`ratio`)** and **dropout**: the planned randomisation ratio (treatment : control) and dropout / loss to follow-up in comparable trials.",
    "- **Sample sizes (`n_min`, `n_max`, `n_step`)**: a range per group that brackets the likely answer. Compute an approximate conventional sample size for 80-90% power at the design prior's best guess, then choose a range from well below to well above it (respecting my constraints), with a step giving about 15-25 points.",
    "- **Target (`target`)**: the probability of success to aim for (commonly 0.8, or 0.9 for confirmatory trials).",
    "",
    "## What to give me",
    "A. A short summary of the evidence you found (with full references: authors, year, title, journal, DOI or URL).",
    "B. A table with one row per input: value, how it was derived, source(s), and your confidence (high / moderate / low).",
    "C. Two alternative scenarios I can compare in the app: a pessimistic one (smaller effect and/or more uncertainty, less favourable control-group assumptions) and an optimistic one, each with the inputs that change.",
    "D. Caveats: gaps in the evidence, assumptions I should check with the clinical team, and anything that makes this outcome type or its assumptions a poor approximation (e.g. skewed or bounded outcomes, clustering, repeated measures, non-proportional hazards, very rare events). If a different outcome type or effect measure would suit my trial better, say so and why.",
    paste0("E. Finally, the base-case values as ONE JSON object in a ```json code block with EXACTLY these keys (numbers as plain numbers, no units, no comments; keep ", fixed, "):"),
    field_lines,
    "",
    "## Rules",
    "- Do not fabricate studies, numbers or DOIs. If you cannot find evidence for a value, say so and give a clearly labelled judgement-based value.",
    "- Show the arithmetic for every conversion.",
    "- Keep the explanation understandable to a clinician who is not a statistician.",
    paste0("- Write your answer in ", lang, " (keep the JSON keys and the values of `otype`, `measure` and `alt` exactly as specified).")
  ), collapse = "\n")
}

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
    return(list(values = list(), messages = L("Nothing was pasted.", "Nada foi colado.")))
  }

  # Prefer the last ```json fenced block; otherwise the outermost { ... }.
  blocks <- regmatches(text, gregexpr("```(json|JSON)?\\s*\\{[\\s\\S]*?\\}\\s*```", text, perl = TRUE))[[1]]
  json <- if (length(blocks)) {
    sub("^```(json|JSON)?\\s*", "", sub("\\s*```$", "", blocks[length(blocks)]))
  } else {
    start <- regexpr("\\{", text); ends <- gregexpr("\\}", text)[[1]]
    if (start < 0 || ends[1] < 0) {
      return(list(values = list(), messages = L("No JSON object was found in the pasted text.",
                                                "Nenhum objeto JSON foi encontrado no texto colado.")))
    }
    substr(text, start, max(ends))
  }

  raw <- tryCatch(jsonlite::fromJSON(json, simplifyVector = TRUE),
                  error = function(e) NULL)
  if (!is.list(raw) || is.null(names(raw))) {
    return(list(values = list(),
                messages = L("The JSON could not be read. Check that it is a single object like {\"sigma\": 10, ...}.",
                             "N\u00E3o foi poss\u00EDvel ler o JSON. Verifique se \u00E9 um \u00FAnico objeto como {\"sigma\": 10, ...}.")))
  }
  # backwards compatibility with the earlier `margin` key (continuous only)
  if (!is.null(raw$margin) && is.null(raw$threshold)) {
    m <- suppressWarnings(as.numeric(raw$margin))
    alt <- tolower(as.character(raw$alt %||% ""))
    if (length(m) == 1 && is.finite(m)) raw$threshold <- if (alt == "less") -m else m
    raw$margin <- NULL
  }

  unknown <- setdiff(names(raw), names(LLM_FIELDS))
  if (length(unknown)) msgs <- c(msgs, paste0(L("Ignored unknown field(s): ", "Campo(s) desconhecido(s) ignorado(s): "),
                                              paste(unknown, collapse = ", "), "."))

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
    } else if (type == "otype") {
      v <- tolower(trimws(as.character(v)))
      v <- switch(v, "cont" = , "continuous" = "cont", "ancova" = "ancova",
                  "binary" = "binary", "surv" = , "survival" = , "time to event" = "surv", NA)
      ok <- !is.na(v)
    } else if (type == "measure") {
      v <- tolower(trimws(as.character(v)))
      ok <- length(v) == 1 && v %in% c("or", "rr", "rd")
    } else if (type == "text") {
      v <- trimws(as.character(v))
      ok <- length(v) == 1 && nzchar(v) && nchar(v) <= 30
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
        pct_open = v > 0 && v < 100,
        rd       = abs(v) < 100,
        rho      = v >= 0 && v <= 0.95,
        prob     = v > 0 && v < 1,
        target   = v >= 0.5 && v <= 0.99)
    }
    if (isTRUE(ok)) out[[k]] <- v
    else msgs <- c(msgs, paste0(L("Skipped `", "Ignorado `"), k, L("`: invalid value.", "`: valor inv\u00E1lido.")))
  }
  if (!is.null(out$n_min) && !is.null(out$n_max) && out$n_max <= out$n_min) {
    msgs <- c(msgs, L("Skipped the sample-size range: n_max must exceed n_min.",
                      "Faixa de tamanhos amostrais ignorada: n_max deve ser maior que n_min."))
    out$n_min <- NULL; out$n_max <- NULL
  }
  for (pair in list(c("ratio_design_lo", "ratio_design_hi"),
                    c("ratio_analysis_lo", "ratio_analysis_hi"))) {
    if (!is.null(out[[pair[1]]]) && !is.null(out[[pair[2]]]) &&
        out[[pair[2]]] <= out[[pair[1]]]) {
      msgs <- c(msgs, paste0(L("Skipped `", "Ignorados `"), pair[1], "`/`", pair[2],
                             L("`: the upper end must exceed the lower end.", "`: o limite superior deve ser maior que o inferior.")))
      out[[pair[1]]] <- NULL; out[[pair[2]]] <- NULL
    }
  }
  list(values = out, messages = msgs)
}
