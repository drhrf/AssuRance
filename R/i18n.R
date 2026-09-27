# =============================================================================
# Internationalisation (English / Brazilian Portuguese).
#
# How it works
#   * Static UI text is written in both languages with tt() (inline) or
#     tp() (paragraphs). Each language sits in an element with a `lang`
#     attribute and CSS shows only the active one, so switching language is
#     instant and never resets any input.
#   * Text produced by the server (summaries, plots, tables, messages) is
#     chosen with L(en, pt), which reads the current language from the R
#     option "assurance.lang". The server wraps every render in with_lang().
#   * Model labels built once per run are stored as both languages with
#     LL(en, pt) and picked at display time with tx().
#   * The language toggle (see lang_head()) stores the choice in the
#     browser, sets <html data-lang>, and tells Shiny via input$lang.
#
# Source files stay ASCII: accented characters are written as \u escapes.
# =============================================================================

current_lang <- function() {
  if (identical(getOption("assurance.lang"), "pt")) "pt" else "en"
}

# Evaluate `expr` with the given language active.
with_lang <- function(lang, expr) {
  old <- options(assurance.lang = if (identical(lang, "pt")) "pt" else "en")
  on.exit(options(old), add = TRUE)
  expr
}

# Pick the string for the current language (server-side text).
L <- function(en, pt) if (current_lang() == "pt") pt else en

# Store both languages (for labels kept inside a model object) ...
LL <- function(en, pt) c(en = en, pt = pt)
# ... and pick one at display time. Plain strings pass through unchanged.
tx <- function(x) {
  if (length(x) == 2 && identical(names(x), c("en", "pt"))) unname(x[[current_lang()]])
  else x
}

# Static UI: both languages, CSS shows the active one.
tt <- function(en, pt) tagList(tags$span(lang = "en", en), tags$span(lang = "pt", pt))
tp <- function(en, pt, ...) tagList(tags$p(lang = "en", ...,  en), tags$p(lang = "pt", ..., pt))

# Translations for <option> labels and placeholders, which cannot contain
# markup. The JavaScript in lang_head() swaps them when the language changes.
I18N_OPTIONS <- list(
  otype = list(
    cont   = c("Continuous: difference in means", "Cont\u00EDnuo: diferen\u00E7a de m\u00E9dias"),
    ancova = c("Continuous, adjusted for baseline (ANCOVA)", "Cont\u00EDnuo, ajustado pelo valor basal (ANCOVA)"),
    binary = c("Binary (event yes / no)", "Bin\u00E1rio (evento sim / n\u00E3o)"),
    surv   = c("Time to event (survival)", "Tempo at\u00E9 o evento (sobrevida)")),
  pg_phase = list(
    "Pilot / feasibility"       = c("Pilot / feasibility", "Piloto / viabilidade"),
    "Phase II"                  = c("Phase II", "Fase II"),
    "Phase III / confirmatory"  = c("Phase III / confirmatory", "Fase III / confirmat\u00F3rio"),
    "Pragmatic / effectiveness" = c("Pragmatic / effectiveness", "Pragm\u00E1tico / efetividade"),
    "Non-inferiority"           = c("Non-inferiority", "N\u00E3o inferioridade"),
    "Other / not sure"          = c("Other / not sure", "Outro / n\u00E3o sei")),
  pg_language = list(
    "English"             = c("English", "Ingl\u00EAs"),
    "Portuguese (Brazil)" = c("Portuguese (Brazil)", "Portugu\u00EAs (Brasil)"),
    "Spanish"             = c("Spanish", "Espanhol"),
    "French"              = c("French", "Franc\u00EAs"),
    "German"              = c("German", "Alem\u00E3o"))
)

I18N_PLACEHOLDERS <- list(
  scenario_label  = c("Scenario name (optional)", "Nome do cen\u00E1rio (opcional)"),
  pg_condition    = c("e.g. adults with uncontrolled hypertension", "ex.: adultos com hipertens\u00E3o n\u00E3o controlada"),
  pg_intervention = c("e.g. drug X 10 mg daily", "ex.: f\u00E1rmaco X 10 mg/dia"),
  pg_comparator   = c("e.g. placebo, usual care", "ex.: placebo, cuidado usual"),
  pg_outcome      = c("e.g. systolic BP (mmHg); death from any cause", "ex.: PA sist\u00F3lica (mmHg); \u00F3bito por qualquer causa"),
  pg_timepoint    = c("e.g. 12 weeks", "ex.: 12 semanas"),
  pg_setting      = c("e.g. primary care, Brazil", "ex.: aten\u00E7\u00E3o prim\u00E1ria, Brasil"),
  pg_mcid         = c("e.g. 5 mmHg; HR 0.8", "ex.: 5 mmHg; HR 0,8"),
  pg_known        = c("Pilot results, key trials, meta-analyses, DOIs...", "Resultados de piloto, ensaios-chave, metan\u00E1lises, DOIs..."),
  pg_constraints  = c("e.g. can recruit at most 150 per arm in 2 years", "ex.: consigo recrutar no m\u00E1ximo 150 por bra\u00E7o em 2 anos")
)

# CSS + JavaScript for the language toggle, placed in the page <head>.
lang_head <- function() {
  opts <- jsonlite::toJSON(lapply(I18N_OPTIONS, function(o) lapply(o, as.list)), auto_unbox = TRUE)
  phs  <- jsonlite::toJSON(lapply(I18N_PLACEHOLDERS, as.list), auto_unbox = TRUE)
  tags$head(
    tags$style(HTML(paste(
      'html[data-lang="en"] body [lang="pt"], html[data-lang="pt"] body [lang="en"] { display: none !important; }',
      '#lang_toggle { font-weight: 600; font-size: 0.9rem; }'))),
    tags$script(HTML(paste0('
(function () {
  var OPTS = ', opts, ', PHS = ', phs, ';
  function initial() {
    try {
      var q = new URLSearchParams(location.search).get("lang");
      if (q === "pt" || q === "en") return q;
      var s = localStorage.getItem("assurance-lang");
      if (s === "pt" || s === "en") return s;
    } catch (e) {}
    return (navigator.language || "").toLowerCase().indexOf("pt") === 0 ? "pt" : "en";
  }
  function translateControls(lang) {
    var k = lang === "pt" ? 1 : 0;
    Object.keys(OPTS).forEach(function (id) {
      var sel = document.getElementById(id); if (!sel) return;
      Array.prototype.forEach.call(sel.options, function (o) {
        var t = OPTS[id][o.value]; if (t) o.textContent = t[k];
      });
    });
    Object.keys(PHS).forEach(function (id) {
      var el = document.getElementById(id); if (el) el.setAttribute("placeholder", PHS[id][k]);
    });
  }
  window.assuranceSetLang = function (lang) {
    var root = document.documentElement;
    root.setAttribute("data-lang", lang);
    root.setAttribute("lang", lang === "pt" ? "pt-BR" : "en");
    try { localStorage.setItem("assurance-lang", lang); } catch (e) {}
    translateControls(lang);
    // formulas in text that was hidden at page load have not been typeset yet
    if (window.MathJax && MathJax.Hub) MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue("lang", lang);
  };
  window.assuranceToggleLang = function () {
    window.assuranceSetLang(document.documentElement.getAttribute("data-lang") === "pt" ? "en" : "pt");
  };
  document.documentElement.setAttribute("data-lang", initial());
  document.addEventListener("DOMContentLoaded", function () { translateControls(initial()); });
  $(document).on("shiny:connected", function () { window.assuranceSetLang(initial()); });
})();')))
  )
}

# The toggle button shown in the page header.
lang_toggle_button <- function() {
  tags$button(id = "lang_toggle", type = "button", class = "btn btn-sm btn-outline-secondary ms-auto",
              onclick = "assuranceToggleLang()",
              `aria-label` = "Switch language / Mudar idioma",
              tt("\U0001F1E7\U0001F1F7 Portugu\u00EAs", "\U0001F1EC\U0001F1E7 English"))
}
