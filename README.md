# AssuRance

**Bayesian assurance vs. frequentist power for two-arm clinical trials with a
continuous outcome.**

> **Live app:** _coming soon_

AssuRance helps you choose a sample size by comparing two ways of asking "how
likely is this trial to succeed?":

- **Frequentist power** assumes the treatment effect is known exactly.
- **Bayesian assurance** (also called the probability of success) averages the
  chance of success over every effect size you consider plausible.

The Bayesian calculations use the
[`bayesassurance`](https://github.com/jpan928/bayesassurance_rpackage) R
package.

![Results tab](docs/screenshot-results.png)

## Features

- **Separate design and analysis priors.** The design prior is what you
  believe and is used to simulate trials. The analysis prior is what the final
  analysis will use. You can make them identical with one click.
- **Assurance and power curves** across a range of sample sizes, with a
  target line and 95% Monte Carlo bands for the simulated values.
- **Two engines.** Simulation with
  `bayesassurance::bayes_sim_unbalanced()` is the default. The exact
  closed-form formula is instant and can also be overlaid as a check.
- **Sample-size finder.** It finds the smallest sample size that reaches your
  target assurance or power. It also shows the **assurance ceiling**, the
  highest assurance any sample size can reach, and tells you when your target
  is out of reach.
- **Flexible success rule.** You can use one- or two-sided tests and require
  the effect to exceed a clinically meaningful difference (margin).
- **Unequal allocation** (for example, 2:1) and **dropout**: the app reports
  how many people to enrol.
- **A plain-language summary** for non-statisticians. It flags a gap of more
  than 15 percentage points between assurance and power, and for two-sided
  tests it notes how much of the "success" is a finding in the unexpected
  direction.
- **Priors tab.** It shows your design and analysis priors and the effects
  that count as a success.
- **Success vs true effect tab.** It shows how assurance averages the success
  probability over the design prior.
- **Sensitivity tab.** Heat map of assurance across design-prior means and
  SDs, and a curve of assurance against the analysis-prior SD.
- **Compare scenarios.** Save up to 8 runs and overlay them.
- **Downloads and sharing.** CSV table, PNG plot, a text report, and a
  bookmark link that restores all inputs.
- **LLM prompt helper.** Describe your project (condition, intervention,
  outcome and so on). The app writes a prompt for an AI assistant, ideally
  one that can search the web, asking it to research evidence-based values
  for every input, show its sources and conversions, and end with a JSON
  block. Paste the answer back and the app fills in its inputs, skipping any
  value that isn't valid. Always check the numbers and references it gives.
- A **Methods & help** tab with formulas and references.

![Success vs true effect](docs/screenshot-success-vs-effect.png)

## Run it locally (RStudio)

1. **Get the code.** In RStudio, go to *File → New Project → Version Control
   → Git*. Enter `https://github.com/drhrf/AssuRance.git` as the repository
   URL, choose a folder and click *Create Project*. (From a terminal you can
   instead run `git clone https://github.com/drhrf/AssuRance.git`, then open
   `AssuRance.Rproj`.)
2. **Install the packages.** Run this once in the RStudio console:
   ```r
   source("setup.R")
   ```
   It installs `shiny`, `bslib`, `plotly`, `ggplot2`, `DT`, `rsconnect` and
   `bayesassurance`. If `bayesassurance` isn't available on CRAN, it installs
   it from GitHub instead.
3. **Run the app.** Open `app.R` and click **Run App**, or run
   `shiny::runApp()`.
4. **Get updates later.** Open the *Git* tab (top-right pane) and click
   **Pull**.

## Publish on Posit Cloud

1. Sign in at [posit.cloud](https://posit.cloud) and open *Your Workspace*.
2. Click **New Project → New Project from Git Repository**, paste
   `https://github.com/drhrf/AssuRance`, and click **OK**. Posit Cloud clones
   the repository and opens it in RStudio in your browser.
3. In the console, run `source("setup.R")`. This takes a few minutes the first
   time.
4. Open `app.R` and click **Run App** to check that it works.
5. With `app.R` open, click the blue **Publish** icon at the top right of the
   editor pane. In a Posit Cloud project, the dialog is already set up to
   publish to Posit Cloud.
   - Make sure `app.R` and all five files in `R/` are ticked. `tests/`,
     `docs/`, `setup.R` and this README aren't needed (a `.rscignore` file
     leaves them out).
   - Give it a title, such as *AssuRance*, and click **Publish**.
6. Once it has deployed, the app appears under **Content** in the same space.
   To make it public, open the app's **Settings → Access**, choose **Anyone**
   (no login required), and copy the URL.

**Updating the published app.** Pull the latest code (*Git* tab → **Pull**),
then click **Publish** again and choose the existing deployment. The URL stays
the same.

**Notes**

- The free Posit Cloud plan has limited monthly compute hours and memory, and
  apps go to sleep when idle, so the first visit after a break can take a
  few seconds.
- The simulation engine gets slower as sample sizes grow (its cost scales with
  the square of the sample size). The sidebar shows an estimated run time,
  and the app refuses jobs estimated at over 15 minutes. For quick
  exploration, use the exact engine.
- If publishing fails with an error about `bayesassurance`'s source, reinstall
  it from GitHub with
  `remotes::install_github("jpan928/bayesassurance_rpackage")`, restart R,
  and publish again.

## Files

| File | Purpose |
| --- | --- |
| `app.R` | User interface and server logic |
| `R/calculations.R` | Statistical engine; documents how the inputs map onto `bayesassurance`'s parameters |
| `R/summary_text.R` | Plain-language summary |
| `R/plots.R` | Plot builders (plotly for the screen, ggplot2 for PNG export) |
| `R/methods_ui.R` | Content of the *Methods & help* tab |
| `R/prompt_generator.R` | Builds the prompt for the LLM helper and reads its JSON answer |
| `tests/test_calculations.R` | Checks the simulation against the exact formula, and power against `power.t.test` |
| `tests/test_prompt_generator.R` | Checks the prompt builder and the JSON answer parser |
| `setup.R` | Installs the required packages |

Run the tests with `Rscript tests/test_calculations.R` (about a minute) and
`Rscript tests/test_prompt_generator.R` (instant).

## Method notes

- **Model.** Outcomes are normal with a known common SD σ. The treatment
  effect Δ = μ<sub>T</sub> − μ<sub>C</sub> has a design prior
  N(m<sub>d</sub>, s<sub>d</sub>²) and an analysis prior
  N(m<sub>a</sub>, s<sub>a</sub>²). A trial succeeds when the posterior
  probability that Δ exceeds the margin (in the tested direction) is greater
  than 1 − α, or 1 − α/2 per tail for a two-sided test.
- **Package mapping.** `bayesassurance` expresses prior variances relative to
  σ², so a prior SD *s* enters as *s*²/σ². The analysis prior is placed only
  on the treatment–control contrast, with a flat prior on the common level.
  `R/calculations.R` spells this out step by step.
- **Why not `assurance_nd_na()`?** The package's closed-form function ties
  the analysis prior mean to the null value, and it doesn't match the exact
  assurance when the analysis prior is informative. The app uses the
  simulation functions, plus its own verified closed form.
- **Power.** The app uses two-sample t-test power (noncentral t), which is
  identical to `stats::power.t.test()` for equal groups and extends it to
  unequal groups and a margin.

## References

- O'Hagan A, Stevens JW, Campbell MJ (2005). Assurance in clinical trial
  design. *Pharmaceutical Statistics* 4(3):187–201.
- Pan J, Banerjee S (2023). bayesassurance: An R package for calculating
  sample size and Bayesian assurance. *The R Journal*.
  [RJ-2023-066](https://journal.r-project.org/articles/RJ-2023-066/)
