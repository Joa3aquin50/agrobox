
# 🌱 agrobox

### Statistical analysis, visualization and decision support for agricultural experiments

[![CRAN
status](https://www.r-pkg.org/badges/version/agrobox)](https://CRAN.R-project.org/package=agrobox)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/agrobox)](https://CRAN.R-project.org/package=agrobox)
[![R-CMD-check](https://github.com/Joa3aquin50/agrobox/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Joa3aquin50/agrobox/actions)

------------------------------------------------------------------------

## 🌾 Why agrobox?

Agricultural experiments can be carefully designed, rigorously
conducted, and full of valuable information.

But after collecting the data, researchers often face another challenge:

> **How do I turn my experimental data into a statistical result — and,
> more importantly, into a decision?**

The statistical workflow can quickly become complicated:

    Data → Choose the model → Check assumptions → ANOVA → Post-hoc → Interpret → Figure → Decision

For researchers who are not specialized in R or statistics, this becomes
a real barrier.

**agrobox was created to reduce that barrier.**

The goal is not to replace experimental design or statistical thinking.
The goal is to make the analytical workflow:

- ✅ Easier to access
- ✅ Easier to reproduce
- ✅ Easier to understand
- ✅ Faster to execute
- ✅ Closer to the final scientific decision

> ⚠️ **agrobox does not rescue a poorly designed experiment.**  
> It helps you get more efficiently from a *well-designed* experiment to
> its analysis and interpretation.

------------------------------------------------------------------------

## 🚀 What is agrobox?

**agrobox** is an R package designed for statistical analysis,
visualization, and reporting of agricultural and agroindustrial
experiments.

It provides automated workflows for:

- One-way and two-way ANOVA
- Post-hoc comparisons (Tukey HSD, Duncan MRT, Games-Howell)
- Heteroscedastic data (Welch ANOVA)
- Statistical assumption checking (Shapiro-Wilk, Fligner-Killeen)
- Coefficient of variation and statistical power
- Publication-ready visualizations (ggplot2)
- Multi-variable analysis and factor clustering
- Excel export and high-resolution table export
- Structured decision-oriented summaries

------------------------------------------------------------------------

## 📦 Installation

Install the stable version from CRAN:

``` r
install.packages("agrobox")
```

Then load the package:

``` r
library(agrobox)
```

------------------------------------------------------------------------

## 🧠 Core idea: `agrobox()`

The main function organizes the entire statistical workflow around your
experimental factors and response variable:

``` r
resultado <- agrobox(
  data     = datos,
  factor   = "tratamiento",
  variable = "rendimiento"
)

resultado
```

The function automatically evaluates the statistical assumptions and
selects the appropriate analysis path:

    Shapiro-Wilk (normality)
            ↓
    Fligner-Killeen (homogeneity of variances)
            ↓
    ┌──────────────────────┐        ┌──────────────────────┐
    │  Variances adequate  │        │  Heteroscedasticity  │
    │                      │   OR   │                      │
    │       ANOVA          │        │     Welch ANOVA      │
    │         ↓            │        │          ↓           │
    │  Tukey / Duncan      │        │    Games-Howell      │
    └──────────────────────┘        └──────────────────────┘

The researcher does not need to manually reproduce every step for every
variable.

------------------------------------------------------------------------

## 🔬 Statistical workflow

**Parametric analysis** - One-way and two-way ANOVA - Tukey HSD - Duncan
Multiple Range Test

**Robust analysis** (when variance homogeneity is not supported) - Welch
ANOVA - Games-Howell post-hoc test

**Diagnostics** - Shapiro-Wilk test for residual normality -
Fligner-Killeen test for homogeneity of variances

**Additional output** - Coefficient of variation (CV) - Statistical
power - Means, grouping letters, and significance annotations

------------------------------------------------------------------------

## 📊 Visualization

`agrobox()` returns **ggplot2-based figures** ready for publication,
including:

- Experimental means
- Statistical grouping letters
- Boxplots with treatment comparisons
- CV and statistical power annotations

The output can be further customized using the full ggplot2 ecosystem.

------------------------------------------------------------------------

## 🧩 Multiple factors and experimental structures

Agricultural experiments frequently involve more than one factor (e.g.,
Variety × Treatment, Treatment × Location).

`agrobox()` supports one and two experimental factors, with options for:

- Factor ordering and relabeling
- Grouping and clustering
- Block information (RCBD)
- Customized graphical output

------------------------------------------------------------------------

## 🧠 `agrosintesis()` — From numbers to decisions

A single experiment rarely measures only one variable. You might record
yield, fruit weight, firmness, color, soluble solids, acidity,
incidence, severity — and more.

Running each analysis independently produces a lot of output without
necessarily making the experiment easier to understand.

**`agrosintesis()` solves this.**

It applies the `agrobox()` workflow to multiple response variables
simultaneously and consolidates the results into a structured,
decision-oriented synthesis:

``` r
resultado <- agrosintesis(
  data      = datos,
  variables = c("rendimiento", "peso_fruto", "firmeza", "solidos_solubles")
)
```

Instead of:

    Variable 1 → analysis
    Variable 2 → analysis
    Variable 3 → analysis

You get:

    EXPERIMENT
        ↓
    Variable 1 + Variable 2 + Variable 3
        ↓             ↓             ↓
     Analysis      Analysis      Analysis
             \        |        /
              agrosintesis()
                    ↓
               SYNTHESIS
                    ↓
               DECISION

> Statistical analysis should help you **understand** the experiment,
> not just produce more numbers.

------------------------------------------------------------------------

## 📑 `agrotabla()` — Publication-ready tables

Export statistical results as high-resolution images suitable for
reports, presentations, and scientific publications:

``` r
agrotabla(resultado)
```

------------------------------------------------------------------------

## 📊 `agroexcel()` — Excel export

Export results directly to Excel, organized by variable and experimental
cluster:

``` r
agroexcel(resultado)
```

Particularly useful when an experiment contains several variables —
results are organized into worksheets, eliminating manual copy-paste
from R to Excel.

------------------------------------------------------------------------

## 🧪 A typical workflow

``` r
library(agrobox)

# Single variable
resultado <- agrobox(
  data     = datos,
  factor   = "tratamiento",
  variable = "rendimiento"
)

resultado

# Multiple variables
resultado <- agrosintesis(
  data      = datos,
  variables = c("rendimiento", "peso", "firmeza", "calidad")
)

# Export
agroexcel(resultado)
agrotabla(resultado)
```

**The complete pipeline:**

    EXPERIMENTAL DATA
           ↓
       agrobox()
           ↓
    Statistical diagnostics → Analysis → Post-hoc → Graphics
           ↓
     agrosintesis()
           ↓
        SYNTHESIS
           ↓
        DECISION
           ↓
    Excel / Tables

------------------------------------------------------------------------

## ⚠️ What agrobox does NOT do

agrobox simplifies the statistical workflow. It does **not** replace
experimental design or statistical reasoning.

No package can compensate for:

- Poor experimental design
- Pseudoreplication
- Inadequate randomization
- Insufficient replication
- Uncontrolled sources of variation

> **Good statistics cannot rescue bad experimental design.**  
> When the experiment has been designed correctly, agrobox makes the
> analysis more accessible and reproducible.

------------------------------------------------------------------------

## 💡 Help shape agrobox

agrobox is open source. Its development is driven by real agricultural
problems.

If you find yourself thinking *“I wish agrobox could do this…”* — please
tell me.

- 💡 [Suggest a feature](https://github.com/Joa3aquin50/agrobox/issues)
- 🐛 [Report a bug](https://github.com/Joa3aquin50/agrobox/issues)
- 💬 [Start a
  discussion](https://github.com/Joa3aquin50/agrobox/discussions)

When reporting a bug, please include: your R version, your agrobox
version, a reproducible example, the error message, and what you
expected to happen.

------------------------------------------------------------------------

## 🤝 Contributing

Contributions are welcome. You can help by:

- Reporting bugs
- Suggesting new features or experimental designs
- Improving documentation
- Sharing reproducible examples
- Submitting pull requests

Every contribution helps make statistical analysis more accessible to
agricultural researchers.

------------------------------------------------------------------------

## 📚 Scientific methods

The procedures implemented in agrobox are based on established
statistical methods:

| Method                     | Reference                |
|----------------------------|--------------------------|
| Tukey HSD                  | Tukey (1949)             |
| Duncan Multiple Range Test | Duncan (1955)            |
| Welch ANOVA                | Welch (1951)             |
| Games-Howell               | Games & Howell (1976)    |
| Shapiro-Wilk               | Shapiro & Wilk (1965)    |
| Fligner-Killeen            | Fligner & Killeen (1976) |
| Statistical power          | Cohen (1988)             |

The package builds on the R ecosystem, including `ggplot2` and
`agricolae`.

------------------------------------------------------------------------

## 🌱 Citation

If you use agrobox in your research, please cite the package:

``` r
citation("agrobox")
```

------------------------------------------------------------------------

## 👨‍🔬 Author

**Joaquin Alejandro Salinas Angeles**  
Agronomist & Agricultural Researcher

Interests: agricultural experimentation · statistical analysis ·
postharvest research · reproducible research · R programming

------------------------------------------------------------------------

## ⭐ Support the project

If agrobox is useful to you:

- ⭐ Star the repository
- 🐛 Report problems
- 💡 Suggest improvements
- 📢 Share it with another researcher

------------------------------------------------------------------------

> 🌾 *From experiment to decision — let’s make agricultural statistics
> more accessible.*
