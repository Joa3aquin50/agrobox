# agrobox 0.4.0

## New features

- New automatic route selection in `agrobox()`. The goal is to place
  post-hoc letters through a viable and defensible statistical route.
  For each cluster the function checks sufficient data, Shapiro-Wilk
  (residual normality) and Fligner-Killeen (homogeneity of variances),
  and then selects:
  - **Route A** (normal + homogeneous): ANOVA + Duncan / Tukey
    (block and `factor2` included in the model).
  - **Route B** (normal + heteroscedastic): Welch ANOVA + Games-Howell.
  - **Route C** (not normal + homogeneous): Kruskal-Wallis, or Friedman
    when a block is present.
  - **Route D** (not normal + heteroscedastic): same tests as route C,
    flagged with a note on heterogeneous variances.

- Added non-parametric analysis:
  - Kruskal-Wallis with letters from `agricolae::kruskal()` or Dunn
    post-hoc test, selected with the new argument
    `np_test = c("kruskal", "dunn")`.
  - Friedman test for blocked designs (RCBD). Replicates per
    treatment x block are averaged and incomplete blocks are excluded.
    Letters are protected by the global Friedman test (all treatments
    share "a" when it is not significant).
  - New argument `p.adj` (default `"bonferroni"`) for p-value
    adjustment in Kruskal-Wallis and Dunn comparisons.

- Fallback chain: if the post-hoc test of the selected route cannot be
  computed, the next viable route is tried (A/B -> non-parametric,
  Friedman -> Kruskal-Wallis, Kruskal-Wallis <-> Dunn). Means without
  letters are shown only as a last resort, always with an explicit note.

- Each `agrobox()` figure now includes a method note (caption) describing
  the route used in each panel: why it was selected, its advantages,
  limitations and scope. It can be removed with
  `+ ggplot2::labs(caption = NULL)`.

- Route B now computes the Welch ANOVA p-value (`oneway.test()`).

- New columns in `$stats`: `ruta`, `metodo`, `p_ruta` (p-value of the
  global test used by the route) and `nota`.

- Added `agroppt()` to export a ggplot figure to a single editable
  PowerPoint slide (vector graphic), with custom width and height.

## Bug fixes

- Games-Howell letters were relabeled as whole strings, so a group such
  as "ab" became a new letter and produced wrong groupings. Letters are
  now relabeled letter by letter.
- Games-Howell failed silently when `bloque` or `factor2` were supplied
  (letters disappeared). It now compares treatments only and adds a note.
- `agrobox()` failed with *could not find function "%>%"* when `dplyr`
  was not loaded. `%>%` and `.data` are now imported.
- `estructura = "row~"` (documented) produced a parsing error in
  `facet_grid()`; it is now completed internally as `"row ~ ."`.
- Group labels containing `"_"` (or non-ASCII characters in non-UTF-8
  sessions) sent means and letters to an extra `NA` panel. Grouping
  columns are now recovered by joining on the cluster instead of
  splitting its label.
- Negative responses: the automatic upper y-axis limit fell below the
  maximum and removed observations; the CV was reported as negative.
  The CV now uses the absolute value of the mean.
- Manual `lim_sup` / `lim_inf` removed observations outside the range and
  altered the boxplots. Limits now use `coord_cartesian()` (zoom only).
- Statistical power was underestimated with blocks or `factor2`; it now
  uses partial eta squared (identical results in one-way models).
- Numeric levels (e.g. doses 0, 50, 100, 150) were ordered as text; they
  are now ordered numerically, and factor levels are respected.
- `$tabla` columns and `$stats` rows now follow the facet order defined
  by `grupo1_orden` / `grupo2_orden`.
- Empty cells are shown instead of `NA` in `$tabla` when treatments are
  nested within groups.
- LaTeX `\makecell` headers in `$tabla` no longer split labels that
  contain `"_"`.

## Improvements

- A message now lists factor levels that are excluded because they are
  not included in `orden_factor`, `grupo1_orden` or `grupo2_orden`.
- CV and Power are reported in every route (computed from the classic
  ANOVA fit).
- The `ANOVA` row of `$tabla` shows significance stars only for route A;
  other routes show `"-"`.
- Example 5 of `agrobox()` wrapped in `\donttest{}` to keep example
  run time within CRAN limits.

## Deprecated

- `var.equal` no longer changes the results: the route is selected
  automatically. The argument is kept for backward compatibility and
  a message is shown when it is supplied.

## Changes in results compared with 0.3.0

- Panels that previously showed means without letters (e.g. failed
  normality) now receive letters through routes B, C or D.
- Games-Howell letters may differ where groups overlap (bug fix above).
- Statistical power changes in models with block or `factor2`.
- Route A results (means, letters, ANOVA, CV) are unchanged.

## Dependencies

- Added `officer` and `rvg` to Suggests (only required by `agroppt()`).

---

# agrobox 0.3.0

## New features

- Added support for combined ordering and relabeling through:
  - `orden_factor`
  - `grupo1_orden`
  - `grupo2_orden`
  
  These arguments now accept both named and unnamed vectors:
  - Unnamed vectors reorder levels.
  - Named vectors reorder and relabel simultaneously.

- Added two real-world datasets:
  - `nitrogeno_liberacion`: nitrogen release dynamics from fertilizers.
  - `pimiento_hibridacion`: hybrid seed production dynamics in pepper.

- Added new outputs to `agrobox()`:
  - `$data`: summarized dataset used in the analysis (means, sd, n).
  - `$stats`: ANOVA diagnostics per cluster (p-values, CV, Power).

## Improvements

- Faceting now correctly respects factor level order defined by:
  - `grupo1_orden`
  - `grupo2_orden`
  
  This resolves previous inconsistencies with `facet_grid()` ordering.

- Improved handling of factor levels across all internal steps:
  - Consistent propagation of levels from raw data → analysis → plotting.
  - Prevents unintended reordering in ggplot facets.

- Cluster construction is now more robust:
  - Uses `interaction()` when both grouping variables are present.
  - Preserves factor levels in all grouping scenarios.

- Enhanced statistical robustness:
  - Improved handling of missing values (NA) in post-hoc comparisons.
  - Automatic suppression of letters when statistical assumptions fail.
  
- Improved compatibility with ggplot2 faceting behavior by enforcing factor levels prior to plotting.

## Internal changes

- Refactored factor handling through `aplicar_orden_labels()`:
  - Unified logic for ordering and relabeling across all variables.

- Reworked cluster generation logic to support:
  - Single grouping
  - Dual grouping
  - No grouping (single panel)

- Improved reconstruction of facet variables for `geom_text()`:
  - Ensures correct placement of labels within panels.

- Cleaner separation between:
  - data preprocessing
  - statistical analysis
  - visualization
  - reporting

---

# agrobox 0.2.1

## Minor fixes

- Resubmission to CRAN after incoming checks.
- Moved `magick`, `kableExtra`, and `tinytex` from Imports to Suggests.
- No user-facing changes.
---

# agrobox 0.2.0

## New features

- Added `agrosintesis()`, a high-level wrapper to automate multi-variable ANOVA
  summaries using `agrobox()` as the core engine.
  - Supports multiple response variables.
  - Supports optional clustering through a formula interface
    (e.g. `Variedad ~ Localidad`).
  - Automatically iterates over cluster combinations and returns
    publication-ready summary tables.
  - When `estructura = NULL`, the analysis is performed globally
    (no clustering).

- Added `agroexcel()` to export agrobox and agrosintesis results
  to Excel (`.xlsx`) files.
  - Works with single tables and lists of tables.
  - Automatically creates one worksheet per cluster.
  - Sheet names are sanitized to comply with Excel and Windows limitations.

- Added `agrotabla()` to export result tables as high-resolution PNG images.
  - Uses LaTeX for high-quality table rendering.
  - Supports both single tables and lists of tables.
  - File names are sanitized for LaTeX and filesystem compatibility.
  - Designed for fast reporting and presentation-ready outputs.

## Improvements

- Cluster labels now preserve original factor levels instead of numeric codes.
- Improved handling of factor levels when clustering is enabled.
- Column names are automatically escaped to avoid LaTeX compilation issues.
- File and sheet name sanitization improves Windows compatibility.

## Internal changes

- Refactored reporting logic to separate analysis from export utilities.
- Avoided side effects in core analytical functions.
- Improved CRAN compliance by:
  - Using fully qualified namespace calls.
  - Avoiding `library()` calls inside functions.
  - Wrapping file-generating examples in `\dontrun{}`.

---

# agrobox 0.1.1

## Changes

- Improved main function performance and robustness.
