## Update submission

This is an update of agrobox (0.3.0 -> 0.4.0).

Main changes (see NEWS.md for details):

* `agrobox()` now selects the statistical route automatically (ANOVA,
  Welch + Games-Howell, Kruskal-Wallis / Dunn, Friedman) and adds a method
  note to each figure.
* Several bug fixes, including the Games-Howell letter display, the missing
  import of `%>%`, faceting with `estructura = "row~"`, and statistical power
  in blocked designs.
* New function `agroppt()` to export a figure to an editable PowerPoint
  slide. Its dependencies (`officer`, `rvg`) are in Suggests and are checked
  with `requireNamespace()`.

## Test environments

* local Windows 11 x64, R 4.5.1
* win-builder: R-devel (2026-09-30 r90605 ucrt)

## R CMD check results

0 errors | 0 warnings | 0 notes (win-builder R-devel: Status OK)

The local check showed a WARNING when building the PDF manual. It is caused
by the local LaTeX installation; the manual builds correctly on win-builder.

## Reverse dependencies

There are currently no reverse dependencies on CRAN.
