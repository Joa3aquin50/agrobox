#' Boxplot con anotaciones post-hoc estadisticas para experimentos agronomicos
#' / Boxplot with Statistical Post-hoc Annotations for Agronomic Experiments
#'
#' Genera boxplots con puntos jitter, etiquetas de medias y letras de comparación
#' multiple para experimentos agronómicos. Soporta facetas con hasta dos variables
#' de agrupación y selecciona automáticamente el método estadístico en función de
#' la normalidad (Shapiro-Wilk) y la homogeneidad de varianzas (Fligner-Killeen).
#'
#' Generates boxplots with jittered points, mean labels, and post-hoc letter
#' annotations for agronomic experiments. Supports faceting by up to two grouping
#' variables and automatic selection of the statistical method based on normality
#' (Shapiro-Wilk) and homoscedasticity (Fligner-Killeen) tests.
#'
#' @details
#' Objetivo: colocar letras de comparación múltiple mediante una ruta
#' estadística viable y defendible. Para cada panel (cluster) la función:
#'
#' Goal: place multiple-comparison letters through a viable, defensible
#' statistical route. For each facet panel (cluster) the function:
#'
#' \enumerate{
#'   \item Verifica datos suficientes (>= 2 tratamientos, >= 3 observaciones
#'         por tratamiento, datos no constantes). Si no, solo muestra medias.
#'         / Checks for sufficient data; otherwise only means are shown.
#'   \item Evalúa normalidad de residuos (Shapiro-Wilk) y homogeneidad de
#'         varianzas (Fligner-Killeen). / Tests residual normality and
#'         homoscedasticity.
#'   \item Elige la ruta / Chooses the route:
#'     \itemize{
#'       \item \strong{A} normal + homogéneo: ANOVA (con bloque y
#'             \code{factor2} si se indican) + Duncan o Tukey.
#'       \item \strong{B} normal + heterogéneo: ANOVA de Welch + Games-Howell
#'             (bloque y \code{factor2} no se consideran).
#'       \item \strong{C} no normal + homogéneo: Friedman si hay bloque
#'             (DBCA completo; réplicas promediadas, bloques incompletos
#'             excluidos; letras protegidas por el Friedman global), si no
#'             Kruskal-Wallis con letras de \code{agricolae::kruskal}
#'             (\code{np_test = "kruskal"}) o Dunn (\code{np_test = "dunn"}).
#'       \item \strong{D} no normal + heterogéneo: mismas pruebas que C,
#'             con una nota de varianzas heterogéneas.
#'     }
#'   \item Si el post-hoc de la ruta falla, prueba la siguiente ruta viable
#'         (A/B -> no paramétrica; Friedman -> Kruskal; Kruskal <-> Dunn).
#'         / If the post-hoc fails, the next viable route is tried.
#' }
#'
#' El gráfico incluye una nota (caption) con la ruta usada en cada panel,
#' por qué, ventajas, desventajas y alcance; puede quitarse con
#' \code{+ ggplot2::labs(caption = NULL)}. CV y Power se calculan siempre a
#' partir del ANOVA clásico.
#'
#' The plot includes a caption describing the route used in each panel, why,
#' advantages, disadvantages and scope. CV and Power are always computed from
#' the classic ANOVA fit.
#'
#' Los argumentos \code{orden_factor}, \code{grupo1_orden} y \code{grupo2_orden}
#' aceptan vectores nombrados o no:
#'
#' The \code{orden_factor}, \code{grupo1_orden}, and \code{grupo2_orden}
#' arguments accept either unnamed or named vectors:
#'
#' \itemize{
#'   \item Vector sin nombre: solo cambia el orden.
#'   \item Vector nombrado: cambia orden y etiquetas.
#' }
#'
#' \itemize{
#'   \item Unnamed vector: order only.
#'   \item Named vector: order + relabeling.
#' }
#'
#' Para vectores nombrados, el nombre corresponde al nivel original y el valor
#' es la etiqueta mostrada. Elementos sin nombre conservan su valor original.
#'
#' For named vectors, names correspond to original levels and values to display
#' labels. Elements without a name keep their original value.
#'
#' Si algún tratamiento tiene todas sus observaciones como NA dentro de un cluster,
#' la función se detiene mostrando un error informativo.
#'
#' If any treatment has all observations as NA within any cluster, the function
#' stops with an informative error.
#'
#' @param data Data frame con los datos experimentales /
#'   A data frame containing the experimental data.
#'
#' @param test Método post-hoc: \code{"Duncan"} (default) o \code{"Tukey"} /
#'   Post-hoc test: \code{"Duncan"} (default) or \code{"Tukey"}.
#'
#' @param var.equal Obsoleto desde v0.4.0: se mantiene por compatibilidad
#'   pero ya no cambia el resultado; la ruta se elige automáticamente /
#'   Deprecated since v0.4.0: kept for backward compatibility, ignored.
#'
#' @param np_test Post-hoc no paramétrico sin bloque: \code{"kruskal"}
#'   (default, \code{agricolae::kruskal}) o \code{"dunn"} (Dunn, \code{rstatix}) /
#'   Non-parametric post-hoc when there is no block.
#'
#' @param p.adj Ajuste de p-valores para Kruskal y Dunn (default
#'   \code{"bonferroni"}; también \code{"holm"}, \code{"hochberg"},
#'   \code{"BH"}, \code{"BY"}, \code{"fdr"}, \code{"none"}) /
#'   P-value adjustment for Kruskal and Dunn comparisons.
#'
#' @param factor Nombre de la variable categórica principal /
#'   Name of the main categorical variable (treatment).
#'
#' @param factor2 Segundo factor opcional para interacción /
#'   Optional second factor for interaction.
#'
#' @param orden_factor Vector para ordenar/renombrar tratamientos /
#'   Vector to reorder/relabel factor levels.
#'
#' @param grupo1_orden Igual que orden_factor pero para filas de facetas /
#'   Same as orden_factor for row facets.
#'
#' @param grupo2_orden Igual que orden_factor pero para columnas de facetas /
#'   Same as orden_factor for column facets.
#'
#' @param bloque Variable de bloque opcional /
#'   Optional blocking variable.
#'
#' @param variable Variable numérica de respuesta /
#'   Numeric response variable.
#'
#' @param titulo Etiqueta del eje Y /
#'   Y-axis label.
#'
#' @param estructura Especificación de facetas ("fila~columna") /
#'   Faceting structure ("row~col").
#'
#' @param lim_sup Límite superior del eje Y /
#'   Upper y-axis limit.
#'
#' @param lim_inf Límite inferior del eje Y /
#'   Lower y-axis limit.
#'
#' @param colores Vector de colores /
#'   Color vector for factor levels.
#'
#' @return Lista con cinco elementos /
#'   A list with five elements:
#' \describe{
#'   \item{\code{plot}}{Objeto ggplot2 con boxplots y anotaciones /
#'     ggplot2 object with boxplots and annotations}
#'   \item{\code{tabla}}{Tabla resumen con medias, letras, ANOVA, CV y Power /
#'     Summary table with means, letters, ANOVA, CV and Power}
#'   \item{\code{levels}}{Niveles del factor mostrados /
#'     Displayed factor levels}
#'   \item{\code{data}}{Datos procesados usados en el análisis /
#'     Processed data used in the analysis}
#'   \item{\code{stats}}{Diagnósticos por panel: \code{anova_p},
#'     \code{shapiro_p}, \code{fligner_p}, \code{CV}, \code{Power},
#'     \code{ruta} (A/B/C/D, o 0 si datos insuficientes), \code{metodo},
#'     \code{p_ruta} (p de la prueba global usada) y \code{nota} /
#'     Per-panel diagnostics including route, method, global p and notes}
#' }
#'
#' #'
#' @importFrom dplyr as_tibble filter mutate group_by summarise ungroup
#'   distinct left_join bind_rows select pull across all_of any_of
#'   relocate slice case_when if_else n_distinct recode %>%
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_text labs
#'   theme_bw theme element_text element_blank scale_color_manual
#'   scale_y_continuous facet_grid coord_cartesian
#' @importFrom stringr str_split_fixed str_trim str_split
#' @importFrom tidyr separate pivot_wider unite
#' @importFrom rlang sym set_names .data
#' @importFrom stats reformulate lm aov shapiro.test fligner.test
#'   df.residual deviance setNames sd
#' @importFrom grDevices hcl.colors
#' @importFrom utils tail head
#' @importFrom agricolae HSD.test duncan.test
#' @importFrom pwr pwr.anova.test
#' @importFrom rstatix games_howell_test
#' @importFrom multcompView multcompLetters
#'
#' @examples
#' library(dplyr)
#'
#' # Example 1: Single panel, classic ANOVA with Duncan test
#' set.seed(42)
#' df1 <- data.frame(
#'   trat = rep(c("T0", "T1", "T2", "T3"), each = 5),
#'   resp = c(
#'     rnorm(5, 10, 1),
#'     rnorm(5, 14, 1),
#'     rnorm(5, 18, 1),
#'     rnorm(5, 22, 1)
#'   )
#' )
#'
#' result1 <- agrobox(
#'   data         = df1,
#'   factor       = "trat",
#'   variable     = "resp",
#'   titulo       = "Response variable",
#'   orden_factor = c("T0" = "Control", "T1" = "Low",
#'                    "T2" = "Medium",  "T3" = "High"),
#'   var.equal    = TRUE,
#'   test         = "Duncan"
#' )
#' result1$plot
#' result1$tabla
#'
#' # Example 2: One grouping variable (column facets), Welch + Games-Howell
#' set.seed(99)
#' df2 <- data.frame(
#'   trat  = rep(c("A", "B", "C", "D"), each = 12),
#'   epoca = rep(rep(c("Dry", "Rainy", "Transition"), each = 4), 4),
#'   resp  = c(
#'     rnorm(12, 5,  0.3),
#'     rnorm(12, 9,  3.5),
#'     rnorm(12, 7,  2.0),
#'     rnorm(12, 12, 4.8)
#'   )
#' )
#'
#' result2 <- agrobox(
#'   data         = df2,
#'   factor       = "trat",
#'   variable     = "resp",
#'   estructura   = "~epoca",
#'   titulo       = "Response by season",
#'   grupo2_orden = c("Dry", "Transition", "Rainy"),
#'   orden_factor = c("A" = "Control",     "B" = "Treatment 1",
#'                    "C" = "Treatment 2", "D" = "Treatment 3"),
#'   var.equal    = FALSE,
#'   colores      = c("gray40", "steelblue", "orange", "red3")
#' )
#' result2$plot
#' result2$tabla
#'
#' # Example 3: Two grouping variables (row x column facets), with NAs
#' set.seed(7)
#' df3 <- expand.grid(
#'   trat  = c("T1", "T2", "T3"),
#'   suelo = c("Clay", "Sand"),
#'   riego = c("Drip", "Sprinkler", "Rainfed"),
#'   rep   = 1:5
#' )
#' df3$yield <- with(df3, {
#'   base  <- c(T1 = 8,    T2 = 14,   T3 = 20)[as.character(trat)]
#'   s     <- c(Clay = 0,  Sand = 3)[as.character(suelo)]
#'   r     <- c(Drip = 2,  Sprinkler = 0, Rainfed = -4)[as.character(riego)]
#'   rnorm(nrow(df3), base + s + r, 2)
#' })
#' set.seed(15)
#' df3$yield[sample(nrow(df3), size = round(nrow(df3) * 0.08))] <- NA
#'
#' result3 <- agrobox(
#'   data         = df3,
#'   factor       = "trat",
#'   variable     = "yield",
#'   estructura   = "suelo~riego",
#'   titulo       = "Yield (t/ha)",
#'   orden_factor = c("T1" = "Variety 1", "T2" = "Variety 2",
#'                    "T3" = "Variety 3"),
#'   grupo1_orden = c("Clay" = "Clay soil", "Sand" = "Sandy soil"),
#'   grupo2_orden = c("Rainfed", "Drip", "Sprinkler"),
#'   var.equal    = FALSE,
#'   colores      = c("darkgreen", "royalblue", "firebrick")
#' )
#' result3$plot
#' result3$tabla
#' result3$data
#'
#' # Example 4: Real Data nitrogeno_liberacion
#' data(nitrogeno_liberacion)
#'
#' agrobox(
#' data = nitrogeno_liberacion,
#' test = "Tukey",
#' factor = "trata",
#' variable = "nh4_mg_lt",
#' orden_factor = c( "T1" = "Fertilizante (14-4-4)",
#'                   "T2" = "Pellet (14-4-4)",
#'                   "T3" = "Fertilizante (7-7-19)",
#'                   "T4" = "Pellet (7-7-19)"),
#' grupo1_orden = c("14.4.4" = "Ley 14 N - 4 P2O5 - 4 K2O",
#'                  "7.7.19" = "Ley 7 N - 7 P2O5 - 19 K2O"),
#' grupo2_orden = c("0" = "Día 0",
#'                  "1" = "Día 1",
#'                  "8" = "Día 8",
#'                  "16" = "Día 16",
#'                  "41" = "Día 41"),
#' estructura = "formu~dias",
#' colores = c("purple3", "green4","black", "red3")
#' )
#'
#'
#' # Example 5: Real Data pimiento_hibridacion
#' \donttest{
#'
#' data(pimiento_hibridacion)
#'
#'
#' agrobox(
#' data = pimiento_hibridacion,
#' test = "Tukey",
#' factor = "trata",
#' bloque = "bloque",
#' variable = "g_pla",
#' titulo = "Rendimiento (g/pla)",
#' orden_factor = c( "T1" = "5",
#'                   "T2" = "4",
#'                   "T3" = "3"),
#' grupo2_orden = c("1" = "Día 0",
#'                  "4"= "Día 4",
#'                  "8" = "Día 8",
#'                  "12" = "Día 12",
#'                  "16" = "Día 16",
#'                  "20" = "Día 20"),
#' estructura = "~dia",
#' colores = c("purple3", "green4","black", "red3")
#' )$plot +
#'   ggplot2::labs(col = "Semanas de hibridación")
#' }
#'
#'
#' @export
agrobox <- function(data,
                    test      = c("Duncan", "Tukey"),
                    var.equal = TRUE,
                    np_test   = c("kruskal", "dunn"),
                    p.adj     = "bonferroni",
                    factor,
                    factor2   = NULL,
                    orden_factor  = NULL,
                    grupo1_orden  = NULL,
                    grupo2_orden  = NULL,
                    bloque    = NULL,
                    variable,
                    titulo    = NULL,
                    estructura = NULL,
                    lim_sup   = NULL,
                    lim_inf   = NULL,
                    colores   = NULL) {

  # =========================================================================
  # agrobox()
  #
  # Generates boxplots with statistical post-hoc letter annotations for
  # agronomic experiments. Supports faceting by up to two grouping variables,
  # factor reordering and relabeling, and automatic selection between ANOVA
  # (Duncan / Tukey) and Welch + Games-Howell depending on normality and
  # homoscedasticity tests.
  #
  # Returns a list with:
  #   $plot   - ggplot2 object
  #   $tabla  - summary table (tibble) with means, letters, ANOVA sig, CV, Power
  #   $levels - character vector of factor levels as used in the plot
  #   $data   - summary of the processed data used for analysis
  # =========================================================================

  test    <- match.arg(test)
  np_test <- match.arg(np_test)
  p.adj   <- match.arg(p.adj, c("bonferroni", "holm", "hochberg", "BH",
                                "BY", "fdr", "none"))

  # var.equal is kept for backward compatibility only. Since v0.4.0 the
  # statistical route (A/B/C/D) is chosen automatically from the Shapiro-Wilk
  # and Fligner-Killeen diagnostics, so this argument no longer changes results.
  if (!missing(var.equal)) {
    message("agrobox: 'var.equal' ya no se usa desde v0.4.0; la ruta estadistica ",
            "se elige automaticamente (Shapiro + Fligner). / 'var.equal' is ",
            "ignored since v0.4.0.")
  }

  # -------------------------------------------------------------------------
  # HELPER: apply order and optional relabeling to a factor column.
  #
  # orden_vec can be:
  #   - NULL              : convert to factor with alphabetical levels
  #   - unnamed vector    : c("T2", "T0", "T1")  -> order only
  #   - named vector      : c("T2" = "Medio", "T0" = "Testigo", "T1")
  #                         names = original levels, values = display labels
  #                         elements without a name keep their original value
  # -------------------------------------------------------------------------
  aplicar_orden_labels <- function(x, orden_vec) {

    x_orig <- x
    x <- as.character(x)

    if (is.null(orden_vec)) {
      # Keep a meaningful default order: numeric values in numeric order
      # (0, 50, 100 instead of 0, 100, 50) and factors in their own level order
      if (is.numeric(x_orig))
        return(factor(x, levels = as.character(sort(unique(x_orig)))))
      if (is.factor(x_orig))
        return(factor(x, levels = intersect(levels(x_orig), unique(x))))
      return(factor(x))
    }

    # Normalize: fill empty names with the element value itself
    # so every element follows the pattern  original_level = display_label
    nms <- names(orden_vec)
    if (is.null(nms)) {
      names(orden_vec) <- orden_vec
    } else {
      faltantes <- nms == ""
      names(orden_vec)[faltantes] <- orden_vec[faltantes]
    }

    niveles_originales <- names(orden_vec)
    labels_deseados    <- unname(orden_vec)

    # Levels present in the data but not listed are excluded: say so
    omitidos <- setdiff(unique(stats::na.omit(x)), niveles_originales)
    if (length(omitidos) > 0)
      message("agrobox: niveles no incluidos en el vector de orden (se excluyen ",
              "del analisis): ", paste(omitidos, collapse = ", "))

    factor(x, levels = niveles_originales, labels = labels_deseados)
  }

  # -------------------------------------------------------------------------
  # Coerce data to tibble and apply factor ordering / labeling
  # -------------------------------------------------------------------------
  df <- dplyr::as_tibble(data)
  df[[factor]] <- aplicar_orden_labels(df[[factor]], orden_factor)

  # -------------------------------------------------------------------------
  # Parse the 'estructura' string into grupe1 (rows) and grupe2 (columns)
  # Format expected: "row_var~col_var", "~col_var", or "row_var~"
  # -------------------------------------------------------------------------
  grupe1 <- ""
  grupe2 <- ""
  if (!is.null(estructura) && nzchar(estructura)) {
    parts  <- stringr::str_split_fixed(estructura, "~", 2)
    grupe1 <- stringr::str_trim(parts[, 1])
    grupe2 <- stringr::str_trim(parts[, 2])
  }

  # Helper: TRUE when a string is non-empty and non-NA
  has_name <- function(x) nzchar(x) && !is.na(x)

  # -------------------------------------------------------------------------
  # Pre-check on raw data: if any treatment has ALL observations as NA within
  # any cluster combination, stop before any processing begins.
  # The check runs on the original data before NA rows are dropped.
  # -------------------------------------------------------------------------
  df_check <- dplyr::as_tibble(data)

  check_groups <- c(
    factor,
    if (has_name(grupe1) && grupe1 %in% names(df_check)) grupe1 else NULL,
    if (has_name(grupe2) && grupe2 %in% names(df_check)) grupe2 else NULL
  )

  conteos_check <- df_check %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(check_groups))) %>%
    dplyr::summarise(
      n_validos = sum(!is.na(.data[[variable]])),
      .groups   = "drop"
    )

  vacios_check <- conteos_check %>%
    dplyr::filter(n_validos == 0)

  if (nrow(vacios_check) > 0) {
    detalle <- vacios_check %>%
      tidyr::unite("combinacion", dplyr::all_of(check_groups), sep = " | ") %>%
      dplyr::pull(combinacion)

    stop(
      "Analysis cannot proceed: the following treatment x group combinations ",
      "have no valid observations (all NA):\n  ",
      paste(detalle, collapse = "\n  "),
      "\nPlease remove or impute these rows before calling agrobox()."
    )
  }

  # -------------------------------------------------------------------------
  # Build the 'cluster' column that identifies each facet panel.
  # The cluster drives the loop over ANOVA groups.
  #
  # Cases:
  #   1. grupe1 + grupe2 -> interaction(grupe1, grupe2, sep = "_")
  #   2. grupe1 only     -> cluster = grupe1
  #   3. grupe2 only     -> cluster = grupe2
  #   4. no groups       -> cluster = "A" (single panel)
  # -------------------------------------------------------------------------
  make_cluster_col <- function(df) {

    if (has_name(grupe1) && grupe1 %in% names(df)) {
      df[[grupe1]] <- aplicar_orden_labels(df[[grupe1]], grupo1_orden)
    }

    if (has_name(grupe2) && grupe2 %in% names(df)) {
      df[[grupe2]] <- aplicar_orden_labels(df[[grupe2]], grupo2_orden)
    }

    df2 <- df

    if (has_name(grupe1) && has_name(grupe2) &&
        all(c(grupe1, grupe2) %in% names(df2))) {

      # Case 1: both grouping variables present
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]]),
                      !is.na(.data[[grupe1]]),
                      !is.na(.data[[grupe2]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          cluster = interaction(.data[[grupe1]], .data[[grupe2]],
                                sep = "_", drop = TRUE)
        )

    } else if (has_name(grupe1) && grupe1 %in% names(df2)) {

      # Case 2: only grupe1
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]]),
                      !is.na(.data[[grupe1]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          cluster = factor(.data[[grupe1]],
                           levels = levels(.data[[grupe1]]))
        )

    } else if (has_name(grupe2) && grupe2 %in% names(df2)) {

      # Case 3: only grupe2
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]]),
                      !is.na(.data[[grupe2]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          cluster = factor(.data[[grupe2]],
                           levels = levels(.data[[grupe2]]))
        )

    } else {

      # Case 4: no grouping - single panel
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cluster = factor("A", levels = "A"))
    }

    return(df2)
  }

  data2 <- make_cluster_col(df)

  # -------------------------------------------------------------------------
  # Compact letter display from pairwise p-values (shared by Games-Howell
  # and Dunn).
  #
  # multcompLetters() returns letter strings such as "a", "ab", "b". They are
  # relabelled LETTER BY LETTER (never the whole string) so that the group
  # with the highest mean receives "a", following the agricolae convention.
  # (v0.3.0 relabelled whole strings, which turned "ab" into a new letter and
  # produced wrong groupings.)
  #
  # Returns NULL if any p-value is NA or any treatment is missing a letter,
  # so the caller can move on to the next viable route.
  # -------------------------------------------------------------------------
  letras_desde_pvalores <- function(pvals, datis2, factor_name, variable_name) {

    if (is.null(pvals) || nrow(pvals) == 0) return(NULL)
    if (any(is.na(pvals$p.adj))) return(NULL)

    grupos <- unique(c(as.character(pvals$group1), as.character(pvals$group2)))

    # Full symmetric p-value matrix
    mat_full <- matrix(1,
                       nrow     = length(grupos),
                       ncol     = length(grupos),
                       dimnames = list(grupos, grupos))
    for (i in seq_len(nrow(pvals))) {
      g1i <- as.character(pvals$group1[i])
      g2i <- as.character(pvals$group2[i])
      mat_full[g1i, g2i] <- pvals$p.adj[i]
      mat_full[g2i, g1i] <- pvals$p.adj[i]
    }

    letras_vec <- tryCatch(
      multcompView::multcompLetters(mat_full < 0.05)$Letters,
      error = function(e) NULL
    )
    if (is.null(letras_vec)) return(NULL)

    means <- datis2 %>%
      dplyr::group_by(.data[[factor_name]]) %>%
      dplyr::summarise(
        medias = mean(.data[[variable_name]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(!!factor_name := as.character(.data[[factor_name]])) %>%
      dplyr::arrange(dplyr::desc(medias))

    # Every treatment must have a letter; otherwise the display is incomplete
    if (length(setdiff(means[[factor_name]], names(letras_vec))) > 0) return(NULL)

    # Relabel letter by letter, in order of first appearance from the
    # highest mean downwards
    orden  <- means[[factor_name]]
    vistos <- character(0)
    for (g in orden) {
      for (ch in strsplit(letras_vec[[g]], "")[[1]]) {
        if (!ch %in% vistos) vistos <- c(vistos, ch)
      }
    }
    mapa <- stats::setNames(letters[seq_along(vistos)], vistos)

    letras_final <- vapply(orden, function(g) {
      paste(sort(unname(mapa[strsplit(letras_vec[[g]], "")[[1]]])), collapse = "")
    }, character(1))

    means %>%
      dplyr::mutate(groups = unname(letras_final)) %>%
      dplyr::select(!!rlang::sym(factor_name), medias, groups)
  }

  # -------------------------------------------------------------------------
  # Games-Howell post-hoc (route B). Always uses y ~ factor: Games-Howell
  # cannot include block or factor2 terms.
  # -------------------------------------------------------------------------
  games_howell_letras <- function(datis2, factor_name, variable_name) {
    df_games <- tryCatch(
      rstatix::games_howell_test(
        datis2, stats::reformulate(factor_name, response = variable_name)),
      error = function(e) NULL
    )
    if (is.null(df_games)) return(NULL)
    letras_desde_pvalores(dplyr::select(df_games, group1, group2, p.adj),
                          datis2, factor_name, variable_name)
  }

  # -------------------------------------------------------------------------
  # Dunn post-hoc (routes C/D when np_test = "dunn")
  # -------------------------------------------------------------------------
  dunn_letras <- function(datis2, factor_name, variable_name, p_adj) {
    df_dunn <- tryCatch(
      rstatix::dunn_test(
        datis2, stats::reformulate(factor_name, response = variable_name),
        p.adjust.method = p_adj),
      error = function(e) NULL
    )
    if (is.null(df_dunn)) return(NULL)
    letras_desde_pvalores(dplyr::select(df_dunn, group1, group2, p.adj),
                          datis2, factor_name, variable_name)
  }

  # -------------------------------------------------------------------------
  # Join agricolae letters (kruskal / friedman) onto the means table.
  # agricolae returns ranks in its first column, so the displayed values are
  # always the arithmetic means from means_tbl.
  # -------------------------------------------------------------------------
  unir_letras_agricolae <- function(ph, means_tbl, factor_name) {
    if (is.null(ph) || is.null(ph$groups)) return(NULL)
    gdf <- as.data.frame(ph$groups)
    if (!"groups" %in% names(gdf)) names(gdf)[ncol(gdf)] <- "groups"
    letras <- stats::setNames(trimws(as.character(gdf$groups)),
                              trimws(rownames(gdf)))
    out <- means_tbl %>%
      dplyr::mutate(groups = unname(letras[.data[[factor_name]]])) %>%
      dplyr::select(!!rlang::sym(factor_name), medias, groups)
    if (any(is.na(out$groups))) return(NULL)
    out
  }

  # -------------------------------------------------------------------------
  # Core statistical analysis for one cluster (facet panel).
  #
  # Goal: place post-hoc letters through a viable, defensible route.
  #
  #   1. Sufficient data?  (>= 2 treatments, >= 3 obs per treatment,
  #                         non-constant data)  -> otherwise means only
  #   2. Shapiro-Wilk on model residuals         -> normal / not normal
  #   3. Fligner-Killeen (y ~ factor)            -> homogeneous / heterogeneous
  #   4. Route:
  #        A  normal + homogeneous    : ANOVA + Duncan / Tukey
  #                                     (model includes block and factor2)
  #        B  normal + heterogeneous  : Welch ANOVA + Games-Howell
  #                                     (block / factor2 not considered)
  #        C  not normal + homogeneous: Friedman if block (complete RCBD),
  #                                     otherwise Kruskal-Wallis (np_test)
  #        D  not normal + heterog.   : same tests as C, flagged with a note
  #   5. Fallback: if the post-hoc of the chosen route fails (error / NA),
  #      the next viable route is tried: A/B -> C/D, Friedman -> Kruskal,
  #      Kruskal <-> Dunn. Means without letters only as a last resort,
  #      always with an explicit note.
  #
  # CV and Power are always computed from the classic ANOVA fit.
  # -------------------------------------------------------------------------
  run_anova_for_group <- function(datis, formula_term, factor_name,
                                  variable_name, test_method) {

    res <- list(oti       = NULL,
                cv        = NA_real_,
                power     = NA_real_,
                shapiro_p = NA_real_,
                fligner_p = NA_real_,
                anova_p   = NA_real_,
                p_ruta    = NA_real_,
                ruta      = NA_character_,
                metodo    = NA_character_,
                nota      = NA_character_)

    add_nota <- function(res, txt) {
      res$nota <- if (is.na(res$nota)) txt else paste0(res$nota, "; ", txt)
      res
    }

    tryCatch({

      # Count valid observations per treatment
      qq    <- ifelse(is.na(datis[[variable_name]]), 0L, 1L)
      datis2 <- datis %>%
        dplyr::mutate(qq = qq) %>%
        dplyr::group_by(cluster, .data[[factor_name]]) %>%
        dplyr::mutate(n = sum(qq, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(sum_n = min(n, na.rm = TRUE))

      # Means table (always computed regardless of route)
      means_tbl <- datis2 %>%
        dplyr::group_by(.data[[factor_name]]) %>%
        dplyr::summarise(
          medias = mean(.data[[variable_name]], na.rm = TRUE),
          n      = sum(!is.na(.data[[variable_name]])),
          .groups = "drop"
        ) %>%
        dplyr::mutate(!!factor_name := as.character(.data[[factor_name]]))

      # ---- Step 1: sufficient data ----------------------------------------
      cond_valid <- all(!is.infinite(datis2$sum_n)) &&
        all(!is.na(datis2$sum_n))                   &&
        all(datis2$sum_n >= 3)                       &&
        suppressWarnings(
          max(abs(datis2[[variable_name]]), na.rm = TRUE) > 0
        )                                            &&
        length(unique(datis2[[factor_name]])) >= 2

      if (!cond_valid) {
        warning("ANOVA skipped for cluster '", unique(datis$cluster),
                "': insufficient data.", call. = FALSE)
        res$shapiro_p <- tryCatch(
          stats::shapiro.test(
            stats::residuals(
              stats::lm(
                stats::reformulate(factor_name, response = variable_name),
                datis2
              )
            )
          )$p.value,
          error = function(e) NA_real_
        )
        res$fligner_p <- tryCatch(
          stats::fligner.test(
            stats::reformulate(factor_name, response = variable_name),
            data = datis2
          )$p.value,
          error = function(e) NA_real_
        )
        res$ruta   <- "0"
        res$metodo <- "-"
        res <- add_nota(res, "datos insuficientes (se requieren >= 2 tratamientos y >= 3 obs. por tratamiento)")
        res$oti <- dplyr::mutate(means_tbl, groups = NA_character_)
        return(res)
      }

      # Fit model and run diagnostic tests
      lm_fit  <- stats::lm(formula_term, datis2)
      aov_fit <- stats::aov(lm_fit)

      ss_p    <- tryCatch(
        stats::shapiro.test(stats::residuals(aov_fit))$p.value,
        error = function(e) NA_real_
      )
      ff_p    <- tryCatch(
        stats::fligner.test(
          stats::reformulate(factor_name, response = variable_name),
          data = datis2
        )$p.value,
        error = function(e) NA_real_
      )
      anova_p <- tryCatch(
        summary(aov_fit)[[1]][["Pr(>F)"]][1],
        error = function(e) NA_real_
      )

      res$shapiro_p <- ss_p
      res$fligner_p <- ff_p
      res$anova_p   <- anova_p

      # Helper: extract post-hoc groups from agricolae output (Duncan / Tukey)
      extract_ph_groups <- function(ph, factor_name, means_tbl) {
        if (!is.null(ph) && !is.null(ph$groups)) {
          gdf <- as.data.frame(ph$groups)
          names(gdf)[1] <- "medias"
          if (!"groups" %in% names(gdf)) names(gdf)[ncol(gdf)] <- "groups"
          gdf %>%
            dplyr::mutate(!!factor_name := rownames(gdf)) %>%
            dplyr::select(!!rlang::sym(factor_name), medias, groups) %>%
            dplyr::mutate(!!factor_name := as.character(.data[[factor_name]]))
        } else {
          dplyr::mutate(means_tbl, groups = NA_character_)
        }
      }

      # Helper: compute CV and power from ANOVA fit
      compute_cv_power <- function(aov_fit, datis2, variable_name, factor_name) {
        df_res      <- stats::df.residual(aov_fit)
        MSerror     <- stats::deviance(aov_fit) / df_res
        # abs(): CV is defined on the magnitude of the mean (negative responses)
        cv_val      <- sqrt(MSerror) /
          abs(mean(datis2[[variable_name]], na.rm = TRUE)) * 100

        # Partial eta^2 = SS_factor / (SS_factor + SS_residual).
        # Identical to the classic eta^2 in a one-way model; with block or
        # factor2 it no longer counts their SS against the treatment effect.
        ss_table    <- summary(aov_fit)[[1]]
        ss_res      <- utils::tail(ss_table$`Sum Sq`, 1)
        eta2        <- ss_table$`Sum Sq`[1] /
          (ss_table$`Sum Sq`[1] + ss_res)
        effect_size <- sqrt(eta2 / (1 - eta2))
        k           <- length(unique(datis2[[factor_name]]))
        n_per_group <- nrow(datis2) / k

        pwr_res <- tryCatch(
          pwr::pwr.anova.test(k         = k,
                              n         = n_per_group,
                              f         = effect_size,
                              sig.level = 0.05),
          error = function(e) NULL
        )
        power_val <- if (!is.null(pwr_res)) pwr_res$power else NA_real_
        list(cv = cv_val, power = power_val)
      }

      # CV and Power: always reported (from the classic ANOVA fit)
      stats_out <- tryCatch(
        compute_cv_power(aov_fit, datis2, variable_name, factor_name),
        error = function(e) list(cv = NA_real_, power = NA_real_)
      )
      res$cv    <- stats_out$cv
      res$power <- stats_out$power

      has_bloque  <- !is.null(bloque)  && bloque  %in% names(datis2)
      has_factor2 <- !is.null(factor2) && factor2 %in% names(datis2)

      # ---- Steps 2-3: diagnostics define the route -------------------------
      # NA in a diagnostic is treated as "assumption not met" (safer route).
      es_normal   <- !is.na(ss_p) && ss_p > 0.05
      es_homog    <- !is.na(ff_p) && ff_p > 0.05
      ruta_diag   <- if (es_normal && es_homog) "A" else
        if (es_normal)             "B" else
          if (es_homog)              "C" else "D"

      # ---- Route intentos ---------------------------------------------------
      intento_A <- function() {
        ph <- tryCatch(
          if (test_method == "Tukey") {
            agricolae::HSD.test(lm_fit, factor_name, group = TRUE)
          } else {
            agricolae::duncan.test(lm_fit, factor_name, group = TRUE)
          },
          error = function(e) NULL
        )
        if (is.null(ph) || is.null(ph$groups)) return(NULL)
        oti <- extract_ph_groups(ph, factor_name, means_tbl)
        if (any(is.na(oti$groups))) return(NULL)
        list(oti = oti, p = anova_p,
             metodo = paste0("ANOVA + ", test_method), nota = NA_character_)
      }

      intento_B <- function() {
        oti <- games_howell_letras(datis2, factor_name, variable_name)
        if (is.null(oti)) return(NULL)
        welch_p <- tryCatch(
          stats::oneway.test(
            stats::reformulate(factor_name, response = variable_name),
            data = datis2, var.equal = FALSE)$p.value,
          error = function(e) NA_real_
        )
        nt <- c(if (has_bloque)  "bloque no considerado en Games-Howell",
                if (has_factor2) "factor2 no considerado en Games-Howell")
        list(oti = oti, p = welch_p, metodo = "Welch + Games-Howell",
             nota = if (length(nt)) paste(nt, collapse = "; ") else NA_character_)
      }

      intento_friedman <- function() {
        if (!has_bloque) return(NULL)
        # One value per treatment x block (replicates / factor2 averaged)
        agg <- datis2 %>%
          dplyr::group_by(.data[[bloque]], .data[[factor_name]]) %>%
          dplyr::summarise(y_ = mean(.data[[variable_name]], na.rm = TRUE),
                           .groups = "drop") %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(c(bloque, factor_name)),
                                      as.character))
        k_trat <- length(unique(agg[[factor_name]]))
        completos <- agg %>%
          dplyr::group_by(.data[[bloque]]) %>%
          dplyr::summarise(nt = dplyr::n_distinct(.data[[factor_name]]),
                           .groups = "drop") %>%
          dplyr::filter(nt == k_trat) %>%
          dplyr::pull(1)
        n_excl <- length(unique(agg[[bloque]])) - length(completos)
        agg <- dplyr::filter(agg, .data[[bloque]] %in% completos)
        if (length(completos) < 2) return(NULL)

        ph <- tryCatch(
          agricolae::friedman(agg[[bloque]], agg[[factor_name]], agg$y_,
                              group = TRUE),
          error = function(e) NULL
        )
        oti <- unir_letras_agricolae(ph, means_tbl, factor_name)
        if (is.null(oti)) return(NULL)
        # agricolae::friedman() has no p-value adjustment for its pairwise
        # comparisons, so they are protected Fisher-style: if the global
        # Friedman test is not significant, all treatments share "a".
        fr_p <- ph$statistics$p.chisq
        protegido <- !is.na(fr_p) && fr_p > 0.05
        if (protegido) oti$groups <- "a"
        nt <- c(if (protegido)
          "Friedman global no significativo: sin diferencias entre tratamientos",
          if (any(duplicated(datis2[, c(bloque, factor_name)])))
            "replicas por tratamiento x bloque promediadas",
          if (n_excl > 0) paste0(n_excl, " bloque(s) incompleto(s) excluido(s)"),
          if (has_factor2) "factor2 no considerado en Friedman")
        list(oti = oti, p = ph$statistics$p.chisq, metodo = "Friedman",
             nota = if (length(nt)) paste(nt, collapse = "; ") else NA_character_)
      }

      kw_p <- function() tryCatch(
        stats::kruskal.test(
          stats::reformulate(factor_name, response = variable_name),
          data = datis2)$p.value,
        error = function(e) NA_real_
      )

      intento_kruskal <- function() {
        ph <- tryCatch(
          agricolae::kruskal(datis2[[variable_name]],
                             as.character(datis2[[factor_name]]),
                             p.adj = p.adj, group = TRUE),
          error = function(e) NULL
        )
        oti <- unir_letras_agricolae(ph, means_tbl, factor_name)
        if (is.null(oti)) return(NULL)
        nt <- c(if (has_bloque)  "bloque no considerado en Kruskal-Wallis",
                if (has_factor2) "factor2 no considerado en Kruskal-Wallis")
        list(oti = oti, p = kw_p(), metodo = "Kruskal-Wallis",
             nota = if (length(nt)) paste(nt, collapse = "; ") else NA_character_)
      }

      intento_dunn <- function() {
        oti <- dunn_letras(datis2, factor_name, variable_name, p.adj)
        if (is.null(oti)) return(NULL)
        nt <- c(if (has_bloque)  "bloque no considerado en Kruskal-Wallis/Dunn",
                if (has_factor2) "factor2 no considerado en Kruskal-Wallis/Dunn")
        list(oti = oti, p = kw_p(), metodo = "Kruskal-Wallis + Dunn",
             nota = if (length(nt)) paste(nt, collapse = "; ") else NA_character_)
      }

      # Non-parametric chain: Friedman (if block) -> chosen np_test -> the other
      np_cadena <- c(if (has_bloque) "friedman",
                     if (np_test == "dunn") c("dunn", "kruskal") else c("kruskal", "dunn"))

      cadena <- switch(ruta_diag,
                       A = c("A", np_cadena),
                       B = c("B", np_cadena),
                       C = np_cadena,
                       D = np_cadena)

      intentos <- list(A = intento_A, B = intento_B, friedman = intento_friedman,
                       kruskal = intento_kruskal, dunn = intento_dunn)

      # ---- Step 4-5: run the chain until letters are obtained -------------
      for (paso in cadena) {
        out <- tryCatch(intentos[[paso]](), error = function(e) NULL)
        if (!is.null(out)) {
          res$oti    <- out$oti
          res$p_ruta <- out$p
          res$metodo <- out$metodo
          res$ruta   <- ruta_diag
          if (paso != cadena[1]) {
            res <- add_nota(res, paste0("ruta de respaldo: el post-hoc previsto (",
                                        cadena[1], ") no pudo calcularse"))
          }
          if (ruta_diag == "D") res <- add_nota(res, "varianzas heterogeneas")
          if (!is.na(out$nota)) res <- add_nota(res, out$nota)
          return(res)
        }
      }

      # Last resort: means without letters, with explicit reason
      res$ruta   <- ruta_diag
      res$metodo <- "-"
      res <- add_nota(res, "ningun post-hoc pudo calcularse; se muestran solo medias")
      res$oti <- dplyr::mutate(means_tbl, groups = NA_character_)
      return(res)

    }, error = function(e) {

      # Fallback: return means without letters on unexpected error
      means_tbl2 <- tryCatch(
        datis %>%
          dplyr::group_by(.data[[factor_name]]) %>%
          dplyr::summarise(
            medias = mean(.data[[variable_name]], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(!!factor_name := as.character(.data[[factor_name]])),
        error = function(e2) NULL
      )

      res$oti    <- if (!is.null(means_tbl2))
        dplyr::mutate(means_tbl2, groups = NA_character_) else NULL
      res$metodo <- "-"
      res$nota   <- paste0("error inesperado: ", conditionMessage(e))
      return(res)
    })
  }

  # -------------------------------------------------------------------------
  # Build the model formula from factor, optional factor2, and optional block
  # -------------------------------------------------------------------------
  build_formula <- function(factor_name, factor2_name, bloque_name, response_name) {
    if (is.null(factor2_name) && is.null(bloque_name)) {
      stats::reformulate(factor_name, response = response_name)
    } else if (!is.null(bloque_name) && is.null(factor2_name)) {
      stats::reformulate(c(factor_name, bloque_name), response = response_name)
    } else if (is.null(bloque_name) && !is.null(factor2_name)) {
      stats::reformulate(paste0(factor_name, "*", factor2_name),
                         response = response_name)
    } else {
      stats::reformulate(c(paste0(factor_name, "*", factor2_name), bloque_name),
                         response = response_name)
    }
  }

  # -------------------------------------------------------------------------
  # Loop over clusters: run ANOVA / Welch for each facet panel
  # -------------------------------------------------------------------------
  # Level order (not order of appearance) so that $tabla columns and $stats
  # rows follow grupo1_orden / grupo2_orden, like the facets do
  clusters   <- levels(droplevels(factor(data2$cluster)))
  oti_list   <- list()
  cv_list    <- list()
  power_list <- list()

  for (grp in clusters) {
    datis        <- dplyr::filter(data2, cluster == grp)
    if (nrow(datis) == 0) next

    formula_curr <- build_formula(factor, factor2, bloque, variable)
    res_anova    <- run_anova_for_group(datis, formula_curr,
                                        factor, variable, test)

    if (!is.null(res_anova$oti)) {
      oti_list[[length(oti_list) + 1]] <- res_anova$oti %>%
        dplyr::mutate(
          cluster   = grp,
          shapiro_p = res_anova$shapiro_p,
          fligner_p = res_anova$fligner_p,
          anova_p   = res_anova$anova_p,
          metodo    = res_anova$metodo,
          ruta      = res_anova$ruta,
          p_ruta    = res_anova$p_ruta,
          nota      = res_anova$nota
        )
    } else {
      empty_cols <- c("groups", factor)
      oti_list[[length(oti_list) + 1]] <- dplyr::tibble(
        !!!rlang::set_names(rep(list(character(0)), length(empty_cols)),
                            empty_cols),
        cluster = character(0)
      )
    }

    cv_list[[length(cv_list) + 1]] <- dplyr::tibble(
      cluster = grp,
      CV      = res_anova$cv
    )
    power_list[[length(power_list) + 1]] <- dplyr::tibble(
      cluster = grp,
      Power   = res_anova$power
    )
  }

  # -------------------------------------------------------------------------
  # Merge all results into a single table
  # -------------------------------------------------------------------------
  oti_all   <- dplyr::bind_rows(oti_list)
  cv_all    <- dplyr::bind_rows(cv_list)
  power_all <- dplyr::bind_rows(power_list)

  oti_merged <- oti_all %>%
    dplyr::left_join(cv_all,    by = "cluster") %>%
    dplyr::left_join(power_all, by = "cluster") %>%
    dplyr::distinct()

  # -------------------------------------------------------------------------
  # Factor levels and color palette
  # -------------------------------------------------------------------------
  dosis.a      <- levels(as.factor(data2[[factor]]))
  labels_union <- dosis.a   # display labels (equal to levels after relabeling)

  max_val <- suppressWarnings(max(data2[[variable]], na.rm = TRUE))
  min_val <- suppressWarnings(min(data2[[variable]], na.rm = TRUE))

  if (is.null(lim_sup))
    # max + 20% of |max| (= max * 1.2 for positive data, but stays above
    # the maximum when the response is negative); if max <= 0, 30% of range
    lim_sup <- ifelse(is.finite(max_val) && !is.na(max_val),
                      if (isTRUE(max_val <= 0)) max_val + 0.3 * (max_val - min_val)
                      else max_val + 0.2 * abs(max_val),
                      NA_real_)
  if (is.null(lim_inf))
    lim_inf <- ifelse(is.finite(min_val) && !is.na(min_val),
                      ifelse(min_val <= 0, min_val * 2, min_val * 0.7),
                      NA_real_)

  if (is.null(colores))
    colores <- grDevices::hcl.colors(length(dosis.a), "Dynamic")

  # Single-panel flag: no faceting when the only cluster is "A"
  clusters_unique <- unique(as.character(data2$cluster))
  single_A <- length(clusters_unique) == 1 && clusters_unique == "A"

  # -------------------------------------------------------------------------
  # Ensure facet variables are proper factors in data2 so that facet_grid
  # respects the level order defined by grupo1_orden / grupo2_orden
  # -------------------------------------------------------------------------
  if (has_name(grupe1) && grupe1 %in% names(data2))
    data2[[grupe1]] <- factor(data2[[grupe1]], levels = levels(data2[[grupe1]]))

  if (has_name(grupe2) && grupe2 %in% names(data2))
    data2[[grupe2]] <- factor(data2[[grupe2]], levels = levels(data2[[grupe2]]))

  # -------------------------------------------------------------------------
  # Build base ggplot
  # -------------------------------------------------------------------------
  base_theme <- ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p_base <- ggplot2::ggplot(
    data2,
    ggplot2::aes(y     = .data[[variable]],
                 x     = .data[[factor]],
                 color = .data[[factor]])
  ) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_jitter(alpha = 0.4, size = 1) +
    ggplot2::labs(y = titulo, x = NULL, col = NULL) +
    base_theme +
    ggplot2::scale_color_manual(values = colores,
                                breaks = dosis.a,
                                labels = labels_union)

  # Legend and x-axis text depend on whether there is more than one panel
  if (single_A) {
    p_base <- p_base + ggplot2::theme(legend.position = "none")
  } else {
    p_base <- p_base + ggplot2::theme(
      legend.position = "bottom",
      axis.text.x     = ggplot2::element_blank()
    )
  }

  # Add faceting when estructura is provided
  # "row~" (documented) is not a valid facet formula: complete it as "row~."
  if (!is.null(estructura) && nzchar(estructura)) {
    estructura_facet <- if (has_name(grupe1) && !has_name(grupe2))
      paste0(grupe1, " ~ .") else estructura
    p_base <- p_base +
      ggplot2::facet_grid(estructura_facet, switch = "y", space = "free")
  }

  # -------------------------------------------------------------------------
  # Reconstruct grouping columns in oti_merged so that geom_text can be
  # placed inside the correct facet panel.
  # tidyr::separate() outputs character columns; we restore factor levels
  # from data2 so the facet assignment is correct.
  # -------------------------------------------------------------------------
  g1 <- if (has_name(grupe1)) grupe1 else NA_character_
  g2 <- if (has_name(grupe2)) grupe2 else NA_character_

  restore_levels <- function(df_target, g, data_src) {
    if (!is.na(g) && g %in% names(df_target) && g %in% names(data_src)) {
      df_target[[g]] <- factor(df_target[[g]], levels = levels(data_src[[g]]))
    }
    df_target
  }

  if (!"cluster" %in% names(oti_merged)) {
    oti_merged2 <- oti_merged

  } else if (!is.na(g1) || !is.na(g2)) {
    # Recover the grouping columns from data2 by joining on 'cluster'
    # instead of splitting the cluster string on "_": labels that contain
    # "_" (e.g. "Sitio_Norte") or non-ASCII characters are kept intact.
    g_cols <- stats::na.omit(c(g1, g2))
    mapa_cluster <- data2 %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(c("cluster", g_cols)))) %>%
      dplyr::mutate(cluster = as.character(cluster))
    oti_merged2 <- oti_merged %>%
      dplyr::mutate(cluster = as.character(cluster)) %>%
      dplyr::select(-dplyr::any_of(g_cols)) %>%
      dplyr::left_join(mapa_cluster, by = "cluster")
    for (g in g_cols) oti_merged2 <- restore_levels(oti_merged2, g, data2)

  } else {
    oti_merged2 <- oti_merged
  }

  # Safety: create empty grouping columns if still missing
  if (!is.na(g1) && !(g1 %in% names(oti_merged2))) oti_merged2[[g1]] <- NA_character_
  if (!is.na(g2) && !(g2 %in% names(oti_merged2))) oti_merged2[[g2]] <- NA_character_

  # -------------------------------------------------------------------------
  # Prepare corner annotation label (CV, Power, or diagnostic p-values)
  # One label per cluster, placed at top-right of each facet panel
  # -------------------------------------------------------------------------
  labels_corner <- oti_merged2 %>%
    dplyr::group_by(cluster) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      shapiro_lbl = ifelse(!is.na(shapiro_p),
                           formatC(shapiro_p, digits = 3, format = "f"),
                           NA_character_),
      fligner_lbl = ifelse(!is.na(fligner_p),
                           formatC(fligner_p, digits = 3, format = "f"),
                           NA_character_),
      metodo = ifelse(!is.na(ruta) & ruta %in% c("A", "B", "C", "D") &
                        !is.na(metodo) & metodo != "-",
                      paste0(ruta, " - ", metodo), metodo),
      corner_label = dplyr::case_when(
        !is.na(CV) | !is.na(Power) ~ paste0(
          "Metodo: ", ifelse(is.na(metodo), "-", metodo),
          "\nCV: ",    ifelse(is.na(CV),    "N/A", round(CV, 2)), "%",
          "\nPower: ", ifelse(is.na(Power), "N/A", round(Power, 2))
        ),
        !is.na(shapiro_lbl) | !is.na(fligner_lbl) ~ paste0(
          "Metodo: ",     ifelse(is.na(metodo),      "-",   metodo),
          "\nShapiro p: ", ifelse(is.na(shapiro_lbl), "N/A", shapiro_lbl),
          "\nFligner p: ", ifelse(is.na(fligner_lbl), "N/A", fligner_lbl)
        ),
        TRUE ~ paste0("Metodo: ", ifelse(is.na(metodo), "-", metodo))
      )
    )

  # -------------------------------------------------------------------------
  # Assemble final plot: add mean values, post-hoc letters, and corner labels
  # -------------------------------------------------------------------------
  p1 <- p_base +
    ggplot2::geom_text(
      data    = oti_merged2,
      mapping = ggplot2::aes(x     = .data[[factor]],
                             y     = medias,
                             label = round(medias, 2)),
      color   = "black",
      size    = 3.3,
      vjust   = -3.5
    ) +
    ggplot2::geom_text(
      data    = oti_merged2,
      mapping = ggplot2::aes(x     = .data[[factor]],
                             y     = medias,
                             label = groups),
      color    = "red",
      size     = 3,
      vjust    = -2.8,
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      data        = dplyr::filter(labels_corner, !is.na(corner_label)),
      mapping     = ggplot2::aes(x = Inf, y = Inf, label = corner_label),
      hjust       = 1.1,
      vjust       = 1.1,
      size        = 3,
      color       = "blue",
      inherit.aes = FALSE
    )

  if (!is.na(lim_inf) && !is.na(lim_sup))
    # coord_cartesian zooms without removing observations (scale limits
    # dropped points outside the range and altered the boxplots)
    p1 <- p1 + ggplot2::coord_cartesian(ylim = c(lim_inf, lim_sup))

  # -------------------------------------------------------------------------
  # Method note (caption): which statistical route was used in each panel,
  # why, advantages, disadvantages and scope.
  # -------------------------------------------------------------------------
  texto_ruta <- function(ruta, metodo) {
    padj_txt <- paste0("ajuste de p: ", p.adj)
    f2_txt   <- if (!is.null(factor2))
      "; con factor2, las letras corresponden al efecto principal del factor (promediado sobre factor2)" else ""
    if (identical(ruta, "0")) {
      return(c(por = "datos insuficientes (se requieren >= 2 tratamientos y >= 3 obs. por tratamiento).",
               ven = NA, des = NA, alc = "solo se muestran medias descriptivas."))
    }
    if (is.na(metodo) || metodo == "-") {
      return(c(por = "ning\u00fan post-hoc pudo calcularse con los datos del panel.",
               ven = NA, des = NA, alc = "solo se muestran medias descriptivas."))
    }
    por_np <- if (identical(ruta, "C"))
      "residuos no normales (Shapiro p <= 0.05) con varianzas homog\u00e9neas (Fligner p > 0.05): se usa una prueba basada en rangos."
    else if (identical(ruta, "D"))
      "residuos no normales (Shapiro p <= 0.05) y varianzas heterog\u00e9neas (Fligner p <= 0.05): se usa una prueba basada en rangos, la opci\u00f3n m\u00e1s robusta disponible."
    else
      "el post-hoc param\u00e9trico previsto no pudo calcularse; se usa una prueba basada en rangos como respaldo."
    des_D <- if (identical(ruta, "D"))
      " Con varianzas distintas, una diferencia puede reflejar dispersi\u00f3n y no solo posici\u00f3n." else ""

    if (startsWith(metodo, "ANOVA")) {
      c(por = "residuos normales (Shapiro p > 0.05) y varianzas homog\u00e9neas (Fligner p > 0.05): se cumplen los supuestos del ANOVA.",
        ven = "es la ruta con mayor potencia; usa el error del modelo completo (incluye bloque y factor2 si se indicaron).",
        des = if (test == "Tukey")
          "Tukey controla el error por familia de comparaciones; es conservadora cuando hay muchos tratamientos."
        else
          "Duncan es m\u00e1s liberal que Tukey: detecta m\u00e1s diferencias, con mayor riesgo de falsos positivos.",
        alc = paste0("las letras comparan medias aritm\u00e9ticas", f2_txt, "."))
    } else if (startsWith(metodo, "Welch")) {
      c(por = "residuos normales (Shapiro p > 0.05) pero varianzas heterog\u00e9neas (Fligner p <= 0.05): el ANOVA cl\u00e1sico no es v\u00e1lido.",
        ven = "Welch y Games-Howell no asumen varianzas iguales y toleran tama\u00f1os de muestra desiguales.",
        des = "menor potencia con pocas repeticiones; no incorpora bloque ni factor2.",
        alc = "las letras comparan medias de tratamientos; si hubo bloque o factor2, la comparaci\u00f3n los ignora.")
    } else if (metodo == "Friedman") {
      c(por = paste(sub("\\.$", "", por_np), "y hay bloques: Friedman compara tratamientos dentro de cada bloque."),
        ven = "respeta el dise\u00f1o en bloques sin asumir normalidad.",
        des = paste0("usa un valor por tratamiento \u00d7 bloque (promedia r\u00e9plicas) y excluye bloques incompletos; menor potencia que el ANOVA.", des_D),
        alc = "las letras comparan sumas de rangos dentro de bloques, no medias; si Friedman global no es significativo, todos comparten la letra a; las medias mostradas son descriptivas.")
    } else {
      c(por = por_np,
        ven = "no asume normalidad y es robusta a valores at\u00edpicos.",
        des = paste0("menor potencia que el ANOVA cuando los datos s\u00ed son normales; ", padj_txt, ".", des_D),
        alc = "las letras comparan rangos (distribuciones), no medias; las medias mostradas son descriptivas.")
    }
  }

  resumen_rutas <- oti_merged %>%
    dplyr::distinct(cluster, ruta, metodo, nota) %>%
    dplyr::arrange(match(cluster, levels(factor(data2$cluster)))) %>%
    dplyr::mutate(
      respaldo = !is.na(nota) & grepl("ruta de respaldo", nota, fixed = TRUE),
      clave    = paste(ruta, metodo, sep = "|")
    )

  bloques_txt <- character(0)
  for (cl in unique(resumen_rutas$clave)) {
    filas   <- dplyr::filter(resumen_rutas, clave == cl)
    r_ruta  <- filas$ruta[1]
    r_met   <- filas$metodo[1]
    tx      <- texto_ruta(r_ruta, r_met)
    titulo_r <- if (identical(r_ruta, "0") || is.na(r_met) || r_met == "-")
      "Sin letras" else paste0("Ruta ", r_ruta, " - ", r_met)
    if (any(filas$respaldo)) titulo_r <- paste0(titulo_r, " (ruta de respaldo)")
    if (!single_A)
      titulo_r <- paste0(titulo_r, "  [", paste(filas$cluster, collapse = ", "), "]")
    cuerpo <- paste0("Por qu\u00e9: ", tx[["por"]],
                     if (!is.na(tx[["ven"]])) paste0(" Ventajas: ", tx[["ven"]]) else "",
                     if (!is.na(tx[["des"]])) paste0(" Desventajas: ", tx[["des"]]) else "",
                     " Alcance: ", tx[["alc"]])
    bloques_txt <- c(bloques_txt,
                     paste0(titulo_r, "\n",
                            paste(strwrap(cuerpo, width = 105), collapse = "\n")))
  }
  if (any(!is.na(resumen_rutas$metodo) &
          !startsWith(ifelse(is.na(resumen_rutas$metodo), "", resumen_rutas$metodo), "ANOVA") &
          resumen_rutas$metodo != "-"))
    bloques_txt <- c(bloques_txt,
                     "CV y Power se calculan del ANOVA cl\u00e1sico como referencia en todas las rutas.")

  nota_metodo <- paste(bloques_txt, collapse = "\n\n")

  p1 <- p1 +
    ggplot2::labs(caption = nota_metodo) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 7.5,
                                                        colour = "grey25",
                                                        lineheight = 1.05))

  # -------------------------------------------------------------------------
  # Summary table: means + letters + ANOVA significance + CV + Power
  # Rows: one per factor level, plus ANOVA / CV / Power footer rows
  # Columns: one per cluster (facet panel)
  # -------------------------------------------------------------------------
  tabla_resumen_anova <- function(data_in, factor_col) {

    df_t <- dplyr::as_tibble(data_in)

    if (!factor_col %in% names(df_t))
      stop("Column '", factor_col, "' not found in data.")

    # Apply display labels to factor column
    df_t <- df_t %>%
      dplyr::mutate(
        !!factor_col := factor(
          dplyr::recode(.data[[factor_col]],
                        !!!stats::setNames(labels_union, dosis.a)),
          levels = labels_union
        )
      )

    oti_clean <- df_t %>%
      dplyr::mutate(
        celda = dplyr::if_else(
          is.na(groups),
          sprintf("%.2f", medias),
          sprintf("%.2f (%s)", medias, groups)
        )
      )

    cluster_map <- oti_clean %>%
      dplyr::distinct(cluster) %>%
      dplyr::arrange(cluster) %>%
      dplyr::mutate(col_name = as.character(cluster))

    oti_labeled <- dplyr::left_join(oti_clean, cluster_map, by = "cluster")

    wide_main <- oti_labeled %>%
      dplyr::select(dplyr::all_of(factor_col), col_name, celda) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = col_name, values_from = celda,
                         values_fill = "")

    wide_main <- wide_main[order(wide_main[[factor_col]]), ]
    n_trat    <- dplyr::n_distinct(wide_main[[factor_col]])

    # ANOVA significance row
    if (!"anova_p" %in% names(oti_labeled)) {
      sig_row <- wide_main[1, ]
      sig_row[] <- ""
      sig_row[[factor_col]] <- "ANOVA"
    } else {
      sig_row <- oti_labeled %>%
        dplyr::select(cluster, CV, col_name, anova_p, metodo) %>%
        dplyr::distinct() %>%
        dplyr::mutate(sig = dplyr::case_when(
          is.na(CV)         ~ "-",
          is.na(anova_p)    ~ "",
          is.na(metodo) | !startsWith(metodo, "ANOVA") ~ "-",
          anova_p < 0.001   ~ "***",
          anova_p < 0.01    ~ "**",
          anova_p < 0.05    ~ "*",
          TRUE              ~ "n.s."
        )) %>%
        dplyr::select(col_name, sig) %>%
        tidyr::pivot_wider(names_from = col_name, values_from = sig)
      sig_row[[factor_col]] <- "ANOVA"
      sig_row <- dplyr::relocate(sig_row, dplyr::all_of(factor_col), .before = 1)
    }

    # Helper for numeric footer rows (CV, Power)
    build_numeric_row <- function(var_name, label) {
      row <- oti_labeled %>%
        dplyr::select(col_name, !!rlang::sym(var_name)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          value = dplyr::if_else(is.na(.data[[var_name]]),
                                 "",
                                 sprintf("%.2f", .data[[var_name]]))
        ) %>%
        dplyr::select(col_name, value) %>%
        tidyr::pivot_wider(names_from = col_name, values_from = value)
      row[[factor_col]] <- label
      dplyr::relocate(row, dplyr::all_of(factor_col), .before = 1)
    }

    cv_row    <- build_numeric_row("CV",    "CV")
    power_row <- build_numeric_row("Power", "Power")

    tabla_final <- dplyr::bind_rows(wide_main, sig_row, cv_row, power_row)

    # Add a header row showing the factor column name
    fila_extra <- as.list(rep("", ncol(tabla_final)))
    names(fila_extra) <- names(tabla_final)
    fila_extra[[factor_col]] <- factor_col

    tabla_final2 <- dplyr::bind_rows(fila_extra, tabla_final)
    names(tabla_final2)[names(tabla_final2) == factor_col] <- ""

    # Format multi-part column names (grupe1_grupe2) for LaTeX makecell
    enc <- names(tabla_final2)
    # Only when there are two grouping variables; the two parts are taken
    # from data2 (not by splitting on "_", which breaks labels with "_")
    if (has_name(grupe1) && has_name(grupe2) &&
        all(c(grupe1, grupe2) %in% names(data2))) {
      mapa_enc <- data2 %>%
        dplyr::distinct(cluster, .data[[grupe1]], .data[[grupe2]]) %>%
        dplyr::mutate(cluster = as.character(cluster))
      enc[-1] <- vapply(enc[-1], function(x) {
        fila <- mapa_enc[mapa_enc$cluster == x, , drop = FALSE]
        if (nrow(fila) != 1) return(x)
        sprintf("\\makecell{%s \\\\ %s}",
                as.character(fila[[grupe1]]), as.character(fila[[grupe2]]))
      }, character(1))
    }
    names(tabla_final2) <- enc

    tabla_final2
  }

  tabla <- tabla_resumen_anova(data_in    = oti_merged,
                               factor_col = factor)

  # -------------------------------------------------------------------------
  # Return results
  # -------------------------------------------------------------------------

  # Build a clean summary of the processed data used in the analysis.
  # Contains only the columns involved: factor, variable, grouping variables,
  # and cluster. Means are computed per factor x cluster combination.
  cols_keep <- c(factor, variable,
                 if (has_name(grupe1)) grupe1 else NULL,
                 if (has_name(grupe2)) grupe2 else NULL,
                 "cluster")

  data_summary <- data2 %>%
    dplyr::select(dplyr::all_of(cols_keep)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(
      setdiff(cols_keep, variable)
    ))) %>%
    dplyr::summarise(
      mean    = round(mean(.data[[variable]], na.rm = TRUE), 3),
      sd      = round(stats::sd(.data[[variable]], na.rm = TRUE), 3),
      n       = sum(!is.na(.data[[variable]])),
      .groups = "drop"
    )

  anova_summary <- oti_merged %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      anova_p   = dplyr::first(anova_p),
      shapiro_p = dplyr::first(shapiro_p),
      fligner_p = dplyr::first(fligner_p),
      CV        = dplyr::first(CV),
      Power     = dplyr::first(Power),
      ruta      = dplyr::first(ruta),
      metodo    = dplyr::first(metodo),
      p_ruta    = dplyr::first(p_ruta),
      nota      = dplyr::first(nota),
      .groups = "drop"
    ) %>%
    dplyr::arrange(match(cluster, clusters))

  list(
    plot   = p1,
    tabla  = tabla,
    levels = labels_union,
    data   = data_summary,
    stats  = anova_summary
  )
}
