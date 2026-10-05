#' Exportar un gráfico ggplot a PowerPoint editable
#' / Export a ggplot Chart to an Editable PowerPoint Slide
#'
#' Crea un archivo PowerPoint (.pptx) de una sola diapositiva con el gráfico
#' en formato vectorial editable: textos, colores y elementos pueden
#' modificarse directamente en PowerPoint.
#'
#' Creates a single-slide PowerPoint (.pptx) file containing the chart as an
#' editable vector graphic: text, colors and elements can be modified
#' directly in PowerPoint.
#'
#' @details
#' Requiere los paquetes \code{officer} y \code{rvg} (no se instalan con
#' agrobox). Si faltan, la función se detiene indicando cómo instalarlos.
#'
#' Requires the \code{officer} and \code{rvg} packages (not installed with
#' agrobox). If they are missing, the function stops with installation
#' instructions.
#'
#' El gráfico se ubica a 1 pulgada del borde izquierdo y superior de una
#' diapositiva estándar de 10 x 7.5 pulgadas.
#'
#' The chart is placed 1 inch from the left and top edges of a standard
#' 10 x 7.5 inch slide.
#'
#' @param grafico Objeto ggplot, por ejemplo \code{resultado$plot} de
#'   \code{agrobox()} / A ggplot object, e.g. \code{result$plot} from
#'   \code{agrobox()}.
#'
#' @param ancho Ancho del gráfico en pulgadas (default 8) /
#'   Chart width in inches (default 8).
#'
#' @param alto Alto del gráfico en pulgadas (default 4) /
#'   Chart height in inches (default 4).
#'
#' @param archivo Nombre del archivo .pptx a crear (default
#'   \code{"grafico_editable.pptx"}) / Name of the .pptx file to create.
#'
#' @return Invisiblemente, la ruta del archivo creado /
#'   Invisibly, the path of the created file.
#'
#' @examples
#' if (requireNamespace("officer", quietly = TRUE) &&
#'     requireNamespace("rvg", quietly = TRUE)) {
#'
#'   set.seed(42)
#'   df1 <- data.frame(
#'     trat = rep(c("T0", "T1", "T2", "T3"), each = 5),
#'     resp = c(rnorm(5, 10, 1), rnorm(5, 14, 1),
#'              rnorm(5, 18, 1), rnorm(5, 22, 1))
#'   )
#'   result <- agrobox(data = df1, factor = "trat", variable = "resp")
#'
#'   agroppt(result$plot,
#'           ancho   = 8,
#'           alto    = 4,
#'           archivo = file.path(tempdir(), "grafico_editable.pptx"))
#' }
#'
#' @export
agroppt <- function(grafico,
                    ancho   = 8,
                    alto    = 4,
                    archivo = "grafico_editable.pptx") {

  # officer and rvg are optional (Suggests): check before using them
  faltan <- c("officer", "rvg")[!vapply(c("officer", "rvg"), requireNamespace,
                                        logical(1), quietly = TRUE)]
  if (length(faltan) > 0) {
    stop("agroppt() requiere: ", paste(faltan, collapse = ", "),
         ". Instalar con: install.packages(c(",
         paste0('"', faltan, '"', collapse = ", "), "))", call. = FALSE)
  }

  if (!inherits(grafico, "ggplot"))
    stop("'grafico' debe ser un objeto ggplot (por ejemplo resultado$plot).",
         call. = FALSE)

  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, layout = "Title and Content")
  doc <- officer::ph_with(doc,
                          rvg::dml(ggobj = grafico),
                          location = officer::ph_location(left   = 1,
                                                          top    = 1,
                                                          width  = ancho,
                                                          height = alto))
  print(doc, target = archivo)

  message("Archivo creado: ", normalizePath(archivo, mustWork = FALSE))
  invisible(archivo)
}
