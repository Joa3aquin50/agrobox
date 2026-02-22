#' Sintetiza resultados estadísticos para múltiples variables
#'
#' Aplica \code{agrobox()} a una o varias variables y consolida
#' las tablas resumen. Genera además una síntesis decisional
#' orientada a interpretación rápida.
#'
#' @param data data.frame con los datos experimentales.
#' @param variables vector de nombres de variables respuesta.
#' @param estructura fórmula en texto tipo "Factor1~Factor2" para clusters.
#' @param report logical, imprime reporte ejecutivo en consola.
#' @param color logical, usa color en el reporte (si está disponible).
#' @param ... argumentos adicionales pasados a \code{agrobox()}.
#'
#' @return
#' - Sin clusters: lista con \code{tabla} y \code{decision}
#' - Con clusters: lista de dichas listas por cluster
#'
#' @importFrom dplyr bind_cols distinct semi_join across all_of bind_rows
#' @importFrom stats as.formula
#'
#' @export
agrosintesis <- function(
    data,
    variables,
    estructura = NULL,
    report = TRUE,
    color  = TRUE,
    ...
) {

  .get_decision_status <- function(p, power, cv) {

    # Casos sin información suficiente
    if (is.na(p) || is.na(power) || is.na(cv)) {
      return("Información insuficiente")
    }

    # Ensayo ideal
    if (p < 0.05 && power >= 0.80 && cv <= 15) {
      return("Ensayo confiable")
    }

    # Significativo pero débil
    if (p < 0.05 && power < 0.80) {
      return("Significativo con baja potencia")
    }

    # Buen diseño pero sin diferencias
    if (p >= 0.05 && power >= 0.80 && cv <= 15) {
      return("Ensayo preciso sin diferencias detectables")
    }

    # Alta variabilidad
    if (cv > 20) {
      return("Alta variabilidad experimental")
    }

    # Caso general
    "Ensayo no confiable"
  }

  sintetizar <- function(data_sub, titulo = NULL) {

    tablas <- list()
    decisiones <- list()

    for (v in variables) {

      res <- agrobox(
        data       = data_sub,
        variable   = v,
        estructura = NULL,
        ...
      )

      # ---- tabla ----
      tab <- res$tabla
      colnames(tab)[1]  <- "trt"
      colnames(tab)[-1] <- v
      tablas[[v]] <- tab

      # ---- decisión ----
      st <- res$stats[1, ]

      decision <- .get_decision_status(
        p     = st$anova_p,
        power = st$Power,
        cv    = st$CV
      )

      decisiones[[v]] <- data.frame(
        variable = v,
        p_value  = st$anova_p,
        power    = st$Power,
        CV       = st$CV,
        decision = decision
      )
    }

    tabla_final <- Reduce(
      function(x, y) dplyr::bind_cols(x, y[-1]),
      tablas
    )

    decision_df <- dplyr::bind_rows(decisiones)

    # ---- reporte ----
    if (report && requireNamespace("cli", quietly = TRUE)) {

      if (!is.null(titulo)) {
        cli::cli_h2(titulo)
      } else {
        cli::cli_h1("AGROSINTESIS · RESUMEN DE DECISIÓN")
      }

      cli::cli_rule()

      for (i in seq_len(nrow(decision_df))) {

        d <- decision_df[i, ]

        estado <- d$decision
        if (color) estado <- .color_status(estado)

        cli::cli_text("{d$variable}: {estado}")
        cli::cli_text(
          "  p = {format(d$p_value, digits = 3)} | power = {round(d$power, 2)} | CV = {round(d$CV, 1)}%"
        )
        cli::cli_rule()
      }
    }

    list(
      tabla    = tabla_final,
      decision = decision_df
    )
  }

  # --------------------------------------------------
  # CASO 1: SIN CLUSTERS
  # --------------------------------------------------
  if (is.null(estructura)) {
    return(sintetizar(data))
  }

  # --------------------------------------------------
  # CASO 2: CON CLUSTERS
  # --------------------------------------------------
  estructura_formula <- stats::as.formula(estructura)
  vars_cluster <- all.vars(estructura_formula)

  data_local <- data
  data_local[vars_cluster] <- lapply(
    data_local[vars_cluster],
    function(x) factor(x, levels = unique(x))
  )

  clusters <- dplyr::distinct(
    data_local,
    dplyr::across(dplyr::all_of(vars_cluster))
  )

  resultados <- list()

  for (i in seq_len(nrow(clusters))) {

    filtro <- clusters[i, , drop = FALSE]

    data_cluster <- dplyr::semi_join(
      data_local,
      filtro,
      by = vars_cluster
    )

    nombre <- paste(
      mapply(
        function(var, val) paste0(var, "=", as.character(val)),
        vars_cluster,
        filtro,
        SIMPLIFY = TRUE
      ),
      collapse = " | "
    )

    resultados[[nombre]] <- sintetizar(
      data_cluster,
      titulo = paste("AGROSINTESIS ·", nombre)
    )
  }

  resultados
}
