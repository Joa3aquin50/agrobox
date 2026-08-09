# Exporta tablas agrobox o agrosintesis a Excel

Escribe una tabla individual o una lista de tablas en un archivo Excel
(.xlsx). Cuando se trata de una lista, cada elemento se exporta como una
hoja distinta.

## Usage

``` r
agroexcel(x, file = "resultados_agro.xlsx")
```

## Arguments

- x:

  data.frame o lista de data.frames.

- file:

  nombre del archivo Excel de salida.

## Value

Invisiblemente TRUE si el archivo se genera correctamente.

## Examples

``` r
if (FALSE) { # \dontrun{
# Ejemplo 1: tabla simple desde agrobox
aa <- agrobox(
  data = antigua2,
  variable = "harvwt",
  factor = "trt",
  test = "Duncan"
)

write_agro_excel(aa$tabla, file = "tabla_simple.xlsx")

# Ejemplo 2: lista de tablas desde agrosintesis
res <- agrosintesis(
  data = df_multi,
  variables = c("tn_ha", "peso_fruto"),
  estructura = "Variedad~Localidad",
  factor = "Fertilizante",
  factor2 = "Dosis",
  bloque = "Bloque",
  test = "Tukey"
)

write_agro_excel(res, file = "resultados_clusters.xlsx")
} # }
```
