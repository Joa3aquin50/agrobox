# Exporta tablas de resultados a PNG via LaTeX (modo rapido)

Genera una imagen PNG de alta calidad a partir de una tabla (data.frame)
o una lista de tablas (por ejemplo salida de
[`agrosintesis()`](agrosintesis.md)), utilizando LaTeX para el
renderizado y convirtiendo el PDF resultante en imagen.

## Usage

``` r
agrotabla(x, out_dir = getwd(), file_stub = "TABLA", dpi = 600)
```

## Arguments

- x:

  data.frame o lista de data.frames. Puede ser `aa$tabla` o una lista
  producida por [`agrosintesis()`](agrosintesis.md).

- out_dir:

  directorio donde se guardaran los archivos generados.

- file_stub:

  nombre base del archivo cuando `x` es una tabla unica.

- dpi:

  resolucion en puntos por pulgada usada al convertir PDF a PNG.

## Value

Invisiblemente TRUE si la exportacion se realiza correctamente.

## Details

Esta funcion esta pensada para generar reportes rapidos y elegantes.
Debido a que crea archivos PDF y PNG, sus ejemplos no se ejecutan
automaticamente durante los checks de CRAN.

Los nombres de archivos se sanitizan automaticamente para ser
compatibles con Windows, LaTeX y sistemas de archivos estandar. Los
guiones bajos en los encabezados de columnas se escapan para evitar
errores de compilacion LaTeX.

## Examples

``` r
if (FALSE) { # \dontrun{
# Ejemplo 1: tabla individual desde agrobox
aa <- agrobox(
  data = antigua2,
  variable = "harvwt",
  factor = "trt",
  test = "Duncan"
)

write_rapido(
  aa$tabla,
  out_dir = "reportes_png",
  file_stub = "Rendimiento_site"
)

# Ejemplo 2: multiples tablas desde agrosintesis
res <- agrosintesis(
  data = df_multi,
  variables = c("tn_ha", "peso_fruto"),
  estructura = "Variedad~Localidad",
  factor = "Fertilizante",
  factor2 = "Dosis",
  bloque = "Bloque",
  test = "Tukey"
)

write_rapido(
  res,
  out_dir = "png_clusters"
)
} # }

```
