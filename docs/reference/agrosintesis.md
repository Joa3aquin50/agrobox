# Sintetiza resultados estadísticos para múltiples variables

Aplica [`agrobox()`](agrobox.md) a una o varias variables y consolida
las tablas resumen. Permite análisis por clusters cuando se define una
estructura.

## Usage

``` r
agrosintesis(data, variables, estructura = NULL, ...)
```

## Arguments

- data:

  data.frame con los datos experimentales.

- variables:

  vector de nombres de variables respuesta.

- estructura:

  fórmula en texto tipo "Factor1~Factor2" para clusters.

- ...:

  argumentos adicionales pasados a [`agrobox()`](agrobox.md).

## Value

Un data.frame si no hay clusters, o una lista de data.frames si se
define `estructura`.
