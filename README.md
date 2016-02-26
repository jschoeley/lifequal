lifequal: Calculating Life-table Lifespan Equality
==================================================

`lifequal` is an `R` package providing three functions to calculate measures of lifespan equality from a life-table:

1.  `ExDagger(x, ex, wx, ax)` Life expectancy lost by those who die in age interval $[x, x+w_x)$
2.  `EDagger(dx, exdagger, radix)` Total life expectancy lost due to death
3.  `KeyfzEntro(edagger, e0)` Keyfitz's entropy

How to install
--------------

You can install `lifequal` by running from within an `R` session.

``` r
install.packages("devtools")
devtools::install_github("jschoeley/lifequal")
```

Author: Jonas Schöley <jschoeley@health.sdu.dk>
License: GPL-2