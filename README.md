
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Initialization of numerical optimization <a href="https://loelschlaeger.de/ino/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ino)](https://CRAN.R-project.org/package=ino)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-month/ino)](https://CRAN.R-project.org/package=ino)
[![R-CMD-check](https://github.com/loelschlaeger/ino/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/ino/actions)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/ino/branch/master/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/ino?branch=master)
<!-- badges: end -->

The `{ino}` R package provides tools for the analysis of the
initialization for numerical optimization. For detailed examples and
usage instructions, please refer to the
[vignettes](https://loelschlaeger.de/ino/articles/) accompanying the
package.

## Installation

You can install the released version from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ino")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("loelschlaeger/ino")
```

## Example

The [Ackley function](https://en.wikipedia.org/wiki/Ackley_function) has
multiple local minima and one global minimum in the origin.

``` r
f_ackley <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) -
    exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
}
f_ackley(c(0, 0))
#> [1] 0
```

The optimization result depends on the initial value:

``` r
library("ino")
Nop$new(objective = f_ackley, npar = 2)$
  set_optimizer(optimizeR::optimizer_nlm())$
  initialize_random(runs = 100)$
  optimize()$
  optima(digits = 2)
#>   value frequency
#> 1     0        44
#> 2  2.58        32
#> 3  3.57        13
#> 4  4.88         6
#> 5  5.38         5
```

## Contact

Have a question, found a bug, request a feature, want to contribute?
[Please file an
issue](https://github.com/loelschlaeger/ino/issues/new/choose).
