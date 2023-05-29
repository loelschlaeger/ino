
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

The `{ino}` R package provides tools designed for the analysis of the
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
multiple local minima and one global minimum in the origin. The
optimization result depends on the initial value:

``` r
library("ino")
Nop$new(f = f_ackley, npar = 2)$
  set_optimizer(optimizer_nlm())$
  optimize(initial = "random", runs = 100, verbose = FALSE)$
  optima()
#>   value frequency
#> 1     0        39
#> 2  2.58        34
#> 3  3.57        12
#> 4  5.38         6
#> 5  4.88         5
#> 6  6.56         2
#> 7  6.88         2
```

## Contact

Have a question, found a bug, request a feature, want to contribute?
[Please file an
issue](https://github.com/loelschlaeger/ino/issues/new/choose).
