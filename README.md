
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

The `{ino}` R package provides tools for analyzing the initialization of
numerical optimization. For detailed examples and usage instructions,
please refer to the [package
vignettes](https://loelschlaeger.de/ino/articles/).

## Installation

You can install the released version from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ino")
```

## Example

The [Ackley function](https://en.wikipedia.org/wiki/Ackley_function) has
multiple local minima and one global minimum in the origin. We define
the numerical optimization problem, including an optimization algorithm
and initial values:

``` r
library("ino")
Nop_ackley <- Nop$new(f = TestFunctions::TF_ackley, npar = 2)$
  set_optimizer(optimizeR::Optimizer$new(which = "stats::nlm"))$
  initialize_random(runs = 100)
```

We can visualize the surface and the initial values:

``` r
ggplot2::autoplot(Nop_ackley)
```

<img src="man/figures/README-ackley plot-1.png" width="70%" />

The optimization result depends on the initial value:

``` r
Nop_ackley$
  optimize()$
  optima(digits = 2)
#> # A tibble: 6 × 2
#>   value     n
#> * <dbl> <int>
#> 1  0       40
#> 2  2.58    30
#> 3  4.88    11
#> 4  3.57    10
#> 5  5.38     8
#> 6  6.88     1
```

## Contact

Have a question, found a bug, request a feature, want to contribute?
[Please file an
issue](https://github.com/loelschlaeger/ino/issues/new/choose).
