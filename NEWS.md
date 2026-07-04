# ino 1.2.1

* Fixed scalar `lower` and `upper` bounds in `$optimize()` for optimization
  problems with multiple target arguments.

* Fixed `$deviation(which_optimizer = ...)` so that optimizer filters are
  applied to the returned deviations.

* Fixed `$initialize_promising()` to select largest values for `*_large`
  conditions and to compute Hessian condition numbers more robustly.

* Improved validation and error messages.

* Improved plotting validation for relative result plots when the reference
  median is zero or non-finite.
  
* Removed renv from package.

# ino 1.2.0

* Removed `example_hmm.Rmd` vignette to avoid CRAN check failure for flavor r-devel-linux-x86_64-debian-gcc (namespace 'fHMM' not available).

# ino 1.1.0

* Adapted to the new `{optimizeR}` version.

* `$trace()` method transferred to the `{trackopt}` package.

* Some changes in the API.

# ino 1.0.2

* New `$trace()` method to capture the steps during optimization with `stats::nlm()`.

# ino 1.0.1

* Minor bug fixes.

# ino 1.0.0

* The package is now based on an R6 class called `Nop`. See the documentation `?Nop` for details.

# ino 0.1.0

* Initial version.
