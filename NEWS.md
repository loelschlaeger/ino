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
