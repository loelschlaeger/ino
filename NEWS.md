# ino 1.0.0

* The package is now based on an R6 class called `Nop`. See the documentation `?Nop` for details.  

# ino 0.2.0

## New functionality

* The `get_vars()` function prints all available variables saved in an `ino` object.
* The `get_fails()` function is a wrapper for `get_vars()` that provides failure messages of optimization runs.
* The `summary()` method now can compute custom statistics from the variables saved in an `ino` object.

## New optimizer framework

* Optimizer in `setup_ino()` are now specified via the framework provided by the [`{optimizeR}`](https://CRAN.R-project.org/package=optimizeR) package.
* Optimizer can be changed afterwards via the `update_opt()` helper function.

# ino 0.1.0

* Initial version.
