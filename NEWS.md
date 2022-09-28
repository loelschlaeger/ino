# ino 0.2.0

## New functionality

* The `get_vars()` function prints all available variables saved in an `ino` object.
* The `get_fails()` function is a wrapper for `get_vars()` that provides failure messages of optimization runs.
* The `summary()` method now can compute custom statistics from the variables saved in an `ino` object.

## New optimizer framework

* Optimizer in `setup_ino()` are now specified via the framework provided by the [{optimizeR} package](https://CRAN.R-project.org/package=optimizeR).
* Optimizer can be changed afterwards via the `update_opt()` helper function.

# ino 0.1.0

* Initial version.
