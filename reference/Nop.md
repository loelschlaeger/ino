# Nop Object

A `Nop` object defines a numerical optimization problem.

## Getting started

### Step 1: Create a `Nop` object

Call `object <- Nop$new(f, target, npar, ...)` where

- `f` is the objective function,

- `target` are the names of the target arguments,

- `npar` specifies the lengths of the target arguments,

- and `...` are additional arguments for `f`.

You can now evaluate the objective function via the `$evaluate()`
method.

### Step 2: Specify numerical optimizers

Call `object$set_optimizer(<optimizer object>)`, where
`<optimizer object>` is an object of class `optimizer`, which can be
created via the `{optimizeR}` package (please refer to [the package
homepage](https://loelschlaeger.de/optimizeR/) for details).

For example,

- `optimizeR::Optimizer$new(which = "stats::nlm")` defines the
  [`nlm`](https://rdrr.io/r/stats/nlm.html) optimizer,

- `optimizeR::Optimizer$new(which = "stats::optim")` defines the
  [`optim`](https://rdrr.io/r/stats/optim.html) optimizer.

### Step 3: Select initial values

Call initialization methods to define starting values for the
optimization, for example:

- `object$initialize_fixed()` for fixed initial values,

- `object$initialize_random()` for random initial values,

- `object$initialize_continue()` for initial values based on parameter
  estimates from previous optimization runs.

### Step 4: Optimization

Call `object$optimize()` for the optimization.

### Step 5: Analyze the results

- `$results` returns a `tibble` of the optimization results,

- `$optima()` lists all identified optima,

- `$minimum` and `$maximum` return the best minimizer and maximizer

## Input validation and errors

Public methods validate selector arguments, parameter bounds, and
initial values before they are passed to optimizers. Invalid user input
aborts with an error that identifies the affected argument. Errors
raised by optimizers are captured in `$results` via the `error` and
`error_message` columns when supported by the selected `Optimizer`.

## Progress during optimization

Displaying progress during multiple optimization runs via the
`{progressr}` package is supported. To get started, run

    progressr::handlers(global = TRUE)

and see
[`handlers`](https://progressr.futureverse.org/reference/handlers.html)
for details.

## Parallel optimization

Parallel computation of multiple optimization runs via the `{future}`
package is supported. To get started, run one of

    future::plan(future::multisession)

and see [`plan`](https://future.futureverse.org/reference/plan.html) for
details.

## Active bindings

- `initial_values`:

  \[[`list()`](https://rdrr.io/r/base/list.html), read-only\]  
  The currently defined initial values.

  Use the `initialize_*()` methods to add, transform, and reset values.

- `results`:

  \[`tibble`, read-only\]  
  Optimization results with identifiers:

  - `".optimization_label"` (identifies the optimization run)

  - `".optimizer_label"` (identifies the optimizer)

  - `".direction"` (identifies the optimization direction)

  - `".original"` (identifies results obtained on the original problem)

  The output has an associated
  [`autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method.

- `minimum`:

  \[`list(2)`, read-only\]  
  Best value and parameter across all (original) minimizations.

- `maximum`:

  \[`list(2)`, read-only\]  
  Best value and parameter across all (original) maximizations.

- `npar`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html), read-only\]  
  The length of each target argument.

- `verbose`:

  \[`logical(1)`\]  
  Print progress and details?

- `fresh_label`:

  \[`character(1)`, read-only\]  
  An optimization label that has not been used yet.

## Methods

### Public methods

- [`Nop$new()`](#method-Nop-initialize)

- [`Nop$fixed_argument()`](#method-Nop-fixed_argument)

- [`Nop$reduce_argument()`](#method-Nop-reduce_argument)

- [`Nop$standardize_argument()`](#method-Nop-standardize_argument)

- [`Nop$print()`](#method-Nop-print)

- [`Nop$evaluate()`](#method-Nop-evaluate)

- [`Nop$set_optimizer()`](#method-Nop-set_optimizer)

- [`Nop$initialize_fixed()`](#method-Nop-initialize_fixed)

- [`Nop$initialize_random()`](#method-Nop-initialize_random)

- [`Nop$initialize_grid()`](#method-Nop-initialize_grid)

- [`Nop$initialize_custom()`](#method-Nop-initialize_custom)

- [`Nop$initialize_continue()`](#method-Nop-initialize_continue)

- [`Nop$initialize_filter()`](#method-Nop-initialize_filter)

- [`Nop$initialize_promising()`](#method-Nop-initialize_promising)

- [`Nop$initialize_transform()`](#method-Nop-initialize_transform)

- [`Nop$initialize_reset()`](#method-Nop-initialize_reset)

- [`Nop$optimize()`](#method-Nop-optimize)

- [`Nop$optima()`](#method-Nop-optima)

- [`Nop$deviation()`](#method-Nop-deviation)

- [`Nop$clone()`](#method-Nop-clone)

------------------------------------------------------------------------

### `Nop$new()`

Creates a new `Nop` object.

The output has an associated
[`autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method.

#### Usage

    Nop$new(f, target = NULL, npar, gradient = NULL, hessian = NULL, ...)

#### Arguments

- `f`:

  \[`function`\]  
  A `function` to be optimized (the so-called objective function).

  It is expected that

  1.  `f` has at least one `numeric` argument,

  2.  the return value of `f` is of the structure `numeric(1)`.

- `target`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  The argument name(s) that get optimized (the so-called target
  arguments).

  All target arguments must be `numeric`.

  Can be `NULL` (default), then the first function argument is selected.

- `npar`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The length of each target argument, i.e., the length(s) of the
  argument(s) specified via `target`.

- `gradient`:

  \[`function` \| `NULL`\]  
  Optionally a `function` that returns the gradient of `f`.

  The function call of `gradient` must be identical to `f`.

  Ignored for optimizers that do not support user-supplied gradient.

- `hessian`:

  \[`function` \| `NULL`\]  
  Optionally a `function` that returns the Hessian of `f`.

  The function call of `hessian` must be identical to `f`.

  Ignored for optimizers that do not support user-supplied Hessian.

- `...`:

  Optionally additional function arguments passed to `f` (and `gradient`
  and `hessian`, if specified) that are fixed during the optimization.

------------------------------------------------------------------------

### `Nop$fixed_argument()`

Manages fixed arguments for the objective function.

#### Usage

    Nop$fixed_argument(action, ...)

#### Arguments

- `action`:

  \[`character(1)`\]  
  One of:

  - `"set"` to set an argument,

  - `"get"` to extract an argument value,

  - `"remove"` to remove an argument,

  - `"reset"` to reset an argument to its original value,

  - `"modify"` to modify an argument value.

  Note that `"set"` overrides an argument value, while `"modify"`
  preserves the original value, which can be recovered via `"reset"`.

- `...`:

  Additional parameters depending on `action`:

  - named arguments if `action = "set"` or `"modify"`,

  - a single argument name if `action = "get"`, `"remove"`, or
    `"reset"`.

------------------------------------------------------------------------

### `Nop$reduce_argument()`

Reduces a fixed argument for the objective function.

#### Usage

    Nop$reduce_argument(
      argument_name,
      proportion = 0.5,
      how = "random",
      centers = 2L,
      byrow = TRUE,
      ignore = integer()
    )

#### Arguments

- `argument_name`:

  \[`character(1)`\]  
  The name of a fixed argument for the objective function.

- `proportion, how, centers, byrow, ignore`:

  Passed on to
  [`portion`](https://rdrr.io/pkg/portion/man/portion.html).

------------------------------------------------------------------------

### `Nop$standardize_argument()`

Standardizes a fixed argument for the objective function.

#### Usage

    Nop$standardize_argument(
      argument_name,
      center = TRUE,
      scale = TRUE,
      byrow = FALSE,
      ignore = integer(),
      jointly = list()
    )

#### Arguments

- `argument_name`:

  \[`character(1)`\]  
  The name of a fixed argument for the objective function.

- `center, scale, byrow, ignore, jointly`:

  Passed on to
  [`normalize`](https://rdrr.io/pkg/normalize/man/normalize.html).

------------------------------------------------------------------------

### `Nop$print()`

Prints details of the `Nop` object.

#### Usage

    Nop$print(...)

#### Arguments

- `...`:

  Currently not used.

------------------------------------------------------------------------

### `Nop$evaluate()`

Evaluates the objective function.

#### Usage

    Nop$evaluate(
      at = rep(0, sum(self$npar)),
      .gradient_as_attribute = FALSE,
      .hessian_as_attribute = FALSE
    )

#### Arguments

- `at`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The values for the target argument(s), written in a single vector.

  Must be of length `sum(self$npar)`.

- `.gradient_as_attribute, .hessian_as_attribute`:

  \[`logical(1)`\]  
  Add gradient and / or Hessian value as attributes?

  If gradient and / or Hessian function is not specified, numerical
  approximation is used.

------------------------------------------------------------------------

### `Nop$set_optimizer()`

Specifies a numerical optimizer.

#### Usage

    Nop$set_optimizer(optimizer, optimizer_label = optimizer$label)

#### Arguments

- `optimizer`:

  \[`Optimizer`\]  
  An `Optimizer` object, which can be created via
  [`Optimizer`](https://loelschlaeger.de/optimizeR/reference/Optimizer.html).

- `optimizer_label`:

  \[`character(1)`\]  
  A (unique) label for the optimizer.

------------------------------------------------------------------------

### `Nop$initialize_fixed()`

Defines fixed initial values for the optimization.

#### Usage

    Nop$initialize_fixed(at)

#### Arguments

- `at`:

  \[`numeric(sum(self$npar))` \|
  [`list()`](https://rdrr.io/r/base/list.html)\]  
  The fixed initial parameter vector.

  It can also be a `list` of such vectors.

------------------------------------------------------------------------

### `Nop$initialize_random()`

Defines random initial values for the optimization.

#### Usage

    Nop$initialize_random(
      runs = 1L,
      sampler = function() stats::rnorm(sum(self$npar))
    )

#### Arguments

- `runs`:

  \[`integer(1)`\]  
  The number of optimization runs.

- `sampler`:

  \[`function`\]  
  A `function` without any arguments that returns a `numeric` vector of
  length `sum(self$npar)`.

------------------------------------------------------------------------

### `Nop$initialize_grid()`

Defines a grid of initial values for the optimization.

#### Usage

    Nop$initialize_grid(lower = 0, upper = 1, breaks = 3, jitter = FALSE, ...)

#### Arguments

- `lower, upper`:

  \[`numeric(1)` \| `numeric(sum(self$npar))`\]  
  Lower and upper grid bounds for each parameter dimension.

- `breaks`:

  \[`integer(1)` \| `integer(sum(self$npar))`\]  
  The number of breaks for each parameter dimension.

- `jitter`:

  Add noise to the grid points for a random grid layout?

- `...`:

  Optional parameters passed to
  [`jitter`](https://rdrr.io/r/base/jitter.html).

------------------------------------------------------------------------

### `Nop$initialize_custom()`

Defines custom initial values for the optimization.

#### Usage

    Nop$initialize_custom(at, seconds = rep(0, length(at)), type = "custom")

#### Arguments

- `at`:

  \[[`list()`](https://rdrr.io/r/base/list.html)\]  
  A `list` of initial parameter vectors.

- `seconds`:

  \[`numeric(length(at))`\]  
  The number of seconds it took to obtain each initial value in `at`,
  which is added to the overall optimization time.

- `type`:

  \[`character(1)`\]  
  The type of the initial values.

------------------------------------------------------------------------

### `Nop$initialize_continue()`

Defines initial values based on results from previous optimizations.

#### Usage

    Nop$initialize_continue(optimization_label)

#### Arguments

- `optimization_label`:

  \[`character(1)`\]  
  Label of optimization runs from which to select.

------------------------------------------------------------------------

### `Nop$initialize_filter()`

Filters initial values from the defined initial values.

#### Usage

    Nop$initialize_filter(condition)

#### Arguments

- `condition`:

  \[`character(1)`\]  
  Defines the condition on which the initial values are filtered, one
  of:

  - `"gradient_negative"` for points where the gradient is negative,

  - `"gradient_positive"` for points where the gradient is positive,

  - `"hessian_negative"` for points where the Hessian is negative
    definite,

  - `"hessian_positive"` for points where the Hessian is positive
    definite.

------------------------------------------------------------------------

### `Nop$initialize_promising()`

Selects promising initial values from the defined initial values.

#### Usage

    Nop$initialize_promising(proportion, condition)

#### Arguments

- `proportion`:

  \[`numeric(1)`\]  
  The proportion of selected from the defined initial values.

- `condition`:

  \[`character(1)`\]  
  Defines the condition on which the initial values are selected, one
  of:

  - `"value_small"` for points where the function value is smallest,

  - `"value_large"` for points where the function value is largest,

  - `"gradient_small"` for points where the gradient norm is smallest,

  - `"gradient_large"` for points where the gradient norm is largest,

  - `"condition_small"` for points where the Hessian condition is
    smallest,

  - `"condition_large"` for points where the Hessian condition is
    largest.

------------------------------------------------------------------------

### `Nop$initialize_transform()`

Transforms the currently defined initial values.

#### Usage

    Nop$initialize_transform(transformer = function(x) x)

#### Arguments

- `transformer`:

  \[`function`\]  
  A `function` that receives and returns a
  [`numeric()`](https://rdrr.io/r/base/numeric.html) of length
  `sum(self$npar)`.

------------------------------------------------------------------------

### `Nop$initialize_reset()`

Resets the currently defined initial values.

#### Usage

    Nop$initialize_reset()

------------------------------------------------------------------------

### `Nop$optimize()`

Optimizes the target function.

#### Usage

    Nop$optimize(
      optimization_label = self$fresh_label,
      which_optimizer = "all",
      which_direction = "min",
      lower = NULL,
      upper = NULL,
      seconds = Inf,
      hide_warnings = TRUE,
      reset_initial_afterwards = TRUE
    )

#### Arguments

- `optimization_label`:

  \[`character(1)`\]  
  A label for the optimization to distinguish optimization runs.

  Setting a label is useful when using the `$initialize_continue()`
  method.

- `which_optimizer`:

  \[[`character()`](https://rdrr.io/r/base/character.html) \|
  [`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Selects numerical optimizers. Either:

  - `"all"` for all specified optimizers,

  - specific optimizer labels,

  - specified optimizer ids as defined in the
    [`print()`](https://rdrr.io/r/base/print.html) output.

- `which_direction`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Selects the direction of optimization. One or both of:

  - `"min"` for minimization,

  - `"max"` for maximization.

- `lower, upper`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html) \| `NULL`\]  
  Optionally lower and upper parameter bounds.

  Ignored for optimizers that do not support parameter bounds.

- `seconds`:

  \[`numeric(1)`\]  
  A time limit in seconds.

  Optimization is interrupted prematurely if `seconds` is exceeded.

  Note the limitations documented in
  [`setTimeLimit`](https://rdrr.io/r/base/setTimeLimit.html).

- `hide_warnings`:

  \[`logical(1)`\]  
  Hide any warnings during optimization?

- `reset_initial_afterwards`:

  \[`logical(1)`\]  
  Reset the initial values after the optimization?

#### Details

Supports:

- Parallel computation of multiple optimization runs via `{future}`

- Progress messages via `{progressr}`

------------------------------------------------------------------------

### `Nop$optima()`

Lists all identified optima.

The output has an associated
[`autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method.

#### Usage

    Nop$optima(
      which_direction = "min",
      only_original = TRUE,
      group_by = NULL,
      sort_by_value = FALSE,
      digits = getOption("digits", default = 7)
    )

#### Arguments

- `which_direction`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Selects the direction of optimization. One or both of:

  - `"min"` for minimization,

  - `"max"` for maximization.

- `only_original`:

  \[`logical(1)`\]  
  Include only optima obtained on the original problem?

- `group_by`:

  \[`character(1)`\]  
  Selects how the output is grouped. Either:

  - `NULL` to not group,

  - `"optimization"` to group by optimization label,

  - `"optimizer"` to group by optimizer label.

- `sort_by_value`:

  \[`logical(1)`\]  
  Sort by value? Else, sort by frequency.

- `digits`:

  \[`integer(1)`\]  
  The number of decimal places.

------------------------------------------------------------------------

### `Nop$deviation()`

Compute deviations with respect to a reference parameter.

The output has an associated
[`autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method.

#### Usage

    Nop$deviation(
      reference = rep(0, sum(self$npar)),
      which_element = "initial",
      which_direction = "min",
      which_optimizer = "all",
      only_original = TRUE,
      parameter_labels = paste0("x", seq_len(sum(self$npar)))
    )

#### Arguments

- `reference`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The reference vector of length `sum(self$npar)`.

- `which_element`:

  \[`character(1)`\]  
  Either

  - `"initial"` for deviations with respect to the initial values, or

  - `"parameter"` for deviations with respect to the estimated
    parameters.

- `which_direction`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Selects the direction of optimization. One or both of:

  - `"min"` for minimization,

  - `"max"` for maximization.

- `which_optimizer`:

  \[[`character()`](https://rdrr.io/r/base/character.html) \|
  [`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Selects numerical optimizers. Either:

  - `"all"` for all specified optimizers,

  - specific optimizer labels,

  - specified optimizer ids as defined in the
    [`print()`](https://rdrr.io/r/base/print.html) output.

- `only_original`:

  \[`logical(1)`\]  
  Include only optima obtained on the original problem?

- `parameter_labels`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Labels for the parameters of length `sum(self$npar)`.

------------------------------------------------------------------------

### `Nop$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Nop$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
### define objective function, optimizer and initial values
Nop_ackley <- Nop$new(f = TestFunctions::TF_ackley, npar = 2)$
  set_optimizer(optimizeR::Optimizer$new(which = "stats::nlm"))$
  initialize_random(runs = 20)

### plot function surface and initial values
Nop_ackley |> ggplot2::autoplot()


### minimize objective function
Nop_ackley$optimize(which_direction = "min")

### show optima
Nop_ackley$optima(digits = 0)
#> # A tibble: 5 × 2
#>   value     n
#> * <dbl> <int>
#> 1     0     8
#> 2     5     5
#> 3     3     4
#> 4     4     2
#> 5     7     1

### show best value and parameter across all minimizations
Nop_ackley$minimum
#> $value
#> [1] 1.008563e-06
#> 
#> $parameter
#> [1] -2.796084e-07  2.212874e-07
#> 
```
