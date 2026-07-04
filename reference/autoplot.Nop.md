# Plotting methods

- `autoplot.Nop()` plots the objective function

- `autoplot.Nop_results()` plots boxplots of optimization results

- `autoplot.Nop_optima()` plots a bar chart of the found optima

- `autoplot.Nop_deviation()` plots deviations per dimension from a
  reference

## Usage

``` r
# S3 method for class 'Nop'
autoplot(object, xlim = NULL, xlim2 = NULL, ...)

# S3 method for class 'Nop_optima'
autoplot(object, ...)

# S3 method for class 'Nop_deviation'
autoplot(object, jitter = TRUE, ...)

# S3 method for class 'Nop_results'
autoplot(
  object,
  which_element = "seconds",
  group_by = NULL,
  relative = FALSE,
  ...
)
```

## Arguments

- object:

  Depends on the method:

  - for `autoplot.Nop()`, a `Nop` object

  - for `autoplot.Nop_results()`, the value `Nop$results`

  - for `autoplot.Nop_optima()`, the value `Nop$optima`

  - for `autoplot.Nop_deviation()`, the value `Nop$deviation`

- xlim, xlim2:

  \[`numeric(2)`\]  
  Ranges for the first and second parameter to plot.

  If `NULL`, they are derived from the specified initial values in
  `object`.

- ...:

  Other arguments passed to specific methods.

- jitter:

  \[`logical(1)`\]  
  Apply jitter to the points?

- which_element:

  \[`character(1)`\]  
  A column name of `object` to plot.

- group_by:

  \[`character(1)`\]  
  Selects how the plot is grouped. Either:

  - `NULL` to not group,

  - `"optimization"` to group by optimization label,

  - `"optimizer"` to group by optimizer label.

- relative:

  \[`logical(1)`\]  
  Plot values relative to the overall median?

## Value

A `ggplot` object.
