# ino TODOs

The following tasks are on our agenda and will be tackled as time permits.

## Specification functions

- `set_f()`
  - [ ] Multiple argument specifications. (M**)
- `set_optimizer()`
  - [ ] Multiple optimizer (M*)
- `set_data()`
  - [ ] Multiple data sets. (M***)
  
## Initialization strategies

- `fixed_initialization()`
- `random_initialization()`
- `subset_data()` (L**)

## Evaluation functions

- `nr_optima()`. 
  - [x] Tolerance for optima values in plot and table.
- `optimization_time()`
  - [ ] Add plot method. (M***)
- `summary.ino`
  - [ ] Add information about optimization runs. (L**)

## Applications

- Example ino object saved in the package to use for examples. (L***)
- Standard optimization problems
  - [x] Add examples. (M)
- Likelihood functions
  - [x] Poisson-HMM (M)
    - [ ] Ask Roland if we can include data set in package. (M***) 
    - [ ] Transform sandbox example to ino structure. (M*)
    - [ ] Document sandbox example in vignette. (M*)
  - [ ] MMNP (L)
    - [ ] Add simulated data. (L***)
    - [ ] Transform sandbox example to ino structure. (L*)
    - [ ] Document sandbox example in vignette. (L*)
  - [ ] MMNL (L)
    - [ ] Add simulated data. (L***)
    - [ ] Transform sandbox example to ino structure. (L*)
    - [ ] Document sandbox example in vignette. (L*)
