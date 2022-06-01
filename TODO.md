# ino TODOs

The following tasks are on our agenda and will be tackled as time permits.

## Functions to optimize

- [x] change data structure in logit and probit LL to data frame (L***)
- [x] add updated data sets to package (L**)

## Specification functions

- [ ] add test cases for optimizer in `setup_ino()` (L***)
  - check that output 'z' exists and is numeric vector of length 'npar'
      
## Initialization strategies

- [ ] `subset_initialization()` (L***)
- [ ] `standardize_initialization()` (L***)
- [ ] `ao_initialization()` (L*)
- [ ] Option to combine strategies

## Evaluation functions

- [ ] allow empty group in `summary()` to get full table (L***)
- [ ] suppress warnings `plot()` (M***)
- [ ] add facets to `plot()` (M***)
- [ ] reactivate `nr_optima()` function (M***)

## Applications

- [ ] Example ino object for standard numerical problems and HMM likelihood saved in the package to use for examples. (M*)
- [ ] Example ino object for logit and probit likelihood saved in the package to use for examples. (L*)
- [ ] Vignette introduction + workflow (L\*\*\*) with example standard numerical problems + geysir data (M**)
- [ ] Vignette example HMM (M**)
- [ ] Vignette example probit (L**)
- [ ] Vignette example logit (L**)
