# ino TODOs

The following tasks are on our agenda and will be tackled as time permits.

## Example functions to optimize

## Specification functions

- [x] add test cases for optimizer in `setup_ino()` (L***)
- [ ] specify true parameter values (next package version)
- [ ] wrapper for `vntrs()` optimizer to better find the global optimum (next package version)
      
## Initialization strategies

- [x] `subset_initialization()` (L***)
- [x] `standardize_initialization()` (L***)
- [ ] `ao_initialization()` (next package version)
- [ ] save steps of combined initialization strategies (next package version)

## Evaluation functions

- [x] allow empty group in `summary()` to get full table (M***)
- [ ] visualize parameter values in different optimization iterations (next package version)
- [ ] comparison with true parameter values (next package version)
- [ ] maybe rename `nr_optima()` to `overview_optima()` (L***)

## Applications

- [ ] Example ino object for standard numerical problems and HMM likelihood saved in the package to use for examples. (M*)
- [ ] Example ino object for logit and probit likelihood saved in the package to use for examples. (L*)

## Vignettes

- [ ] Vignette introduction + workflow (L\*\*\*) with example standard numerical problems + geysir data (M**)
- [ ] Vignette example HMM (M**)
- [ ] Vignette example probit (L**)
- [ ] Vignette example logit (L**)
- [ ] Document how to specify `optimx::optimx()` optimizer (has special output format) with a wrapper function.
- [ ] Document how to specify custom initialization strategies.
