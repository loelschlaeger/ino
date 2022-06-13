# ino TODOs

The following tasks are on our agenda and will be tackled as time permits.

## Example functions to optimize

## Specification functions

- [ ] add test cases for optimizer in `setup_ino()` (L***)
  - check that output 'z' exists and is numeric vector of length 'npar'
- [ ] specify true parameter values
      
## Initialization strategies

- [x] `subset_initialization()` (L***)
- [x] `standardize_initialization()` (L***)
- [ ] `ao_initialization()` (L*, next package version)

## Evaluation functions

- [x] allow empty group in `summary()` to get full table (M***)
- [ ] visualize parameter values in different optimization iterations

## Applications

- [ ] Example ino object for standard numerical problems and HMM likelihood saved in the package to use for examples. (M*)
- [ ] Example ino object for logit and probit likelihood saved in the package to use for examples. (L*)
- [ ] Vignette introduction + workflow (L\*\*\*) with example standard numerical problems + geysir data (M**)
- [ ] Vignette example HMM (M**)
- [ ] Vignette example probit (L**)
- [ ] Vignette example logit (L**)
