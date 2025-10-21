In response to CRAN check result:

Version: 1.1.0
Check: whether package can be installed
Result: WARN 
  Found the following significant warnings:
    Warning: namespace ‘fHMM’ is not available and has been replaced
Flavor: r-devel-linux-x86_64-debian-gcc

Added safeguard to `example_hmm.Rmd` to conditionally load `Nop_hmm` only if 
`{fHMM}` namespace is available to avoid CRAN checks failing.
