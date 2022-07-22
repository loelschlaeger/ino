
# Installation ------------------------------------------------------------

devtools::load_all()
library("tictoc")

# Random initialization ---------------------------------------------------

### setup
f <- f_ackley
npar <- 2

### time ino
{
  tic()

  x <- setup_ino(
    f = f, npar = npar,
    opt = list("nlm" = set_optimizer_nlm())
  )

  for(i in 1:5)
    x <- random_initialization(x)

  toc()
}

### base R
{
  tic()

  for(i in 1:5) {
    init <- rnorm(npar)
    nlm(f = f, p = init)
  }

  toc()
}
