set_f <- function(f, npar) {
  out <- list(f = f, npar = npar)
  class(out) <- "ino_f"
  return(out)
}
