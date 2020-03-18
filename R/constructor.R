make_strongr_check <- function(x, msg) {
  obj <- list(x = x, msg = msg)
  class(obj) <- "strongr_check"
  obj
}
