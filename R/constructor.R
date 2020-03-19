make_check <- function(x, msg) {
  obj <- list(x = x, msg = msg)
  class(obj) <- "check"
  obj
}
