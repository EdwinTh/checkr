#' Check object length
#'
#'
has_length <- function(x, l) {
  UseMethod("has_length")
}

has_length.default <- function(x, l) {
  has_length(make_strongr_check(x, NULL), l)
}

has_length.strongr_check <- function(x, l) {
  data_part <- x$x
  msg_part  <- x$msg
  if (!is.numeric(l)) {
    stop("In `has_length`, l should be numeric")
  }
  if (l %% 1 != 0) {
    stop("In `has_lenght`, l should be a unit number ")
  }
  if (length(data_part) != l) {
    msg <- paste0("The input ",
                  crayon::blue(deparse(substitute(data_part))),
                  " is not of length ",
                  crayon::green(l),
                  ", but of type ",
                  crayon::red(length(data_part)),
                  ".")
  } else {
    msg <- NULL
  }
  new_msg <- c(msg_part, msg)
  make_strongr_check(data_part, new_msg)
}

