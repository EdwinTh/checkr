check_inputs <- function(...) {
  inputs <- list(...)
  is_strongr_check <- sapply(inputs, function(x) inherits(x, "strongr_check"))

  if (!all(is_strongr_check)) {
    error_msg <- paste0(
      "All inputs of ",
      crayon::blue("check_inputs"),
      " should be of class ",
      crayon::green("strongr_check"),
      "\n",
      "The following positions are of a different class: ",
      crayon::red(paste(which(!is_strongr_check), collapse = " ")),
      "\n"
    )
    cat(error_msg)
    stop("Invalid inputs")
  }

  checks_failed <- sapply(
    inputs, function(x) !is.null(x$msg)
  )

  if (any(checks_failed)) {
    for (x in inputs) {
      if (!is.null(x$msg)){
        cat(x$msg)
      }
    }
    stop("check_inputs failed")
  }
}
