check_inputs <- function(..., trace = TRUE) {
  inputs <- list(...)
  is_check <- sapply(inputs, function(x) inherits(x, "check"))

  if (!all(is_check)) {
    error_msg <- paste0(
      "All inputs of ",
      crayon::blue("check_inputs"),
      " should be of class ",
      crayon::green("check"),
      ".",
      "\n",
      "The inputs at following position(s) is/are of a different class: ",
      crayon::red(paste(which(!is_check), collapse = " ")),
      ".",
      "\n"
    )
    cat(error_msg)
    stop("Invalid inputs.")
  }

  checks_failed <- sapply(
    inputs, function(x) !is.null(x$msg)
  )

  if (any(checks_failed)) {
    if (trace) {
      print_trace()
    }
    for (x in inputs) {
      if (!is.null(x$msg)){
        cat(x$msg)
      }
    }
    stop("check_inputs failed")
  }
}
