
is_chr <- function(x) {
  UseMethod("is_chr")
}

is_dbl <- function(x) {
  UseMethod("is_dbl")
}

is_int <- function(x) {
  UseMethod("is_int")
}

is_lgl <- function(x) {
  UseMethod("is_lgl")
}

is_list <- function(x) {
  UseMethod("is_list")
}

is_chr.default <- function(x) {
  is_chr(make_check(x, msg = NULL))
}

is_dbl.default <- function(x) {
  is_dbl(make_check(x, msg = NULL))
}

is_int.default <- function(x) {
  is_int(make_check(x, msg = NULL))
}

is_lgl.default <- function(x) {
  is_lgl(make_check(x, msg = NULL))
}

is_list.default <- function(x) {
  is_list(make_check(x, msg = NULL))
}

is_chr.check  <- is_type_base("character")
is_dbl.check  <- is_type_base("double")
is_int.check  <- is_type_base("integer")
is_lgl.check  <- is_type_base("logical")
is_list.check <- is_type_base("list")

is_type_base <- function(type) {
  predicate_function <- paste0("is.", type)
  function(x){
    data_part <- x$x
    msg_part  <- x$msg
    if (!eval(parse(text = predicate_function))(data_part)) {
      msg    <- paste0("The input value ",
                      crayon::blue(deparse(substitute(data_part))),
                      " is not of type ",
                      crayon::green(type),
                      ", but of type ",
                      crayon::red(typeof(data_part)),
                      ".\n")
    } else {
      msg <- NULL
    }
    new_msg <- c(msg_part, msg)
    make_check(data_part, new_msg)
  }
}
