
is_chr_type <- function(x) {
  UseMethod("is_chr_type")
}

is_dbl_type <- function(x) {
  UseMethod("is_dbl_type")
}

is_int_type <- function(x) {
  UseMethod("is_int_type")
}

is_lgl_type <- function(x) {
  UseMethod("is_lgl_type")
}

is_list_type <- function(x) {
  UseMethod("is_list_type")
}

is_chr_type.default <- function(x) {
  is_chr_type(make_strongr_check(x, msg = NULL))
}

is_dbl_type.default <- function(x) {
  is_dbl_type(make_strongr_check(x, msg = NULL))
}

is_int_type.default <- function(x) {
  is_int_type(make_strongr_check(x, msg = NULL))
}

is_lgl_type.default <- function(x) {
  is_lgl_type(make_strongr_check(x, msg = NULL))
}

is_list_type.default <- function(x) {
  is_list_type(make_strongr_check(x, msg = NULL))
}

is_chr_type.strongr_check  <- is_type_base("character")
is_dbl_type.strongr_check  <- is_type_base("double")
is_int_type.strongr_check  <- is_type_base("integer")
is_lgl_type.strongr_check  <- is_type_base("logical")
is_list_type.strongr_check <- is_type_base("list")

is_type_base <- function(type) {
  predicate_function <- paste0("is.", type)
  function(x){
    data_part <- x$x
    msg_part  <- x$msg
    if (!eval(parse(text = predicate_function))(data_part)) {
      msg    <- paste0("The input ",
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
    make_strongr_check(data_part, new_msg)
  }
}
