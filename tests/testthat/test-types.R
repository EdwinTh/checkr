context("type checks work as expected")

test_that("is_chr has correct behavior", {
  expect_s3_class(is_chr("a"), "check")
  expect_s3_class(is_chr(1235), "check")
  expect_null(is_chr("a")$msg)
  expect_equal(is_chr("a")$x, "a")
  expect_equal(is_chr(1235)$msg,
               "The input value \033[34m1235\033[39m is not of type \033[32mcharacter\033[39m, but of type \033[31mdouble\033[39m.\n", #nolint
               fixed = TRUE)
  expect_equal(is_chr(1235)$x, 1235)
})

test_that("is_dbl has correct behavior", {
  expect_s3_class(is_dbl(42.2), "check")
  expect_s3_class(is_dbl(TRUE), "check")
  expect_null(is_dbl(42.2)$msg)
  expect_equal(is_dbl(42)$x, 42)
  expect_equal(is_dbl(TRUE)$msg,
               "The input value \033[34mTRUE\033[39m is not of type \033[32mdouble\033[39m, but of type \033[31mlogical\033[39m.\n", #nolint
               fixed = TRUE)
  expect_equal(is_dbl(TRUE)$x, TRUE)
})

test_that("is_int has correct behavior", {
  expect_s3_class(is_int(42L), "check")
  expect_s3_class(is_int(42), "check")
  expect_null(is_int(42L)$msg)
  expect_equal(is_int(42L)$x, 42L)
  expect_equal(is_int(42)$msg,
               "The input value \033[34m42\033[39m is not of type \033[32minteger\033[39m, but of type \033[31mdouble\033[39m.\n", #nolint
               fixed = TRUE)
  expect_equal(is_int(42.2)$x, 42.2)
})

test_that("is_lgl has correct behavior", {
  expect_s3_class(is_lgl(TRUE), "check")
  expect_s3_class(is_lgl(42), "check")
  expect_null(is_lgl(FALSE)$msg)
  expect_equal(is_lgl(FALSE)$x, FALSE)
  expect_equal(is_lgl(42)$msg,
               "The input value \033[34m42\033[39m is not of type \033[32mlogical\033[39m, but of type \033[31mdouble\033[39m.\n", #nolint
               fixed = TRUE)
  expect_equal(is_lgl(42.2)$x, 42.2)
})

test_that("is_list has correct behavior", {
  expect_s3_class(is_list(mtcars), "check")
  expect_s3_class(is_list(42), "check")
  expect_null(is_list(mtcars)$msg)
  expect_equal(is_list(mtcars)$x, mtcars)
  expect_equal(is_list(42)$msg,
               "The input value \033[34m42\033[39m is not of type \033[32mlist\033[39m, but of type \033[31mdouble\033[39m.\n", #nolint
               fixed = TRUE)
  expect_equal(is_list(42.2)$x, 42.2)
})

