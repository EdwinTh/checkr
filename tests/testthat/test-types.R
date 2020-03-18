context("type checks work as expected")

test_that("is_chr_type has correct behavior", {
  expect_s3_class(is_chr_type("a"), "strongr_check")
  expect_s3_class(is_chr_type(1235), "strongr_check")
  expect_null(is_chr_type("a")$msg)
  expect_equal(is_chr_type("a")$x, "a")
  expect_equal(is_chr_type(1235)$msg,
               "The input \033[34m1235\033[39m is not of type \033[32mcharacter\033[39m, but of type \033[31mdouble\033[39m.\n", #nolint
               fixed = TRUE)
  expect_equal(is_chr_type(1235)$x, 1235)
})

test_that("is_dbl_type has correct behavior", {
  expect_s3_class(is_dbl_type(42.2), "strongr_check")
  expect_s3_class(is_dbl_type(TRUE), "strongr_check")
  expect_null(is_dbl_type(42.2)$msg)
  expect_equal(is_dbl_type(42)$x, 42)
  expect_equal(is_dbl_type(TRUE)$msg,
               "The input \033[34mTRUE\033[39m is not of type \033[32mdouble\033[39m, but of type \033[31mlogical\033[39m.\n", #nolint
               fixed = TRUE)
  expect_equal(is_dbl_type(TRUE)$x, TRUE)
})

test_that("is_int_type has correct behavior", {
  expect_s3_class(is_int_type(42L), "strongr_check")
  expect_s3_class(is_int_type(42), "strongr_check")
  expect_null(is_int_type(42L)$msg)
  expect_equal(is_int_type(42L)$x, 42L)
  expect_equal(is_int_type(42)$msg,
               "The input \033[34m42\033[39m is not of type \033[32minteger\033[39m, but of type \033[31mdouble\033[39m.\n", #nolint
               fixed = TRUE)
  expect_equal(is_int_type(42.2)$x, 42.2)
})

test_that("is_lgl_type has correct behavior", {
  expect_s3_class(is_lgl_type(TRUE), "strongr_check")
  expect_s3_class(is_lgl_type(42), "strongr_check")
  expect_null(is_lgl_type(FALSE)$msg)
  expect_equal(is_lgl_type(FALSE)$x, FALSE)
  expect_equal(is_lgl_type(42)$msg,
               "The input \033[34m42\033[39m is not of type \033[32mlogical\033[39m, but of type \033[31mdouble\033[39m.\n", #nolint
               fixed = TRUE)
  expect_equal(is_lgl_type(42.2)$x, 42.2)
})

test_that("is_list_type has correct behavior", {
  expect_s3_class(is_list_type(mtcars), "strongr_check")
  expect_s3_class(is_list_type(42), "strongr_check")
  expect_null(is_list_type(mtcars)$msg)
  expect_equal(is_list_type(mtcars)$x, mtcars)
  expect_equal(is_list_type(42)$msg,
               "The input \033[34m42\033[39m is not of type \033[32mlist\033[39m, but of type \033[31mdouble\033[39m.\n", #nolint
               fixed = TRUE)
  expect_equal(is_list_type(42.2)$x, 42.2)
})

context("is_type_base internal function", {

})
