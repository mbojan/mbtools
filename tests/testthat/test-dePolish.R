test_that("dePolish() just works", {
  x <- .readLetterspl("utf-8")
  expect_silent(
    r <- dePolish(x$nat)
  )
  expect_identical(r, x$ascii)
})
