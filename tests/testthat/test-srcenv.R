context("Testing srcenv()")


test_that("srcenv() works", {
  # Create a test R file
  testfile <- tempfile()
  cat(
    "a <- 1", 
    "l <- list(a=1, b=2)",
    sep="\n",
    file=testfile,
    append=FALSE
  )

  expect_silent(
    e <- srcenv(testfile)
  )
  
  expect_identical(
    e$a, 1
  )
  
  expect_identical(
    e$l, list(a=1, b=2)
  )
  
  })

