context("Checking recale")

test_that("rescale works",
          {
            x <- 1:5
            r <- rescale(x, 0, 1)
            expect_equal( range(r), c(0, 1) )
          } )
