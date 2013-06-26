context("Recoding numeric vectors")

test_that("Recoding with numeric 2x2 rmatrix works", {
          x <- rep(1:4, c(10, 5, 3, 2))
          m <- matrix(1:4, ncol=2, byrow=TRUE)
          r <- recode(x, m)
          expect_true( all( r[ x==1 ] == 2 ) )
          expect_true( all(r[ x==3 ] == 4) )
          # all the others are intact
          i <- !(x %in% m[,1])
          expect_true(all(r[i] == x[i]))
} )
