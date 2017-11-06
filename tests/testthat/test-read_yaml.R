context("Testing read_yaml() and co")

test_that("it works", {
  # Some YAML
  y <- "num: 1
key0:
  key1: value1
  key2:
    key22: value22
rcode_num: !r 1:5
rcode_ch: !r paste(LETTERS[1:10], collapse=', ')
date: !r Sys.Date()
"

expect_silent(
  read_yaml(y)
)
})
