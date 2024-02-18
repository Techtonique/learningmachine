test_that(desc = "multiplication works", code = {
  cat("\n")
  print("1 - Running test_that multiplication ----------")
  testthat::expect_equal(object = 2 * 2, expected = 4)
})


test_that(desc = "sum works", code = {
  cat("\n")
  print("2 - Running test_that sum ----------")
  testthat::expect_equal(object = 2 + 2, expected = 4)
})