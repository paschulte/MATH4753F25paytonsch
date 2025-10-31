test_that("mu is returned correctly", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$mu, 10)
})

test_that("sigma is returned correctly", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$sigma, 5)
})

test_that("probability is between 0 and 1", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)
  expect_true(result$probability >= 0 && result$probability <= 1)
})
