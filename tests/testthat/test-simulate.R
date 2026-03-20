test_that("simulate_sepa_fake_wj returns correct dimensions", {
  d <- simulate_sepa_fake_wj(n = 50, seed = 1)
  expect_equal(nrow(d), 50)
  expect_equal(ncol(d), 8L)   # ID + 7 domains
  expect_true("ID" %in% names(d))
})

test_that("simulate_sepa_fake_wj column names are correct", {
  d <- simulate_sepa_fake_wj(n = 10, seed = 2)
  expect_equal(names(d), c("ID", "LT", "ST", "CP", "AP", "VP", "CK", "FR"))
})

test_that("simulate_sepa_fake_wj is reproducible", {
  d1 <- simulate_sepa_fake_wj(n = 100, seed = 42)
  d2 <- simulate_sepa_fake_wj(n = 100, seed = 42)
  expect_identical(d1, d2)
})

test_that("simulate_sepa_fake_wj B_loadings attribute is orthonormal", {
  d <- simulate_sepa_fake_wj(n = 20, seed = 7)
  B <- attr(d, "B_loadings")
  expect_equal(dim(B), c(7L, 4L))
  # Q^T Q = I_4
  expect_equal(t(B) %*% B, diag(4), tolerance = 1e-10)
})
