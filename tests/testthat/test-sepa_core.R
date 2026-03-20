make_small_data <- function(n = 80, seed = 1) {
  d  <- simulate_sepa_fake_wj(n = n, seed = seed)
  X  <- as.matrix(d[, c("LT","ST","CP","AP","VP","CK","FR")])
  colnames(X)[colnames(X) == "FR"] <- "FL"
  Xs <- X - rowMeans(X)
  sv <- svd(Xs)
  B  <- sv$v[, 1:4, drop = FALSE];  rownames(B) <- colnames(X)
  F  <- sv$u[, 1:4, drop = FALSE] %*% diag(sv$d[1:4])
  list(X = X, Xs = Xs, B = B, F = F, n = n, p = 7L, pid = d$ID)
}

test_that("sepa_stats_all returns a list with correct names", {
  obj <- make_small_data()
  res <- sepa_stats_all(obj$B, obj$F, pid = obj$pid)
  expect_named(res, c("rho", "C_all", "C_plane"))
})

test_that("sepa_stats_all rho has correct dimensions", {
  obj <- make_small_data(n = 60)
  res <- sepa_stats_all(obj$B, obj$F, pid = obj$pid)
  expect_equal(nrow(res$rho), 60L * 2L)   # 2 planes
  expect_true(all(c("id","plane","rho") %in% names(res$rho)))
})

test_that("direction cosines are in [-1, 1]", {
  obj <- make_small_data()
  res <- sepa_stats_all(obj$B, obj$F)
  expect_true(all(res$C_all$C_all >= -1 - 1e-10))
  expect_true(all(res$C_all$C_all <=  1 + 1e-10))
  expect_true(all(res$C_plane$C_plane >= -1 - 1e-10))
  expect_true(all(res$C_plane$C_plane <=  1 + 1e-10))
})

test_that("parallel_analysis_ipsatized returns correct structure", {
  obj <- make_small_data()
  pa  <- parallel_analysis_ipsatized(obj$Xs, B = 20L, Kmax = 5L, seed = 1L)
  expect_named(pa, c("sig_dims", "eig_obs", "thr"))
  expect_length(pa$eig_obs, 5L)
  expect_length(pa$thr, 5L)
})

test_that("percentile_ci_mat output shape is correct", {
  set.seed(1)
  M   <- matrix(stats::rnorm(500 * 3), 500, 3)
  out <- percentile_ci_mat(M, level = 0.90)
  expect_equal(dim(out), c(3L, 2L))
  expect_true(all(out[, 1] < out[, 2]))
})
