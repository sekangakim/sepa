## ============================================================
## Parallel Analysis for ipsatized data
## ============================================================

#' Parallel analysis for ipsatized data
#'
#' Determines the number of statistically significant singular dimensions in an
#' ipsatized score matrix by comparing observed squared singular values to the
#' \code{conf}-quantile of the null distribution obtained by column-permuting
#' and re-ipsatizing the data \code{B} times.
#'
#' @param Xstar  Numeric matrix. Ipsatized (row-mean-centered) data,
#'   \eqn{n \times p}.
#' @param B      Integer. Number of permutation replicates.  Default
#'   \code{2000}.
#' @param Kmax   Integer. Maximum number of dimensions to evaluate.
#'   Internally capped at \eqn{\min(n, p)}.  Default \code{10}.
#' @param conf   Numeric in \eqn{(0, 1)}.  Quantile of the null distribution
#'   used as threshold.  Default \code{0.95}.
#' @param seed   Integer random seed.  Default \code{123}.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{\code{sig_dims}}{Integer vector of dimension indices (1-based)
#'       whose observed eigenvalue exceeds the null threshold.}
#'     \item{\code{eig_obs}}{Numeric vector of length \code{Kmax}: observed
#'       squared singular values.}
#'     \item{\code{thr}}{Numeric vector of length \code{Kmax}: permutation
#'       null thresholds at level \code{conf}.}
#'   }
#'
#' @examples
#' X <- simulate_sepa_fake_wj(n = 300, seed = 1)
#' Xs <- X[, c("LT","ST","CP","AP","VP","CK","FR")]
#' Xs <- as.matrix(Xs) - rowMeans(as.matrix(Xs))   # ipsatize
#' pa <- parallel_analysis_ipsatized(Xs, B = 100, Kmax = 6, seed = 42)
#' pa$sig_dims
#'
#' @export
parallel_analysis_ipsatized <- function(Xstar, B = 2000L, Kmax = 10L,
                                         conf = 0.95, seed = 123L) {
  stopifnot(
    is.matrix(Xstar) || is.data.frame(Xstar),
    is.numeric(B), B >= 1L,
    is.numeric(Kmax), Kmax >= 1L,
    is.numeric(conf), conf > 0, conf < 1
  )
  Xstar  <- as.matrix(Xstar)
  n_loc  <- nrow(Xstar)
  p_loc  <- ncol(Xstar)
  Kmax   <- min(c(Kmax, n_loc, p_loc))

  set.seed(seed)
  sv_obs  <- svd(Xstar)$d[seq_len(Kmax)]
  eig_obs <- sv_obs^2

  null_mat <- matrix(NA_real_, nrow = B, ncol = Kmax)
  for (b in seq_len(B)) {
    Xnull <- Xstar
    for (j in seq_len(p_loc)) Xnull[, j] <- sample(Xnull[, j])
    Xnull         <- ipsatize(Xnull)
    null_mat[b, ] <- svd(Xnull)$d[seq_len(Kmax)]^2
  }
  thr <- apply(null_mat, 2L, stats::quantile, probs = conf, na.rm = TRUE)

  list(sig_dims = which(eig_obs > thr),
       eig_obs  = eig_obs,
       thr      = thr)
}
