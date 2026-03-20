## ============================================================
## Bootstrap confidence interval helpers
## ============================================================

#' BCa (with percentile fallback) confidence intervals for all bootstrap indices
#'
#' Loops over columns of a \code{boot} object and calls
#' \code{\link[boot]{boot.ci}} for each, returning a tidy data frame.  Falls
#' back to percentile intervals if the BCa calculation fails.
#'
#' @param boot_obj  An object of class \code{"boot"} returned by
#'   \code{\link[boot]{boot}}.
#' @param type      Character vector passed to \code{boot.ci}'s \code{type}
#'   argument.  Default \code{c("bca", "perc")}.
#' @param level     Numeric confidence level.  Default \code{0.95}.
#' @param idx_vec   Integer vector of column indices to process.  Defaults to
#'   all columns of \code{boot_obj$t}.
#'
#' @return A data frame with columns \code{index}, \code{lwr}, \code{upr},
#'   and \code{method} (one row per element of \code{idx_vec}).
#'
#' @examples
#' \dontrun{
#' # See run_sepa() for an end-to-end example
#' }
#'
#' @export
boot_cis_all <- function(boot_obj, type = c("bca", "perc"),
                          level = 0.95, idx_vec = NULL) {
  stopifnot(inherits(boot_obj, "boot"))
  if (is.null(idx_vec)) idx_vec <- seq_len(ncol(boot_obj$t))
  res <- lapply(idx_vec, function(k) {
    ci_k <- tryCatch(
      boot::boot.ci(boot_obj, type = type, conf = level, index = k),
      error = function(e) NULL
    )
    if (is.null(ci_k)) {
      data.frame(index = k, lwr = NA_real_, upr = NA_real_,
                 method = paste(type, collapse = ","),
                 stringsAsFactors = FALSE)
    } else if (!is.null(ci_k$bca)) {
      data.frame(index = k, lwr = ci_k$bca[4],
                 upr = ci_k$bca[5], method = "bca",
                 stringsAsFactors = FALSE)
    } else if (!is.null(ci_k$percent)) {
      data.frame(index = k, lwr = ci_k$percent[4],
                 upr = ci_k$percent[5], method = "perc",
                 stringsAsFactors = FALSE)
    } else {
      data.frame(index = k, lwr = NA_real_, upr = NA_real_,
                 method = "NA", stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, res)
}

#' Percentile confidence intervals from a matrix of bootstrap draws
#'
#' @param M      Numeric matrix with bootstrap replicates in rows and
#'   statistics in columns.
#' @param level  Numeric confidence level.  Default \code{0.95}.
#'
#' @return A two-column matrix with columns \code{qlo} and \code{qhi},
#'   one row per column of \code{M}.
#'
#' @examples
#' set.seed(1)
#' M <- matrix(rnorm(1000 * 5), 1000, 5)
#' percentile_ci_mat(M, level = 0.95)
#'
#' @export
percentile_ci_mat <- function(M, level = 0.95) {
  stopifnot(is.numeric(M), is.matrix(M))
  alpha <- 1 - level
  qlo   <- apply(M, 2L, stats::quantile, probs = alpha / 2,
                 na.rm = TRUE, type = 6L)
  qhi   <- apply(M, 2L, stats::quantile, probs = 1 - alpha / 2,
                 na.rm = TRUE, type = 6L)
  cbind(qlo = qlo, qhi = qhi)
}

## ----------------------------------------------------------
## Internal statistic for domain-coordinate bootstrap
## ----------------------------------------------------------
#' @keywords internal
stat_domain <- function(data, idx, ref_B, K = 4L) {
  Xb     <- ipsatize(data[idx, , drop = FALSE])
  fit    <- svd_row_isometric(Xb, K = K)
  B_boot <- fit$B
  R12    <- procrustes2(B_boot[, 1:2, drop = FALSE],
                        ref_B[, 1:2, drop = FALSE])
  R34    <- procrustes2(B_boot[, 3:4, drop = FALSE],
                        ref_B[, 3:4, drop = FALSE])
  B12    <- B_boot[, 1:2, drop = FALSE] %*% R12
  B34    <- B_boot[, 3:4, drop = FALSE] %*% R34
  B_al   <- cbind(B12, B34)
  b_vec  <- as.vector(t(B_al))   # domain-major: (j1 d1..d4, j2 d1..d4, ...)
  len2   <- rowSums(B_al^2)
  c(b_vec, len2)
}

## ----------------------------------------------------------
## Internal statistic for per-person cosine bootstrap
## ----------------------------------------------------------
#' @keywords internal
stat_person_cos <- function(data, idx, ref_fit, Xstar_full, K = 4L) {
  Xb     <- ipsatize(data[idx, , drop = FALSE])
  fit    <- svd_row_isometric(Xb, K = K)
  B_boot <- fit$B
  R12    <- procrustes2(B_boot[, 1:2, drop = FALSE],
                        ref_fit$B[, 1:2, drop = FALSE])
  R34    <- procrustes2(B_boot[, 3:4, drop = FALSE],
                        ref_fit$B[, 3:4, drop = FALSE])
  B12    <- B_boot[, 1:2, drop = FALSE] %*% R12
  B34    <- B_boot[, 3:4, drop = FALSE] %*% R34
  F12    <- Xstar_full %*% B12
  F34    <- Xstar_full %*% B34
  C12    <- person_domain_cosines(F12, B12)
  C34    <- person_domain_cosines(F34, B34)
  B_all  <- cbind(B12, B34)
  F_all  <- Xstar_full %*% B_all
  Call   <- person_domain_cosines(F_all, B_all)
  c(as.vector(C12), as.vector(C34), as.vector(Call))
}
