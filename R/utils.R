## ============================================================
## Internal helpers — not exported
## ============================================================

## ----------------------------------------------------------
## Ipsatization: subtract row mean from every element
## ----------------------------------------------------------
#' @keywords internal
ipsatize <- function(X) X - rowMeans(X, na.rm = TRUE)

## ----------------------------------------------------------
## Row-isometric SVD biplot:  F = U diag(d),  B = V
## ----------------------------------------------------------
#' @keywords internal
svd_row_isometric <- function(X, K = 4L) {
  sv <- svd(X)
  K  <- min(K, length(sv$d))
  U  <- sv$u[, seq_len(K), drop = FALSE]
  d  <- sv$d[seq_len(K)]
  V  <- sv$v[, seq_len(K), drop = FALSE]
  list(F = U %*% diag(d, K, K), d = d, B = V)
}

## ----------------------------------------------------------
## Orthogonal Procrustes rotation (2-column case)
## ----------------------------------------------------------
#' @keywords internal
procrustes2 <- function(X, Y) {
  sv <- svd(t(X) %*% Y)
  sv$u %*% t(sv$v)
}

## ----------------------------------------------------------
## Person–domain cosines in a (sub)space
## ----------------------------------------------------------
#' @keywords internal
person_domain_cosines <- function(F2, B2) {
  Fn <- sqrt(rowSums(F2^2))
  Bn <- sqrt(rowSums(B2^2))
  (F2 %*% t(B2)) / (Fn %o% Bn)
}

## ----------------------------------------------------------
## Column-major cosine indexer  (i = person 1..n, j = domain 1..p)
## ----------------------------------------------------------
#' @keywords internal
cos_index <- function(i, j, plane = c("12", "34", "all"), n, p) {
  plane <- match.arg(plane)
  base  <- switch(plane,
                  "12"  = 0L,
                  "34"  = n * p,
                  "all" = 2L * n * p)
  base + (j - 1L) * n + i
}

## ----------------------------------------------------------
## Row-wise Pearson correlation (each row of M with vector v)
## ----------------------------------------------------------
#' @keywords internal
rowwise_cor <- function(M, v) {
  M        <- as.matrix(M)
  v        <- as.numeric(v)
  M_center <- sweep(M, 1L, rowMeans(M), "-")
  v_center <- v - mean(v)
  num      <- M_center %*% v_center
  den      <- sqrt(rowSums(M_center^2)) * sqrt(sum(v_center^2))
  out      <- as.numeric(num / den)
  replace(out, !is.finite(out), NA_real_)
}

## ----------------------------------------------------------
## Signed, z-standardised domain-length weight vector for a plane
## ----------------------------------------------------------
#' @keywords internal
signed_w <- function(B_r) {
  len <- sqrt(rowSums(B_r^2))
  sgn <- sign(B_r[, 1L, drop = TRUE])
  as.numeric(scale(sgn * len))
}

## ----------------------------------------------------------
## Safe column-name extractor with fallback
## ----------------------------------------------------------
#' @keywords internal
get_domain_names <- function(B_ref, fallback = NULL) {
  p_loc <- nrow(B_ref)
  if (!is.null(rownames(B_ref)))                       return(rownames(B_ref))
  if (!is.null(fallback) && length(fallback) == p_loc) return(fallback)
  paste0("D", seq_len(p_loc))
}

#' @keywords internal
safe_names <- function(M, prefix = "D") {
  nms <- colnames(M)
  if (is.null(nms)) paste0(prefix, seq_len(ncol(M))) else nms
}

## ----------------------------------------------------------
## Segment profile reconstruction  S = F B^T
## ----------------------------------------------------------
#' @keywords internal
reconstruct_S <- function(B_scope, F_scope) F_scope %*% t(B_scope)

## ----------------------------------------------------------
## Plane-fit rho (correlation of projected profile with weight vector)
## ----------------------------------------------------------
#' @keywords internal
rho_from <- function(F2, B2) {
  w <- as.numeric(scale(sign(B2[, 1L]) * sqrt(rowSums(B2^2))))
  S <- F2 %*% t(B2)
  apply(S, 1L, function(si) stats::cor(si, w))
}

## ----------------------------------------------------------
## Reconstruct B (p x 4) from the 28-element vectorised form
## ----------------------------------------------------------
#' @keywords internal
reconstruct_B_from_vec <- function(v28) {
  B_al <- t(matrix(v28, nrow = 4L, byrow = TRUE))   # p x 4
  list(B12 = B_al[, 1:2, drop = FALSE],
       B34 = B_al[, 3:4, drop = FALSE])
}

## ----------------------------------------------------------
## Domain–domain cosines for upper-triangular pairs
## ----------------------------------------------------------
#' @keywords internal
cos_pairs_from_B <- function(B2) {
  num <- B2 %*% t(B2)
  den <- sqrt(rowSums(B2^2)) %o% sqrt(rowSums(B2^2))
  (num / den)[upper.tri(diag(nrow(B2)))]
}

## ----------------------------------------------------------
## Cross-platform PDF device (Quartz > Cairo > base)
## ----------------------------------------------------------
#' @keywords internal
save_pdf <- function(file, expr, width = 8.2, height = 7.0) {
  opened <- FALSE
  if (identical(Sys.info()[["sysname"]], "Darwin") &&
      isTRUE(capabilities("aqua"))) {
    opened <- tryCatch({
      grDevices::quartz(type = "pdf", file = file,
                        width = width, height = height)
      TRUE
    }, error   = function(e) FALSE,
       warning = function(w) FALSE)
  }
  if (!opened && isTRUE(capabilities("cairo"))) {
    opened <- tryCatch({
      grDevices::cairo_pdf(file, width = width, height = height)
      TRUE
    }, error   = function(e) FALSE,
       warning = function(w) FALSE)
  }
  if (!opened)
    grDevices::pdf(file, width = width, height = height, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  eval.parent(substitute(expr))
}

## ----------------------------------------------------------
## Internal bootstrap runner (avoids global references)
## ----------------------------------------------------------
#' @keywords internal
run_boot_internal <- function(data, fun, R,
                               use_parallel  = FALSE,
                               ncores        = 1L,
                               parallel_type = "no") {
  if (!use_parallel || parallel_type == "no") {
    boot::boot(data = data, statistic = fun, R = R,
               parallel = "no", stype = "i", sim = "ordinary")
  } else if (parallel_type == "snow") {
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    boot::boot(data = data, statistic = fun, R = R,
               parallel = "snow", ncpus = ncores, cl = cl,
               stype = "i", sim = "ordinary")
  } else {
    boot::boot(data = data, statistic = fun, R = R,
               parallel = "multicore", ncpus = ncores,
               stype = "i", sim = "ordinary")
  }
}
