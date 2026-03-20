## ============================================================
## SEPA core statistics
## ============================================================

#' Compute SEPA statistics: plane-fit rho and direction cosines
#'
#' Given reference loading vectors \code{B_ref} and person score matrix
#' \code{F_ref} from a row-isometric SVD biplot, computes for every person:
#' \itemize{
#'   \item the plane-fit correlation \eqn{\rho} in each plane,
#'   \item direction cosines with every domain in the full K-dimensional space,
#'   \item direction cosines within each plane.
#' }
#'
#' @param B_ref  Numeric matrix \eqn{p \times K}.  Domain loading vectors
#'   (right singular vectors from the ipsatized data SVD).
#' @param F_ref  Numeric matrix \eqn{n \times K}.  Person score coordinates
#'   (left singular vectors scaled by singular values: \eqn{U \, \text{diag}(d)}).
#' @param planes List of integer vectors, each of length 2, specifying which
#'   pair of dimensions defines a plane.  Default
#'   \code{list(c(1, 2), c(3, 4))}.
#' @param pid    Optional integer or character vector of length \eqn{n}
#'   providing person IDs.  Defaults to \code{1:n}.
#'
#' @return A named list with three elements, each a tidy data frame:
#'   \describe{
#'     \item{\code{rho}}{Columns: \code{id}, \code{plane}, \code{rho}.}
#'     \item{\code{C_all}}{Columns: \code{id}, \code{domain}, \code{C_all}.
#'       Direction cosines across all \eqn{K} dimensions.}
#'     \item{\code{C_plane}}{Columns: \code{id}, \code{domain},
#'       \code{C_plane}, \code{plane}.  Per-plane cosines.}
#'   }
#'
#' @examples
#' X  <- as.matrix(fake_wj[1:200, c("LT","ST","CP","AP","VP","CK","FR")])
#' Xs <- X - rowMeans(X)
#' sv <- svd(Xs)
#' B  <- sv$v[, 1:4]; F <- sv$u[, 1:4] %*% diag(sv$d[1:4])
#' rownames(B) <- c("LT","ST","CP","AP","VP","CK","FR")
#' res <- sepa_stats_all(B, F)
#' head(res$rho)
#'
#' @export
sepa_stats_all <- function(B_ref, F_ref,
                            planes = list(c(1L, 2L), c(3L, 4L)),
                            pid    = NULL) {
  stopifnot(
    is.matrix(B_ref), is.matrix(F_ref),
    ncol(B_ref) == ncol(F_ref)
  )
  n_loc <- nrow(F_ref)
  p_loc <- nrow(B_ref)
  if (is.null(pid)) pid <- seq_len(n_loc)

  ## Overall direction cosines across all K dims (n x p)
  C_all <- person_domain_cosines(F_ref, B_ref)

  ## Per-plane statistics
  plane_out <- lapply(seq_along(planes), function(r) {
    idx <- planes[[r]]
    B_r <- B_ref[, idx, drop = FALSE]
    F_r <- F_ref[, idx, drop = FALSE]
    S_r <- F_r %*% t(B_r)
    w_r <- signed_w(B_r)
    list(idx = idx,
         rho = rowwise_cor(S_r, w_r),
         C_r = person_domain_cosines(F_r, B_r))
  })

  ## rho long table: one row per person x plane
  rho_tbl <- do.call(rbind, lapply(plane_out, function(x) {
    data.frame(id    = pid,
               plane = paste0("(", paste(x$idx, collapse = ","), ")"),
               rho   = x$rho,
               stringsAsFactors = FALSE)
  }))

  ## C_all long: person x domain across all dims
  C_all_df   <- cbind(id = pid, as.data.frame(C_all, check.names = FALSE))
  C_all_long <- stats::reshape(
    C_all_df,
    varying   = 2:(p_loc + 1L),
    v.names   = "C_all",
    timevar   = "domain",
    times     = safe_names(C_all, "D"),
    direction = "long"
  )[, c("id", "domain", "C_all")]
  rownames(C_all_long) <- NULL

  ## C_plane long: person x domain x plane
  C_plane_long <- do.call(rbind, lapply(plane_out, function(x) {
    tmp  <- cbind(id = pid, as.data.frame(x$C_r, check.names = FALSE))
    long <- stats::reshape(
      tmp,
      varying   = 2:(p_loc + 1L),
      v.names   = "C_plane",
      timevar   = "domain",
      times     = safe_names(x$C_r, "D"),
      direction = "long"
    )[, c("id", "domain", "C_plane")]
    long$plane <- paste0("(", paste(x$idx, collapse = ","), ")")
    rownames(long) <- NULL
    long
  }))

  list(rho     = rho_tbl,
       C_all   = C_all_long,
       C_plane = C_plane_long)
}
