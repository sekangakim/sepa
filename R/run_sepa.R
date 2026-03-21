## ============================================================
## High-level SEPA analysis wrapper
## ============================================================

#' Run a complete SEPA analysis
#'
#' Convenience wrapper that executes the full Subprofile Extraction via Pattern
#' Analysis (SEPA) pipeline on a matrix of domain scores.  The function
#' ipsatizes the data, fits a rank-\eqn{K} row-isometric SVD biplot, computes
#' SEPA statistics (plane-fit rho and direction cosines), runs parallel
#' analysis, bootstraps domain coordinates with BCa confidence intervals, and
#' bootstraps per-person cosines with percentile confidence intervals.
#'
#' @param data           A numeric matrix or data frame of domain scores.
#'   Rows are persons; columns are domains.  An optional column named
#'   \code{"ID"} is used as the person identifier and removed before analysis.
#' @param K              Integer. Number of SVD dimensions to retain.
#'   Default \code{4L}.
#' @param target_ids     Optional vector of person IDs (matched against the
#'   \code{ID} column or row position) for which per-person exemplar tables
#'   are assembled.  \code{NULL} disables exemplar output.  Default
#'   \code{NULL}.
#' @param B_dom          Integer. Bootstrap replicates for domain-coordinate
#'   CIs.  Default \code{2000L}.
#' @param B_cos          Integer. Bootstrap replicates for per-person cosine
#'   CIs.  Default \code{2000L}.
#' @param alpha_ci       Numeric confidence level.  Default \code{0.95}.
#' @param seed           Integer random seed.  Default \code{20251003L}.
#' @param pa_B           Integer. Permutation replicates for parallel
#'   analysis.  Default \code{2000L}.
#' @param use_parallel   Logical. Use parallel processing for the bootstrap?
#'   Default \code{FALSE}.
#' @param ncores         Integer or \code{NULL}.  Number of cores.
#'   \code{NULL} uses \code{max(1, detectCores() - 1)}.  Default \code{NULL}.
#' @param run_pa         Logical. Run parallel analysis?  Default \code{TRUE}.
#' @param run_boot_dom   Logical. Run domain-coordinate bootstrap?
#'   Default \code{TRUE}.
#' @param run_boot_cos   Logical. Run per-person cosine bootstrap?
#'   Ignored unless \code{!is.null(target_ids)}.  Default \code{TRUE}.
#' @param verbose        Logical. Print progress messages?  Default \code{TRUE}.
#'
#' @return A named list of class \code{"sepa_result"} containing:
#'   \describe{
#'     \item{\code{call}}{The matched call.}
#'     \item{\code{domains}}{Character vector of domain names.}
#'     \item{\code{pid}}{Person ID vector.}
#'     \item{\code{n}, \code{p}, \code{K}}{Dimensions used.}
#'     \item{\code{ref_fit}}{List with \code{F} (\eqn{n \times K}),
#'       \code{B} (\eqn{p \times K}), \code{d} (singular values), \code{U},
#'       \code{V} — the reference row-isometric SVD.}
#'     \item{\code{Xstar}}{Ipsatized data matrix.}
#'     \item{\code{sepa_stats}}{Output of \code{\link{sepa_stats_all}}:
#'       \code{rho}, \code{C_all}, \code{C_plane}.}
#'     \item{\code{pa}}{Output of
#'       \code{\link{parallel_analysis_ipsatized}}, or \code{NULL}.}
#'     \item{\code{boot_dom}}{Raw \code{boot} object for domain coordinates,
#'       or \code{NULL}.}
#'     \item{\code{dom_coords}}{Data frame of domain coordinates with BCa CIs,
#'       or \code{NULL}.}
#'     \item{\code{len2}}{Data frame of \eqn{\|b_j\|^2} with BCa CIs and
#'       marker flag, or \code{NULL}.}
#'     \item{\code{boot_cos}}{Raw \code{boot} object for per-person cosines,
#'       or \code{NULL}.}
#'     \item{\code{cosine_tables}}{Named list of data frames (one per plane
#'       plus \code{"all"}) with point estimates and percentile CIs for the
#'       persons in \code{target_ids}, or \code{NULL}.}
#'     \item{\code{dom_dom_cosines}}{List with \code{plane12} and
#'       \code{plane34} data frames of domain–domain cosines, or \code{NULL}.}
#'     \item{\code{norms}}{Data frame with \eqn{\|F_i^{(r)}\|} for exemplar
#'       persons, or \code{NULL}.}
#'     \item{\code{rho_exemplar}}{Data frame with plane-fit rho for exemplar
#'       persons, or \code{NULL}.}
#'   }
#'
#' @examples
#' \donttest{
#' res <- run_sepa(
#'   data         = fake_wj,
#'   K            = 4L,
#'   target_ids   = c(724, 944),
#'   B_dom        = 200L,
#'   B_cos        = 200L,
#'   seed         = 1L,
#'   pa_B         = 100L,
#'   run_boot_cos = TRUE,
#'   verbose      = TRUE
#' )
#' head(res$sepa_stats$rho)
#' res$pa$sig_dims
#' }
#'
#' @export
run_sepa <- function(data,
                      K            = 4L,
                      target_ids   = NULL,
                      B_dom        = 2000L,
                      B_cos        = 2000L,
                      alpha_ci     = 0.95,
                      seed         = 20251003L,
                      pa_B         = 2000L,
                      use_parallel = FALSE,
                      ncores       = NULL,
                      run_pa       = TRUE,
                      run_boot_dom = TRUE,
                      run_boot_cos = TRUE,
                      verbose      = TRUE) {

  mc <- match.call()

  ## ---- 0. Resolve parallel settings --------------------------------
  if (use_parallel) {
    if (is.null(ncores))
      ncores <- max(1L, parallel::detectCores() - 1L)
    parallel_type <- if (.Platform$OS.type == "windows") "snow" else "multicore"
  } else {
    ncores        <- 1L
    parallel_type <- "no"
  }

  ## ---- 1. Prepare data -------------------------------------------
  data <- as.data.frame(data)
  pid  <- if ("ID" %in% names(data)) data$ID else seq_len(nrow(data))
  X    <- as.matrix(data[, setdiff(names(data), "ID"), drop = FALSE])

  ## Standardise FR -> FL if needed
  if ("FR" %in% colnames(X) && !("FL" %in% colnames(X)))
    colnames(X)[colnames(X) == "FR"] <- "FL"

  target_cols <- c("LT", "ST", "CP", "AP", "VP", "CK", "FL")
  common      <- intersect(colnames(X), target_cols)
  if (length(common) < 7L)
    stop("Expecting 7 WJ-IV domain columns.  Found: ",
         paste(common, collapse = ", "))
  X       <- X[, target_cols, drop = FALSE]
  domains <- colnames(X)
  n       <- nrow(X)
  p       <- ncol(X)

  if (verbose) {
    cat(sprintf("SEPA: n = %d, p = %d, K = %d\n", n, p, K))
  }

  ## ---- 2. Ipsatize & reference SVD --------------------------------
  set.seed(seed)
  Xstar   <- ipsatize(X)
  ref_fit <- svd_row_isometric(Xstar, K = K)
  B_ref   <- ref_fit$B
  F_ref   <- ref_fit$F
  rownames(B_ref) <- domains

  ## Expose full U/V for biplot helper
  sv_raw        <- svd(Xstar)
  ref_fit$U     <- sv_raw$u
  ref_fit$V     <- sv_raw$v
  ref_fit$d_all <- sv_raw$d

  ## ---- 3. SEPA statistics -----------------------------------------
  if (verbose) cat("Computing SEPA statistics...\n")
  sepa_res <- sepa_stats_all(B_ref, F_ref,
                              planes = list(c(1L, 2L), c(3L, 4L)),
                              pid    = pid)

  ## ---- 4. Parallel analysis ----------------------------------------
  pa <- NULL
  if (run_pa) {
    if (verbose) cat(sprintf("Running parallel analysis (B = %d)...\n", pa_B))
    pa <- parallel_analysis_ipsatized(Xstar, B = pa_B, Kmax = 10L,
                                       conf = alpha_ci, seed = seed + 1L)
    if (verbose)
      cat("PA significant dims:", paste(pa$sig_dims, collapse = ", "), "\n")
  }

  ## ---- 5. Bootstrap domain coordinates (BCa) ----------------------
  boot_dom   <- NULL
  dom_coords <- NULL
  len2_df    <- NULL

  if (run_boot_dom) {
    if (verbose) cat(sprintf("Bootstrapping domain coordinates (B = %d)...\n", B_dom))
    boot_dom <- run_boot_internal(
      data          = X,
      fun           = function(d, idx) stat_domain(d, idx, ref_B = B_ref, K = K),
      R             = B_dom,
      use_parallel  = use_parallel,
      ncores        = ncores,
      parallel_type = parallel_type
    )

    dom_coord_index <- function(j, k) (j - 1L) * K + k
    len2_index      <- function(j)    (K * p) + j

    idx_all  <- seq_len(K * p + p)
    cis_dom  <- boot_cis_all(boot_dom, type = c("bca", "perc"),
                              level = alpha_ci, idx_vec = idx_all)
    ests     <- colMeans(boot_dom$t)

    dom_coords <- data.frame(
      domain = rep(domains, each = K),
      dim    = rep(seq_len(K), times = p),
      index  = mapply(dom_coord_index,
                      rep(seq_len(p), each = K), rep(seq_len(K), times = p)),
      est    = ests[seq_len(K * p)],
      stringsAsFactors = FALSE
    )
    ci_map <- cis_dom[cis_dom$index %in% dom_coords$index, ]
    dom_coords$lwr <- ci_map$lwr[match(dom_coords$index, ci_map$index)]
    dom_coords$upr <- ci_map$upr[match(dom_coords$index, ci_map$index)]

    len2_df <- data.frame(
      domain = domains,
      index  = sapply(seq_len(p), len2_index),
      est    = ests[(K * p + 1L):(K * p + p)],
      stringsAsFactors = FALSE
    )
    ci_len2           <- cis_dom[cis_dom$index %in% len2_df$index, ]
    len2_df$lwr       <- ci_len2$lwr[match(len2_df$index, ci_len2$index)]
    len2_df$upr       <- ci_len2$upr[match(len2_df$index, ci_len2$index)]
    marker_thr        <- K / p
    len2_df$marker_flag <- len2_df$lwr > marker_thr
    if (verbose) {
      cat(sprintf("\n=== Domain ||b||^2 BCa %g%%; marker threshold K/p = %.3f ===\n",
                  alpha_ci * 100, marker_thr))
      print(len2_df[, c("domain", "est", "lwr", "upr", "marker_flag")])
    }
  }

  ## ---- 6. Bootstrap per-person cosines (percentile) ---------------
  boot_cos      <- NULL
  cosine_tables <- NULL
  norms_df      <- NULL
  rho_ex_df     <- NULL
  dom_dom_cos   <- NULL

  if (!is.null(target_ids)) {
    ## ID -> row index
    id_to_index <- function(id) which(pid == id)[1L]
    idx_sel     <- sapply(target_ids, id_to_index)
    bad         <- is.na(idx_sel)
    if (any(bad))
      stop("IDs not found: ",
           paste(target_ids[bad], collapse = ", "))

    ## Reference cosines
    B12_ref  <- B_ref[, 1:2, drop = FALSE]
    B34_ref  <- B_ref[, 3:4, drop = FALSE]
    C12_ref  <- person_domain_cosines(Xstar %*% B12_ref, B12_ref)
    C34_ref  <- person_domain_cosines(Xstar %*% B34_ref, B34_ref)
    Call_ref <- person_domain_cosines(F_ref, B_ref)

    ## Profile norms
    norm_p1 <- sqrt(rowSums((Xstar %*% B12_ref)^2))
    norm_p2 <- sqrt(rowSums((Xstar %*% B34_ref)^2))
    norms_df <- data.frame(
      ID          = target_ids,
      Norm_Plane1 = round(norm_p1[idx_sel], 4L),
      Norm_Plane2 = round(norm_p2[idx_sel], 4L),
      stringsAsFactors = FALSE
    )

    ## Rho for exemplars
    rho_p1   <- rho_from(Xstar %*% B12_ref, B12_ref)
    rho_p2   <- rho_from(Xstar %*% B34_ref, B34_ref)
    rho_ex_df <- data.frame(
      ID         = target_ids,
      Rho_Plane1 = round(rho_p1[idx_sel], 3L),
      Rho_Plane2 = round(rho_p2[idx_sel], 3L),
      stringsAsFactors = FALSE
    )

    ## Domain–domain cosines
    if (!is.null(boot_dom)) {
      upper_pairs <- which(upper.tri(diag(p)), arr.ind = TRUE)
      pair_labels <- apply(upper_pairs, 1L, function(a)
        paste0(domains[a[1L]], "-", domains[a[2L]]))

      cos12_point <- cos_pairs_from_B(B12_ref)
      cos34_point <- cos_pairs_from_B(B34_ref)
      cos12_boot  <- t(apply(boot_dom$t[, seq_len(K * p), drop = FALSE],
                              1L, function(v) {
        B <- reconstruct_B_from_vec(v); cos_pairs_from_B(B$B12)
      }))
      cos34_boot  <- t(apply(boot_dom$t[, seq_len(K * p), drop = FALSE],
                              1L, function(v) {
        B <- reconstruct_B_from_vec(v); cos_pairs_from_B(B$B34)
      }))
      ci12 <- percentile_ci_mat(cos12_boot, level = alpha_ci)
      ci34 <- percentile_ci_mat(cos34_boot, level = alpha_ci)
      dom_dom_cos <- list(
        plane12 = data.frame(Pair   = pair_labels,
                              Cosine = cos12_point,
                              CI_Lwr = ci12[, 1L],
                              CI_Upr = ci12[, 2L],
                              stringsAsFactors = FALSE),
        plane34 = data.frame(Pair   = pair_labels,
                              Cosine = cos34_point,
                              CI_Lwr = ci34[, 1L],
                              CI_Upr = ci34[, 2L],
                              stringsAsFactors = FALSE)
      )
    }

    ## Per-person cosine bootstrap
    if (run_boot_cos) {
      if (verbose)
        cat(sprintf("Bootstrapping per-person cosines (B = %d)...\n", B_cos))

      Xstar_full <- Xstar
      boot_cos   <- run_boot_internal(
        data          = X,
        fun           = function(d, idx)
                          stat_person_cos(d, idx, ref_fit = ref_fit,
                                          Xstar_full = Xstar_full, K = K),
        R             = B_cos,
        use_parallel  = use_parallel,
        ncores        = ncores,
        parallel_type = parallel_type
      )
      stopifnot(ncol(boot_cos$t) == 3L * n * p)

      extract_row_cis <- function(person_row_index,
                                   plane = c("12", "34", "all")) {
        plane <- match.arg(plane)
        idxs  <- sapply(seq_len(p), function(j)
          cos_index(person_row_index, j, plane, n, p))
        percentile_ci_mat(boot_cos$t[, idxs, drop = FALSE],
                           level = alpha_ci)
      }

      build_plane_table <- function(idx_vec_rows, ids, which_plane) {
        plane       <- match.arg(which_plane, c("12", "34", "all"))
        est_mat     <- switch(plane,
                              "12"  = C12_ref,
                              "34"  = C34_ref,
                              "all" = Call_ref)
        plane_label <- switch(plane,
                              "12"  = "Plane 1 (Dims 1-2)",
                              "34"  = "Plane 2 (Dims 3-4)",
                              "all" = "All retained dims (Dims 1-4)")
        tbls <- lapply(seq_along(idx_vec_rows), function(k) {
          i   <- idx_vec_rows[k]; id <- ids[k]
          cis <- extract_row_cis(i, plane)
          data.frame(ID     = id,
                     Domain = domains,
                     Plane  = plane_label,
                     Cosine = as.numeric(est_mat[i, ]),
                     CI_Lwr = as.numeric(cis[, 1L]),
                     CI_Upr = as.numeric(cis[, 2L]),
                     stringsAsFactors = FALSE)
        })
        do.call(rbind, tbls)
      }

      cosine_tables <- stats::setNames(
        lapply(c("12", "34", "all"), function(pl)
          build_plane_table(idx_sel, target_ids, pl)),
        c("12", "34", "all")
      )
    }
  }

  if (verbose) cat("Done.\n")

  structure(
    list(
      call          = mc,
      domains       = domains,
      pid           = pid,
      n             = n,
      p             = p,
      K             = K,
      ref_fit       = ref_fit,
      Xstar         = Xstar,
      sepa_stats    = sepa_res,
      pa            = pa,
      boot_dom      = boot_dom,
      dom_coords    = dom_coords,
      len2          = len2_df,
      boot_cos      = boot_cos,
      cosine_tables = cosine_tables,
      dom_dom_cosines = dom_dom_cos,
      norms         = norms_df,
      rho_exemplar  = rho_ex_df
    ),
    class = "sepa_result"
  )
}

#' Print method for sepa_result objects
#'
#' @param x   A \code{sepa_result} object.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}, the \code{sepa_result} object passed in.
#'   Called primarily for its side effect of printing a compact summary to the
#'   console, including sample size, number of domains, number of dimensions,
#'   parallel-analysis significant dimensions, and marker domains.
#' @export
print.sepa_result <- function(x, ...) {
  cat("SEPA result\n")
  cat(sprintf("  n = %d  p = %d  K = %d\n", x$n, x$p, x$K))
  cat("  Domains:", paste(x$domains, collapse = ", "), "\n")
  if (!is.null(x$pa))
    cat("  PA significant dims:",
        paste(x$pa$sig_dims, collapse = ", "), "\n")
  if (!is.null(x$len2)) {
    markers <- x$len2$domain[x$len2$marker_flag]
    cat("  Marker domains:", if (length(markers)) paste(markers, collapse = ", ")
                             else "none", "\n")
  }
  invisible(x)
}
