## ============================================================
## SEPA row-isometric SVD biplot
## ============================================================

#' Draw a SEPA row-isometric SVD biplot
#'
#' Produces a base-R row-isometric biplot for a specified pair of dimensions
#' (\code{p1}, \code{p2}).  All persons are plotted as grey dots; a subset
#' specified by \code{ids_highlight} is overlaid in red and labelled.  Domain
#' loading vectors are drawn as arrows.  The plot is optionally saved to a PDF.
#'
#' @param svd_fit       List with components \code{U} (\eqn{n \times K}
#'   left singular vectors), \code{d} (length-\eqn{K} singular values), and
#'   \code{V} (\eqn{p \times K} right singular vectors), as returned by
#'   \code{\link[base]{svd}} or the format produced inside
#'   \code{\link{run_sepa}}.
#' @param id_vec        Vector of length \eqn{n}.  Person IDs (used to match
#'   \code{ids_highlight}).
#' @param domain_names  Character vector of length \eqn{p}.  Domain labels
#'   used for arrow annotations.
#' @param p1            Integer. First dimension (x-axis).  Default \code{1L}.
#' @param p2            Integer. Second dimension (y-axis).  Default \code{2L}.
#' @param ids_highlight Optional vector of IDs to emphasise.  Matched against
#'   \code{id_vec}.  Default \code{NULL} (no highlighting).
#' @param out_file      Character or \code{NULL}.  If a path is given the plot
#'   is also written to that PDF file.  Default \code{NULL}.
#' @param a_scale       Numeric. Arrow scaling factor.  Default \code{35}.
#' @param t_scale       Numeric. Label scaling factor.  Default \code{40}.
#' @param arrow_col     Colour string for domain arrows and labels.
#'   Default \code{"#1F4E79"}.
#' @param hi_col        Colour string for highlighted persons.
#'   Default \code{"red3"}.
#' @param others_alpha  Alpha transparency for background persons.
#'   Default \code{0.30}.
#'
#' @return Invisibly returns a list with the plotting coordinates:
#'   \code{Fx}, \code{Fy} (person scores), \code{end_x}, \code{end_y}
#'   (arrow tips), \code{lab_x}, \code{lab_y} (domain labels).
#'
#' @examples
#' X  <- as.matrix(fake_wj[, c("LT","ST","CP","AP","VP","CK","FR")])
#' Xs <- X - rowMeans(X)
#' sv <- svd(Xs)
#' draw_sepa_biplot(
#'   svd_fit       = list(U = sv$u, d = sv$d, V = sv$v),
#'   id_vec        = fake_wj$ID,
#'   domain_names  = c("LT","ST","CP","AP","VP","CK","FR"),
#'   p1 = 1L, p2 = 2L,
#'   ids_highlight = c(724, 944)
#' )
#'
#' @export
draw_sepa_biplot <- function(svd_fit,
                              id_vec,
                              domain_names,
                              p1            = 1L,
                              p2            = 2L,
                              ids_highlight = NULL,
                              out_file      = NULL,
                              a_scale       = 35,
                              t_scale       = 40,
                              arrow_col     = "#1F4E79",
                              hi_col        = "red3",
                              others_alpha  = 0.30) {
  stopifnot(
    is.list(svd_fit),
    all(c("U", "d", "V") %in% names(svd_fit)),
    p1 != p2,
    p1 >= 1L, p2 >= 1L,
    p1 <= length(svd_fit$d), p2 <= length(svd_fit$d)
  )

  U       <- svd_fit$U
  d       <- svd_fit$d
  V       <- svd_fit$V
  n_bp    <- nrow(U)
  var_pct <- round(100 * d^2 / sum(d^2), 1L)

  Fx <- U[, p1] * d[p1]; Fy <- U[, p2] * d[p2]
  Vx <- V[, p1];          Vy <- V[, p2]

  end_x <- a_scale * Vx;  end_y <- a_scale * Vy
  lab_x <- t_scale * Vx;  lab_y <- t_scale * Vy
  lens   <- sqrt(Vx^2 + Vy^2)
  Udir   <- cbind(Vx, Vy) / pmax(lens, .Machine$double.eps)
  back   <- 0.015 * max(abs(c(end_x, end_y)))
  start_x <- -back * Udir[, 1L]; start_y <- -back * Udir[, 2L]

  xlab     <- sprintf("Dimension %d (%.1f%%)", p1, var_pct[p1])
  ylab     <- sprintf("Dimension %d (%.1f%%)", p2, var_pct[p2])
  xr       <- range(c(Fx, end_x, lab_x, 0))
  yr       <- range(c(Fy, end_y, lab_y, 0))
  xlim     <- xr * 1.08; ylim <- yr * 1.08
  plane_n  <- if (p1 == 1L && p2 == 2L) 1L else
              if (p1 == 3L && p2 == 4L) 2L else NA

  plot_fn <- function() {
    op <- graphics::par(mar = c(4.5, 5, 3.5, 2) + 0.1)
    on.exit(graphics::par(op), add = TRUE)

    graphics::plot(NA, NA, xlim = xlim, ylim = ylim,
                   xlab = xlab, ylab = ylab,
                   cex.lab = 1.20, cex.axis = 1.0,
                   main = if (!is.na(plane_n))
                            sprintf("Plane %d (Dims %d\u2013%d)",
                                    plane_n, p1, p2)
                          else sprintf("Dims %d\u2013%d", p1, p2),
                   cex.main = 1.25, xaxs = "i", yaxs = "i", asp = 1)
    graphics::abline(h = 0, col = "grey75", lty = 2L)
    graphics::abline(v = 0, col = "grey75", lty = 2L)

    others_col <- grDevices::adjustcolor("grey60", alpha.f = others_alpha)

    idx_hi  <- if (!is.null(ids_highlight) && length(ids_highlight) > 0L)
                 match(ids_highlight, id_vec) else integer(0L)
    idx_hi  <- idx_hi[!is.na(idx_hi)]
    idx_oth <- setdiff(seq_len(n_bp), idx_hi)

    if (length(idx_oth))
      graphics::points(Fx[idx_oth], Fy[idx_oth],
                       pch = 16L, cex = 0.70, col = others_col)

    graphics::arrows(start_x, start_y, end_x, end_y,
                     length = 0.14, angle = 28L,
                     lwd = 2.0, col = arrow_col, code = 2L)

    ## Highlighted persons: nudged outward so they do not overlap arrow tips
    if (length(idx_hi)) {
      scale_fac      <- t_scale / a_scale
      Fx_hi          <- Fx; Fy_hi <- Fy
      Fx_hi[idx_hi]  <- Fx[idx_hi] * scale_fac
      Fy_hi[idx_hi]  <- Fy[idx_hi] * scale_fac
      graphics::points(Fx_hi[idx_hi], Fy_hi[idx_hi],
                       pch = 21L, cex = 1.15, lwd = 1.2,
                       col = hi_col, bg = "white")
      graphics::text(Fx_hi[idx_hi], Fy_hi[idx_hi],
                     labels = as.character(id_vec[idx_hi]),
                     pos = 3L, offset = 0.48, cex = 0.95,
                     col = hi_col, font = 2L)
    }
    graphics::text(lab_x, lab_y, labels = domain_names,
                   cex = 1.10, col = arrow_col)
    graphics::box()
  }

  if (!is.null(out_file)) {
    save_pdf(out_file, plot_fn())
    message("Saved biplot to: ", out_file)
  } else {
    plot_fn()
  }

  invisible(list(Fx = Fx, Fy = Fy,
                 end_x = end_x, end_y = end_y,
                 lab_x = lab_x, lab_y = lab_y))
}
