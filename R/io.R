## ============================================================
## Data reshaping and file I/O helpers
## ============================================================

#' Reshape a long data frame to wide and write a CSV
#'
#' Pivots a three-column long data frame (id, time, value) to wide format and
#' optionally prefixes the new column names.
#'
#' @param df        Data frame to pivot.
#' @param id_col    Name of the person-identifier column.
#' @param time_col  Name of the within-person variable column (e.g. domain).
#' @param value_col Name of the value column.
#' @param file      Character path for the output CSV.  Pass \code{NULL} or
#'   \code{""} to skip writing.
#' @param prefix    Optional prefix prepended to the new wide-format column
#'   names (empty string = no prefix).
#'
#' @return The wide data frame, invisibly.
#'
#' @examples
#' long_df <- data.frame(
#'   id     = rep(1:3, each = 2),
#'   domain = rep(c("LT", "ST"), 3),
#'   value  = c(100, 105, 98, 110, 102, 107)
#' )
#' wide <- write_long_to_wide(long_df, "id", "domain", "value",
#'                            file = NULL)
#' wide
#'
#' @export
write_long_to_wide <- function(df, id_col, time_col, value_col,
                                file, prefix = "") {
  stopifnot(all(c(id_col, time_col, value_col) %in% names(df)))
  tmp        <- df[, c(id_col, time_col, value_col)]
  names(tmp) <- c("id", "time", "value")
  wide       <- stats::reshape(tmp,
                                idvar     = "id",
                                timevar   = "time",
                                v.names   = "value",
                                direction = "wide")
  new_names  <- sub("^value\\.", "", names(wide))
  if (nzchar(prefix))
    new_names <- ifelse(new_names == "id", new_names,
                        paste0(prefix, new_names))
  names(wide) <- new_names
  if (!is.null(file) && nzchar(file))
    utils::write.csv(wide, file, row.names = FALSE)
  invisible(wide)
}

#' Write an n x p matrix as a wide CSV with an ID column
#'
#' @param M            Numeric matrix, \eqn{n \times p}.
#' @param id           Vector of length \eqn{n} providing row identifiers.
#' @param file         Character path for the output CSV.  Pass \code{NULL}
#'   or \code{""} to skip writing.
#' @param domain_names Optional character vector of length \eqn{p}.  Column
#'   names for the domain columns.  Defaults to \code{colnames(M)} or
#'   \code{"D1", "D2", ...} if those are absent.
#'
#' @return The data frame (ID + matrix columns), invisibly.
#'
#' @examples
#' M   <- matrix(rnorm(6), nrow = 2)
#' out <- write_matrix_wide(M, id = c("A", "B"), file = NULL,
#'                          domain_names = c("X1","X2","X3"))
#' out
#'
#' @export
write_matrix_wide <- function(M, id, file, domain_names = NULL) {
  stopifnot(nrow(M) == length(id))
  if (is.null(domain_names) || length(domain_names) != ncol(M))
    domain_names <- if (!is.null(colnames(M))) colnames(M) else
                      paste0("D", seq_len(ncol(M)))
  df              <- data.frame(id = id, M, check.names = FALSE)
  colnames(df)[-1] <- domain_names
  if (!is.null(file) && nzchar(file))
    utils::write.csv(df, file, row.names = FALSE)
  invisible(df)
}
