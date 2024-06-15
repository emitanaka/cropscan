
#' Is it observational unit?
#'
#' @param .data The data frame
#' @param ... The columns in the data frame that potentially index the observational unit.
#' @return A logical value.
#' @export
is_observational_unit <- function(.data, ..., .message = TRUE) {

  N <- nrow(.data)
  n <- .data |>
    dplyr::distinct(...) |>
    nrow()

  res <- N == n

  if(res) {
    ## Check if any are redundant
    sel_cols <- tidyselect::eval_select(expr(c(...)), .data)
    for(icol in seq_along(sel_cols)) {
      .n <- .data |>
        dplyr::select(sel_cols[-icol]) |>
        dplyr::distinct() |>
        nrow()

      if(N == .n) cli::cli_alert_danger(paste0(format_var(names(sel_cols[icol])), " is redundant."))
    }
  }

  if(.message) {
    if(res) cli::cli_alert_success("Identifies observational unit")
    else cli::cli_alert_danger("Not the observational unit")
  }

  invisible(res)
}


