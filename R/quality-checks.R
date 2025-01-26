
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

#' Identify constant columns
#'
#' Similar to janitor::remove_constant, except this function
#' aims to identify the constant columns without removing them.
#' If there are no constant columns, it will return NA.
#'
#' @param data The data frame
#' @param na.rm Should missing values be removed?
#' @return A character vector of constant columns.
#' @export
cols_constant <- function (data, na.rm = FALSE) {
  if(nrow(data) <= 1) {
    cli::cli_alert("The data has only one row.")
    return(colnames(data))
  }
  cols <- sapply(X = seq_len(ncol(data)), FUN = function(idx) {
    column_to_test <- if (is.matrix(data)) {
      data[, idx]
    }
    else {
      data[[idx]]
    }
    length(unique(if (na.rm) {
      stats::na.omit(column_to_test)
    } else {
      column_to_test
    })) <= 1
  })
  if(!any(cols)) {
    cli::cli_alert("No constant columns.")
    return(NA)
  }
  colnames(data)[cols]
}


#' Identify columns that have certain proportion of missing values
#'
#' Similar to janitor::remove_empty, except this function
#' aims to identify the columns without removing them.
#' If there are no identified columns, it will return NA.
#'
#' @param data The data frame
#' @param cutoff The minimum cutoff for the proportion of missing values.
#' @return A character vector of column names with missing values with at least a certain proportion.
#' @export
cols_missing <- function (data, cutoff = 1) {
  cols <- if (cutoff >= 1) {
    colSums(is.na(data)) == nrow(data)
  }
  else {
    (colSums(is.na(data))/nrow(data)) >= cutoff
  }
  if(!any(cols)) {
    cli::cli_alert("No identified columns with missing proportion.")
    return(NA)
  }
  colnames(data)[cols]
}

#' Identify rows that have certain proportion of missing values
#'
#' Similar to janitor::remove_empty, except this function
#' aims to identify the columns without removing them.
#' If there are no identified columns, it will return NA.
#'
#' @param data The data frame
#' @param cutoff The minimum cutoff for the proportion of missing values.
#' @return An integer vector of rows with missing values with at least a certain proportion.
#' @export
rows_missing <- function(data, cutoff = 1) {
  rows <- if (cutoff >= 1) {
    rowSums(is.na(data)) == ncol(data)
  }
  else {
    (rowSums(is.na(data))/ncol(data)) >= cutoff
  }
  if(!any(rows)) {
    cli::cli_alert("No identified rows with missing proportion.")
    return(NA)
  }
  which(rows)
}
