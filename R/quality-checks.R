
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

  res
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

get_one_to_one_value_order <- function (x) {
  if (any(is.na(x))) {
    new_value <- as.integer(factor(x))
    new_value[is.na(new_value)] <- -1L
    ulevels <- unique(new_value)
    new_value <- as.integer(factor(new_value, levels = ulevels))
  }
  else {
    # same as janitor:::get_one_to_one_value_order
    # but below line is changed to as.character(x)
    # must be character as it can cause duplicated levels below sometimes
    ulevels <- unique(as.character(x))
    new_value <- as.integer(factor(x, levels = ulevels))
  }
  new_value
}

#' Identify columns with all unique values
#'
#' @param data The data frame
#' @return A character vector of column names with all unique values.
#' @export
cols_all_unique <- function(data) {
  cols <- sapply(data, function(x) length(unique(x)) == nrow(data))
  if(!any(cols)) {
    cli::cli_alert("No columns with all unique values.")
    return(NA)
  }
  colnames(data)[cols]
}

#' Identify bijective (one-to-one correspondence) columns
#'
#' Any values that have all unique values will not be included in the
#' output.
#'
#' @param data The data frame
#' @return A list of character vectors of bijective columns.
#' @export
cols_bijective <- function(data) {
    stopifnot(ncol(data) > 0)
    stopifnot(!any(duplicated(names(data))))
    ndistinct_cols <- sapply(data, function(x) length(unique(x)))
    data_alt <- data[, ndistinct_cols < nrow(data), drop = FALSE]
    for (idx in seq_along(data_alt)) {
      data_alt[[idx]] <- get_one_to_one_value_order(data_alt[[idx]])
    }
    remaining_cols <- names(data_alt)
    ret <- list()
    while (length(remaining_cols) > 0) {
      nm1 <- remaining_cols[1]
      remaining_cols <- remaining_cols[-1]
      current_ret <- nm1
      for (nm2 in remaining_cols) {
        if (identical(data_alt[[nm1]], data_alt[[nm2]])) {
          current_ret <- c(current_ret, nm2)
          remaining_cols <- setdiff(remaining_cols, nm2)
        }
      }
      if (length(current_ret) > 1) {
        ret[[length(ret) + 1]] <- current_ret
      }
    }
    if (length(ret) == 0) {
      cli::cli_alert("No bijective columns.")
    }
    ret
}


#' Identify all columns with potential issues
#'
#' @inheritParams cols_missing
#' @inheritParams cols_constant
#' @export
cols_identify_all <- function(data, cutoff = 1, na.rm = FALSE) {
  cli::cli_h1("Checking if all values are unique in a column")
  cols_all_unique(data)
  cli::cli_h1("Checking if any two columns are bijective")
  cols_bijective(data)
  cli::cli_h1("Checking if any column is all constant")
  cols_constant(data, na.rm = na.rm)
  cli::cli_h1("Checking if columns with preset cut-off in missing values")
  cols_missing(data, cutoff = cutoff)
}
