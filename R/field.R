
#' Construct a rectangular field
#'
#' The function constructs a rectangular field with the specified number of rows, columns, and environments.
#' If `nrow` or `ncol` is a vector of length greater than 1, then each element is assumed to be an environment.
#'
#' If `nenv` is greater than 1 then every environment will have the same number of rows and columns.
#'
#' @param nrow The number of rows. Must be a vector of the same size as `ncol` or 1.
#' @param ncol The number of columns. Must be a vector of the same size as `nrow` or 1.
#' @param nenv The number of environments. Unused if `nrow` or `ncol` is a vector of size greater than 1.
#' @examples
#' field(12, 8) # A field of 12 rows x 8 columns.
#' field(12, 8, 3) # Three fields of 12 rows x 8 columns.
#' field(c(12, 8), 6) # Two fields of 12 rows x 6 columns and 8 rows x 6 columns.
#' field(c(4, 3), c(5, 4)) # Two fields of 4 rows x 5 columns and 3 rows x 4 columns.
#' @export
field <- function(nrow = 1, ncol = 1, nenv = 1) {
  if(length(nrow) == 1 & length(ncol) == 1 & length(nenv) == 1) {
    if(nenv >= 2) {
      res <- expand.grid(row = factor(1:nrow), col = factor(1:ncol), env = factor(1:nenv))
      tibble::new_tibble(res,
                         class = "field",
                         .row = "row",
                         .col = "col",
                         .env = "env")
    } else if(nenv == 1) {
      res <- expand.grid(row = factor(1:nrow), col = factor(1:ncol))
      tibble::new_tibble(res,
                         class = "field",
                         .row = "row",
                         .col = "col")
    }
  } else if(length(nrow) > 1 | length(ncol) > 1) {
    dims <- vctrs::vec_recycle_common(nrow, ncol)
    nenv <- length(dims[[1]])
    res <- lapply(1:nenv, function(i) {
      expand.grid(row = 1:dims[[1]][i],
                  col = 1:dims[[2]][i],
                  env = i)
    })
    res <- do.call(rbind, res)
    res$row <- factor(res$row)
    res$col <- factor(res$col)
    res$env <- factor(res$env)
    tibble::new_tibble(res,
                       class = "field",
                       .row = "row",
                       .col = "col",
                       .env = "env")

  } else {
    cli::cli_abort("Invalid input: {.arg nenv} must be of length 1.")
  }
}

#' Add the field class to a data frame
#'
#' Specify the row, column, and environment identifiers to add the field class to a data frame.
#'
#' @param data A data frame to be converted to a field.
#' @param row A column name or expression to be used as the row identifier.
#' @param col A column name or expression to be used as the column identifier.
#' @param env A column name or expression to be used as the environment identifier.
#' @param ... Not used.
#'
#' @export
as_field <- function(data, row = NULL, col = NULL, env = NULL, ...) {
  colq <- names(eval_select(enexpr(col), data)) %0% detect_col_name(data)
  rowq <- names(eval_select(enexpr(row), data)) %0% detect_row_name(data)
  envq <- names(eval_select(enexpr(env), data)) %0% detect_env_name(data) %0% NULL
  tibble::new_tibble(data,
                     class = "field",
                     .row = rowq,
                     .col = colq,
                     .env = envq)
}


#' @importFrom tibble tbl_sum
#' @export
tbl_sum.field <- function(x, ...) {
  c(NextMethod(), "Dimension" = paste0(max(as.integer(as.character(x[[x %@% ".row"]])), na.rm = TRUE), " rows ",
                                       mult_sign(), " ",
                                       max(as.integer(as.character(x[[x %@% ".col"]])), na.rm = TRUE), " cols ",
                                       if(!is.null(x %@% ".env")) {
                                         nenvs <- x |>
                                           dplyr::distinct(!!!syms(x %@% ".env")) |>
                                           nrow()
                                         paste0(mult_sign(), " ", nenvs, " environments")
                                        }))
}


mult_sign <- function() cli::symbol$times
