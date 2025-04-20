
#' Construct a rectangular field
#'
#' @param nrow The number of rows.
#' @param ncol The number of columns.
#' @param nenv The number of envs.
#' @export
field <- function(nrow = 1, ncol = 1, nenv = 1) {
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
}

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
