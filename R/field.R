
#' Construct a rectangular field
#'
#' @param nrow The number of rows.
#' @param ncol The number of columns.
#' @param ntrial The number of trials.
#' @export
field <- function(nrow = 1, ncol = 1, ntrial = 1) {
  if(ntrial >= 2) {
    res <- expand.grid(row = factor(1:nrow), col = factor(1:ncol), trial = factor(1:ntrial))
    tibble::new_tibble(res,
                       class = "field",
                       .row = "row",
                       .col = "col",
                       .trial = "trial")
  } else if(ntrial == 1) {
    res <- expand.grid(row = factor(1:nrow), col = factor(1:ncol))
    tibble::new_tibble(res,
                       class = "field",
                       .row = "row",
                       .col = "col")
  }
}

#' @export
as_field <- function(data, row = NULL, col = NULL, trial = NULL, ...) {
  colq <- names(eval_select(enexpr(col), data)) %0% detect_col_name(data)
  rowq <- names(eval_select(enexpr(row), data)) %0% detect_row_name(data)
  trialq <- names(eval_select(enexpr(trial), data)) %0% detect_trial_name(data) %0% NULL
  tibble::new_tibble(data,
                     class = "field",
                     .row = rowq,
                     .col = colq,
                     .trial = trialq)
}


#' @importFrom tibble tbl_sum
#' @export
tbl_sum.field <- function(x, ...) {
  c(NextMethod(), "Dimension" = paste0(max(as.integer(as.character(x[[x %@% ".row"]])), na.rm = TRUE), " rows ",
                                       mult_sign(), " ",
                                       max(as.integer(as.character(x[[x %@% ".col"]])), na.rm = TRUE), " cols ",
                                       if(!is.null(x %@% ".trial")) {
                                         ntrials <- x |>
                                           dplyr::distinct(!!!syms(x %@% ".trial")) |>
                                           nrow()
                                         paste0(mult_sign(), " ", ntrials, " trials")
                                        }))
}


mult_sign <- function() cli::symbol$times
