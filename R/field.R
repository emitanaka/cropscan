
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
                       .by = "trial")
  } else if(ntrial == 1) {
    res <- expand.grid(row = factor(1:nrow), col = factor(1:ncol))
    tibble::new_tibble(res,
                       class = "field",
                       .row = "row",
                       .col = "col")
  }
}


as_field <- function(x, ...) {

}


#' @importFrom tibble tbl_sum
#' @export
tbl_sum.field <- function(x, ...) {
  c(NextMethod(), "Dimension" = paste0(dplyr::n_distinct(x[[x %@% ".row"]]), " rows ",
                                       mult_sign(), " ",
                                       dplyr::n_distinct(x[[x %@% ".col"]]), " cols ",
                                       if(!is.null(x %@% ".by")) paste0(mult_sign(), " ",
                                                                        dplyr::n_distinct(x[[x %@% ".by"]]), " trials")))
}


mult_sign <- function() cli::symbol$times
