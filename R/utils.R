

format_var <- function(x) cli::col_blue(paste0("`", x, "`"))


`%0%` <- function(a, b) if(length(a)) a else b



count_ceil <- function(x, total = 0, i = 1, max = 30) {
  if(length(x)) {
    total <- total + x[1]
    if(total <= max) {
      c(i, count_ceil(x[-1], total = total, i = i, max = max))
    } else {
      c(i + 1, count_ceil(x[-1], total = x[1], i = i + 1, max = max))
    }
  }
}


check_input_size_1 <- function(x, xname = rlang::caller_arg(x)) {
  if(length(x) != 1) {
    cli::cli_abort("{.arg {xname}} must be of length 1")
  }
}



mult_sign <- function() cli::symbol$times
