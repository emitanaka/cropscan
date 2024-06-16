
#' Find mismatch names
#'
#' @param x,y A character vector.
#' @export
mismatches <- function(x, y) {
  namex <- unique(x)
  namey <- unique(y)
  xin <- setdiff(x, y)
  yin <- setdiff(y, x)
  if(length(xin) | length(yin)) {
    cli::cli_alert_info("Mismatched names detected")
    cli::cli_alert_info("Names in x but not in y: {paste0(xin, collapse = ', ')}")
    cli::cli_alert_info("Names in y but not in x: {paste0(yin, collapse = ', ')}")
    return(TRUE)
  } else {
    return(FALSE)
  }
}
