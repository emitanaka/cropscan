

detect_name <- function(.data, possible_names, what = "name", error = TRUE, message = TRUE) {
  nms <- colnames(.data)
  pnames <- tolower(possible_names)
  res <- nms[na.omit(match(pnames, tolower(nms)))]
  if(error && length(res) == 0) cli::cli_abort("Cannot identify {.var {what}}")
  if(message && length(res)) cli::cli_alert_info("Identified {.var {what}} with {.var {paste0(res, collapse = ',')}}")
  res
}


COMMON_NAMES <- list(env = c("trial", "site", "year", "loc", "location", "env", "expt", "county"),
                     row = c("row"),
                     col = c("col", "column", "range"),
                     yield = c("response", "y", "yield"))

detect_yield_name <- function(.data) {
  res <- detect_name(.data, COMMON_NAMES$yield, "yield", error = FALSE)
  if(length(res) == 0) {
    tt <- sapply(.data, function(x) inherits(x, "numeric") | inherits(x, "integer"))
    return(setdiff(names(tt)[tt], unlist(COMMON_NAMES))[1])
  }
  res
}


detect_genotype_name <- function(.data) {
  res <- detect_name(.data, c("genotype", "gen", "line"), "genotype", error = FALSE)
  if(length(res) == 0) {
    tt <- sapply(.data, function(x) inherits(x, "character") | inherits(x, "factor"))
    return(setdiff(names(tt)[tt], unlist(COMMON_NAMES))[1])
  }
  res
}


detect_env_name <- function(.data) {
  detect_name(.data, COMMON_NAMES$env, "env", error = FALSE)
}

detect_row_name <- function(.data) {
  detect_name(.data, COMMON_NAMES$row, "row")[1]
}

detect_col_name <- function(.data) {
  detect_name(.data, COMMON_NAMES$col, "column")[1]
}
