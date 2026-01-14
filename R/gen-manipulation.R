
#' Make dense symmetric matrix sparse
asr_sparse <- function(M, names = rownames(M), inverse = FALSE) {
  res <- data.frame(Row = as.vector(row(M)[!upper.tri(M)]),
                    Column = as.vector(col(M)[!upper.tri(M)]),
                    value = as.vector(M[!upper.tri(M)]))
  res <- res[order(res$Row, res$Column), ]
  attr(res, "rowNames") <- names
  attr(res, "INVERSE") <- inverse
  res
}

#' Construct genomic relationship matrix
#'
#' @param M A matrix
GRM <- function(M, pseudo = 1e-6, type = "vanraden") {
  if(!inherits(M, "matrix")) {
    M <- as.matrix(M)
  }
  vals <- as.vector(M)
  vals_min <- min(vals, na.rm = TRUE)
  vals_max <- max(vals, na.rm = TRUE)
  if(vals_min >= 0 & vals_max <= 1) {
    M <- 2 * M
  } else if(vals_min >= -1 & vals_max <= 1) {
    M <- M + 1
  } else if(!(vals_min >= 0 & vals_max <= 2)) {
    cli::cli_abort("The supplied matrix is expected to be coded as 0-1, -1 to 1, or 0-2.")
  }
  N <- nrow(M)
  if(type == "vandraden") {
    pm <- colSums(M) / (2 * N)
    pm <- pmin(pmax(pm, pseudo), 1 - pseudo)
    W <- sweep(M, 2, 2 * pm, "-")
    W <- sweep(W, 2, sqrt(2 * pm * (1 - pm)), "/")
    tcrossprod(W) / ncol(M)
  }
}
