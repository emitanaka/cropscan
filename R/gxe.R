
#' @export
plot_ge <- function(data, gen = NULL, env = NULL, resp = NULL) {
  respq <- names(eval_select(enexpr(resp), data)) %0% detect_yield_name(data)
  envq <- names(eval_select(enexpr(env), data)) %0% detect_env_name(data) %0% NULL
  genq <- names(eval_select(enexpr(gen), data)) %0% detect_genotype_name(data) %0% NULL

  g <- data |>
    dplyr::mutate(gen = forcats::fct_reorder(factor(.data[[genq]]), .data[[respq]])) |>
    ggplot2::ggplot(ggplot2::aes(.data[[respq]], gen)) +
    ggplot2::geom_point()

  if(!any(is.null(envq))) {
    g <- g + ggplot2::facet_wrap(as.formula(paste0("~", paste0(envq, collapse = "+"))))
  }

  g
}

#' Get the concurrence matrix or table
#'
#' Find the number of common genotypes across environments.
#' The diagonal shows the total number of genotypes at the corresponding environment.
#' The off-diagonal shows the number of common genotypes between the two environments.
#' The matrix is symmetric.
#'
#' @param data A data frame containing the MET data
#' @param gen The name of the genotype column
#' @param env The name of the environment column
#' @return A concurrence matrix or table
#' @export
concurrence_matrix <- function(data, gen = NULL, env = NULL) {
  gen <- names(eval_select(enexpr(gen), data)) %0% detect_genotype_name(data) %0% NULL
  env <- names(eval_select(enexpr(env), data)) %0% detect_env_name(data) %0% NULL
  tt <- table(data[[env]], data[[gen]]) > 0
  tt %*% t(tt)
}


#' @rdname concurrence_matrix
#' @export
concurrence_table <- function(data, gen = NULL, env = NULL) {
  mat <- concurrence_matrix(data, gen, env)
  res <- as.data.frame(mat)
  res |>
    tibble::rownames_to_column("env1") |>
    tidyr::pivot_longer(-env1, names_to = "env2", values_to = "concurrence") |>
    dplyr::mutate(env1 = reorder(env1, concurrence),
                  env2 = reorder(env2, concurrence)) |>
    dplyr::mutate(prop_in_env1 = concurrence / sum(concurrence[env1 == env2]),
                  .by = env1) |>
    dplyr::mutate(prop_in_env2 = concurrence / sum(concurrence[env1 == env2]),
                  .by = env2)
}


