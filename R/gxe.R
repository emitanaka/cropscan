
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
