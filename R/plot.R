
#' @export
plot.field <- function(data, fill = NULL, ...) {
  row <- data %@% "row"
  col <- data %@% "col"
  trial <- data %@% "trial"
  plot_field(data, fill = {{ fill }}, row = row, col = col, trial = trial, ...)
}

#' Plot field
#'
#' @param data The data
#' @param fill The fill variable.
#' @param row,col The row and column name. If not supplied, it will try to detect one.
#' @param trial The division of the trial.
#' @param max_col The maximum number of columns to plot in a row if more than one trial.
#' @param max_trial The maximum number of trials.
#' @param max_cat The maximum number of categorical levels to plot (if fill is categorical).
#' @export
plot_field <- function(data, fill = NULL, row = NULL, col = NULL, trial = NULL, max_col = 40, max_trial = 20, max_cat = 8) {

  if(inherits(data, "field")) {
    if(missing(row)) rowq <- data %@% ".row"
    if(missing(col)) colq <- data %@% ".col"
    if(missing(trial)) trialq <- data %@% ".trial"
  } else {
    colq <- names(eval_select(enexpr(col), data)) %0% detect_col_name(data)
    rowq <- names(eval_select(enexpr(row), data)) %0% detect_row_name(data)
    trialq <- names(eval_select(enexpr(trial), data)) %0% detect_trial_name(data) %0% NULL
  }
  fillq <- names(eval_select(enexpr(fill), data)) %0% detect_yield_name(data)
  if(any(is.na(fillq))) {
    fillq <- detect_genotype_name(data)
  }

  if(!is_observational_unit(data, !!!syms(c(rowq, colq, trialq)))) cli::cli_abort("The row, col and trial do not uniquely index the observation.")

  data <- data |>
    dplyr::mutate(!!colq := factor(.data[[colq]]),
                  !!rowq := factor(.data[[rowq]]))

  g <- data |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[colq]],
                 y = .data[[rowq]],
                 fill = .data[[fillq]]) +
    ggplot2::geom_tile(color = "black", size = 1.2, show.legend = TRUE) +
    ggplot2::labs(x = colq, y = rowq, fill = fillq)

  if(is.numeric(data[[fillq]])) {
    g <- g + ggplot2::scale_fill_distiller(palette = "Greens", direction = 1,
                                           limits = c(min(data[[fillq]], na.rm = TRUE),
                                                      max(data[[fillq]], na.rm = TRUE)))
  } else if(is.factor(data[[fillq]]) | is.character(data[[fillq]])) {
    ncat <- dplyr::n_distinct(data[[fillq]])
    if(max_cat < ncat) cli::cli_alert_info("Too many levels in fill, only {max_cat} levels are plotted.")
    g <- g + ggplot2::scale_fill_discrete(limits = sample(unique(data[[fillq]]), min(max_cat, ncat)))
  }

  if(!all(is.null(trialq))) {

    data_split <- data |>
      dplyr::group_split(!!!syms(trialq))

    if(length(data_split) > max_trial) {
      data_split <- data_split[1:max_trial]
      cli::cli_alert_info("Too many levels in trial, only the first {max_trial} levels are plotted.")
    }

    dims <- data_split |>
      lapply(function(.data) {
          dat <- data.frame(nrow = max(as.integer(.data[[rowq]]), na.rm = TRUE),
                            ncol = max(as.integer(.data[[colq]]), na.rm = TRUE))
          for(atrial in trialq) {
            dat[[atrial]] <- unique(.data[[atrial]])
          }
          dat
        }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(row_no = count_ceil(ncol, max = max_col))

    rowh_max <- dims |>
      dplyr::summarise(rowh = max(nrow),
                       .by = row_no) |>
      dplyr::pull(rowh)

    layout <- dims |>
      dplyr::mutate(r = cumsum(ncol),
                    l = c(0, r[-dplyr::n()]) + 1,
                    b = c(0, cumsum(rowh_max))[row_no] + nrow,
                    t = c(0, cumsum(rowh_max))[row_no] + 1,
                    .by = row_no) |>
      dplyr::rowwise() |>
      dplyr::mutate(area = list(patchwork::area(t = t, l = l, r = r, b = b))) |>
      dplyr::pull(area) |>
      Reduce("c", x = _)


    #g <- g + ggplot2::facet_wrap(as.formula(paste0("~", paste(trialq, collapse = "+"))))



    gl <- data_split |>
      lapply(function(d) {
          title_with_names <- d |>
            dplyr::select(!!!syms(trialq)) |>
            dplyr::slice(1) |>
            unlist()

          title <- paste0(paste0(names(title_with_names), ": ", title_with_names),
                          collapse = "\n")

          (g %+% d) + ggplot2::ggtitle(title) +
            ggplot2::theme(panel.background = ggplot2::element_blank())
        })

    #al <- patchwork::align_patches(gl)

    g <- patchwork::wrap_plots(gl, design = layout)

    g <- g + patchwork::plot_layout(guides = "collect", axis_titles = "collect")
  }

  g
}


#' @export
plot_field_genotype <- function(data, row = NULL, col = NULL, trial = NULL) {
  fill <- detect_genotype_name(data)
  plot_field(data, fill = fill, row = row, col = col, trial = trial)
}

#' @export
plot_field_yield <- function(data, row = NULL, col = NULL, trial = NULL) {
  fill <- detect_yield_name(data)
  plot_field(data, fill = fill, row = row, col = col, trial = trial) +
    ggplot2::scale_fill_distiller(palette = "Greens", direction = 1)

}
