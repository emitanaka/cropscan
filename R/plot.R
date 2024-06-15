
#' @export
plot.field <- function(data, fill = NULL) {
  row <- data %@% "row"
  col <- data %@% "col"
  by <- data %@% "by"
  plot_field(data, fill = {{ fill }}, row = row, col = col, by = by)
}

#' Plot field
#'
#' @param data The data
#' @param fill The fill variable.
#' @param row,col The row and column name. If not supplied, it will try to detect one.
#' @param by The division of the trial.
#' @param max_col The maximum number of columns to plot in a row if more than one trial.
#' @param max_by The maximum number of trials.
#' @param max_cat The maximum number of categorical levels to plot (if fill is categorical).
#' @export
plot_field <- function(data, fill = NULL, row = NULL, col = NULL, by = NULL, max_col = 40, max_by = 20, max_cat = 8) {

  colq <- names(eval_select(enexpr(col), data)) %0% detect_row_name(data)
  rowq <- names(eval_select(enexpr(row), data)) %0% detect_col_name(data)
  fillq <- names(eval_select(enexpr(fill), data)) %0% detect_yield_name(data)
  byq <- names(eval_select(enexpr(by), data)) %0% detect_trial_name(data) %0% NULL

  if(missing(row) && inherits(data, "field")) rowq <- data %@% "row"
  if(missing(col) && inherits(data, "field")) colq <- data %@% "col"

  if(!is_observational_unit(data, !!!syms(c(rowq, colq, byq)))) cli::cli_abort("The row, col and by do not uniquely index the observation.")

  data <- data |>
    dplyr::mutate(!!colq := factor(.data[[colq]]),
                  !!rowq := factor(.data[[rowq]]))

  g <- data |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[colq]],
                 y = .data[[rowq]],
                 fill = .data[[fillq]]) +
    ggplot2::geom_tile(color = "black", size = 1.2, show.legend = TRUE)

  if(is.numeric(data[[fillq]])) {
    g <- g + ggplot2::scale_fill_distiller(palette = "Greens", direction = 1,
                                           limits = c(min(data[[fillq]], na.rm = TRUE),
                                                      max(data[[fillq]], na.rm = TRUE)))
  } else if(is.factor(data[[fillq]]) | is.character(data[[fillq]])) {
    ncat <- dplyr::n_distinct(data[[fillq]])
    if(max_cat < ncat) cli::cli_alert_info("Too many levels in fill, only {max_cat} levels are plotted.")
    g <- g + ggplot2::scale_fill_discrete(limits = sample(unique(data[[fillq]]), min(max_cat, ncat)))
  }

  if(!all(is.null(byq))) {

    data_split <- data |>
      group_split(!!!syms(byq))

    if(length(data_split) > max_by) {
      data_split <- data_split[1:max_by]
      cli::cli_alert_info("Too many levels in by, only the first {max_by} levels are plotted.")
    }



    dims <- data_split |>
      lapply(function(.data) {
          dat <- data.frame(nrow = max(as.integer(.data[[rowq]]), na.rm = TRUE),
                            ncol = max(as.integer(.data[[colq]]), na.rm = TRUE))
          for(aby in byq) {
            dat[[aby]] <- unique(.data[[aby]])
          }
          dat
        }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(row_no = count_ceil(ncol, max = max_col))

    rowh_max <- dims |>
      dplyr::summarise(rowh = max(nrow),
                       .by = row_no) |>
      pull(rowh)

    layout <- dims |>
      dplyr::mutate(r = cumsum(ncol),
                    l = c(0, r[-n()]) + 1,
                    b = c(0, cumsum(rowh_max))[row_no] + nrow,
                    t = c(0, cumsum(rowh_max))[row_no] + 1,
                    .by = row_no) |>
      dplyr::rowwise() |>
      dplyr::mutate(area = list(patchwork::area(t = t, l = l, r = r, b = b))) |>
      dplyr::pull(area) |>
      Reduce("c", x = _)



    #g <- g + ggplot2::facet_wrap(as.formula(paste0("~", paste(byq, collapse = "+"))))



    gl <- data_split |>
      lapply(function(d) {
          title_with_names <- d |>
            dplyr::select(!!!syms(byq)) |>
            dplyr::slice(1) |>
            unlist()

          title <- paste0(paste0(names(title_with_names), ": ", title_with_names),
                          collapse = "\n")

          (g %+% d) + ggplot2::ggtitle(title) +
            ggplot2::theme(panel.background = ggplot2::element_blank(),
                           axis.title = ggplot2::element_blank())
        })

    #al <- patchwork::align_patches(gl)

    g <- patchwork::wrap_plots(gl, design = layout)

    g <- g + patchwork::plot_layout(guides = "collect")
  }

  g
}


#' @export
plot_field_genotype <- function(data, row = NULL, col = NULL, by = NULL) {
  fill <- detect_genotype_name(data)
  plot_field(data, fill = fill, row = row, col = col, by = by)
}

#' @export
plot_field_yield <- function(data, row = NULL, col = NULL, by = NULL) {
  fill <- detect_yield_name(data)
  plot_field(data, fill = fill, row = row, col = col, by = by) +
    ggplot2::scale_fill_distiller(palette = "Greens", direction = 1)

}
