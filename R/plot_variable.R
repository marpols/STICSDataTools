#'@export
plot_variable <- function(dataset,
                          variable,
                          x_axis = "ian",
                          filters = character(),
                          by = "soilcode",
                          title = character(),
                          axis_title_x = x_axis,
                          axis_title_y = variable,
                          legend_title = by){

  subset <- dataset |> dplyr::filter(!!!rlang::parse_exprs(filters))

  ggplot2::ggplot(subset, ggplot2::aes(x = .data[[x_axis]])) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[variable]],
                  color = .data[[by]])) +
    ggplot2::labs(title = title,
         x = axis_title_x,
         y = axis_title_y,
         color = legend_title) +
    ggplot2::theme_minimal()
}

#'@export
plot_variable_grid <- function(dataset,
                               variable,
                               x_axis = "jul",
                               filters = character(),
                               by = "soilcode",
                               grid_cols = "soilcode",
                               grid_rows = "ssp",
                               title = character(),
                               axis_title_x = x_axis,
                               axis_title_y = variable,
                               legend_title = by,
                               color_values = c(ARY="#33A02C",
                                                CTW="#CAB2D6",
                                                CLO="#FDBF6F"),
                               linewidth = 0.9){

  cols_expr <- if (!is.null(grid_cols)) ggplot2::vars(!!dplyr::sym(grid_cols)) else NULL
  rows_expr <- if (!is.null(grid_rows)) ggplot2::vars(!!dplyr::sym(grid_rows)) else NULL

  subset <- dataset |>  dplyr::filter(!!!rlang::parse_exprs(filters))

  ggplot2::ggplot(subset, ggplot2::aes(x = .data[[x_axis]])) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[variable]],
                  color = .data[[by]]),
              linewidth = linewidth) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::facet_grid(cols = cols_expr,
               rows = rows_expr) +
    ggplot2::labs(title = title,
         x = axis_title_x,
         y = axis_title_y,
         color = legend_title) +
    ggplot2::theme_minimal()
}
