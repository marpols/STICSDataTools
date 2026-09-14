#'@export
plot_boxplot <- function(dataset,
                         variable,
                         x_axis = "stncode",
                         factors = character(),
                         title = character(),
                         axis_title_x = x_axis,
                         axis_title_y = variable,
                         colour = "red") {

  if(!rlang::is_empty(factors)){
  dataset[[x_axis]]  <- factor(dataset[[x_axis]] ,
                           levels = factors)
  }

  ggplot2::ggplot(dataset,
                 ggplot2::aes(x = .data[[x_axis]],
                     y = .data[[variable]])) +
    ggplot2::geom_boxplot(fill = colour) +
    ggplot2::labs(
      title = title,
      x = axis_title_x,
      y = axis_title_y
    ) +
    ggplot2::theme_minimal()

}

plot_boxplot_grid <- function(dataset,
                              variable,
                              x_axis = "stncode",
                              factors = character(),
                              title = character(),
                              grid_cols = "soilcode",
                              grid_rows = "ssp",
                              axis_title_x = x_axis,
                              axis_title_y = variable,
                              colour = "red") {
  if (!rlang::is_empty(factors)) {
    dataset[[x_axis]]  <- factor(dataset[[x_axis]] , levels = factors)
  }

  cols_expr <- if (!is.null(grid_cols)) ggplot2::vars(!!dplyr::sym(grid_cols)) else NULL
  rows_expr <- if (!is.null(grid_rows)) ggplot2::vars(!!dplyr::sym(grid_rows)) else NULL

  boxplot_layer <- if (colour %in% names(dataset)) {
    ggplot2::geom_boxplot(
      ggplot2::aes(fill = .data[[colour]])
    )
  } else {
    ggplot2::geom_boxplot(fill = colour)
  }

  ggplot2::ggplot(dataset, ggplot2::aes(x = .data[[x_axis]], y = .data[[variable]])) +
    boxplot_layer +
    ggplot2::facet_grid(cols = cols_expr,
                        rows = rows_expr) +
    ggplot2::labs(title = title, x = axis_title_x, y = axis_title_y) +
    ggplot2::theme_minimal()

}
