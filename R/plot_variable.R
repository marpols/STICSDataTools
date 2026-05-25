plot_variable <- function(dataset,
                          variable,
                          x_axis = "ian",
                          filters = NULL,
                          by = "soil_code",
                          title = NULL,
                          axis_title_x = x_axis,
                          axis_title_y = variable,
                          legend_title = by){

  subset <- dataset |> filter(!!!rlang::parse_exprs(filters))

  ggplot(subset, aes(x = .data[[x_axis]])) +
    geom_line(aes(y = .data[[variable]],
                  color = .data[[by]])) +
    labs(title = title,
         x = axis_title_x,
         y = axis_title_y,
         color = legend_title) +
    theme_minimal()
}

plot_variable_grid <- function(dataset,
                               variable,
                               x_axis = "jul",
                               filters = "",
                               by = "soil_code",
                               grid_cols = "soil_code",
                               grid_rows = "ssp",
                               title = NULL,
                               axis_title_x = x_axis,
                               axis_title_y = variable,
                               legend_title = by,
                               color_values = c(ARY="#33A02C",
                                                CTW="#CAB2D6",
                                                CLO="#FDBF6F"),
                               linewidth = 0.9){

  cols_expr <- if (!is.null(grid_cols)) vars(!!sym(grid_cols)) else NULL
  rows_expr <- if (!is.null(grid_rows)) vars(!!sym(grid_rows)) else NULL

  subset <- dataset |> filter(!!!rlang::parse_exprs(filters))

  ggplot(subset, aes(x = .data[[x_axis]])) +
    geom_line(aes(y = .data[[variable]],
                  color = .data[[by]]),
              linewidth = linewidth) +
    scale_color_manual(values = color_values) +
    facet_grid(cols = cols_expr,
               rows = rows_expr) +
    labs(title = title,
         x = axis_title_x,
         y = axis_title_y,
         color = legend_title) +
    theme_minimal()
}
