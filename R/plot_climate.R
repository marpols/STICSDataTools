#requires ggplot2, rlang

plot_climate <- function(dataset,
                               hist_avg,
                               variable,
                               x_axis = "ian",
                               filters = "",
                               title = NULL,
                               subtitle = NULL,
                               color_values = c(low  = "red",
                                                mid = "lightblue",
                                                high = "blue"),
                               axis_title_x = x_axis,
                               axis_title_y = variable,
                               y_min = 0){

  subset <- dataset |> filter(!!!rlang::parse_exprs(filters))
  max <- max(subset[[variable]])
  mid <- hist_avg |> filter(!!!rlang::parse_exprs(filters)) |> select(2)

  plot <- ggplot(subset, aes(x = .data[[x_axis]],
                              y = .data[[variable]],
                              fill = .data[[variable]])) +
    geom_col() +
    scale_fill_gradient2(color_values,
                         midpoint = mid[[1]]) +
    geom_hline(yintercept = mid,
               linewidth = 0.8) +
    annotate("text",
             x = min(.data[[x_axis]]),
             y = mid, label = "20 year Average",
             hjust = 0.4,
             vjust = -0.5,
             color = "black",
             size = 3) +
    scale_x_continuous(breaks = seq(min(.data[[ian]]),
                                    max(.data[[ian]]),
                                    by = 1)) +
    scale_y_continuous(breaks = seq(0,
                                    ceiling(max / 100) * 100,
                                    by = 100)) +
    coord_cartesian(ylim = c(y_min, NA)) +
    labs(title = title,
         subtitle = subtitle,
         x = axis_title_x,
         y = axis_title_y,
         fill = "") +
    guides(fill = "none") +
    theme_minimal()

  plot
}

plot_climate2 <- function(dataset,
                                hist_avg,
                                variable,
                                x_axis = "ian",
                                filters = "",
                                title = NULL,
                                subtitle = NULL,
                                axis_title_x = x_axis,
                                axis_title_y = variable,
                                legend_title = variable,
                                y_min = 0,
                                palette = "pals::warmcool"
                                ){

  subset <- .filter_data(dataset, filters)
  max <- max(subset[[variable]])
  mid <- .filter_data(hist_avg, filters) |> select(2)

  n_slices <- 200

  bar_slices <- subset |>
    mutate(
      x = as.numeric(factor(subset[[x_axis]])),
      ymax_bar = Precipitation_cum_avg,
      y_bottom = purrr::map(ymax_bar,
                            ~ seq(y_min, .x,
                                  length.out = n_slices + 1)[-length(seq(y_min,
                                                                         .x,
                                                                         length.out = n_slices + 1))]),
      y_top    = purrr::map(ymax_bar,
                            ~ seq(y_min,
                                  .x,
                                  length.out = n_slices + 1)[-1])
    ) |>
    tidyr::unnest(c(y_bottom, y_top)) |>
    mutate(
      fill_y = (y_bottom + y_top) / 2
    )

  plot <- ggplot(bar_slices) +
    geom_rect(
      aes(
        xmin = x - 0.45,
        xmax = x + 0.45,
        ymin = y_bottom,
        ymax = y_top,
        fill = fill_y
      )
    ) +
    scale_x_continuous(
      breaks = unique(bar_slices$x),
      labels = levels(factor(dataset[[x_axis]]))
    ) +
    scale_y_continuous(limits = c(y_min, NA)) +
    scale_fill_paletteer_c(palette)  +
    geom_hline(yintercept = mid[[1,1]],
               linewidth = 0.8) +
    labs(
      x = axis_title_x,
      y = axis_title_y,
      fill = legend_title
    ) +
    theme_minimal()

  return(plot)
}




