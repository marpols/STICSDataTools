#'@export
plot_projections <- function(dataset,
                             variable = character(),
                             title = NULL,
                             x_axis = "ian",
                             axis_title_x = x_axis,
                             axis_title_y = variable,
                             grid_cols = "soilcode",
                             grid_rows = "ssp") {


  ggplot2::ggplot(dataset, ggplot2::aes(x = .data[[x_axis]])) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[variable]],
                  color = model)) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data[[grid_cols]]),
               rows = ggplot2::vars(.data[[grid_rows]])) +
    ggplot2::labs(
      title = title,
      x = axis_title_x,
      y = axis_title_y
    ) +
    ggplot2::theme_minimal()

}

#'@export
plot_projections2 <- function(dataset,
                              variable = character(),
                              title = character(),
                              subtitle = character(),
                              x_axis = "ian",
                              axis_title_x = x_axis,
                              axis_title_y = variable,
                              ribbon_group = "ssp",
                              fill_group = "soilcode",
                              grid_cols = "soilcode",
                              grid_rows = "ssp",
                              y_range,
                              fill_values = c(ARY="#33A02C",
                                              CTW="#CAB2D6",
                                              CLO="#FDBF6F"),
                              fill_alphas = c(ssp126=1.00,
                                              ssp370=0.75,
                                              ssp585=0.55),
                              color_values = c(ARY="#000000",
                                               CTW="#000000",
                                               CLO="#000000"),
                              linetype_values = c(ssp126="solid",
                                                  ssp370="dashed",
                                                  ssp585="dotted"),
                              line_labels = c("SSP1-2.6",
                                              "SSP3-7.0",
                                              "SSP5-8.5")
){
  #plots projection data over time with se ribbons (method = loess)

  ggplot2::ggplot(dataset, ggplot2::aes(x = ian)) +
    ggplot2::geom_smooth(
      ggplot2::aes(y = .data[[variable]],
          group = .data[[ribbon_group]],
          fill = .data[[fill_group]]),
      colour = NA,    # ribbon only
      alpha = 0.15
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(y = .data[[variable]],
          group = .data[[ribbon_group]],
          colour = .data[[fill_group]],
          linetype = .data[[ribbon_group]],
          alpha = .data[[ribbon_group]]),
      se = FALSE,
      linewidth = 0.9
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data[[fill_group]])) +
    ggplot2::coord_cartesian(ylim = y_range) +
    ggplot2::scale_fill_manual(values = fill_values) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::scale_linetype_manual(values = linetype_values,
                          labels = line_labels) +
    ggplot2::scale_alpha_manual(values = fill_alphas) +
    ggplot2:: guides(alpha = "none",
           fill = "none",
           color = "none",
           linetype = guide_legend(
             override.aes = list(
               fill = NA,
               colour = "#000000"
             ))) +
    ggplot2::labs(title = title,
         subtitle = subtitle,
         x = axis_title_x,
         y = axis_title_y,
         linetype = ribbon_group) +
    ggplot2::theme_minimal()

}
