plot_projections <- function(dataset,
                             variable,
                             title = NULL,
                             x_axis = "ian",
                             axis_title_x = x_axis,
                             axis_title_y = variable,
                             grid_cols = "soil_code",
                             grid_rows = "ssp"
){

  ggplot(dataset, aes(x = .data[[x_axis]])) +
    geom_line(aes(y = .data[[variable]],
                  color = model)) +
    facet_grid(cols = vars(.data[[grid_cols]]),
               rows = vars(.data[[grid_rows]])) +
    labs(
      title = title,
      x = axis_title_x,
      y = axis_title_y
    ) +
    theme_minimal()

}

plot_projections2 <- function(dataset,
                              variable,
                              title = NULL,
                              subtitle = NULL,
                              x_axis = "ian",
                              axis_title_x = x_axis,
                              axis_title_y = variable,
                              ribbon_group = "ssp",
                              fill_group = "soil_code",
                              grid_cols = "soil_code",
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

  ggplot(dataset, aes(x = ian)) +
    geom_smooth(
      aes(y = .data[[variable]],
          group = .data[[ribbon_group]],
          fill = .data[[fill_group]]),
      colour = NA,    # ribbon only
      alpha = 0.15
    ) +
    geom_smooth(
      aes(y = .data[[variable]],
          group = .data[[ribbon_group]],
          colour = .data[[fill_group]],
          linetype = .data[[ribbon_group]],
          alpha = .data[[ribbon_group]]),
      se = FALSE,
      linewidth = 0.9
    ) +
    facet_grid(cols = vars(.data[[fill_group]])) +
    coord_cartesian(ylim = y_range) +
    scale_fill_manual(values = fill_values) +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = linetype_values,
                          labels = line_labels) +
    scale_alpha_manual(values = fill_alphas) +
    guides(alpha = "none",
           fill = "none",
           color = "none",
           linetype = guide_legend(
             override.aes = list(
               fill = NA,
               colour = "#000000"
             ))) +
    labs(title = title,
         subtitle = subtitle,
         x = axis_title_x,
         y = axis_title_y,
         linetype = ribbon_group) +
    theme_minimal()

}
