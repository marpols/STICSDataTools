#requires ggplot2

#'@export
plot_style <- function(plot,
                       plot_fs = NULL,
                       x_axis = 16,
                       y_axis = 16,
                       elements = 16,
                       grids = 15,
                       legend = 15,
                       legend_pos = "right",
                       plot_size = c(28,18),
                       angle = 45,
                       hjust = 1
){
  plot +
    theme(
      plot.title = element_text(size = plot_fs),
      axis.title.x = element_text(size = x_axis),
      axis.title.y = element_text(size = y_axis),
      axis.text = element_text(size = elements),
      axis.text.x = element_text(angle = angle, hjust = hjust),
      strip.text.x = element_text(size = grids),
      legend.text = element_text(size = legend),
      legend.position = legend_pos
    )

}

#'@export
plot_labels <- function(plot,
                        title = "",
                        axis_title_x = "",
                        axis_title_y = "",
                        ...){
  plot +
    labs(title = title,
      x = axis_title_x,
      y = axis_title_y,
      ...
      )

}

#'@export
remove_legend <- function(plot,
                       type = c("fill", "colour", "linetype", "alpha")){
  lookup <- list(fill = "none",
              colour = "none",
              linetype = "none",
              alpha = "none")
  args <- match.arg(type, several.ok = TRUE)

  plot + guides(lookup[args])
}




