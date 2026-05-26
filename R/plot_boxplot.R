#'@export
plot_boxplot <- function(dataset,
                         variable,
                         x_axis = "stncode",
                         factors = character(),
                         title = character(),
                         axis_title_x = x_axis,
                         axis_title_y = variable,
                         colour = "red") {

  if(!is_empty(factors)){
  dataset[[x_axis]]  <- factor(dataset[[x_axis]] ,
                           levels = factors)
  }

  ggplot2::ggplot(dataset,
                 aes(x = .data[[x_axis]],
                     y = .data[[variable]])) +
    geom_boxplot(fill = colour) +
    labs(
      title = title,
      x = axis_title_x,
      y = axis_title_y
    ) +
    theme_minimal()

}
