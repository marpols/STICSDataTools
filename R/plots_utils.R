#requires ggplot2

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
  plot <- plot +
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

  return(plot)
}

plot_labels <- function(plot,
                        title = "",
                        axis_title_x = "",
                        axis_title_y = "",
                        ...){
  plot <- plot +
    labs(title = title,
      x = axis_title_x,
      y = axis_title_y,
      ...
      )

  return(plot)

}

remove_legend <- function(plot,
                       type = c("fill", "colour", "linetype", "alpha")){
  lookup <- list(fill = "none",
              colour = "none",
              linetype = "none",
              alpha = "none")
  args <- match.arg(type, several.ok = TRUE)
  return(plot + guides(lookup[args]))
}

save_plot <- function(plot,
                      fname,
                      outdir,
                      ...){
  UseMethod("save_plot")
}

save_plot.default <- function(plot,
                      fname,
                      outdir,
                      width = 17,
                      height = 19.5,
                      unit = "cm"
){
  ggsave(filename = sprintf("%s.png", fname),
         plot = plot,
         path = outdir,
         width = width,
         height = height,
         unit = unit)
}

save_plot.list <- function(plots,
                           fnames,
                           outdir,
                           width = 17,
                           height = 19.5,
                           unit = "cm"){
  if(length(fnames) == 1){

  }
  mapply(function(p,f){
    save_plot.default(p,f,outdir,
                      width = width,
                      height = height,
                      unit = unit)
  },plots, fnames)
}






