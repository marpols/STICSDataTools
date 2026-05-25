#'@export
save_plot <- function(plot,
                      fname,
                      outdir,
                      ...){
  UseMethod("save_plot")
}

#'@export
save_plot.default <- function(plot,
                              fname,
                              outdir,
                              width = 17,
                              height = 19.5,
                              unit = "cm"
){
  ggplot2::ggsave(filename = sprintf("%s.png", fname),
         plot = plot,
         path = outdir,
         width = width,
         height = height,
         unit = unit)
}

#'@export
save_plot.list <- function(plots,
                           fnames,
                           outdir,
                           width = 17,
                           height = 19.5,
                           unit = "cm"){
  if(length(fnames) == 1){
    fnames <- paste0(fnames, 1:length(plots))
  }
  tryCatch({
  mapply(function(p,f){
    save_plot.default(p,f,outdir,
                      width = width,
                      height = height,
                      unit = unit)
  },plots, fnames)
  }, error = function(e){
    message()
    return(NA)
  })
}



