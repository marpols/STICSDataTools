.get_data <- function(df_list,
                      func,
                      return_type = NULL,
                      ...) {
  UseMethod(".get_data")
}

.get_data.list <- function(df_list,
                           func,
                     return_type = 0,
                     ...){

  args <- list(...)
  # return_type <- match.args(return_type)
  file_list <- lapply(df_list, function(df){
    return(do.call(func, c(list(df), args)))
  }) |>
    set_names(names(df_list)) |>
    .restore_attrs(df_list, "list")

  if(return_type == 0){
    return(file_list)
  } else if (return_type == 1){
    return(purrr::list_rbind(file_list) |>
             data.table::as.data.table() |>
             .restore_attrs(df_list, "list"))
  }
}

.get_data.default <- function(df,
                              func,
                              return_type = 1,
                              ...){
  args <- list(...)
  summary <- do.call(func, c(list(df), args))
  if(return_type == 0){
    tryCatch({
      return(df_to_list(summary, by = args[["by"]]) |>
               .restore_attrs(df, c("data.table","data.frame"))
             )
    }, error = function(e){
      message("To return as list, set grouping using 'by'")
    })
  } else if (return_type == 1){
    return(summary |> data.table::as.data.table())
  }

}

















