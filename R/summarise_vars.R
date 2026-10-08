#'@export
summarise_vars <- function(df_list,
                           var_list,
                           type = c("cum", "sum", "mean", "sum mean", "max"),
                           ...){


  args <- list(...)

  func <- match.arg(type) |>
    {\(x) switch(x,
                 "cum" = get_cum_var,
                 "sum" = get_sum,
                 "mean" = get_mean,
                 "sum mean" = get_sum_mean,
                 "max" = get_max)}()

  by <- match.arg(type) |>
    {\(x) switch(x,
                 "cum" = c("file_name", "ian", "jul"),
                 "sum" = args[["by"]],
                 "mean" = args[["by"]],
                 "sum mean" = args[["mean_by"]],
                 "max" = args[["by"]])}()

  df <- if("list" %in% class(df_list)){
    df_list_to_df(df_list)
  } else {
    df_list
  }
  x <- c(list(df_list = df, var = var_list[[1]]), args)

  new_df <- lapply(var_list, function(v) {
    do.call(func, c(list(df_list = df, var = v), args))
  }) |>
    purrr::list_flatten(name_repair = "unique_quiet") |>
    join_dfs(by = by)

  return(new_df)

}
