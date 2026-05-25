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
             as.data.table() |>
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
    return(summary |> as.data.table())
  }

}

.calc_cum_var <- function(...){

  args <- list(...)
  df <- args[[1]]
  var <- args[["var"]]
  monthly <- args[["monthly"]]
  mo_range <- args[["mo_range"]]


  group <- if(monthly){ c("ian","mo") } else { "ian" }
  tag <- ifelse(monthly, "_mo", "")

  col_name <- paste(var, tag, "_cum", sep = "")

  tryCatch({
    df <- if(monthly){ df[mo %in% mo_range,] } else {df} |>
      mutate({{col_name}} := cumsum(.data[[var]]),
             .by = all_of(group)) |>
      ungroup()
  }, error = function(msg) {
    message("Variable not found. Ensure name and case matches column headings.")
    message(msg$message)
    return(NA)
  })

  return(df)

}

.calc_sum_stat <- function(op = c("sum", "mean", "max"),
                           ...){

  args <- list(...)
  df <- args[[1]]
  var <- args[["var"]]
  by <- args[["by"]]
  mo_range <- args[["mo_range"]]

  op <- match.arg(op)
  func <- switch(op,
                 sum = sum,
                 mean = mean,
                 max = max)
  tag <- switch(op,
                sum = "_sum",
                mean = "_mean",
                max = "_max")

  by_tag <- ifelse(("mo" %in% by), "_mo", "")
  #calc_tag <- ifelse(identical(op, sum), "_sum", "_mean")
  col_name <- paste(var, by_tag, tag, sep = "")
  monthly <- "mo" %in% by

  tryCatch({
    df_sum <- if(monthly){ df[mo %in% mo_range,] } else {df} |>
      summarise({{col_name}} := func(.data[[var]]),
                .by = all_of(by))

  }, error = function(msg) {
    message("Variable not found. Ensure name and case matches column headings.")
    return(NA)
  })

  return(df_sum)
}

get_cum_var <- function(df_list,
                        var = "",
                        mo_range = 1:12,
                        monthly = FALSE,
                        ...) {

  return(.get_data(df_list, .calc_cum_var,
                   var = var,
                   mo_range = mo_range,
                   monthly = monthly,
                   ...))
}

get_sum <- function(df_list,
                    var,
                    by,
                    mo_range = 1:12,
                    ...){

  return(.get_data(df_list, .calc_sum_stat,
                  var = var,
                  by = by,
                  mo_range = mo_range,
                  op = "sum",
                  ...))

}

get_mean <- function(df_list,
                      var,
                      by,
                      mo_range = 1:12,
                     ...){

  return(.get_data(df_list, .calc_sum_stat,
                  var = var,
                  by = by,
                  mo_range = mo_range,
                  op = "mean",
                  ...))

}

get_sum_mean <- function(df_list,
                         var,
                         mean_by,
                         sum_by,
                         mo_range = 1:12,
                         ...){

  df <- if ("list" %in% class(df_list)){df_list_to_df(df_list)} else { df_list }

  df_mean <- get_mean(df_list = df,
                      var = var,
                      by = mean_by,
                      mo_range = mo_range,
                      ...)

  df_sum <- get_sum(df_list = df_mean,
                    var = sprintf("%s_mean",var),
                    by = sum_by,
                    mo_range = mo_range,
                    ...) |>
    .restore_attrs(df_mean, "data.frame")

  return(df_sum)
}

get_max <- function(df_list,
                    var,
                    by,
                    mo_range = 1:12,
                    ...){

  return(.get_data(df_list, .calc_sum_stat,
                   var = var,
                   by = by,
                   mo_range = mo_range,
                   op = "max",
                   ...))
}

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

  df <- df_list_to_df(df_list)

  new_df <- lapply(var_list, function(var){
    do.call(func, c(list(df_list, var), args))
  }) |>
    list_flatten(name_repair = "unique_quiet") |>
    join_dfs(by)

  return(new_df)

}

