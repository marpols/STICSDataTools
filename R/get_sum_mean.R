#'@export
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
