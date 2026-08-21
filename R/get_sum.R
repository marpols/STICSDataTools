#'@export
get_sum <- function(df_list,
                    var,
                    by,
                    mo_range = 1:12,
                    ...){

  .get_data(df_list, .calc_sum_stat,
                   var = var,
                   by = by,
                   mo_range = mo_range,
                   op = "sum",
                   ...)

}
