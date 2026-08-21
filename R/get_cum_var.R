#'@export
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
