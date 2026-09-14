#' Df list to df
#'
#' @export
df_list_to_df<- function(df_list) {
  UseMethod("df_list_to_df")
}

#' @method  df_list_to_df cropr_simulation
#' @export
df_list_to_df.cropr_simulation <- function(df_list){
  tryCatch({
    class(df_list) <- c("list")
    dplyr::bind_rows(df_list,.id = "file_name") |>
      mutate(
        ian  = lubridate::year(Date),
        mo = lubridate::month(Date),
        jo   = lubridate::day(Date),
        jul = lubridate::yday(as.Date(Date))
      ) |>
      relocate(ian, mo, jo, jul, .after = Date)  |>
      add_ids.default(format = "model_ssp_year_stncode_soilcode_verid")
  }, error = function(e){
    e
  })
}

#' @method  df_list_to_df list
#' @export
df_list_to_df.default <- function(df_list){
  tryCatch({
    purrr::list_rbind(df_list) |>
      data.table::as.data.table() |>
      .restore_attrs(df_list, "list")
  }, error = function(e){
    if (grepl("must be a list", e$message)) {
      message("`obj` is already a data.frame or data.table or is not a list")
      return(df)
    }
  })
}
