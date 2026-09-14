#' @param name_format
#'
#' @param js_path
#' @param ws
#' @param return_type
#'
#'@export
get_climate_files <- function(name_format = "stn name",
                              js_path = javastics_path,
                              ws = workspace,
                              return_type = 0){

  climate_files <- list.files(file.path(js_path,
                                        ws),
                              full.names = TRUE) |>
    grep("\\d{4}$", x=_, value = TRUE)

  names <- stringr::str_extract(climate_files, "\\w*-*\\w*-*\\w*.\\d{4}$") |>
    stringr::str_remove("\\W\\d{4}$") |> unique()

  climate_data <- lapply(names, function(n){
    files <- grep(n, climate_files, value = TRUE)
    file_data <- setNames(do.call(rbind, lapply(files, data.table::fread,
                                                sep = " ",
                                                stringsAsFactors = FALSE,
                                                header = FALSE)),
                          c("file_name",
                            "ian","mo","jo","jul",
                            "MinTemp","MaxTemp",
                            "SolarRad",
                            "PET",
                            "Precipitation",
                            "WindSpeed",
                            "VP",
                            "CO2")
    ) |>
      data.table::as.data.table() |>
      as.climate() |>
      add_ids(name_format)

  }) |>
    as.climate() |>
    purrr::set_names(names)

  if (return_type == 0){
    climate_data
  } else if (return_type == 1){
    purrr::list_rbind(climate_data) |>
      data.table::as.data.table() |>
      as.climate()
  }
}
#
