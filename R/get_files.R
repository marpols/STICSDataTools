#'read simulation result files from STICS workspace directory
#'
#'`.get_files` returns a data.table or a list of data.tables of STICS simulations
#'
#' @param ...
#'
#' @returns a data.table or a list of named data.tables of each usm.
.get_files <- function(...) {

  args <- list(...)

  read_mod_s <- function(f, name) {

    mod_file <- data.table::fread(f, sep = ";",
                                  stringsAsFactors = FALSE) |>
      dplyr::mutate(file_name = name,
             ian = NULL,
             !!!exp_ids) |>
      add_ids(args[['usm_name_format']]) |>
      as.cropr()

    return(mod_file)
  }

  read_mod_b <- function(f){
    mod_file <- readLines(f)
    return(mod_file)
  }

  exp_dir <- args[['exp_dir']]
  exp_ids <- .set_ids(exp_dir, args[['exp_name_format']])

  exp_files <- list.files(file.path(args[['js_path']],
                                    args[['ws']],
                                    args[['dir']],
                                    exp_dir),
                          full.names = TRUE,
                          recursive = TRUE) |>
    grep(args[['type']], x=_, value = TRUE)

  names <- stringr::str_extract(exp_files, "\\w*-*\\w*-*\\w*.sti") |>
    stringr::str_remove(args[["type"]]) |>
    stringr::str_remove(".sti")

  if(args[["usm_list"]] != ""){
    exp_files <- exp_files[grepl(paste(args[["usm_list"]], collapse="|"),
                                 exp_files,
                                 value = TRUE)]
  } else {

    filters <- c(args[["stn_code"]],
                 args[["soil_code"]],
                 args[["ssp"]])

    filters <- filters[filters != ""]

    exp_files <- Reduce(
      \(x, f) grep(f, x, value = TRUE),
      filters,
      init = exp_files
    )

  }
  tryCatch({
    sims <- purrr::map2(exp_files, names, \(f, name) {
      if (args[["type"]] == "mod_s") {
        read_mod_s(f, name)
      } else if (args[["type"]] == "mod_b") {
        read_mod_b(f)
      }
    }) |>
      purrr::set_names(names)
  }, error = function(e){
      return(NA)
  })

  if(!is.null(args[["group"]])){
    new_sim_list <- regroup_df_list(sims, by = args[["group"]]) |>
      lapply(as.data.table) |>
      lapply(.restore_attrs, sims[[1]], c("data.table", "data.frame"))
    sims <- new_sim_list
  }

  if (args[["return_type"]] == 0){
    sims
  } else if (args[["return_type"]] == 1){
    purrr::list_rbind(sims) |>
             as.data.table()
  }
}

#' @param exp_dir
#'
#' @param exp_name_format
#' @param usm_name_format
#' @param js_path
#' @param ws
#' @param dir
#' @param usm_list
#' @param ver_num
#' @param stncode
#' @param soilcode
#' @param ssp
#' @param group
#' @param return_type
#'
#'@export
get_mod_s <- function(exp_dir,
                      exp_name_format = "expid_runid",
                      usm_name_format = "model_ssp_year_stncode_soilcode_verid",
                      js_path = javastics_path,
                      ws = workspace,
                      dir = "RESULTS",
                      usm_list = "",
                      ver_num = NULL,
                      stncode = "",
                      soilcode = "",
                      ssp = "",
                      group = NULL,
                      return_type = 0) {

  files <- .get_files(
    exp_dir = exp_dir,
    exp_name_format = exp_folder_format,
    usm_name_format = usm_name_format,
    js_path = js_path,
    ws = ws,
    dir = dir,
    usm_list = usm_list,
    ver_num = ver_num,
    stn_code = stn_code,
    soil_code = soil_code,
    ssp = ssp,
    group = group,
    type = "mod_s",
    return_type = return_type
  ) |>
    as.cropr()

  files
}

#' @param exp_dir
#'
#' @param exp_name_format
#' @param usm_name_format
#' @param js_path
#' @param ws
#' @param dir
#' @param usm_list
#' @param ver_num
#' @param stn_code
#' @param soil_code
#' @param ssp
#' @param group
#' @param type
#' @param return_type
#'
#'@export
get_mod_b <- function(exp_dir,
                      exp_name_format = "expid_runid",
                      usm_name_format = "model_ssp_year_stncode_soilcode_verid",
                      js_path = javastics_path,
                      ws = workspace,
                      dir = "RESULTS",
                      usm_list = "",
                      ver_num = NULL,
                      stn_code = "",
                      soil_code = "",
                      ssp = "",
                      group = "",
                      type = "mod_b",
                      return_type = 0) {

  files <- .get_files(
    js_path = js_path,
    ws = ws,
    dir = dir,
    usm_list = usm_list,
    exp_id = exp_id,
    run_id = run_id,
    ver_num = ver_num,
    stn_code = stn_code,
    soil_code = soil_code,
    ssp = ssp,
    group = group,
    type = "mod_b",
    projections = projections,
    return_type = return_type
  ) |>
    as.cropr()

  files

}

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
      as.data.table() |>
      as.climate() |>
      add_ids(name_format)

  }) |>
    as.climate() |>
    set_names(names)

  if (return_type == 0){
    climate_data
  } else if (return_type == 1){
    purrr::list_rbind(climate_data) |>
      as.data.table() |>
      as.climate()
  }
}
