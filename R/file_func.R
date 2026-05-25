#requires purrr, stringr, dplyr

.get_files <- function(...) {

  args <- list(...)

  read_mod_s <- function(f, name) {

    mod_file <- data.table::fread(f, sep = ";",
                                  stringsAsFactors = FALSE) |>
      {\(x) structure(x,
                      class = c("simulations",
                                if (args[["projections"]]) "projections",
                                class(x)),
                      source = "STICS",
                      file_type = "mod_s")}() |>
      mutate(file_name = name,
             ian = NULL,
             exp_id = args[["exp_id"]],
             run_id = args[["run_id"]]) |>
      add_ids()

    return(mod_file)
  }

  read_mod_b <- function(f){
    mod_file <- readLines(f)
    return(mod_file)
  }


  exp_dir <- ifelse(is.null(args[["ver_num"]]),
                    sprintf("%s_%s", args[["exp_id"]], args[["run_id"]]),
                    sprintf("%s_%s_v%d",
                            args[["exp_id"]],
                            args[["run_id"]],
                            args[["ver_num"]]))

  exp_files <- list.files(file.path(args[["js_path"]],
                                    args[["ws"]],
                                    args[["dir"]],
                                    exp_dir),
                          full.names = TRUE,
                          recursive = TRUE) |>
    grep(args[["type"]], x=_, value = TRUE)

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
    if (grepl("values were too long", e$message)) {
      message("File names had more elements than expected.\nSet projections = TRUE if reading files with CMIP model and ssp in file names.")
      return(NA)
    }
    else {
      message(e$message)
    }
  }
  )

  if(!is.null(args[["group"]])){
    new_sim_list <- regroup_df_list(sims, by = args[["group"]]) |>
      lapply(as.data.table) |>
      lapply(.restore_attrs, sims[[1]], c("data.table", "data.frame"))
    sims <- new_sim_list
  }

  if (args[["return_type"]] == 0){
    return(sims)
  } else if (args[["return_type"]] == 1){
    return(purrr::list_rbind(sims) |>
             as.data.table())
  }
}

get_mod_s <- function(js_path = javastics_path,
                      ws = workspace,
                      exp_id = "",
                      run_id = "",
                      dir = "RESULTS",
                      usm_list = "",
                      ver_num = NULL,
                      stn_code = "",
                      soil_code = "",
                      ssp = "",
                      group = NULL,
                      type = "mod_s",
                      projections = FALSE,
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
    type = "mod_s",
    projections = projections,
    return_type = return_type
  ) |>
    {\(x) structure(x,
                    class = c("simulations",
                              if (projections) "projections",
                              class(x)),
                    source = "STICS",
                    file_type = "mod_s")}()

  return(files)
}

get_mod_b <- function(js_path = javastics_path,
                      ws = workspace,
                      dir = "RESULTS",
                      usm_list = "",
                      exp_id = exp_id,
                      run_id = run_id,
                      ver_num = NULL,
                      stn_code = "",
                      soil_code = "",
                      ssp = "",
                      group = "",
                      type = "mod_b",
                      projections = FALSE,
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
    {\(x) structure(x,
                    class = c("simulations",
                              if (projections) "projections",
                              class(x)),
                    source = "STICS",
                    file_type = "mod_b")}()

  return(files)

}

get_climate_files <- function(js_path = javastics_path,
                              ws = workspace,
                              projections = FALSE,
                              return_type = 0){

  climate_files <- list.files(file.path(js_path,
                                        ws),
                              full.names = TRUE) |>
    grep("\\d{4}$", x=_, value = TRUE)

  names <- str_extract(climate_files, "\\w*-*\\w*-*\\w*.\\d{4}$") |>
    str_remove("\\W\\d{4}$") |> unique()

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
    ) |> as.data.table() |>
      {\(x) structure(x,
                      class = c("climate",
                                if (projections) "projections",
                                class(x)),
                source = "STICS")}() |>
      add_ids()

  }) |>
    {\(x) structure(x,
                    class = c("climate",
                              if (projections) "projections",
                              class(x)),
                    source = "STICS")}() |>
    set_names(names)

  if (return_type == 0){
    return(climate_data)
  } else if (return_type == 1){
    df <- purrr::list_rbind(climate_data) |>
      as.data.table() |>
      {\(x) structure(x,
                      class = c("climate",
                                if (projections) "projections",
                                class(x)),
                      source = "STICS")}()
    return(df)
  }
}
