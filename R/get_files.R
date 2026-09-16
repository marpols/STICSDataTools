#'read simulation result files from STICS workspace directory
#'
#'`.get_files` returns a data.table or a list of data.tables of STICS simulations
#'
#' @param exp_name_format (string) format of experiment name
#' @param usm_name_format (string) file name format
#' @param js_path (string) path to STICS javascript directory
#' @param ws (string) path to workspace
#' @param dir (string) name of mod_s file directory
#' @param usm_list (char) list of usms names
#' @param ver_num (numeric)(optional) version number
#' @param stncode (string)(optional) weather station code id
#' @param soilcode (string)(optional) soil code id
#' @param ssp (string)(optional) ssp
#' @param group (string)(optional) variables to group by if returning list
#' @param return_type 0 = list 1 = data.frame
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

    cli::cli_progress_update(id = pb_id)

    return(mod_file)
  }

  read_mod_b <- function(f){
    mod_file <- readLines(f)

    cli::cli_progress_update(id = pb_id)

    return(mod_file)
  }

  exp_dir <- args[['exp_dir']]
  exp_ids <- set_ids(exp_dir, args[['exp_name_format']])

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
    pb_id <- cli::cli_progress_bar(name = "Loading simulations",
                                   total = length(exp_files),
                                   format_done = paste0(
                                     "{.green {cli::symbol$tick}} Read {cli::pb_total} usms ",
                                     "in {cli::pb_elapsed}."),
                                   clear = FALSE
                                   )

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
      lapply(data.table::as.data.table) |>
      lapply(.restore_attrs, sims[[1]], c("data.table", "data.frame"))
    sims <- new_sim_list
  }

  if (args[["return_type"]] == 0){
    sims
  } else if (args[["return_type"]] == 1){
    purrr::list_rbind(sims) |>
      data.table::as.data.table()
  }
}




