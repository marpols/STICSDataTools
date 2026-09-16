#' get mod s files
#'
#' @param exp_dir (string) name of directory where experiment files are stored
#' @param exp_name_format (string) format of experiment name
#' @param usm_name_format (string) file name format
#' @param js_path (string) path to STICS javascript directory
#' @param ws (string) path to workspace
#' @param dir (string) name of directory where results are stored (exp_dir parent folder)
#' @param usm_list (char) list of usms names
#' @param ver_num (numeric)(optional) version number
#' @param stncode (string)(optional) weather station code id
#' @param soilcode (string)(optional) soil code id
#' @param ssp (string)(optional) ssp
#' @param group (string)(optional) variables to group by if returning list
#' @param return_type 0 = list 1 = data.frame
#'
#'@export
get_mod_s <- function(exp_dir,
                      exp_name_format = "expid_runid",
                      usm_name_format = "model_ssp_year_stncode_soilcode_verid",
                      js_path = javastics_path,
                      ws = workspace,
                      dir = "RESULTS",
                      usm_list = character(),
                      ver_num = NULL,
                      stn_code = character(),
                      soil_code = character(),
                      ssp = character(),
                      group = NULL,
                      return_type = 0) {

  files <- .get_files(
    exp_dir = exp_dir,
    exp_name_format = exp_name_format,
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
