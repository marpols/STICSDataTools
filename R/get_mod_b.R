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
    type = "mod_b",
    return_type = return_type
  ) |>
    as.cropr()

  files

}
