#' Add IDs
#'
#' @param df A data frame.
#' @param format file name format.
#' @export
add_ids <- function(df,
                    format){
  UseMethod("add_ids")
}

#' @method add_ids climate
#' @export
add_ids.climate <- function(df,
                            format) {

  if(format == "stn name"){
    df |>
      dplyr::mutate(
        stncode = stringr::str_replace(
          file_name,
          "^([[:upper:]])[^_]*_([[:upper:]]).*$",
          "\\1\\2"
        )
      )
  }
  else{
    names <- get_ids(format)
    .add_ids_func(func = tidyr::separate_wider_delim,
                  df = df,
                  col = "file_name",
                  names = names,
                  delim = "_") |>
      dplyr::relocate(all_of(names))
  }
}

#' @method add_ids default
#' @export
add_ids.default <- function(df,
                            format){

  names <- get_ids(format)

  if("ian" %in% names(df)){
    names[names == "ian"] <- NA_character_
  }

  result <- .add_ids_func(
    func = tidyr::separate_wider_delim,
    df = df,
    col = "file_name",
    delim = "_",
    names = names
  ) |>
    dplyr::relocate(dplyr::all_of(names[!is.na(names)]))


  if ("mo" %in% names(result)) {
    result <- result |>
      dplyr::relocate(dplyr::any_of("ian"), .before = "mo") |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of("ian"), as.integer)
      )
  }

  result

}



.add_ids_func <- function(func,
                          df,
                          col,
                          ...){

  args <- list(...)
  old_class <- class(df)
  unclass_df <- as.data.frame(unclass(df))

  func_args <- list(
    data = unclass_df,
    cols = col,
    cols_remove = FALSE,
    too_few = "align_start",
    names_repair = "minimal"
  )

  if ("delim" %in% names(args)) {
    func_args[['names']] <- args[["names"]]
    func_args[['delim']] <- args[["delim"]]
  } else {
    func_args[['patterns']] <- args[["patterns"]] |>
                                    stats::setNames("stncode")
  }

  rlang::exec(func, !!!func_args) |>
           structure(class = old_class)
}

#' @export
set_ids <- function(name,
                    format){

  ids <- get_ids(format)

  stringr::str_split(name, "_") |>
    unlist() |>
    purrr::set_names(ids) |>
    as.list()
}

#' @export
get_ids <- function(format){

  names <- stringr::str_split(format, "_") |> unlist()
  if("year" %in% tolower(names)){
    names[grep("year", names, TRUE)] <- "ian"
  }

  names
}
