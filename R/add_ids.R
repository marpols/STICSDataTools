#'@export
add_ids <- function(df,
                    format){
  UseMethod("add_ids")
}

#'@export
add_ids.default <- function(df,
                            format){

  names <- get_ids(format)

  .add_ids_func(func = tidyr::separate_wider_delim,
                      df = df,
                      col = 'file_name',
                      delim = "_",
                      names = names) |>
    dplyr::relocate(all_of(names)) |>
    dplyr::relocate("ian", .before = "mo") |>
    dplyr::mutate(across("ian", as.integer))

}

#'@export
add_ids.climate <- function(df,
                            format) {

  if(format == "stn name"){
    .add_ids_func(func = tidyr::separate_wider_regex,
                  df = df,
                  col = "file_name",
                  names = "stncode",
                  patterns = "^[[:upper:]]|(?<=_)[[:upper:]]")
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

.add_ids_func <- function(func,
                          df,
                          col,
                          names,
                          ...){

  args <- list(...)
  old_class <- class(df)
  unclass_df <- as.data.frame(unclass(df))

  func_args <- list(
    data = unclass_df,
    cols = col,
    names = names,
    cols_remove = FALSE,
    too_few = "align_start",
    names_repair = "minimal"
  )

  if ("delim" %in% names(args)) {
    func_args['delim'] <- args[["delim"]]
  } else {
    func_args['patterns'] <- args[["patterns"]]
  }

  return(rlang::exec(func, !!!func_args) |>
           structure(class = old_class))
}

#'@export
set_ids <- function(name,
                    format){

  ids <- get_ids(format)

  stringr::str_split(name, "_") |>
    unlist() |>
    purrr::set_names(ids) |>
    as.list()
}

#'@export
get_ids <- function(format){

  names <- stringr::str_split(format, "_") |> unlist()
  if("year" %in% tolower(names)){
    names[grep("year", names, TRUE)] <- "ian"
  }

  names
}
