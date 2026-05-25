add_ids <- function(df){
  UseMethod("add_ids")
}

add_ids.simulations <- function(df) {

  projections <- "projections" %in% class(df)

  names <- if(projections){
    c("model", "ssp", "ian", "stn_code", "soil_code", "ver_id")
  } else {
    c("location", "ian", "stn_code", "soil_code", "ver_id")
  }

  cols <- c("stn_code", "soil_code", "ver_id",
            if ("model" %in% names) "model",
            if ("ssp" %in% names) "ssp",
            "ian")

  df <- .add_ids_func(func = tidyr::separate_wider_delim,
                       df = df,
                       delim = "_",
                       names = names) |>
    relocate(all_of(cols)) |>
    mutate(across("ian", as.integer))

  return(df)

}

add_ids.climate <- function(df) {
  if("projections" %in% class(df)){
    df <- add_ids_func(func = tidyr::separate_wider_delim,
                         df = df,
                         names = c("model", "ssp", "location"),
                         delim = "_")
    df[,"stn_code"] <- "P"
  } else {
    df <- .add_ids_func(func = tidyr::separate_wider_regex,
                         df = df,
                         names = "stn_code",
                         patterns = "^[[:upper:]]|(?<=_)[[:upper:]]")
    df[,c("ssp", "model", "location")] <- NA
  }

  cols <- c("stn_code",
            "model",
            "ssp",
            "ian")

  return(df |>
           relocate(all_of(cols)))
}

.add_ids_func <- function(func, df, names,...){

  args <- list(...)
  old_class <- class(df)
  unclass_df <- as.data.frame(unclass(df))

  func_args <- list(
    data = unclass_df,
    cols = "file_name",
    names = names,
    cols_remove = FALSE,
    too_few = "align_start",
    names_repair = "minimal"
  )

  if ("delim" %in% names(args)) {
    func_args$delim <- args[["delim"]]
  } else {
    func_args$patterns <- args[["patterns"]]
  }

  return(rlang::exec(func, !!!func_args) |>
           structure(class = old_class))
}
