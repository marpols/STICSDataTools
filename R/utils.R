join_dfs <- function(df_list, by = c("file_name", "ian", "jul")) {
  df <- df_list |> reduce(left_join,
                          by = by,
                          suffix = c("", ".y"),
                          keep = FALSE) |>
    select(-matches("\\.y"))

  return(df)

}

df_to_list <- function(df, by = "file_name") {

  tbl_df <- as_tibble(df)

  if (length(by) > 1) {
    vars <- tbl_df[by] |> lapply(unique)
    names <- tidyr::expand_grid(!!!vars) |> tidyr::unite("name", dplyr::everything(), sep = "_") |>
      dplyr::pull(name)
  } else {
    names <- unique(df[[by]])
  }
  tryCatch({
    return(df |> group_split(across(all_of(by))) |> setNames(names))
  }, error = function(e){
    if (grepl("must be a list", e$message)) {
      message("`obj` is already a list")
      return(df)
    }
  })

}

regroup_df_list <- function(df_list, by = ""){
  df <- purrr::list_rbind(df_list)
  return(df_to_list(df, by))
}

df_list_to_df <- function(df_list){
  tryCatch({
  return(purrr::list_rbind(df_list) |>
           as.data.table() |>
           .restore_attrs(df_list, "list")
           )
  }, error = function(e){
    if (grepl("must be a list", e$message)) {
      message("`obj` is already a data.frame or data.table")
      return(df)
    }
  })
}


as.projection <- function(obj){
  class(obj) <- c("projections",
                  class(obj))
  return(obj)
}

as.climate <- function(obj){
  class(obj) <- c("climate",
                  class(obj))
  return(obj)
}

as.simulations <- function(obj){
  class(obj) <- c("simulations",
                  class(obj))
  return(obj)
}

as.STICSsimulations <- function(obj){
  class(obj) <- c("STICS simulations",
                  class(obj))
  return(obj)
}

.restore_attrs <- function(x, template, rmv_cls = "") {
  structure(
    x,
    class = c(setdiff(class(template), rmv_cls), class(x)),
    source = attr(template, "source", exact = TRUE),
    file_type = attr(template, "file_type", exact = TRUE)
  )
}

.filter_data <- function(dataset,
                         filters = character()) {
  if (length(filters) == 0 || all(filters == "")) {
    return(dataset)
  }

  exprs <- rlang::parse_exprs(filters)

  return(dataset |>
    dplyr::filter(!!!exprs))
}






