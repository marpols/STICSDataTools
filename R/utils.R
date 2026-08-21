#'@export
join_dfs <- function(df_list,
                     by = c("file_name", "ian", "jul")) {
  df_list |> purrr::reduce(dplyr::left_join,
                          by = by,
                          suffix = c("", ".y"),
                          keep = FALSE) |>
    dplyr::select(-matches("\\.y"))

}

#'@export
regroup_df_list <- function(df_list,
                            by = ""){
  org_class <- class(df_list)
  df <- purrr::list_rbind(df_list)
  df_to_list(df, by) |>
    "class<-"(org_class)
}

#'@export
as.climate <- function(obj){
  class(obj) <- c("climate",
                  class(obj))
  return(obj)
}

#'@export
as.cropr <- function(obj){
  class(obj) <- c("cropr_simulation",
                  class(obj))
  return(obj)
}

#'@export
as.projections <- function(obj){
  class(obj) <- c("projections",
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

  dataset |>
    dplyr::filter(!!!exprs)
}

expand_date <- function(df){
  stringr::str_split(format, "_") |> unlist()
}






