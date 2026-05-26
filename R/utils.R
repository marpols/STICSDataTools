#'@export
join_dfs <- function(df_list,
                     by = c("file_name", "ian", "jul")) {
  df_list |> purrr::reduce(left_join,
                          by = by,
                          suffix = c("", ".y"),
                          keep = FALSE) |>
    select(-matches("\\.y"))

}

#'@export
df_to_list <- function(df, by = "file_name") {

  org_class <- class(df)
  tbl_df <- dplyr::as_tibble(df)

  if (length(by) > 1) {
    vars <- tbl_df[by] |> lapply(unique)
    names <- tidyr::expand_grid(!!!vars) |> tidyr::unite("name",
                                                         dplyr::everything(),
                                                         sep = "_") |>
      dplyr::pull(name)
  } else {
    names <- unique(df[[by]])
  }
  tryCatch({
    df |> dplyr::group_split(across(all_of(by))) |>
      purrr::set_names(names) |>
      "class<-"(org_class)

  }, error = function(e){
    if (grepl("must be a list", e$message)) {
      message("`obj` is already a list")
      return(df)
    }
  })

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
df_list_to_df <- function(df_list){
  tryCatch({
  purrr::list_rbind(df_list) |>
      data.table::as.data.table() |>
           .restore_attrs(df_list, "list")
  }, error = function(e){
    if (grepl("must be a list", e$message)) {
      message("`obj` is already a data.frame or data.table")
      return(df)
    }
  })
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






