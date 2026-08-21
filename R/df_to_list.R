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
