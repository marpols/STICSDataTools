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
    result <- df |> dplyr::group_split(across(all_of(by))) |>
      purrr::set_names(names) |>
      purrr::map(\(x) {
        x <- data.table::as.data.table(x)
        class(x) <- org_class
        x
      })

    return(result)

  }, error = function(e){
    if (grepl("must be a list", e$message)) {
      message("`obj` is already a list")
      return(df)
    }
  })

}
