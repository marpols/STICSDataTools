.calc_gdd <- function(df,
                     base = 5,
                     mo_range = 1:12,
                     negatives = FALSE,
                     by = "ian") {

  gdd.default <- function(temp_max, temp_min, base) {
    return((temp_max + temp_min) / 2 - base)
  }

  gdd.max <- function(temp_max, temp_min, base) {
    return(pmax(0, ((
      temp_max + temp_min
    ) / 2 - base)))
  }

  func <- if (negatives) gdd.default else gdd.max

  df[df$mo %in% mo_range, ] |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::mutate(GDD = func(MinTemp, MaxTemp, base),
           GDD_cum = cumsum(GDD)) |>
    dplyr::ungroup()

}

#'@export
get_gdd <- function(df_list,
                    base = 5,
                    mo_range = 1:12,
                    negatives = FALSE,
                    return_type = 0,
                    ...) {

  .get_data(
    df = df_list,
    func = .calc_gdd,
    return_type = return_type,
    base = base,
    mo_range = mo_range,
    negatives = negatives,
    ...
  )

}
