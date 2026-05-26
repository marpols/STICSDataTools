.calc_gdd <- function(df,
                     base = 5,
                     mo_range = NULL,
                     negatives = FALSE) {
  mo_range <- if (is.null(mo_range)) {
    1:12
  } else {
    mo_range
  }

  gdd.default <- function(temp_max, temp_min, base) {
    return((temp_max + temp_min) / 2 - base)
  }

  gdd.max <- function(temp_max, temp_min, base) {
    return(pmax(0, ((
      temp_max + temp_min
    ) / 2 - base)))
  }

  func <- ifelse(negatives, gdd.default, gdd.max)

  df[df$mo %in% mo_range, ] |>
    dplyr::group_by(ian) |>
    dplyr::mutate(GDD = func(MinTemp, MaxTemp, base),
           GDD_cum = cumsum(GDD)) |>
    dplyr::ungroup()

}

#'@export
get_gdd <- function(df_list,
                    base = 5,
                    mo_range = NULL,
                    negatives = FALSE,
                    return_type = 0) {

  get_data(
    calc_gdd,
    df_list,
    return_type,
    base = base,
    mo_range = mo_range,
    negatives = negatives
  )

}
