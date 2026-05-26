#'@export
get_clim_sum <- function(df,
                                 mo_range = 1:12,
                                 by = c("stncode", "ian")){

  df_sums <- .calc_clim_sums(df = df,
                            mo_range = mo_range,
                            by = by) |>
    add_ids() |>
    summarise(Precipitation_cum_avg = mean(Precipitation_cum),
              GDD_cum_avg = mean(GDD_cum),
              .by = by)


  df_means <- .calc_clim_means(df = df,
                              mo_range = mo_range,
                              by = by)

  join_dfs(list(df_means, df_sums), by) |>
    {\(x) structure(x,
                    file_type = "summary")}()

}

get_clim_sum.projections <- function(df,
                                     mo_range = 1:12,
                                     by = c("ssp", "model", "ian")){
  df_sums <- .calc_clim_sums(df = df,
                            mo_range = mo_range,
                            by = by) |>
    add_ids() |>
    dplyr::summarise(Precipitation_cum_avg = mean(Precipitation_cum),
              GDD_cum_avg = mean(GDD_cum),
              .by = by)


  df_means <- .calc_clim_means(df = df,
                              mo_range = mo_range,
                              by = by)

  new_df <- join_dfs(list(df_means, df_sums), by) |>
    {\(x) structure(x,
                    file_type = "summary")}()

  return(new_df)

}

.calc_clim_sums <- function(...) {

  args <- list(...)
  df <- args[["df"]]
  mo_range <- args[["mo_range"]]
  by <- args[["by"]]


  if (!is.data.frame(df)) {
    df <- df_list_to_df(df)
  }

  if (!("gdd" %in% tolower(names(df)))) {
    df_gdd <- .calc_gdd(df, mo_range = mo_range)
    df <- join_dfs(list(df, df_gdd))
  }

  sum_by <- c("ian", "file_name", if ("mo" %in% by) "mo")


  df[mo %in% mo_range, ] |>
    dplyr::summarise(
      Precipitation_cum = sum(Precipitation, na.rm = TRUE),
      GDD_cum = sum(GDD, na.rm = TRUE),
      .by = sum_by
    ) |>
    {\(x) structure(x,
                    class = class(df),
                    source = "STICS",
                    file_type = "summary")}()

}

.calc_clim_means <- function(...) {

  args <- list(...)
  df <- args[["df"]]
  mo_range <- args[["mo_range"]]
  by <- args[["by"]]

  df[mo %in% mo_range, ] |>
    dplyr::summarise(
      MinTemp_avg = mean(MinTemp, na.rm = TRUE),
      MaxTemp_avg = mean(MaxTemp, na.rm = TRUE),
      WindSpeed_avg = mean(WindSpeed, na.rm = TRUE),
      SolarRad_avg = mean(SolarRad, na.rm = TRUE),
      VP_avg = mean(VP),
      .by = all_of(by)
    ) |>
    {\(x) structure(x,
                    class = class(df),
                    source = "STICS",
                    file_type = "summary")}()

}


