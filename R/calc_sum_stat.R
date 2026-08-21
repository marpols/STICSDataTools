.calc_sum_stat <- function(op = c("sum", "mean", "max"),
                           ...){

  args <- list(...)
  df <- args[[1]]
  var <- args[["var"]]
  by <- args[["by"]]
  mo_range <- args[["mo_range"]]

  op <- match.arg(op)
  func <- switch(op,
                 sum = sum,
                 mean = mean,
                 max = max)
  tag <- switch(op,
                sum = "_sum",
                mean = "_mean",
                max = "_max")

  by_tag <- ifelse(("mo" %in% by), "_mo", "")
  #calc_tag <- ifelse(identical(op, sum), "_sum", "_mean")
  col_name <- paste(var, by_tag, tag, sep = "")
  monthly <- "mo" %in% by

  tryCatch({
    df_sum <- if(monthly){ df[mo %in% mo_range,] } else {df} |>
      summarise({{col_name}} := func(.data[[var]]),
                .by = all_of(by))

  }, error = function(msg) {
    message("Variable not found. Ensure name and case matches column headings.")
    return(NA)
  })

  return(df_sum)
}
