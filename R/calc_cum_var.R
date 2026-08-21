.calc_cum_var <- function(...){

  args <- list(...)
  df <- args[[1]]
  var <- args[["var"]]
  monthly <- args[["monthly"]]
  mo_range <- args[["mo_range"]]


  group <- if(monthly){ c("ian","mo") } else { "ian" }
  tag <- ifelse(monthly, "_mo", "")

  col_name <- paste(var, tag, "_cum", sep = "")

  tryCatch({
    df <- if(monthly){ df[mo %in% mo_range,] } else {df} |>
      mutate({{col_name}} := cumsum(.data[[var]]),
             .by = all_of(group)) |>
      ungroup()
  }, error = function(msg) {
    message("Variable not found. Ensure name and case matches column headings.")
    message(msg$message)
    return(NA)
  })

  return(df)

}
