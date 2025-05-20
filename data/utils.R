get_rate <- function(rates_list, period, category = NULL, subcategory = NULL) {
  if (is.null(category) && is.null(subcategory)) {
    return(rates_list[[period]])
  } else if (!is.null(category) && is.null(subcategory)) {
    return(rates_list[[category]][[period]])
  } else {
    return(rates_list[[category]][[subcategory]][[period]])
  }
}

get_period <- function(year) {
  if (year %in% c("2024_Q4", "2025")) return("period_1")
  if (year %in% as.character(2026:2028)) return("period_2")
  if (year %in% as.character(2029:2030)) return("period_3")
  if (year == "terminal") return("period_4")
  stop(paste("Invalid year:", year))
}
