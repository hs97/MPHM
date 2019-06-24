#' Takes the log of all columns but date
#'
#' @param x the data frame we want to take the log of
#'
#' @export

take_log <- function(x) {
  x[colnames(x)[colnames(x) != "date"]] = lapply(x[colnames(x)[colnames(x) != "date"]], log)
  x
}

deflate <- function(x, cpi, country) {
  cpi <- cpi %>%
    select(date, ends_with(country)) %>%
    right_join(., select(x, date, ends_with(country)), by = "date")
  x[colnames(select(x, ends_with(country)))] = x[colnames(select(x, ends_with(country)))] * 100 /
    cpi[paste("cpi", country, sep = "_")]
  x %>%
    select(date, ends_with(country))
}
