#' @import xts
#' @export

to_ts <- function(dat, country) {
  dat.country <- dat %>%
    select("date", country) %>% 
    na.omit()
  dat.country <- ts(dat.country[, country], start = as.yearqtr(dat.country$date[1]), freq = 4)
  return (dat.country)
}