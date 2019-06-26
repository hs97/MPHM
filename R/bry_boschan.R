#' @import BCDating
#' @import zoo
#' @export

bry_boschan <- function(dat, country, mincycle = 5, minphase = 2) {
  dat_country <- dat %>%
    select("date", ends_with(country)) %>%
    na.omit()
  dat_country_ts <- ts(select(dat_country, ends_with(country)), start = dat_country$date[1], freq = 4)
  try(
    {bb <- BBQ(dat_country_ts, mincycle, minphase)@states
    res <- data.frame(date = time(bb), state = as.matrix(bb)) %>%
      mutate(date = as.yearqtr(date)) %>%
      right_join(y = dat_country, by = "date") %>%
      setNames(gsub("state",paste("state", country, sep = "_"), names(.)))
    return(res)},
    silent = TRUE)
  dat_country %>%
    mutate(state = 1) %>%
    setNames(gsub("state",paste("state", country, sep = "-"), names(.)))
}
