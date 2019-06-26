#' @import dplyr
#' @export
lag_corr <- function(dat, country) {
  dat_country <- dat %>%
    select(date,ends_with(country)) %>%
    na.omit() %>%
    arrange_at("date") # Additional safeguard. Making sure the order of dates isn't messed up somehow
  # Growth rate for housing credit
  gr_hc <- dat_country %>%
    select(starts_with(country)) %>%
    as.matrix() %>%
    diff %>%
    as.data.frame()
  # Growth rate for
  gr_pp <- dat_country %>%
    select(starts_with("RPP")) %>%
    as.matrix() %>%
    diff %>%
    as.data.frame()
  corr <- ccf(gr_hc, gr_pp, lag.max = 4, plot = FALSE)
  data.frame(matrix(NA, nrow = 1, ncol = 9)) %>%
    setNames(corr$lag) %>%
    rbind(corr$acf) %>%
    na.omit() %>%
    mutate(country = country) %>%
    mutate(max_corr_t = corr$lag[which.max(abs(corr$acf))])
}
