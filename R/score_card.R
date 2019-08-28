#' @export
sum_actions <- function(row) {
  actions <- row %>%
    sapply(., function(x)as.integer(as.character(x)))
  if (sum(actions > 0) > 0 && sum(actions < 0 ) > 0) {
    'neither'
  } else if (sum(actions > 0) > 0) {
    'tighten'
  } else if (sum(actions < 0) > 0) {
    'loosen'
  } else {
    'neither'
  }
}

#' @export
score_card <- function(ks_dat, mp, country, ks_dat_2 = NA, intersect = FALSE){
  if (intersect == TRUE && !is.na(ks_dat_2)) {
    ks_dat$cyc = cyc_intercept(ks_dat, ks_dat_2[[country]])
  }
  x <- mp %>%
    select(date, ends_with(country))%>%
    mutate(date = as.yearqtr(date)) %>%
    full_join(., select(ks_dat, - ends_with(country, ignore.case = FALSE)), by = 'date') %>%
    arrange(date) %>%
    select(-date, -trend, -trend_growth, -ma) %>%
    na.omit() %>%
    mutate(action = apply(select(., -cyc, -gap_cyc), 1, sum_actions))
  hp_score <- table(x$action, x$cyc)

  gap_score <-  table(x$action, x$gap_cyc)
  list(hp = hp_score, gap = gap_score)
}

#' @export
contingency_table <- function(ks_pp, ks_ratio, country) {
  pp_cyc <- ks_pp%>%
    select(date, cyc) %>%
    rename(pp_cyc = cyc)
  cyc <- ks_ratio %>%
    select(date, cyc) %>%
    rename(ratio_cyc = cyc) %>%
    left_join(., pp_cyc, by='date') %>%
    na.omit()
  table(cyc$pp_cyc, cyc$ratio_cyc)
}

#' @export
monetary_macroprudential <- function(mp, monetary, country) {
  x <- mp %>%
    select(date, ends_with(country, ignore.case = FALSE))%>%
    na.omit() %>%
    mutate(action = apply(select(., -date), 1, sum_actions)) %>%
    select(date, action) %>%
    full_join(., select(monetary, date, ends_with(country, ignore.case = FALSE)), by = 'date') %>%
    na.omit() %>%
    select(action, ends_with(country, ignore.case = FALSE)) %>%
    setNames(c('action', 'rates'))
  table(x$action, x$rates)
}
