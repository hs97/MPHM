#' This function calculated the growth rate and take the 3 year moving average
#' @import mFilter
#' @import gridExtra
#' @import zoo
#' @import ggplot2
#' @export

smooth_ks <- function(dat, country) {
  dat %>%
    select(date, ends_with(country)) %>%
    na.omit %>%
    mutate(diff = 100 * c(diff(NA, log(.[[country]]))),
           date = as.yearqtr(date)) %>%
    mutate(ma = rollmean(.$diff, 13, mean, fill = NA))
}

#' This function determines whether a period is boom, bust, or neither based on deviation

det_phase <- function(dat, lambda) {
  dat %>%
    mutate(trend_growth = mean(ma, na.rm = TRUE)) %>%
    mutate(ma_dev = ma - trend_growth) %>%
    mutate(cyc = ifelse(ma_dev > lambda * sd(ma_dev, na.rm = TRUE),
                        'boom', ifelse(ma_dev < (-lambda * sd(ma_dev, na.rm = TRUE)), 'bust', 'neither')))%>%
    na.omit
}

#' This function makes sure the minimum phase length is met.
#' @export
enforce_minphase <- function(x, minphase, gap) {
  # rle stands for run length encoding
  # Will give a list of values and a list of lengths(for each value)
  y <- x %>%
    rle
  # Check if any neither is shorter than minphase
  if (gap) {
    neithers <- which(y$values != 'neither' & y$lengths < minphase)
  } else {
    neithers <- which(y$values == 'neither' & y$lengths < minphase)
  }
  if (any(neithers)) {
    for (i in 1:length(neithers)) {
      ind = neithers[i]
      if (ind == 1) {
        # if neither is the first one to appear, change it to the phase interval right after.
        y$values[ind] = y$values[ind+1]
      } else if (ind == length(y$values)) {
        # If last, change to previous one
        y$values[ind] = y$values[ind-1]
      } else {
        # If left and right are the same(both booms for example), change neither to boom.
        if (y$values[ind - 1] == y$values[ind + 1]) {
          y$values[ind] = y$values[ind-1]
        }
      }
    }
  }
  y <- inverse.rle(y) %>%
    rle()
  # Check for any non-neither intervals that do not meet minphase requirement.
  others <- which(y$values != 'neither' & y$lengths < minphase)
  y$values[others] = 'neither'
  inverse.rle(y)
}


#' @export
ks_ratio_growth <- function(nhc, ngdp, country, lambda, minphase) {
  calculate_ratio(nhc, ngdp, country) %>% # Calculate ratio from nominal hc and gdp
    smooth_ks(., country) %>% # Take difference and moving average
    det_phase(., lambda) %>% # Determine phase
    mutate(cyc = enforce_minphase(., minphase), # Enforce minphase
           date = as.yearqtr(date)) %>%
    mutate(cyc = enforce_minphase(., minphase)) %>%
    select(date, ends_with(country), cyc, trend_growth, ma)
}

#' @export
ks_ratio_growth_hp <- function(x, country, lambda, minphase, lambda_hp, logged = FALSE) {
  x %>%
    select(date, ends_with(country)) %>%
    arrange(date) %>%
    setNames(c('date', country)) %>%
    mutate(logged = logged) %>%
    mutate(diff = if_else(logged, 100 * c(NA, diff(.[[country]])),
                         100 * c(NA, diff(log(.[[country]]))))) %>% # Calculate quarterly growth rates
    na.omit() %>%
    mutate(ma = rollmean(.$diff, 13, mean, fill = NA)) %>% # Moving average
    na.omit() %>%
    mutate(trend = hpfilter(x = .[[country]], freq = lambda_hp)$trend) %>%
    mutate(trend_growth = if_else(logged, 100 * c(NA, diff(trend)),
                                  100 * c(NA, diff(log(trend))))) %>%
    mutate(ma_dev = ma - trend_growth,
           gap_cyc = if_else(.[[country]] - trend > 0, 'boom', 'bust')) %>%
    mutate(cyc = ifelse(ma_dev > lambda * sd(ma_dev, na.rm = TRUE),
                        'boom', ifelse(ma_dev < (-lambda * sd(ma_dev, na.rm = TRUE)), 'bust', 'neither')))%>%
    na.omit %>%
    mutate(cyc = enforce_minphase(cyc, minphase, FALSE),
           gap_cyc = enforce_minphase(gap_cyc, minphase, TRUE),
           date = as.yearqtr(date)) %>%
    mutate(cyc = enforce_minphase(cyc,minphase, FALSE)) %>%
    select(date, ends_with(country), cyc, starts_with('trend'), ma, gap_cyc)
}

#' @export
plot_value <- function(x, country, hp=FALSE) {
  p <- ggplot(x, aes(y = get(country), x = date)) +
    geom_point(aes(color = cyc)) + theme_bw() +
    ylab('Value') + scale_x_yearqtr()
  if (hp == TRUE) {
    p + geom_line(aes(x = date, y = trend))
  } else {
    p
  }
}

#' @export
plot_growth <- function(x) {
  ggplot(x, aes(y = ma, x = date)) +
    geom_point(aes(color = cyc)) + theme_bw() + geom_line(aes(x = date, y = trend_growth)) +
    ylab('Growth Rate') + scale_x_yearqtr()
}


#' @export
calculate_ratio <- function(nhc, ngdp, country) {
  nhc_country <- nhc %>%
    select(date, ends_with(country)) %>%
    set_names(., c('date', 'nhc'))
  ngdp_country <- ngdp %>%
    select(date, ends_with(country)) %>%
    set_names(., c('date', 'ngdp'))
  nhc_country %>%
    left_join(., ngdp_country, by = 'date') %>%
    na.omit() %>%
    mutate(ratio = nhc / 1000 / ngdp) %>%
    select(date, ratio) %>%
    set_names(., c('date', country))
}

#' @export
cyc_intercept <- function(ks_pp, ks_ratio) {
  ks_pp <- ks_pp %>%
    rename(cyc_pp = cyc)
  ks_ratio %>%
    rename(cyc_ratio = cyc) %>%
    right_join(., ks_pp, by = 'date') %>%
    mutate(cyc = ifelse(cyc_pp == cyc_ratio, cyc_pp, 'neither')) %>%
    .$cyc
}

