#' Implements method from Mendoza and Terrones
#' @import mFilter
#' @export

mt <- function(nhc, ngdp, country, lambda_peak, lambda_boom, lambda_hp) {
  ratio_country <- calculate_ratio(nhc, ngdp, country)
  x <- ratio_country %>%
    select(date, ends_with(country)) %>%
    na.omit %>%
    mutate(detrend = hpfilter(x = .[[country]], freq = lambda_hp)$cycle,
           trend = hpfilter(x = .[[country]], freq = lambda_hp)$trend,
           date = as.yearqtr(date)) %>%
    mutate(dist = abs(detrend - lambda_boom * sd(detrend)),
           peak = ifelse(detrend >= lambda_peak * sd(detrend), 'boom', 'none')) %>%
    mutate(cyc = find_boom(.))

  ggplot(x, aes(y = get(country), x = date)) + geom_point(aes(color = cyc)) +
    geom_line(aes(x=date, y=trend)) + theme_bw() + ylab('MT') + scale_x_yearqtr()
}

#' Finding boom
#' @export

find_boom <- function(x) {
  cyc <- x$peak
  y <- x$peak %>%
    rle()
  end_ind <- cumsum(y$lengths)
  start_ind <- c(1, lag(end_ind)[-1] + 1)
  peak <- which(y$values == 'boom')
  if (any(peak)) {
    for (i in 1:length(peak)) {
      ind = peak[i]
      if (ind == 1) {
        left_min = 0
        right_min = which(x$dist == min(x$dist[start_ind[ind+1]:end_ind[ind+1]]))

      } else if (ind == length(y$values)) {
        left_min = which(x$dist == min(x$dist[start_ind[ind-1]:end_ind[ind - 1]]))
        right_min = length(x$peak)
      } else {
        left_min = which(x$dist == min(x$dist[start_ind[ind-1]:end_ind[ind-1]]))
        right_min = which(x$dist == min(x$dist[start_ind[ind+1]:end_ind[ind+1]]))
      }
      cyc[left_min:right_min] = 'boom'
    }
  }
  cyc
}
