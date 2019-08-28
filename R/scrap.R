plot_ma <- function(x, country, ratio= FALSE) {
  cred <- ggplot(x, aes(y = get(country), x = date)) +
    geom_point(aes(color = cyc)) + theme_bw() +
    ylab(paste(country, "credit", sep = ' ')) + scale_x_yearqtr()
  cred_growth <- ggplot(x, aes(y = ma, x = date)) + geom_point(aes(color = cyc)) +
    theme_bw() + ylab(paste(country, "credit growth", sep = ' ')) +
    scale_x_yearqtr() + geom_hline(yintercept = mean(x$ma, na.rm = TRUE))
  if (ratio == TRUE) {
    ratio <- ggplot(x, aes(y = ratio, x = date)) + geom_point(aes(color = cyc)) +
      theme_bw() + ylab(paste(country, "ratio", sep = ' ')) +
      scale_x_yearqtr()
    ratio_growth = cred_growth + ylab(paste(country, "ratio growth", sep = ' '))
    grid.arrange(ratio, ratio_growth, cred, ncol = 1, top = country)
  } else {
    grid.arrange(cred, cred_growth, ncol = 1, top = country)}
}

kuttner_sheng <- function(dat, country, lambda, minphase) {
  dat %>%
    smooth_ks(., country)
  det_phase(., lambda) %>%
    mutate(cyc = enforce_minphase(., minphase)) %>%
    plot_ma(., country)
}
