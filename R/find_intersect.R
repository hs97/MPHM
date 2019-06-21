#' This function finds the intersection by omitting rows containing NAs
#' @param x dataframe containing information on 2/3 data series
#' @param country the country we'd like to look at
#'
#' @import dplyr
#' @export
find_intersect <- function(x, country) {
  x %>%
    select(date, union(starts_with(country), ends_with(country))) %>%
    na.omit()
}

#' This function turn the entries into NA if not covered in coverage years
#' @param x coverage years
#' @param mp macroprudential action data
#'
#' @import dplyr
#' @export
mp_reduce_range <- function(cy, mp) {
  mp %>%
    select(date, starts_with(cy["Country"])) %>%
    filter((date >= cy["mp_start"]) & (date <= cy["mp_end"]))
}

#' This function finds the start and end date of a country given a data series
#' @param country country name
#' @param dat dataset
#' @param dat_name name of the series
#'
#' @import dplyr
#' @export

find_start_end <- function(country, dat, dat_name){
  x <- dat %>%
    select(date, union(starts_with(country), ends_with(country))) %>%
    na.omit()
  # If there's no column starting/ending with country, initialize as NA
  if (ncol(x) == 1) {
    return ()
  }else {
    x <- data.frame(country, min(x$date), max(x$date))
  }
  names(x) <- c("Country", paste(dat_name, 'start', sep = "_"), paste(dat_name, 'end', sep = "_"))
  x
}

#' This function is a helper that calls find_start_end
#' @param country_list list of country names
#' @param dat dataset
#' @param dat_name name of the series
#'
#' @import dplyr
#' @import data.table
#' @export

find_start_end_all <- function(country_list, dat, dat_name) {
  lapply(country_list, find_start_end, dat = dat, dat_name = dat_name) %>%
    rbindlist
}
