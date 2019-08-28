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
#' @param dates coverage years
#' @param dat dataframe being reduced
#' @param country which country to look at
#' @param coverage_name which coverage to look at. We are usually using 'all'
#'
#' @import dplyr
#' @export
reduce_coverage <- function(dates, dat, coverage_name = 'all') {
  dat <- dat %>%
    select(date, ends_with(dates[["Country"]])) %>%
    mutate(date = as.yearqtr(date)) %>%
    # Filter out all the dates that are outside of valid coverage years
    filter(date >= dates[[paste0(coverage_name, "_start")]], date <= dates[[paste0(coverage_name, "_end")]])
  if (nrow(dat) < 13) {
    # Return nothing if there are fewer than 13 entries
    return ()
  } else {
    return(dat)
  }
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

#' This function summarizes the number of mp actions for a given country
#' @param mp_hc the dataset merging macroprudential data and housing credit data
#' @param country the country we are examining
#' @export
summarize_mp <- function(mp_hc, country) {
  x <- mp_hc %>%
    # Deselect the housing credit data column
    select(-ends_with(country)) %>%
    # Select the macroprudential data column
    select(starts_with(country)) %>%
    # Converting all values in cells into integer
    sapply(., function(x) abs(as.integer(as.character(x))))
  if (length(x) != 0) {
    # Change all nonzero actions into 1, representing a quarter of action
    x[x!=0] = 1
    # Summing the quarters of actions
    x %>%
      colSums(., na.rm = TRUE) %>%
      t %>%
      as.data.frame() %>%
      mutate() %>%
      setnames(., colnames(.), gsub(paste0(country, '_'), "", colnames(.))) %>%
      mutate(sum = rowSums(abs(.)),
             country = country)
  }
}
