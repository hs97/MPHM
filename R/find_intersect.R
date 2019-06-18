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
