#'This function merges the read data.
#'It can either merge two with the given keys, or merge all three
#'
#' @param x a list of dataframes containing the read data
#' @param first first dataframe to merge
#' @param second second dataframe to merge
#' @param all boolean indicating whether all three are merged
#'
#' @import dplyr
#' @import purrr
#' @export
merge_dat <- function(x, first = "mp", second = "pp", all = FALSE) {
  if (all == TRUE) {
    x %>%
      reduce(full_join, by = "date") %>%
      arrange_at("date")
  } else {
    full_join(get(first), get(second), by="date") %>%
      arrange_at("date")
  }
}
