#' This function reads in the housing market data and cleans
#' the date formatting
#'
#' @param x is the filepath to the credit data
#' @import readxl
#' @import zoo
#' @import dplyr
#' @export

read_hc <- function(x) {
  dat <- read_excel(x, sheet = "HouseCredit_NTcurr") %>%
    rename(date = X__1) %>%
    mutate(date = as.yearqtr(date, format = "%Y-%m-%d"))
  dat[dat == 0] <- NA
  return(dat)
}

#' This function reads in the property price data and cleans
#' the date formatting
#'
#' @param x is the filepath to the price data
#' @import readxl
#' @import zoo
#' @import dplyr
#' @export
read_pp <- function(x) {
  read_excel(x, sheet = "Quarterly Series", skip = 3) %>%
    rename(date = Period) %>%
    mutate(date = as.yearqtr(date, format = "%Y-%m-%d"))
}

#' This function reads in the macroprudential policy actions and cleans
#' the date formatting
#'
#' @param x is the specific prudential policy
#' @param path is the file path
#' @import readxl
#' @import zoo
#' @import dplyr
#' @export
read_mp <- function(x, path){
  mp <- read_excel(path, sheet = x, skip = 3) %>%
    t %>%
    as.data.frame()
  names(mp) <- as.character(unlist(mp[1,]))
  mp = mp[-1, ]
  colnames(mp) <- paste(colnames(mp), x, sep = "_")
  mp %>%
    tibble::rownames_to_column("date") %>%
    mutate(date = as.Date(as.numeric(date) - 1, origin = "1899-12-30")) %>% # Confirm -1
    na.omit() %>%
    mutate(date = as.yearqtr(date))
}