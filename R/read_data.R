#' This function reads in the CPI data from two datasets.
#'
#' @param x is the filepath to the cp1904 spreadsheet
#' @param y is the filepath to the MacroVar spreadsheet
#' @import readxl
#' @import zoo
#' @import dplyr
#' @export
read_cpi <- function(x, y) {
  x <- read_xlsx(x, sheet = "Monthly Series", skip = 3) %>%
    rename(date = Period) %>%
    select(date, ends_with("628"))  %>%
    mutate(date = as.yearqtr(date)) %>%
    # Take quarterly averages for cpi
    group_by(date) %>%
    summarise_at(.vars = names(.)[-1], mean)
  # The column names were formatted as M:(name):628. Here we only keep the names
  country_list <- substr(names(x)[-1], 3, 4)
  # rename columns by prepending "cpi_"
  names(x)[-1] <- paste("cpi", country_list, sep = "_")
  # Choosing columns containing AR and CO from MacroVar
  y <- read_xlsx(y, sheet = "cpi") %>%
    rename(date = X__1) %>%
    select(date, union(ends_with("AR"),  ends_with("CO"))) %>%
    mutate(date = as.yearqtr(date))
  # Combine the two to make a complete CPI dataset
  x %>%
    select(-ends_with("AR")) %>%
    full_join(y, by = "date")
}

#' This function reads in the housing market data and cleans
#' the date formatting
#'
#' @param x is the filepath to the credit data
#' @param cpi is the cpi data we deflate with
#' @import readxl
#' @import zoo
#' @import dplyr
#' @export

read_hc <- function(x, cpi) {
  dat <- read_excel(x, sheet = "HouseCredit_NTcurr") %>%
    rename(date = X__1) %>%
    mutate(date = as.yearqtr(date, format = "%Y-%m-%d"))
  dat[dat == 0] <- NA
  country_list <- colnames(dat)[colnames(dat) != "date"]
  lapply(country_list, deflate, x = dat, cpi = cpi) %>%
    reduce(full_join, by = "date") %>%
    take_log()
}

#' This function reads in the property price data and cleans
#' the date formatting
#'
#' The returned data frame is deflated and in log form.
#'
#' @param x is the filepath to the price data
#' @param cpi is the cpi data
#' @param country_list is the list of countries
#' @import readxl
#' @import zoo
#' @import dplyr
#' @export
read_pp <- function(pps, ppl, pp_saar, cpi, country_list) {
  pps <- read_excel(pps, sheet = "Quarterly Series", skip = 3) %>%
    rename(date = Period) %>%
    select(date, ends_with("R:628")) %>%
    mutate(date = as.yearqtr(date))
  names(pps)[-1] <- substr(names(pps)[-1], 3, 4)
  ppl <- read_excel(ppl, sheet = "Quarterly Series", skip = 3) %>%
    rename(date = Period) %>%
    mutate(date = as.yearqtr(date))
  # Reformat strings to only contain country names
  names(ppl)[-1] <- substr(names(ppl)[-1], 3, 4)
  ppl_complete <- read_excel(pp_saar, sheet = "HousePriceIndex") %>%
    rename(date = X__1) %>%
    mutate(date = as.yearqtr(date)) %>%
    # Merge SA&AR with ppl since they are both in nominal terms
    full_join(ppl, by = "date")
  pp <- lapply(country_list, select_pp,
         pp = list("pps" = pps, "ppl" = ppl_complete, "cpi" = cpi))  %>%
    reduce(full_join, by = "date") %>%
    take_log()
  # Prepend RPP(Real Property Price) to all columns but date
  names(pp)[-1] <- paste("RPP", names(pp)[-1], sep = "_")
  return(pp)
}

#' This function selects the appropriate price data for the given country
#'
#' @param country is the country name
#' @param pp is the pp data returned by `read_pp`
#' @import dplyr
#' @export

select_pp <- function(country, pp) {
  # Load ppl and pps
  ppl <- pp$ppl %>%
    select(date, ends_with(country)) %>%
    na.omit()
  pps <- pp$pps %>%
    select(date, ends_with(country)) %>%
    na.omit()
  # If pp-long has no data or pps has longer and valid data, use pps
  if (ncol(ppl) == 1 || (length(pps$date) > length(ppl$date)) && ncol(pps) != 1) {
    pps
  } else{
    # deflate ppl
    deflate(ppl, cpi, country)
  }
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
    # Transpose the table to have countries as col names
    t %>%
    as.data.frame()
  # Make country actions as col names
  names(mp) <- as.character(unlist(mp[1,]))
  mp = mp[-1, ]
  # Reformat as mpaction_country
  colnames(mp) <- paste(colnames(mp), x, sep = "_")
  mp %>%
    tibble::rownames_to_column("date") %>%
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
    na.omit() %>%
    mutate(date = as.yearqtr(date)) %>%
    select(-starts_with("Grand Total"))
}

#' This function reads in coverage years for macroprudential actions.
#' For end dates that are not the last day of the quarter, we set them
#' to the quarters before.
#'
#' @param path is the file path
#' @import readxl
#' @import zoo
#' @import dplyr
#' @import timeDate
#' @export

read_cy <- function(path) {
  read_excel(path, sheet = "Coverage years") %>%
    # Filter out excessive rows
    filter(!is.na(Country)) %>%
    # Reformat start
    mutate(mp_start = as.yearqtr(paste(Start, "1", sep = "-"))) %>%
    # Check if date is last day in quarter. If not, move back to last quarter.
    rename(mp_end = `Position (End) Date`) %>%
    mutate(mp_end = if_else(mp_end == as.Date(timeLastDayInQuarter(mp_end)),
                            as.yearqtr(mp_end),
                            as.yearqtr(as.Date(timeFirstDayInQuarter(mp_end)) - 1))) %>% # Use last quarter as last date
    select(Country, mp_start, mp_end)
}
