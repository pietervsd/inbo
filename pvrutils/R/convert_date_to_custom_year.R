#' Convert date to a year that does not start with the first of january
#'
#' @param date the date as an unambiguous character or POSIXct vector
#' @param start_month the month in which the current year starts
#' @param start_day_in_month the first day in the month where the current year starts
#'
#' @return numeric vector containing the new year value
#' @export
#' @importFrom lubridate year month day
#'
#' @examples
#' dates <- c("2018-02-28", "2018-03-01", "2018-03-02")
#' convert_date_to_custom_year(dates, start_month = 3, start_day = 1)
convert_date_to_custom_year <- function(date, start_month = 3, start_day_in_month = 1) {
  
  nas <- sum(is.na(date))
  date <- as.POSIXct(date)
  nass <- sum(is.na(date))
  if (nass > nas) warning("NA values generated when converting to POSIXct")
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  day <- lubridate::day(date)
  cyear <- year
  cyear[(month < start_month) | (month == start_month & day < start_day_in_month)] <- 
    cyear[(month < start_month) | (month == start_month & day < start_day_in_month)] - 1
  cyear
}