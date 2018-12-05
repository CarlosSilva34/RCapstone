#' Cleans raw data from U.S. National Oceanographic and Atmospheric Administation's (NOAA)
#'
#' This function takes data from NOAA website as an input
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}. It adds
#' a column DATE with cleaned date, transforms LATITUDE and
#' LONGITUDE columns as numeric objects.
#'
#' @param raw_data A data frame with raw data obtained from NOAA website
#'
#' @return A data.frame containing the cleaned NOAA data
#'
#' @importFrom lubridate years
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' clean_data <- eq_clean_data(data)
#' }
#'
#' @export
eq_clean_data <- function(raw_data){

        clean_data <- eq_location_clean(raw_data)

        dates <- apply(clean_data, 1,
                       function(row) ifelse(as.numeric(row["YEAR"]) < 0,
                                            as.Date(paste(abs(as.numeric(row["YEAR"])),
                                                          ifelse(is.na(row["MONTH"]),"01", row["MONTH"]),
                                                          ifelse(is.na(row["DAY"]), "01", row["DAY"]), sep = "-")) - lubridate::years(abs(as.numeric(row["YEAR"]))*2),
                                            as.Date(paste(row["YEAR"],
                                                          ifelse(is.na(row["MONTH"]),"01", row["MONTH"]),
                                                          ifelse(is.na(row["DAY"]), "01", row["DAY"]), sep = "-"))))

        clean_data$DATE <- as.Date(dates,format = "%Y-%m-%d", origin = "1970-01-01", tz = "GMT")
        clean_data$LATITUDE <- as.numeric(clean_data$LATITUDE)
        clean_data$LONGITUDE <- as.numeric(clean_data$LONGITUDE)

        return(clean_data)
}

#' Function to clean the LOCATION_NAME column in the NOAA
#' earthquake dataset
#'
#' @param raw_data A data frame with raw data obtained from NOAA website
#'
#' @return A data frame with cleaned LOCATION_NAME column
#'
#' @examples
#' \dontrun{
#' raw_data <- readr::read_delim("signif.txt", delim = "\t")
#' clean_data <- eq_location_clean(raw_data)
#' }
#'
eq_location_clean <- function(raw_data){

        raw_data$LOCATION_NAME <- sapply(raw_data$LOCATION_NAME,
                                         function(loc) {
                                                 x <- gsub(".*:  ", "", loc)
                                                 s <- strsplit(tolower(x), " ")[[1]]
                                                 paste(toupper(substring(s, 1,1)), substring(s, 2),
                                                       sep="", collapse=" ") })

        return(raw_data)
}
