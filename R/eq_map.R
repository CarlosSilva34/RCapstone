
#' Creates a map earthquake
#'
#' This function creates a leaflet map of selected earthquakes based on
#' NOAA earthquake cleaned data.
#'
#' @param data A data frame containing cleaned NOAA earthquake data
#' @param annot_col A character. The name of the column in the data
#'
#' @return generates a map of earthquakes, The size of the circles reflects the magnitude of the earthquake.
#'
#' @importFrom leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{
#' eq_map(clean_data, annot_col = "LOCATION_NAME")
#' }
#'
#' @export
eq_map <- function(data, annot_col) {

        map <- leaflet::leaflet() %>%
                leaflet::addTiles() %>%
                leaflet::addCircleMarkers(lng = data$LONGITUDE, lat = data$LATITUDE,
                                          radius = data$EQ_PRIMARY, weight = 1,
                                          popup = data[[annot_col]])

        return(map)
}


#' Creates a label for a map of earthquake
#'
#' This function creates a label for the map based on location
#' name, magnitude and casualties from NOAA earthquake data
#'
#' @param data A data frame containing cleaned NOAA data
#'
#' @return A character vector with labels
#'
#' @examples
#' \dontrun{
#' eq_create_label(clean_data)
#' }
#'
#' @export
eq_create_label <- function(data) {
        popup_text <- with(data, {
                part1 <- ifelse(is.na(LOCATION_NAME), "",
                                paste("<strong>Location:</strong>",
                                      LOCATION_NAME))
                part2 <- ifelse(is.na(EQ_PRIMARY), "",
                                paste("<br><strong>Magnitude</strong>",
                                      EQ_PRIMARY))
                part3 <- ifelse(is.na(TOTAL_DEATHS), "",
                                paste("<br><strong>Total deaths:</strong>",
                                      TOTAL_DEATHS))
                paste0(part1, part2, part3)
        })
}
