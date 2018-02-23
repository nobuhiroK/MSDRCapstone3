#' leaflet map for NOAA sifnificant earthquakes data
#'
#' @description The function maps the epicenters (LATITUDE/LONGITUDE) and annotates
#' each point with in pop up window containing annotation data stored in a column of the data frame.
#'
#'
#' @details  The user should be able to choose which column is used for the annotation
#' in the pop-up with a function argument named annot_col. Each earthquake should be
#' shown with a circle, and the radius of the circle should be proportional
#' to the earthquake's magnitude (EQ_PRIMARY).
#'
#' @param dataframe with significant earthquakes data
#' @param annot_col a column for annotation
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{
#' eq_location_clean() %>%
#'    dplyr::filter(COUNTRY %in% c('JAPAN')) %>%
#'    dplyr::filter(DATE > '2000-01-01') %>%
#'    eq_map(annot_col="DATE")
#' }
#'
#' @export

eq_map <-  function(data,  annot_col = "DATE"){
  data%>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(radius = ~EQ_PRIMARY,
                     lng = ~ LONGITUDE, lat = ~ LATITUDE,
                     popup =as.character(data[[annot_col]])
    )
  }


#' function to create a HTML label for Location, Magnitude, and Total deaths
#'
#' @description  This function should put together a character string for each earthquake
#' that will show the cleaned location (as cleaned by the eq_location_clean() function created in Module
#'  1), the magnitude (EQ_PRIMARY), and the total number of deaths (TOTAL_DEATHS), with boldface labels
#'  for each ("Location", "Total deaths", and "Magnitude").
#'
#'  @details If an earthquake is missing values for any of these,
#'  both the label and the value should be skipped for that element of the tag.
#'
#'
#'
#' @param dataframe with significant earthquakes data
#'
#' @examples
#' \dontrun{
#' eq_location_clean() %>%
#'    dplyr::filter(COUNTRY %in% c('JAPAN')) %>%
#'    dplyr::filter(DATE > '2000-01-01') %>%
#'    dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'    eq_map(annot_col = "popup_text")
#' }
#'
#'
#' @export

eq_create_label<- function(df){
  dff <- df%>%
  dplyr::mutate(location = ifelse(!is.na(LOCATION_NAME),
                                  paste0("<b>Location name:</b>", LOCATION_NAME, "<br />"),  " ")) %>%
    dplyr::mutate(mag = ifelse(!is.na(EQ_PRIMARY),
                               paste0("<b>Magnitude:</b>", EQ_PRIMARY, "<br />"),  " ")) %>%
    dplyr::mutate(tot = ifelse(!is.na(TOTAL_DEATHS),
                               paste0("<b>Total deaths:</b>", TOTAL_DEATHS, "<br />"),  " "))
  return(paste(dff$location, dff$mag, dff$tot, sep = " "))
}
