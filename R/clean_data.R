#' Read NOAA significant earthquke data
#'
#' @importFrom readr read_delim
#' @param path  defalut path set to inst/extdata/"signif.txt"
#' @param url  set TRUE to download data from NOAA website
#'
#' @return a tibble containing 47 vaiables
#'
#'
#' @examples
#' \dontrun{
#' dat <- read_signif(url = TRUE)
#' }
#' @export
read_signif <- function(path = system.file("extdata", "signif.txt", package = "MSDRCapstone3"), url =FALSE) {
  if (url ==  TRUE){
    signif_dat <- readr::read_delim(url("https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"),  delim = '\t')
  }else{
    signif_dat <- readr::read_delim(path,  delim = '\t')
  }
}


#' Clean the row signif_dat
#'
#' #refactor to standard evaluation instead of dplyr
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom lubridate make_date
#' @importFrom tools toTitleCase
#' @importFrom SparkR to_date
#' @importFrom lubridate ymd
#'
#'
#' @description
#'     A date column created by uniting the year, month, day and converting it to the Date class
#'     LATITUDE and LONGITUDE columns converted to numeric class
#'
#'
#'
#' @param dat
#' A tibble of NOAA signif data with 47 variables, default for signif.txt
#' @return A tibble containing 48 vaiables
#'
#'
#' @examples
#' \dontrun{
#' clean_dat <- eq_clean_data()
#' }
#'
#' @export
#'
eq_clean_data <-function(dat = read_signif()){
  df <- data.frame(dat)
  df$MONTH <- ifelse(is.na(df$MONTH), 1, df$MONTH)
  df$DAY <- ifelse(is.na(df$DAY), 1, df$DAY)

  df$DATE <- lubridate::make_date(df$YEAR, df$MONTH, df$DAY)
  df$LATITUDE <- as.numeric(df$LATITUDE)
  df$LONGITUDE <- as.numeric(df$LONGITUDE)
  df

}

# eq_clean_data <- function(dat = read_signif()) {
#   dt_clean_month_day_date_long_lat <- dat %>%
#     dplyr::mutate(MONTH = ifelse(is.na(MONTH), 1, MONTH)) %>%
#     dplyr::mutate(DAY = ifelse(is.na(DAY), 1, DAY)) %>%
#     dplyr::mutate(DATE = lubridate::make_date(YEAR, MONTH, DAY)) %>%
#     dplyr::mutate(LATITUDE = as.numeric(LATITUDE)) %>%
#     dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE))
#
#
# }

#' cleans the LOCATION_NAME
#'
#' #refactor to standard evaluation instead of dplyr
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rowwise
#' @importFrom tidyr separate
#' @importFrom stringi stri_trans_totitle
#' @importFrom magrittr %>%
#'
#'
#' @description
#' stripping out the country name (including the colon)
#' and converts names to title case (as opposed to all caps)
#'
#'
#' @param
#' dat default for cleaned signif.txt from eq_clean_data()
#'
#' @return
#' a cleand tibble of 48 variables
#'
#'
#' @examples
#' clean_location <- eq_location_clean()
#'
#' @export
eq_location_clean <- function(dat = eq_clean_data()) {
  df <- data.frame(dat)
  df$LOCATION_NAME <- stringi::stri_trans_totitle(trimws(tolower(sub("\\S+: +", "", df$LOCATION))))
  df
}




# eq_location_clean <- function(dat = eq_clean_data()) {
#   clean_location <- dat %>%
#     tidyr::separate(LOCATION_NAME, c("first", "second"), sep = ":") %>%
#     dplyr::select(-first) %>%
#     ###dplyr::rowwise() %>%
#     dplyr::mutate(LOCATION_NAME = ifelse(is.na(second), "Unidentified",
#                                        stringi::stri_trans_totitle(trimws(tolower(second))))) %>%
#
#
#     dplyr::select(-second)
#
#
# }
#



