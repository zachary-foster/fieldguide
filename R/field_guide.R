#' Return species for a coord range
#'
#' Return species for a coordinate range according to GBIF.
#'
#' @param lat_range (\code{numeric} of length 2) The range of latitudes to search species occurance data for.
#' @param long_range (\code{numeric} of length 2) The range of longitudes to search species occurance data for.
#' @param taxon (\code{character} of length 1) A taxon name to search for.
#'
#' @return Path to the output file
#'
#' @export
search_area <- function(lat_range,
                        long_range,
                        taxon = NULL) {
  # Search for species in range
  gbif_occ <- rgbif::occ_search(decimalLatitude = lat_range,
                                decimalLongitude = long_range,
                                hasCoordinate = TRUE,
                                return = "data",
                                scientificName = taxon,
                                limit = 10000)

  # Filter out unneed columns
  gbif_occ <- gbif_occ[, c("name", "scientificName", "kingdom", "phylum", "order", "family", "genus", "species")]

  # Make unique
  gbif_occ <- unique(gbif_occ)

  # Convert to taxmap
  output <- suppressWarnings(taxa::parse_tax_data(gbif_occ,
                                                  class_cols = c("kingdom", "phylum", "order", "family", "genus", "species")))

  return(output)
}


#' Return species for a coord and radius
#'
#' Return species for a coordinate and radius according to GBIF.
#'
#' @param long_range (\code{numeric} of length 2) The range of longitudes to search species occurance data for.
#' @param lat_range (\code{numeric} of length 2) The range of latitudes to search species occurance data for.
#' @param radius (\code{numeric} of length 1) How far in kilometers from \code{place_name} to look for species.
#' @param taxon (\code{character} of length 1) A taxon name to search for.
#'
#' @return Path to the output file
#'
#' @export
search_radius <- function(lat, long, radius = 40, taxon = NULL) {
  # Get range of coords to search
  location <- as.numeric(c(lat, long))
  lat_diff <- abs(radius / 110.574 / 2) # http://stackoverflow.com/questions/1253499/simple-calculations-for-working-with-lat-lon-km-distance
  long_diff <- abs(radius / (111.32 * cos(location[2] * pi / 180)) / 2)
  lat_range <- paste(location[1] - lat_diff, location[1] + lat_diff, sep = ",")
  long_range <- paste(location[2] - long_diff, location[2] + long_diff, sep = ",")

  # Search area
  search_area(lat_range = lat_range, long_range = long_range, taxon = taxon)
}


#' Make a field guide for a location
#'
#' Uses a location name to make a pdf of wikipedia articles for all species found within a given radius according to GBIF.
#'
#' @param place_name (\code{character} of length 1) Where to search for species.
#' @param radius (\code{numeric} of length 1) How far in kilometers from \code{place_name} to look for species.
#' @param taxon (\code{character} of length 1) A taxon name to search for.
#' @param ask (\code{logical}) if \code{TRUE}, ask the user questions when there are ambiguities.
#'
#' @return Path to the output file
#'
#' @examples
#' \dontrun{
#' search_place(place_name = "prineville reservoir")
#' }
#'
#' @export
search_place <- function(place_name,
                         radius = 40,
                         taxon = NULL,
                         ask = FALSE) {
  # Get geographic coordinates for place name
  possible_places <- geonames::GNsearch(name = place_name)
  if (nrow(possible_places) == 0) {
    stop(paste0('Could not find any matches to place name "', place_name,'"'))
  }

  # Ask user to choose if there are multiple hits
  if (nrow(possible_places) > 1) {
    possiblities <- apply(possible_places, MARGIN = 1,
                          function(x) paste(x['name'], x['adminName1'], x['countryName'], sep = ", "))
    if (ask) {
      my_print('Multiple possiblities found for "', place, '":\n')
      my_print(paste(seq_along(possiblities), possiblities, collapse = "\n", sep = ":"))
      choice = -1
      while(choice < 1 ){
        choice <- readline("Which location do you want?: ")
        choice <- ifelse(grepl("\\D",choice),-1,as.integer(choice))
        if(is.na(choice)){break}  # breaks when hit enter
      }
    } else {
      choice <- 1
      my_print('Found "', possiblities[choice], '".')
    }
  } else {
    choice <- 1
  }

  # Get location coords
  location <- as.numeric(unlist(possible_places[choice, c('lat', 'lng')]))

  # Search place
  search_radius(lat = location[1], long = location[2], radius = radius, taxon = taxon)
}
