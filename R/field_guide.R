

rgbif::occ_search(hasCoordinate = TRUE, decimalLatitude = "43,45", decimalLongitude = "-122,-120")



#' Make a field guide for a location
#'
#' Uses a location name to make a pdf of wikipedia articles for all species found within a given radius according to GBIF.
#'
#' @param place (\code{character} of length 1) Where to search for species
#' @param output_path (\code{character} of length 1) Path to the output file
#' @param radius (\code{numeric} of length 1) How far in kilometers from \code{place} to look for species.
#'
#' @return Path to the output file
#'
#' @examples
#' \dontrun{
#' make_field_guide("prineville reservoir", "~/)
#' }
make_field_guide <- function(place,
                             output_path = paste0(gsub(" ", "_", place), ".pdf"),
                             radius = 10) {
  # Get geographic coordinates for place name
  possible_places <- geonames::GNsearch(name = place)
  if (nrow(possible_places) == 0) {
    stop(paste0('Could not find any matches to place name "', place,'"'))
  }

  # Ask user to choose if there are multiple hits
  if (nrow(possible_places) > 1) {
    possiblities <- apply(possible_places, MARGIN = 1,
                          function(x) paste(x['name'], x['adminName1'], x['countryName'], sep = ", "))
    cat(paste0('Multiple possiblities found for "', place, '":\n'))
    cat(paste(seq_along(possiblities), possiblities, collapse = "\n", sep = ":"))
    choice = -1
    while(choice < 1 ){
      choice <- readline("Which location do you want?: ")
      choice <- ifelse(grepl("\\D",choice),-1,as.integer(choice))
      if(is.na(choice)){break}  # breaks when hit enter
    }
    location <- as.numeric(unlist(possible_places[choice, c('lat', 'lng')]))
  } else {
    location <- as.numeric(unlist(possible_places[1, c('lat', 'lng')]))
  }

  # Get range of coords to search in
  lat_diff <- abs(radius / 110.574 / 2) # http://stackoverflow.com/questions/1253499/simple-calculations-for-working-with-lat-lon-km-distance
  long_diff <- abs(radius / (111.32 * cos(location[2] * pi / 180)) / 2)
  lat_range <- paste(location[1] - lat_diff, location[1] + lat_diff, sep = ",")
  long_range <- paste(location[2] - long_diff, location[2] + long_diff, sep = ",")

  # Search for species in range
  gbif_occ <- rgbif::occ_search(decimalLatitude = lat_range,
                                decimalLongitude = long_range,
                                hasCoordinate = TRUE,
                                return = "data",
                                limit = 10000)
  species_in_range <- unique(gbif_occ$species)
  species_in_range <- species_in_range[!is.na(species_in_range)]

  # Check that pages exist on wikipedia
  is_on_wiki <- function(a_name) {
    info <- WikipediR::page_info("en", "wikipedia", page = a_name)
    names(info$query$pages)[1] != -1
    Sys.sleep(0.5) # be nice to wiki servers
  }
  on_wiki <- vapply(species_in_range, is_on_wiki, logical(1))
  species_in_range <- species_in_range[on_wiki]

  # Make PDF of wiki articles
  wiki_urls <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", species_in_range))
  command <- paste("wkhtmltopdf", "--zoom", 0.5, paste(wiki_urls, collapse = " "), output_path)
  system(command)
}
