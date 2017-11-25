#' Return species for a coord range
#'
#' Return species for a coordinate range according to GBIF.
#'
#' @param lat_range (\code{numeric} of length 2) The range of latitudes to
#'   search species occurance data for.
#' @param long_range (\code{numeric} of length 2) The range of longitudes to
#'   search species occurance data for.
#' @param taxon (\code{character} of length 1) A taxon name to search for.
#' @param max_occ (\code{numeric} of length 1) The maximum number of occurances
#'   to retreive in order to determine which species are present. A larger
#'   number might return more species. There is a hard maximum of 200,000.
#' @param max_img (\code{numeric} of length 1) The maximum number of images URLs
#'   to return per species.
#' @param common_name_db (\code{character} of length 1) The name of a database
#'   to get common names from. Choices include "inat" (default), "eol", "itis"
#'   "ncbi", "worms", or "iucn". "inat" is by far the fastest because it is used
#'   either way to get image urls.  In increasing order of number of common
#'   names: inat, ncbi, itis, eol
#' @param max_species The maximum number of species that will be returned. The
#'   species with the least occurances will be dropped. What is actually
#'   returned may be less than this if either of the \code{img_needed} and \code{wiki_needed} options
#'   are true, since this filter is applied first for performance reasons.
#'
#' @return Path to the output file
#'
#' @export
search_area <- function(lat_range,
                        long_range,
                        taxon = NULL,
                        max_species = 50,
                        max_occ = 500) {
  # Internal parameters
  batch_size <- 30 # maximum is 300
  cols_to_keep <- c("name", "key", "decimalLatitude", "decimalLongitude", "issues",
                    "datasetKey", "publishingOrgKey", "publishingCountry", "protocol",
                    "lastCrawled", "lastParsed", "crawlId", "basisOfRecord", "taxonKey",
                    "kingdomKey", "phylumKey", "classKey", "orderKey", "familyKey",
                    "genusKey", "scientificName", "kingdom", "phylum", "order", "family",
                    "genus", "genericName", "specificEpithet", "taxonRank", "dateIdentified",
                    "year", "month", "day", "eventDate", "modified", "lastInterpreted",
                    "references", "license", "geodeticDatum", "class", "countryCode",
                    "country", "rightsHolder", "identifier", "verbatimEventDate",
                    "datasetName", "verbatimLocality", "collectionCode", "gbifID",
                    "occurrenceID", "taxonID", "catalogNumber", "recordedBy",
                    "institutionCode", "rights", "identificationID")

  # Search for species in range
  my_print("Searching for species observations",
           ifelse(is.null(taxon), "...", paste0('for the taxon "', taxon, '"...')))
  taxa_found <- c()
  overall_occ_data <- NULL
  start <- 0
  while (length(taxa_found) < max_species) {
    one_search <- rgbif::occ_data(decimalLatitude = lat_range,
                                  decimalLongitude = long_range,
                                  scientificName = taxon,
                                  limit = batch_size,
                                  start = start,
                                  hasCoordinate = TRUE,
                                  hasGeospatialIssue = FALSE)
    if (is.null(one_search$data)) {
      my_print("   No more occurances. Ending search.")
      break
    } else {
      one_search$data <- one_search$data[ , cols_to_keep]
      one_search$data <- one_search$data[! is.na(one_search$data$name), ]
      start <- start + batch_size
      taxa_found <- unique(c(taxa_found, one_search$data$taxonKey))
      my_print("   Searched ", start, " occurances so far and found ", length(taxa_found), " species.")
      if (all(one_search$data$taxonKey %in% overall_occ_data$taxonKey)) {
        my_print("   No new species found. Ending search.")
        break
      }
      overall_occ_data <- rbind(overall_occ_data, one_search$data)
    }
  }

  # Filter for only the most common species if there are to many
  occ_counts <- table(overall_occ_data$taxonKey)[as.character(taxa_found)]
  occ_counts <- occ_counts[order(occ_counts, decreasing = TRUE)]
  if (length(taxa_found) > max_species) {
    my_print("   Found occurances for ", length(taxa_found),
             " species, but limiting results to the ", max_species," most common.\n")
    occ_counts <- occ_counts[seq_len(max_species)] # Already sorted by number of occurances
    taxa_found <- names(occ_counts)
  } else {
    my_print("   Found occurances for ", length(taxa_found), " species.\n")
  }

  # Get full occurance data for each species
  my_print("Looking up full occurance data for the ", length(taxa_found), " species found.")
  progress_bar <- txtProgressBar(min = 0, max = length(taxa_found), style = 3)
  get_species_occ <- function(i) {
    result <- rgbif::occ_data(decimalLatitude = lat_range,
                              decimalLongitude = long_range,
                              taxonKey = taxa_found[i],
                              limit = max_occ,
                              hasCoordinate = TRUE,
                              hasGeospatialIssue = FALSE)
    result$data <- result$data[ , cols_to_keep]
    result$data <- result$data[! is.na(result$data$name), ]
    setTxtProgressBar(progress_bar, i)
    return(result$data)
  }
  species_occ_data <- do.call(rbind, lapply(seq_len(length(taxa_found)), get_species_occ))
  close(progress_bar)

  # Add root to the taxonomy
  species_occ_data$root <- "Life"

  # Sort by number of occurances
  species_occ_data$occ_count <- as.numeric(table(species_occ_data$name)[species_occ_data$name])
  species_occ_data <- species_occ_data[order(species_occ_data$occ_count, decreasing = TRUE), ]


  # Convert to taxmap
  output <- suppressWarnings(taxa::parse_tax_data(species_occ_data,
                                                  class_cols = c("root", "kingdom", "phylum", "order", "family", "genus", "specificEpithet")))
  names(output$data) <- "occ"

  # Add coordinates
  output$data$lat_range <- lat_range
  output$data$long_range <- long_range

  return(output)
}


#' Return species for a coord and radius
#'
#' Return species for a coordinate and radius according to GBIF.
#'
#' @param long_range (\code{numeric} of length 2) The range of longitudes to search species occurance data for.
#' @param lat_range (\code{numeric} of length 2) The range of latitudes to search species occurance data for.
#' @param radius (\code{numeric} of length 1) How far in kilometers from \code{place_name} to look for species.
#' @param ... Passed to \code{\link{search_area}}
#'
#' @return Path to the output file
#'
#' @export
search_radius <- function(lat, long, radius = 40, ...) {
  # Get range of coords to search
  location <- as.numeric(c(lat, long))
  lat_diff <- abs(radius / 110.574 / 2) # http://stackoverflow.com/questions/1253499/simple-calculations-for-working-with-lat-lon-km-distance
  long_diff <- abs(radius / (111.32 * cos(location[2] * pi / 180)) / 2)
  lat_range <- paste(location[1] - lat_diff, location[1] + lat_diff, sep = ",")
  long_range <- paste(location[2] - long_diff, location[2] + long_diff, sep = ",")

  # Search area
  search_area(lat_range = lat_range, long_range = long_range, ...)
}


#' Make a field guide for a location
#'
#' Uses a location name to make a pdf of wikipedia articles for all species found within a given radius according to GBIF.
#'
#' @param place_name (\code{character} of length 1) Where to search for species.
#' @param ... Passed to \code{\link{search_radius}}
#'
#' @return Path to the output file
#'
#' @examples
#' \dontrun{
#' search_place(place_name = "prineville reservoir")
#' }
#'
#' @export
search_place <- function(place_name, ...) {
  my_print("Looking up location from Google Maps...")
  location <- suppressMessages(ggmap::geocode(place_name, output = "more", messaging = FALSE)[1, ])
  my_print('   Found "', location$address, '".\n')

  # Search place
  search_radius(lat = location$lat, long = location$lon, ...)
}
