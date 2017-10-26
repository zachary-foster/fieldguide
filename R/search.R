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
                        max_occ = 10000,
                        max_img = 10,
                        common_name_db = "inat",
                        max_species = 50) {

  # Search for species in range
  my_print("Searching GBIF for observation records...")
  gbif_occ <- rgbif::occ_search(decimalLatitude = lat_range,
                                decimalLongitude = long_range,
                                hasCoordinate = TRUE,
                                return = "data",
                                scientificName = taxon,
                                limit = max_occ)
  if (length(gbif_occ) == 1 && gbif_occ == "no data found, try a different search") {
    stop("No species found in the given area.", call. = FALSE)
  }

  # Filter out unneed columns
  gbif_occ <- gbif_occ[, c("name", "scientificName", "kingdom", "phylum", "order", "family", "genus", "species")]

  # Add root to the taxonomy
  gbif_occ$root <- "Life"

  # Make unique
  gbif_occ$occ_count <- as.numeric(table(gbif_occ$name)[gbif_occ$name])
  gbif_occ <- gbif_occ[! duplicated(gbif_occ$name), ]

  # Sort by number of occurances
  gbif_occ <- gbif_occ[order(gbif_occ$occ_count, decreasing = TRUE), ]

  # Remove any taxa with no species information
  gbif_occ <- gbif_occ[! is.na(gbif_occ$species), ]

  # Filter out uncommon species
  if (nrow(gbif_occ) > max_species) {
    my_print("   Found occurances for ", nrow(gbif_occ),
             " species, but limiting results to the ", max_species," most common.\n")
    gbif_occ <- gbif_occ[seq_len(max_species), ] # Already sorted by number of occurances
  } else {
    my_print("   Found occurances for ", nrow(gbif_occ), " species.\n")
  }

  # Get URLs of photos and other iNaturalist data
  my_print("Looking up image URLs from iNaturalist...")
  raw_inat_data <- lapply(gbif_occ$name, function(x) {
    tryCatch({
      out <- rinat::get_inat_obs(taxon_name = x, quality = "research", maxresults = max_img)
      out$name <- x
      return(out)
    }, error = function(err) {
      return(NULL)
    })
  })
  inat_data <- do.call(rbind, raw_inat_data) # combine into a single table
  inat_data <- inat_data[, c("name", "common_name", "url", "image_url", "user_login",
                             "license", "num_identification_agreements", "num_identification_disagreements")]

  # Print results
  my_print("   Found images for ", length(unique(inat_data$name)), " species.\n")

  # Get common name
  if (common_name_db != "inat") {
    my_print("Looking up common names from ", toupper(common_name_db), "...")
    common_name <- taxize::sci2comm(gbif_occ$name, db = common_name_db,
                                    verbose = FALSE, ask = FALSE, rows = 1)
    common_name <- lapply(common_name, function(x) {
      x <- x[!is.na(x)]
      if (length(x) > 0) {
        x <- Hmisc::capitalize(x)
        x <- unique(x)
      }
      paste0(x, collapse = ", ")
    })
    my_print("   Found common names.\n")
    gbif_occ$common_name <- common_name
  } else {
    gbif_occ$common_name <- vapply(raw_inat_data, FUN.VALUE = character(1),
                                   function(x) {
                                     if (is.null(x) || nrow(x) == 0 || is.na(x[1, "common_name"])) {
                                       return(NA_character_)
                                     } else {
                                       return(Hmisc::capitalize(x[1, "common_name"]))
                                     }
                                   })
  }

  # Get wikipedia content
  my_print("Looking up wikipedia content for taxa...")
  wiki_data <- dplyr::tibble(name =  gbif_occ$name,
                             content = vapply(gbif_occ$name, get_wiki_content, character(1)))

  # Print results
  my_print("   Found Wikipedia content for ", length(unique(wiki_data$name)), " species.\n")

  # Convert to taxmap
  output <- suppressWarnings(taxa::parse_tax_data(gbif_occ,
                                                  class_cols = c("root", "kingdom", "phylum", "order", "family", "genus", "species"),
                                                  datasets = list(inat_data = inat_data, wiki_data = wiki_data),
                                                  mappings = c("name" = "name", "name" = "name")))

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
