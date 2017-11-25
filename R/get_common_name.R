#' Get common names for species occurance data
#'
#' Get common names for species occurance data
#'
#' @param gbif_occ A \code{\link{taxmap}} object with species occurance data.
#'   The result of running search_* functions.
#' @param db The database to look up common names from.
#'
#' @export
get_common_name <- function(gbif_occ, db = "itis") {
  # Get list of species to look up
  species <- unique(gbif_occ$data$occ$name)
  names(species) <- gbif_occ$data$occ$taxon_id[match(species, gbif_occ$data$occ$name)]

  # Get common name
  my_print("Looking up common names from ", toupper(db), "...")
  common_names <- taxize::sci2comm(species, db = db,
                                   verbose = FALSE, ask = FALSE, rows = 1)
  common_names <- lapply(common_names, Hmisc::capitalize)
  common_names <- lapply(common_names, unique)
  gbif_occ$data$common <- data.frame(stringsAsFactors = FALSE,
                                     taxon_id = rep(names(species), vapply(common_names, length, numeric(1))),
                                     query = rep(names(common_names), vapply(common_names, length, numeric(1))),
                                     name = unlist(common_names))
  gbif_occ$data$common <- dplyr::as.tbl(gbif_occ$data$common)

  my_print("   Found common names.\n")

  return(gbif_occ)
}
