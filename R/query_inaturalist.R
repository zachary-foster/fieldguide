#' Search iNaturalist for images
#'
#' Search iNaturalist for images for species occurance data
#'
#' @param obj A \code{\link{taxmap}} object produced from \code{search_*} functions.
#' @param max_img The maximum number of images per taxon to return
#'
#' @family query functions
#'
#' @export
query_inat <- function(obj, max_img = 6) {
  # Get list of species to look up
  species <- unique(obj$data$occ$name)
  names(species) <- obj$data$occ$taxon_id[match(species, obj$data$occ$name)]

  # Look up images
  my_print("Looking up image URLs from iNaturalist...")
  progress_bar <- txtProgressBar(min = 0, max = length(species), style = 3)
  raw_inat_data <- lapply(seq_len(length(species)), function(i) {
    tryCatch({
      out <- rinat::get_inat_obs(taxon_name = species[i], quality = "research", maxresults = max_img)
      out$name <- species[i]
      setTxtProgressBar(progress_bar, i)
      return(out)
    }, error = function(err) {
      setTxtProgressBar(progress_bar, i)
      return(NULL)
    })
  })
  close(progress_bar)
  inat_data <- do.call(rbind, raw_inat_data) # combine into a single table
  colnames(inat_data)[colnames(inat_data) == "taxon_id"] <- "inat_taxon_id"

  # Print results
  my_print("   Found images for ", length(unique(inat_data$name)), " species.\n")

  # Add results to input object
  row_counts <- vapply(raw_inat_data, function(x) ifelse(is.null(x), 0, nrow(x)), numeric(1))
  inat <- dplyr::tibble(taxon_id = rep(names(species), row_counts),
                        query = rep(species, row_counts))
  inat <- dplyr::as.tbl(cbind(inat, inat_data))
  obj$data$inat <- inat

  return(obj)
}


