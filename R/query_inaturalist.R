

query_inat <- function(species_names) {
  my_print("Looking up image URLs from iNaturalist...")
  raw_inat_data <- lapply(species_names, function(x) {
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

  return(inat_data)
}


