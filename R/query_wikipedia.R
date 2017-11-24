query_wikipedia <- function(species_names) {
  my_print("Looking up wikipedia content for taxa...")
  wiki_data <- dplyr::tibble(name =  species_names,
                             content = vapply(gbif_occ$name, get_wiki_content, character(1)))

  # Print results
  my_print("   Found Wikipedia content for ", length(unique(wiki_data$name)), " species.\n")
}
