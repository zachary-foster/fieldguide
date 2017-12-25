

#' Get wikipedia content for occurance data
#'
#' Get wikipedia content for occurance data
#'
#' @param obj A \code{\link{taxmap}} object produced from \code{search_*} functions.
#'
#' @family query functions
#'
#' @export
query_wikipedia <- function(obj) {

  # Get list of species to look up
  species <- unique(obj$data$occ$name)
  names(species) <- obj$data$occ$taxon_id[match(species, obj$data$occ$name)]

  # Look up raw wikipedia content
  my_print("Looking up wikipedia content for taxa...")
  progress_bar <- txtProgressBar(min = 0, max = length(species), style = 3)
  content <- vapply(seq_len(length(species)), FUN.VALUE = character(1), function(i) {
    result <- get_wiki_content(species[i])
    Sys.sleep(.3) # Be nice to Wikipedia's servers
    setTxtProgressBar(progress_bar, i)
    return(result)
  })
  close(progress_bar)

  # Split into sections
  sections <- strsplit(content, split ="(={2,3}[ a-zA-Z0-9]+={2,3})")

  # Get section titles
  titles <- stringr::str_match_all(content, "(={2,3}[ a-zA-Z0-9]+={2,3})")
  titles <- lapply(seq_len(length(titles)), function(i) {
    sec_titles <- titles[[i]][, 2]
    if (length(sections[[i]]) != length(sec_titles)) {
      sec_titles <- c("== Summary ==", sec_titles)
    }
    return(sec_titles)
  })

  # Combine sections and titles into a table
  wiki <- dplyr::tibble(taxon_id = rep(names(species), vapply(sections, length, numeric(1))),
                        query = rep(species, vapply(sections, length, numeric(1))),
                        title = unlist(titles),
                        title_level = nchar(stringr::str_match(unlist(titles), pattern = "(=+).*")[, 2]),
                        content = unlist(sections))

  # Remove markup from titles
  wiki$title <- stringr::str_match(wiki$title, pattern = "=+ *([ a-zA-Z0-9]+) *=+")[, 2]
  wiki$title <- trimws(wiki$title)

  # Print results
  my_print("  Found Wikipedia content for ", length(unique(wiki$query)), " species.\n")

  # Add results to input object
  obj$data$wiki <- wiki

  return(obj)
}




#' Download wikipedia content for a taxon
#'
#' Download wikipedia content for a taxon
#'
#' @param taxon THe taxon the get information for.
#'
#' @keywords internal
get_wiki_content <- function(taxon) {
  # Query wikipedia
  base_url <- "https://en.wikipedia.org/w/api.php"
  raw_result <- RCurl::getURL(paste0(base_url,
                                     "?format=json&action=query&prop=extracts&exlimit=max&explaintext&titles=",
                                     gsub(taxon, pattern = " ", replacement = "_")))

  # Parse result
  parsed_result <- RJSONIO::fromJSON(raw_result)

  # Get content of article
  content <- unname(parsed_result$query$pages[[1]]$extract)

  # Handle redirects
  # if (! is.null(content) && grepl(content, pattern = "^#REDIRECT \\[\\[.+\\]\\].*$")) {
  #   redirect <- gsub(content, pattern = "^#REDIRECT \\[\\[(.+?)\\]\\].*$", replacement = "\\1")
  #   return(get_wiki_content(redirect))
  # }

  if (is.null(content)) {
    return("")
  } else {
    return(content)
  }
}
