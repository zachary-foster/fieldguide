get_wiki_content <- function(taxon) {
  raw_result <-  RCurl::getForm(
    "https://en.wikipedia.org/w/api.php",
    action  = "query",
    titles = taxon,
    prop = "revisions",
    rvprop = "content",
    format = "json",
    formatversion = "2"
  )

  # Parse result
  parsed_result <- RJSONIO::fromJSON(raw_result)

  # Get content of article
  content <- unname(parsed_result$query$pages[[1]]$revisions[[1]]["content"])

  # Convert to markdown
  content %>%
    gsub(pattern = "\\[", replacement =  "") %>%
    gsub(pattern = "\\]", replacement =  "") %>%
    gsub(pattern = "'''''", replacement =  "**", fixed = TRUE) %>%
    gsub(pattern = "''", replacement =  "*", fixed = TRUE) %>%
    gsub(pattern = "\\{\\{(.+?)\\}\\}", replacement =  "")
}
