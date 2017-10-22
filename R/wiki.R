
#' Download wikipedia content for a taxon
#'
#' Download wikipedia content for a taxon
#'
#' @param taxon THe taxon the get information for.
#'
#' @keywords internal
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
  wiki_to_markdown(content)
}


#' Convert wikipedia markup to markdown
#'
#' Convert wikipedia markup to markdown
#'
#' @param text The mediawiki content
#'
#' @keywords internal
wiki_to_markdown <- function(text) {
  # pandoc -f mediawiki -t markdown -s wiki.txt -o wiki.md
  raw_md <- system2(command = "pandoc", args = c("-f", "mediawiki", "-t", "markdown"),
                    input = text, stdout = TRUE)
  raw_md <- paste0(raw_md, collapse =  "\n")

  raw_md <- raw_md %>%
    gsub(pattern = "\nReferences(.|\n)*$", replacement = "") %>%
    gsub(pattern = "\\[\\^[0-9]+\\]", replacement = "") %>%
    gsub(pattern = "!\\[(.|\n)*?\\]\\((.|\n)+?\\)", replacement = "") %>%
    gsub(pattern = "\\[((.|\n)+?)\\]\\((.|\n)+?\\)", replacement = "\\1") %>%
    gsub(pattern = "<(.|\n)+?>", replacement = "") %>%
    gsub(pattern = "\\]\\((.|\n)+?\\)", replacement = "") %>%
    gsub(pattern = "\\{(.|\n)+?\\}", replacement = "") %>%
    gsub(pattern = "\n\\*\n", replacement = "\n") %>%
    gsub(pattern = "\n(.+?)\n-+\n", replacement = "\n### \\1\n", perl = T)


  return(raw_md)
}
