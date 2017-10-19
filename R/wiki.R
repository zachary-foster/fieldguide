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
    gsub(pattern = "\\[\\[File:.+?\\]\\]", replacement =  "") %>%
    gsub(pattern = "\\[\\[Image:.+?\\]\\]", replacement =  "") %>%
    gsub(pattern = "'{3,5}", replacement =  "**") %>%
    gsub(pattern = "'{2}", replacement =  "*") %>%
    gsub(pattern = "\\{\\{(.+?)\\}\\}", replacement =  "") %>%
    gsub(pattern = "\\{(.+?)\\}", replacement =  "") %>%
    gsub(pattern = "<ref></ref>", replacement =  "") %>%
    gsub(pattern = "<ref>(.*?)</ref>", replacement =  "") %>%
    gsub(pattern = "\\[|\\]", replacement =  "") %>%
    gsub(pattern = "^.+\\}\\}", replacement =  "") %>%
    gsub(pattern = "==References==.*$", replacement =  "") %>%
    gsub(pattern = "===(.+?)===", replacement =  "### \\1\n") %>%
    gsub(pattern = "==(.+?)==", replacement =  "## \\1\n") %>%
    # gsub(pattern = "\n\\*(.+?)\n\\*(.+?)\n\n", replacement =  "\n\n\\*\\1\n\\*\\2\n\n") %>%
    # gsub(pattern = "\n\\*\\*", replacement =  "\n     \\*") %>%
    gsub(pattern = '<ref name=".+" />', replacement =  "") %>%
    gsub(pattern = '<ref name=".+"></ref>', replacement =  "") %>%
    cat
}

wiki_to_markdown <- function(text) {
  # pandoc -f mediawiki -t markdown -s wiki.txt -o wiki.md

  # remove images [TODO]
}
