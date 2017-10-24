
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

  # Handle redirects
  if (! is.null(content) && grepl(content, pattern = "^#REDIRECT \\[\\[.+\\]\\].*$")) {
    redirect <- gsub(content, pattern = "^#REDIRECT \\[\\[(.+)\\]\\].*$", replacement = "\\1")
    return(get_wiki_content(redirect))
  }

  # Convert to markdown
  if (is.null(content)) {
    return("")
  } else {
    return(wiki_to_markdown(content))
  }
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

  # Remove sections
  raw_md <- gsub(raw_md, pattern = "\nReferences(.|\n)*$", replacement = "")
  raw_md <- gsub(raw_md, pattern = "\nGallery(.|\n)*$", replacement = "")
  raw_md <- gsub(raw_md, pattern = "\nImages(.|\n)*$", replacement = "")
  raw_md <- gsub(raw_md, pattern = "\nExternal links(.|\n)*$", replacement = "")
  raw_md <- gsub(raw_md, pattern = "\nSee also(.|\n)*$", replacement = "")

  # Remove reference markup
  raw_md <- gsub(raw_md, pattern = "\\[\\^[0-9]+\\]", replacement = "", perl = TRUE)

  # Remove images
  raw_md <- remove_images(raw_md)

  # Remove thumbnails
  raw_md <- remove_thumbnails(raw_md)

  # Remove links
  raw_md <- remove_links(raw_md)

  # Remove HTML tags
  raw_md <- gsub(raw_md, pattern = "<(.|\n)+?>", replacement = "", perl = TRUE)

  # Remove {}
  raw_md <- gsub(raw_md, pattern = "\\{([^{}]|(?R))*\\}", replacement = "", perl = TRUE)

  # Replace headers with markdown
  raw_md <- gsub(raw_md, pattern = "\n(.+?)\n-+\n", replacement = "\n### \\1\n", perl = TRUE)

  # Remove unicode characters
  raw_md <- iconv(raw_md, "UTF-8", "ASCII", sub = "")

  # Remove escaped brackets
  raw_md <- gsub(raw_md, pattern = "\\[", replacement = "[", fixed = TRUE)
  raw_md <- gsub(raw_md, pattern = "\\]", replacement = "]", fixed = TRUE)

  # Handle nested lists
  raw_md <- gsub(raw_md, pattern = "\n:::\\*", replacement = "\n    * ", fixed = TRUE)

  return(raw_md)
}


#' Remove image text
#'
#' Remove image text from mediawiki markup. This complicated function is need
#' because regex does not handle nested patterns well.
#'
#' @param text The mediawiki text.
#'
#' @keywords internal
remove_images <- function(text) {
  # text = "12345![bla[bla]bla](bla(bla)bla)6789  12345![bla[bla]bla](bla(bla)bla)6789 ![] ![](x![x]()x)123"

  get_nested_range <- function(start, open, close) {
    open_bracket <- gregexpr(text, pattern = open, fixed = TRUE)[[1]]
    close_bracket <- gregexpr(text, pattern = close, fixed = TRUE)[[1]]
    both_bracket <- c(open_bracket, close_bracket)

    n_opened <- 1
    pos <- start
    while (n_opened != 0) {
      next_bracket <- min(both_bracket[both_bracket > pos])
      if (next_bracket %in% open_bracket) {
        n_opened <- n_opened + 1
      } else {
        n_opened <- n_opened - 1
      }
      pos <- next_bracket
    }

    return(c(start, pos))
  }

  get_img_range <- function(start) {
    bracket_range <- get_nested_range(start, "[", "]")
    if (substr(text, bracket_range[2] + 1, bracket_range[2] + 1) == "(") {
      par_range <- get_nested_range(bracket_range[2] + 1, "(", ")")
      return(c(start, par_range[2]))
    } else { # Not followed by parentheses
      return(NA)
    }
  }

  starts <- gregexpr(text, pattern = "![", fixed = TRUE)[[1]] + 1

  if (starts[1] != 0) {
    ranges <- lapply(starts, get_img_range)
    ranges <- ranges[!is.na(ranges)]
    img_text <- vapply(ranges, function(a_range) substr(text, a_range[1] -1, a_range[2]), character(1))

    for (to_remove in img_text) {
      text <- gsub(text, pattern = to_remove, replacement = "", fixed = TRUE)
    }
  }
  return(text)
}


#' Remove link text
#'
#' Remove link text from mediawiki markup. This complicated function is need
#' because regex does not handle nested patterns well.
#'
#' @param text The mediawiki text.
#'
#' @keywords internal
remove_links <- function(text) {
  # text = "12345[bla[bla]bla](bla(bla)bla)6789  12345[bla[bla]bla](bla(bla)bla)6789 [](x![x]()x)123"

  get_nested_range <- function(start, open, close) {
    open_bracket <- gregexpr(text, pattern = open, fixed = TRUE)[[1]]
    close_bracket <- gregexpr(text, pattern = close, fixed = TRUE)[[1]]
    both_bracket <- c(open_bracket, close_bracket)

    n_opened <- 1
    pos <- start
    while (n_opened != 0) {
      next_bracket <- min(both_bracket[both_bracket > pos])
      if (next_bracket %in% open_bracket) {
        n_opened <- n_opened + 1
      } else {
        n_opened <- n_opened - 1
      }
      pos <- next_bracket
    }

    return(c(start, pos))
  }

  get_img_range <- function(start) {
    bracket_range <- get_nested_range(start, "[", "]")
    if (substr(text, bracket_range[2] + 1, bracket_range[2] + 1) == "(") {
      par_range <- get_nested_range(bracket_range[2] + 1, "(", ")")
      return(list(start, par_range[2], substr(text, bracket_range[1] + 1, bracket_range[2] - 1)))
    } else { # Not followed by parentheses
      return(NA)
    }
  }

  starts <- gregexpr(text, pattern = "[", fixed = TRUE)[[1]]

  if (starts[1] != 0) {
    ranges <- lapply(starts, get_img_range)
    ranges <- ranges[!is.na(ranges)]
    to_remove <- vapply(ranges, function(a_range) substr(text, a_range[[1]], a_range[[2]]), character(1))

    for (i in seq_len(length(ranges))) {
      to_add <- ranges[[i]][[3]]
      text <- gsub(text, pattern = to_remove[i], replacement = to_add, fixed = TRUE)
    }
  }

  return(text)
}


#' Remove thumbnail text
#'
#' Remove thumbnail text from mediawiki markup. This complicated function is need
#' because regex does not handle nested patterns well.
#'
#' @param text The mediawiki text.
#'
#' @keywords internal
remove_thumbnails <- function(text) {

    get_nested_range <- function(start, open, close) {
    open_bracket <- gregexpr(text, pattern = open, fixed = TRUE)[[1]]
    close_bracket <- gregexpr(text, pattern = close, fixed = TRUE)[[1]]
    both_bracket <- c(open_bracket, close_bracket)

    n_opened <- 1
    pos <- start
    while (n_opened != 0) {
      next_bracket <- min(both_bracket[both_bracket > pos])
      if (next_bracket %in% open_bracket) {
        n_opened <- n_opened + 1
      } else {
        n_opened <- n_opened - 1
      }
      pos <- next_bracket
    }

    return(c(start, pos))
  }

  get_img_range <- function(start) {
    bracket_range <- get_nested_range(start, "[", "]")
    if (substr(text, bracket_range[2] + 1, bracket_range[2] + 6) == "(File:") {
      par_range <- get_nested_range(bracket_range[2] + 1, "(", ")")
      return(c(start, par_range[2]))
    } else { # Not followed by parentheses
      return(NA)
    }
  }

  starts <- gregexpr(text, pattern = "[", fixed = TRUE)[[1]] + 1

  if (starts[1] != 0) {
    ranges <- lapply(starts, get_img_range)
    ranges <- ranges[!is.na(ranges)]
    img_text <- vapply(ranges, function(a_range) substr(text, a_range[1] - 1, a_range[2]), character(1))

    for (to_remove in img_text) {
      text <- gsub(text, pattern = to_remove, replacement = "", fixed = TRUE)
    }
  }
  return(text)
}
