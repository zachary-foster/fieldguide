
#' Create a field guide
#'
#' Make a PDF or HTML field guide.
#'
#' @param placename A name of a place to look up. Google maps will be used to
#'   find the coordinates. Cannot be used with "longitude" or " latitude".
#' @param latitude The latitude of the center of the area of interest. Cannot be
#'   used with "placename".
#' @param longitude The longitude of the center of the area of interest. Cannot
#'   be used with "placename".
#' @param radius The radius around the place to search.
#' @param format The format of the output file.
#' @param img_needed If \code{TRUE}, only species for which images are available
#'   will be included.
#' @param wiki_needed If \code{TRUE}, only species for which Wikipedia
#'   descriptions are available will be included.
#' @param max_species The maximum number of species that will be returned. The
#'   species with the least occurances will be dropped. What is actually
#'   returned may be less than this if either of the \code{img_needed} and \code{wiki_needed} options
#'   are true, since this filter is applied first for performance reasons.
#'
#' @export
make_guide <- function(output_file, placename = NULL, latitude = NULL, longitude = NULL,
                       radius = NULL, taxon = NULL, format = "pdf_document",
                       img_needed = TRUE, wiki_needed = TRUE, max_species = 50) {
  # Internal parameters
  max_img <- 6
  font_size <- 16 # pt
  margin <- 0.5 # in

  # Check that a correct combination of parameters are used
  if (! is.null(placename) && (! is.null(latitude) || ! is.null(longitude))) {
    stop('The "placename" option cannot be used with the "latitude"/"longitude" options.')
  }

  # Look up taxonomic information
  if (! is.null(placename)) {
    title <- Hmisc::capitalize(placename)
    tax_data <- search_place(place_name = placename,
                             radius = radius,
                             taxon = taxon,
                             max_species = max_species)
  } else if (! is.null(latitude) || ! is.null(longitude)) {
    title <- paste0(latitude, ", ", longitude)
    tax_data <- search_place(latitude = latitude,
                             longitude = longitude,
                             radius = radius,
                             taxon = taxon,
                             max_species = max_species)
  }

  # Remove taxa with no images
  if (img_needed) {
    tax_data$data$tax_data <- tax_data$data$tax_data[tax_data$data$tax_data$name %in% tax_data$data$inat_data$name, ]
  }

  # Remove taxa with no wikipedia descriptions
  if (wiki_needed) {
    sp_with_content <- tax_data$data$wiki_data$name[nchar(tax_data$data$wiki_data$content) > 100]
    tax_data$data$tax_data <- tax_data$data$tax_data[tax_data$data$tax_data$name %in% sp_with_content, ]
  }

  # Create markdown for each page
  my_print("Creating the fieldguide...")
  species <- tax_data$data$tax_data$name
  sp_md <- vapply(species, FUN.VALUE = character(1),
                  function(sp_name) {
                    gbif <- tax_data$data$tax_data[sp_name == tax_data$data$tax_data$name, ]

                    taxonomy <- paste0(gbif[, c("kingdom", "phylum", "order", "family", "genus", "species")], collapse = " | ")

                    common_name <- gbif$common_name
                    if (is.na(common_name)) {
                      common_name <- ""
                    }
                    code_common_name <- gsub(common_name, pattern = "'", replacement = "\\'", fixed = TRUE)

                    images <- tax_data$data$inat_data[sp_name ==  tax_data$data$inat_data$name, "image_url"]
                    images <- images[[1]]
                    images <- images[seq_len(min(length(images), max_img))]

                    if (length(images) == 0) {
                      image_chunk <- ""
                    } else {
                      image_chunk <- paste0("\n```{r fig.cap='", sp_name, " (", code_common_name, ")",
                                            "', fig.height=", 11 - margin * 2,
                                            ", fig.width=", 8.5 - margin * 2,
                                            ", echo=FALSE}\nurls <- c('", paste0(images, collapse = "', '"), "')",
                                            "\nprint(fieldguide::image_grid(urls))\n```\n")
                    }

                    wiki_content <- tax_data$data$wiki_data[sp_name == tax_data$data$wiki_data$name, "content"][[1]]

                    authority <- sub(gbif$scientificName, pattern = paste0("^", sp_name, " "), replacement = "")

                    paste0(
                      "## *", sp_name, "*\n\n",
                      "**Authority:** ", authority, "\n\n",
                      "**Common names:** ", common_name, "\n\n",
                      "**Taxonomic classification:** ", taxonomy, "\n\n",
                      wiki_content, "\n",
                      image_chunk, "\n"
                    )
                  })

  main_content <- paste0(sp_md, collapse = "\n******\n")

  # Make header
  head_content <- paste0("---\noutput: ", format,
                         "\nfontsize: ", font_size,
                         "pt\ngeometry: margin=", margin,
                         "in\n---\n\n")

  # Render markdown
  all_md <- paste0(head_content, main_content)
  in_path <- tempfile(fileext = ".rmd")
  writeChar(all_md, con = in_path)
  rmarkdown::render(input = in_path, output_format = format, output_file = output_file, quiet = TRUE, knit_root_dir = dirname(in_path))
  my_print('   Written to "', output_file, '"')
}


