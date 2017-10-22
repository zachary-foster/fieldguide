
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
#'
#' @export
make_guide <- function(output_file, placename = NULL, latitude = NULL, longitude = NULL,
                       radius = NULL, taxon = NULL, format = "pdf_document") {
  # Internal parameters
  max_img <- 4

  # Check that a correct combination of parameters are used
  if (! is.null(placename) && (! is.null(latitude) || ! is.null(longitude))) {
    stop('The "placename" option cannot be used with the "latitude"/"longitude" options.')
  }

  # Look up taxonomic information
  if (! is.null(placename)) {
    tax_data <- search_place(place_name = placename, radius = radius, taxon = taxon)
  } else if (! is.null(latitude) || ! is.null(longitude)) {
    tax_data <- search_place(latitude = latitude, longitude = longitude,
                             radius = radius, taxon = taxon)
  }

  # Create markdown for each page
  my_print("Creating the fieldguide...")
  species <- tax_data$data$tax_data$name
  sp_md <- vapply(species, FUN.VALUE = character(1),
                  function(sp_name) {
                    gbif <- tax_data$data$tax_data[sp_name == tax_data$data$tax_data$name, ]

                    taxonomy <- paste0(gbif[, c("kingdom", "phylum", "order", "family", "genus", "species")], collapse = " | ")

                    common_name <- gbif$common_name

                    images <- tax_data$data$inat_data[sp_name ==  tax_data$data$inat_data$name, "image_url"]
                    images <- images[[1]]
                    images <- images[1:min(length(images), max_img)]

                    wiki_content <- tax_data$data$wiki_data[sp_name ==  tax_data$data$wiki_data$name, "content"][[1]]

                    paste0(
                      "## ", sp_name, "n\n",
                      "**Common names:** ", common_name, "\n\n",
                      "**Taxonomic classification:** ", taxonomy, "\n\n",
                      paste0("![", sp_name, "](", images, ")", collapse = "\n\n"), "\n\n",
                      wiki_content, "\n",
                      '\\newpage <P style="page-break-before: always">\n\n'
                    )
                  })

  md_content <- paste0(sp_md, collapse = "\n")

  # Render markdown
  in_path <- tempfile()
  writeChar(md_content, con = in_path)
  rmarkdown::render(input = in_path, output_format = format, output_file = output_file)
}
