
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
    tax_data <- search_place(place_name = placename, radius = radius, taxon = taxon)
  } else if (! is.null(latitude) || ! is.null(longitude)) {
    title <- paste0(latitude, ", ", longitude)
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
                    if (is.na(common_name)) {
                      common_name <- ""
                    }

                    images <- tax_data$data$inat_data[sp_name ==  tax_data$data$inat_data$name, "image_url"]
                    images <- images[[1]]
                    images <- images[seq_len(min(length(images), max_img))]

                    if (length(images) == 0) {
                      image_chunk <- ""
                    } else {
                      image_chunk <- paste0("\n```{r fig.cap='", sp_name,
                                            "', fig.height=", 11 - margin * 2,
                                            ", fig.width=", 8.5 - margin * 2,
                                            ", echo=FALSE}\nurls <- c('", paste0(images, collapse = "', '"), "')",
                                            "\nprint(fieldguide::image_grid(urls))\n```\n")
                    }

                    wiki_content <- tax_data$data$wiki_data[sp_name == tax_data$data$wiki_data$name, "content"][[1]]

                    paste0(
                      "## *", sp_name, "*\n\n",
                      "**Common names:** ", common_name, "\n\n",
                      "**Taxonomic classification:** ", taxonomy, "\n\n",
                      wiki_content, "\n",
                      image_chunk, "\n"
                    )
                  })

  main_content <- paste0(sp_md, collapse = "\n")

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


#' Plot a grid of images
#'
#' Plot a grid of images from URLs
#'
#' @param urls The URLs to the images
#'
#' @export
image_grid <- function(urls) {
  temp_paths <- vapply(urls, FUN.VALUE = character(1),
                       function(url) {
                         path <- tempfile(fileext = basename(gsub(url, pattern = "\\?[0-9]+$", replacement = "")))
                         path <- sub(path, pattern = "\\.$", replacement = "\\.jpg") # Some paths have no extension...
                         download.file(url, path, quiet = TRUE)
                         return(path)
                       })
  sub_plots <- lapply(temp_paths, plot_image)
  cowplot::plot_grid(plotlist = sub_plots,
                     ncol = round(sqrt(6)),
                     scale = 1,
                     label_size = 30,
                     label_colour = "#777777")
}


#' Plot an image
#'
#' Plot an image from a file path
#'
#' @param path The path to the image to plot
#'
#' @keywords internal
plot_image <- function(path) {
  if (endsWith(tolower(path), "png")) {
    image <- png::readPNG(path)
  } else if (endsWith(tolower(path), "jpg") || endsWith(tolower(path), "jpeg")) {
    image <- jpeg::readJPEG(path)
  } else {
    stop(paste0('Not an accepted file format: "', path, '".'))
  }
  y_max <- dim(image)[1] / dim(image)[2]
  ggplot2::ggplot() +
    ggplot2::annotation_raster(image, ymin = 0, xmin = 0, xmax = 1, ymax = y_max) +
    ggplot2::ylim(0, y_max) +
    ggplot2::xlim(0, 1) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void()
}
