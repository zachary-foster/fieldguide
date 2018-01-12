#' Plot a grid of images"
#'
#' Plot a grid of images from URLs
#'
#' @param urls The URLs to the images
#'
#' @export
image_grid <- function(urls = NULL, file_paths = NULL) {
  if (sum(c(is.null(urls), is.null(file_paths))) != 1) {
    stop("Either 'urls' or 'file_paths' must be supplied, but not both")
  }

  if (is.null(file_paths)) {
    file_paths <- vapply(urls, FUN.VALUE = character(1),
                         function(url) {
                           path <- tempfile(fileext = basename(gsub(url, pattern = "\\?[0-9]+$", replacement = "")))
                           path <- sub(path, pattern = "\\.$", replacement = "\\.jpg") # Some paths have no extension...
                           download.file(url, path, quiet = TRUE)
                           return(path)
                         })
  }
  sub_plots <- lapply(file_paths, plot_image)
  cowplot::plot_grid(plotlist = sub_plots,
                     ncol = round(sqrt(length(file_paths))),
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
