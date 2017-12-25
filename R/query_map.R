#' Look up a map for occurrences
#'
#' Look up a map and plot for occurences and plot the occurances on the map.
#'
#' @param obj A \code{\link{taxmap}} object produced from \code{search_*} functions.
#' @param max_colors The maximum number of most common species to plot. Should be less than 10.
#'
#' @family query functions
#'
#' @export
query_overall_map <- function(obj, max_colors = 8) {
  # Get list of species to look up
  species <- unique(obj$data$occ$name)
  names(species) <- obj$data$occ$taxon_id[match(species, obj$data$occ$name)]

  # Download map
  coord_range <- function(x) as.numeric(strsplit(x, split = ",")[[1]])
  mean_coord <- function(x) mean(coord_range(x))
  coord_diff <- function(x) diff(coord_range(x))
  radius_to_zoom <- function(x) round(14 - log(x * 69) / 0.693)
  radius <- max(c(coord_diff(obj$data$long_range), coord_diff(obj$data$lat_range))) / 2
  map <- ggmap::get_googlemap(center = c(mean_coord(obj$data$long_range), mean_coord(obj$data$lat_range)),
                              zoom = radius_to_zoom(radius), maptype = "hybrid")

  sp_data <- obj$data$occ

  # Find the most common speices to plot
  if (length(unique(obj$data$occ$name)) > max_colors) {
    species_counts <- sort(table(obj$data$occ$name), decreasing = TRUE)
    common_sp <- names(species_counts)[1:(max_colors - 1)]
    sp_data$plot_name <- ifelse(sp_data$name %in% common_sp, sp_data$name, "Other")
    colored_species <- c(common_sp, "Other")
    sp_data$plot_name <- factor(sp_data$plot_name, levels = colored_species, ordered = TRUE)
  } else {
    sp_data$plot_name <- sp_data$name
  }

  color_to_use <-  c(rev(RColorBrewer::brewer.pal(max_colors - 1, "Set1")), "#888888")

  ggmap::ggmap(map, extent = "normal", maprange = FALSE) +
    ggplot2::geom_point(data = sp_data,
                        mapping = ggplot2::aes_string(x = "decimalLongitude",
                                                      y = "decimalLatitude",
                                                      color = "plot_name"),
                        alpha = .2,
                        size = 5) +
    ggplot2::coord_map(projection = "mercator",
                       xlim = coord_range(obj$data$long_range),
                       ylim = coord_range(obj$data$lat_range)) +
    ggplot2::scale_color_manual(values = color_to_use) +
    ggplot2::scale_fill_manual(values = color_to_use) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Most common species",
                                                  override.aes = list(alpha = 1))) +
    ggplot2::theme(legend.background = ggplot2::element_rect(fill = "#AAAAAA",
                                                             size = 0.5, linetype = "solid",
                                                             colour = "#222222"),
                   legend.position = "bottom",
                   panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   legend.key = ggplot2::element_rect(fill = "transparent", colour = "transparent"))

}




#' Look up a map for each species
#'
#' Look up a map and plot for occurences and plot the occurances on the map.
#'
#' @param obj A \code{\link{taxmap}} object produced from \code{search_*} functions.
#'
#' @family query functions
#'
#' @export
query_species_map <- function(obj) {
  coord_range <- function(x) as.numeric(strsplit(x, split = ",")[[1]])
  mean_coord <- function(x) mean(coord_range(x))
  coord_diff <- function(x) diff(coord_range(x))
  radius_to_zoom <- function(x) round(14 - log(x * 69) / 0.693)
  radius <- max(c(coord_diff(obj$data$long_range), coord_diff(obj$data$lat_range))) / 2
  map <- ggmap::get_googlemap(center = c(mean_coord(obj$data$long_range), mean_coord(obj$data$lat_range)),
                              zoom = radius_to_zoom(radius), maptype = "terrain")


  sp_data <- obj$data$occ[obj$data$occ$name == unique(obj$data$occ$name )[6], ]
  ggmap::ggmap(map, base_layer = ggplot2::ggplot(data = sp_data), extent = "normal", maprange = FALSE) +
    ggplot2::coord_map(projection = "mercator",
                       xlim = coord_range(obj$data$long_range),
                       ylim = coord_range(obj$data$lat_range)) +
    ggplot2::stat_density2d(mapping = ggplot2::aes(x = decimalLongitude,
                                                   y = decimalLatitude,
                                                   alpha = ..level..),
                            bins = 5,
                            size = 1,
                            n = 300, # number of grid points in each direction
                            geom = 'polygon') +
    ggplot2::scale_alpha_continuous(limits = c(0, 100), breaks = seq(0, 100, length.out = 6)) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(x = "decimalLongitude",
                                                      y = "decimalLatitude"),
                        alpha = .2,
                        size = 1) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude")

}

