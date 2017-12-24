plot_map <- function(obj, max_colors = 9) {
  coord_range <- function(x) as.numeric(strsplit(x, split = ",")[[1]])
  mean_coord <- function(x) mean(coord_range(x))
  coord_diff <- function(x) diff(coord_range(x))
  radius_to_zoom <- function(x) round(14-log(x * 69)/0.693)
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


  ggmap(map, extent = "normal", maprange = FALSE) +
    # geom_density2d(data = sp_data, mapping = aes(x = decimalLongitude, y = decimalLatitude)) +
    # stat_density2d(data = sp_data,
    #                mapping = aes(x = decimalLongitude, y = decimalLatitude, fill = ..level.., alpha = ..level..),
    #                bins = 300, geom = 'polygon') +
    geom_point(data = sp_data, mapping = aes(x = decimalLongitude, y = decimalLatitude, color = plot_name)) +
    scale_x_continuous(limits = coord_range(obj$data$long_range), expand = c(0.03, 0)) +
    scale_y_continuous(limits = coord_range(obj$data$lat_range), expand = c(0.03, 0)) +
    scale_colour_manual(values = c(rev(RColorBrewer::brewer.pal(max_colors - 1, "Set1")), "#111111"))
    # scale_fill_gradientn(colours = c("#00000000", "#111111", "#444444", "#999999"), values = c(0, .1, .01, .001)) +
    # scale_alpha_continuous(limits=c(0, 1), breaks = seq(0, .10, by = 0.025)) +
    xlab("Longitude") +
    ylab("Latitude")

}
