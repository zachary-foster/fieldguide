plot_map <- function(obj) {
  coord_range <- function(x) as.numeric(strsplit(x, split = ",")[[1]])
  mean_coord <- function(x) mean(coord_range(x))
  coord_diff <- function(x) diff(coord_range(x))
  radius_to_zoom <- function(x) round(14-log(x * 69)/0.693)
  radius <- max(c(coord_diff(obj$data$long_range), coord_diff(obj$data$lat_range))) / 2
  map <- ggmap::get_googlemap(center = c(mean_coord(obj$data$long_range), mean_coord(obj$data$lat_range)),
                              zoom = radius_to_zoom(radius), maptype = "hybrid")


  sp_data <- obj$data$occ[obj$data$occ$name == "Anas platyrhynchos", ]

  ggmap(map, extent = "normal", maprange = FALSE) +
    # geom_density2d(data = sp_data, mapping = aes(x = decimalLongitude, y = decimalLatitude)) +
    stat_density2d(data = sp_data,
                   mapping = aes(x = decimalLongitude, y = decimalLatitude, fill = ..level.., alpha = ..level..),
                   bins = 300, geom = 'polygon') +
    geom_point(data = sp_data, mapping = aes(x = decimalLongitude, y = decimalLatitude, color = name)) +
    scale_x_continuous(limits = coord_range(obj$data$long_range), expand = c(0, 0)) +
    scale_y_continuous(limits = coord_range(obj$data$lat_range), expand = c(0, 0)) +
    # scale_fill_gradientn(colours = c("#00000000", "#111111", "#444444", "#999999"), values = c(0, .1, .01, .001)) +
    # scale_alpha_continuous(limits=c(0, 1), breaks = seq(0, .10, by = 0.025)) +
    xlab("Longitude") +
    ylab("Latitude")

}
