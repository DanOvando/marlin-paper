# helper function to convert lon/lat to center
shift_center_v <- function(center) {
  ifelse(center >=0, (-180 - center) + 360, -180 - center)
}
# function to take global shape and recenter using equal earth projection
# adapted from https://gist.github.com/valentinitnelav/c7598fcfc8e53658f66feea9d3bafb40
recenter_vector <- function(shp_file, center) {
  prj <- glue::glue("+proj=eqearth +lon_0={center} +wktext")
  shift <- 180 + shift_center_v(center = center)
  # create "split line" to split polygon/lines
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  split_line <- sp::SpatialLines(list(sp::Lines(list(sp::Line(cbind(180 - shift, c(-90, 90)))), ID = "line")),
                                 proj4string = WGS84
  )
  split_line <- sf::st_as_sf(split_line)
  # does split intersect shape?
  line_int <- sf::st_intersection(split_line, shp_file)
  # if shape is a point or doesn't intersect the split, just reproject
  if (nrow(line_int) < 1 | grepl("POINT", sf::st_geometry_type(shp_file, by_geometry = FALSE), fixed = TRUE)) {
    output_shape <- sf::st_transform(shp_file, crs = prj)
  } else {
    # create a very thin polygon (buffer) out of the intersecting "split line"
    line_buf <- sf::st_buffer(line_int, dist = 0.00001) %>%
      sf::st_union(.) %>%
      sf::st_cast(., "MULTIPOLYGON")
    # split polygons/lines using intersecting thin polygon (buffer)
    shape_split <- sf::st_difference(shp_file, line_buf)
    # if shape is polygon, identify split shapes and add vertices for smoother edges
    if (!grepl("STRING", sf::st_geometry_type(shp_file, by_geometry = FALSE), fixed = TRUE)) {
      shape_split <- shape_split %>%
        slice(-sf::st_intersects(split_line, shp_file)[[1]]) %>%
        rbind(
          shape_split %>%
            slice(sf::st_intersects(split_line, shp_file)[[1]]) %>%
            smoothr::densify(., n = 100)
        )
    }
    output_shape <- sf::st_transform(shape_split, crs = prj)
  }
  return(output_shape)
}
