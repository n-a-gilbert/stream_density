# calculate stream density (m of stream/km2) within 5 x 5 km grid cells

library(here)
library(tidyverse)
library(raster)
library(sf)
library(hablar)

setwd(here("data"))

# shapefile of streams
rivers <- st_read("wi_rivers_utm.shp")
# template with correct extent/CRS/etc
template_raster <- raster("template_raster_5k.grd")
# have to put some values in the raster for this to work i think
values(template_raster) <- rep(1, ncell(template_raster))

# convert grid cells to polygons and calculate length of lines within each poly
# HT https://gis.stackexchange.com/questions/280760/intersecting-lines-and-polygons-and-calculating-line-length-in-r
template_polys_5k <- rasterToPolygons(template_raster)

poly <- st_as_sf(template_polys_5k)
line <- rivers

int <- st_intersection(line, poly)

int$len <- st_length(int)

poly$ID <- 1:nrow(poly)

join <- st_join(poly, int)

out <- group_by(join, ID) %>% 
  summarise(length = as.vector(sum(len)/25)) %>% # area of grid cell (km2) to normalize 
  mutate(length = hablar::if_else_(is.na(length), 0, length))

stream_density_5k <- template_raster
values(stream_density_5k) <- out$length

plot(stream_density_5k)

# make a prettier ggplot version

stream_density_gg <- rasterToPoints(stream_density_5k) %>% 
  as_tibble()

( stream_plot <- ggplot() +
    geom_tile(data = stream_density_gg, aes(x = x, y = y, fill = layer)) +
    scale_fill_viridis_c() +
    theme_void() +
    labs(fill = "Meters of stream\nper km2") +
    coord_fixed() )
