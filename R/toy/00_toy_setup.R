# libs ----
library(sf)
library(tidyverse)
library(redist)

# make grid ----
bb <-st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,0)))))
grid <- st_make_grid(bb, n = 4)
tb <- st_as_sf(grid)

# add votes ----

