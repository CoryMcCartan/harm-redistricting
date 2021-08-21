# make grid ----
bb <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,0)))))
grid <- st_as_sf(st_make_grid(bb, n = 6))

# add votes ----

mininois = grid %>%
    mutate(row = rep(6:1, each=6),
           col = rep(1:6, 6),
           city = row <= 3 & col <= 3,
           pop = if_else(city, 11, 3),
           dem = if_else(city, 8, 1),
           rep = if_else(city, 3, 2))
