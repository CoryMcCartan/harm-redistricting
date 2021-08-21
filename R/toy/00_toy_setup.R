# make grid ----
bb <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,0)))))
grid <- st_as_sf(st_make_grid(bb, n = 6, crs = 3857)) %>%
    rename(geometry=x)

# add votes ----
minichusetts <- grid %>%
    mutate(row = rep(6:1, each=6),
           col = rep(1:6, 6),
           pop = 5,
           dem = 3,
           rep = 2) %>%
    relocate(geometry, .after=rep) %>%
    redist_map(ndists=3, pop_tol=0.1)

minissouri <- grid %>%
    mutate(row = rep(6:1, each=6),
           col = rep(1:6, 6),
           city1 = row <= 2 & col >= 5,
           city2 = row >= 5 & col <= 2,
           pop = 4,
           pop = if_else(city1, 10, pop),
           pop = if_else(city2, 7, pop),
           dem = 1,
           dem = if_else(city1, 8, dem),
           dem = if_else(city2, 5, dem),
           rep = 3,
           rep = if_else(city1, 2, rep),
           rep = if_else(city2, 2, rep)) %>%
    relocate(geometry, .after=rep) %>%
    redist_map(ndists=3, pop_tol=0.1)

mininois = grid %>%
    mutate(row = rep(6:1, each=6),
           col = rep(1:6, 6),
           city = row <= 3 & col <= 3,
           pop = if_else(city, 11, 3),
           dem = if_else(city, 8, 1),
           rep = if_else(city, 3, 2)) %>%
    relocate(geometry, .after=rep) %>%
    redist_map(ndists=3, pop_tol=0.1)

# functions and helpers

make_people = function(pop, dem, geometry, row, col, ...) {
    url = str_glue("http://hydra.nat.uni-magdeburg.de/packing/csq/txt/csq{pop}.txt")
    offset = as.numeric(st_centroid(geometry))
    dem_vec = c(rep(TRUE, dem), rep(FALSE, pop-dem))
    read_table(url, col_names=F, col_types=cols(.default="d")) %>%
        select(-X1) %>%
        as.matrix() %>%
        apply(1, function(x) st_point(x/6 + offset), simplify=FALSE) %>%
        st_sfc() %>%
        st_as_sf() %>%
        rename(geometry=x) %>%
        mutate(row = row, col = col,
               dem = sample(dem_vec))
}

mininois_people = mininois %>%
    pmap_dfr(make_people)
minichusetts_people = minichusetts %>%
    pmap_dfr(make_people)
minissouri_people = minissouri %>%
    pmap_dfr(make_people)

sim_toy = function(map, N=500) {
    redist_smc(map, N, verbose=F) %>%
        mutate(comp = distr_compactness(map),
               dem = group_frac(map, dem, pop))
}
