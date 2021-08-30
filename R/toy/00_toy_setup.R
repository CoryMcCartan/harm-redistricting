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

# people for plotting -------

make_people = function(pop, dem, geometry, row, col, ...) {
    url = str_glue("http://hydra.nat.uni-magdeburg.de/packing/csq/txt/csq{pop}.txt")
    offset = as.numeric(st_centroid(geometry))
    dem_vec = c(rep(TRUE, dem), rep(FALSE, pop-dem))
    read_table(url, col_names=F, col_types=cols(.default="d")) %>%
        select(-X1) %>%
        as.matrix() %>%
        apply(1, function(x) st_point(x/6.28 + offset), simplify=FALSE) %>%
        st_sfc(crs=3857) %>%
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

# functions and helpers -------
plot_state = function(map, people, plan=NULL, qty=NULL, ppl_size=1.75, ...) {
    p = redist.plot.map(map, fill=qty, ...) +
        scale_fill_party_c(name="Democratic\nshare") +
        geom_sf(data=map, size=0.8, fill=NA, color="white") +
        geom_sf(aes(color=dem), data=people, size=ppl_size) +
        scale_color_party_d(guide="none") +
        theme_repr_map() +
        theme(plot.title = element_text(hjust = 0.5))
    if (!is.null(plan)) {
        distrs = map %>%
            mutate(.plan = plan) %>%
            group_by(.plan) %>%
            summarize()
        p = p + geom_sf(data=distrs, fill=NA, color="black")
    }
    p
}

sim_toy = function(map, N=500) {
    redist_smc(map, N, verbose=F) %>%
        mutate(comp = distr_compactness(map),
               dem = group_frac(map, dem, pop))
}

# initial plots -------
if (!file.exists(fig_path <- here("paper/figures/minis_schematic.pdf"))) {
    list(plot_state(mininois, mininois_people) + labs(title="(a) Mininois"),
         plot_state(minissouri, minissouri_people) + labs(title="(b) Minissouri"),
         plot_state(minichusetts, minichusetts_people) + labs(title="(c) Minichusetts")) %>%
        wrap_plots(nrow=1)
    ggsave(fig_path, width=7, height=2.75)
}

plan = c(2, 2, 2, 3, 3, 3, 2, 2, 2, 3, 3, 3, 2, 2, 2, 3, 3, 3, 2, 2, 2, 3, 3, 3, 1, 1, 1, 3, 3, 3, 1, 1, 1, 3, 3, 3)
