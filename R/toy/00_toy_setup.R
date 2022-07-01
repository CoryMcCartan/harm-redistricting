# make grid ----
bb <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,0)))))
grid <- st_as_sf(st_make_grid(bb, n = 6, crs = 3857)) %>%
    rename(geometry=x)


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
           rep = if_else(city2, 2, rep),
           circle = round((col + row + city1) / 15 * pop)) %>%
    relocate(geometry, .after=everything()) %>%
    redist_map(ndists=3, pop_tol=0.1)


# people for plotting -------

make_people = function(pop, dem, circle, geometry, row, col, ...) {
    if (!file.exists(path <- str_glue("data-raw/csq{pop}.txt"))) {
        url = str_glue("http://hydra.nat.uni-magdeburg.de/packing/csq/txt/csq{pop}.txt")
        download.file(url, path)
    }
    offset = as.numeric(st_centroid(geometry))
    dem_vec = c(rep(TRUE, dem), rep(FALSE, pop-dem))
    circ_vec = c(rep(TRUE, circle), rep(FALSE, pop-circle))
    idx = sample(pop)
    read_table(path, col_names=F, col_types=cols(.default="d")) %>%
        select(-X1) %>%
        as.matrix() %>%
        apply(1, function(x) st_point(x/6.28 + offset), simplify=FALSE) %>%
        st_sfc(crs=3857) %>%
        st_as_sf() %>%
        rename(geometry=x) %>%
        mutate(row = row, col = col,
               dem = dem_vec[idx],
               circle = circ_vec[idx])
}

minissouri_people = minissouri %>%
    pmap_dfr(make_people)

# functions and helpers -------
plot_state = function(map, people, plan=NULL, qty=NULL, ppl_size=1.75, ...) {
    if (is.null(people$harm)) people$harm = FALSE
    people_noharm = filter(people, harm == 0)
    people_harm_dem = filter(people, harm == 1, dem)
    people_harm_gop = filter(people, harm == 1, !dem)

    p = redist.plot.map(map, fill=qty, ...) +
        scale_fill_party_c(name="Democratic\nshare") +
        geom_sf(data=map, size=0.6, fill=NA, color="white") +
        geom_sf(aes(color=dem, shape=factor(circle+2*harm, levels=0:3)),
                    data=people_noharm, size=ppl_size) +
        geom_sf(aes(shape=factor(circle+2*harm, levels=0:3)),
                    data=people_harm_dem, fill=DEM, size=ppl_size) +
        geom_sf(aes(shape=factor(circle+2*harm, levels=0:3)),
                    data=people_harm_gop, fill=GOP, size=ppl_size) +
        scale_color_manual(values=c(GOP, DEM), guide="none") +
        scale_shape_manual(values=c(15, 19, 22, 21), guide="none") +
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

add_lab = function(p, txt, x, y, hjust=0.5, vjust=0.5) {
    p + annotate("text", x, y, hjust=hjust, vjust=vjust, label=txt,
                 family="Times", size=3)
}
fmt_lean = function(x) {
    if (x > 0.5)
        paste0("D+", round(200*(x - 0.5)), "%")
    else
        paste0("R+", round(200*(0.5 - x)), "%")
}
