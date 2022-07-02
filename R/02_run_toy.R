# helpers ----

## make grid ----
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


## people for plotting -------

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

## functions and helpers -------
plot_state = function(map, people, plan=NULL, qty=NULL, ppl_size=1.75, grey_out = FALSE,
                      ...) {
    if (is.null(people$harm)) people$harm = FALSE
    people_noharm = filter(people, harm == 0)
    people_harm_dem = filter(people, harm == 1, dem)
    people_harm_gop = filter(people, harm == 1, !dem)

    p = redist.plot.map(map, fill=qty, ...) +
        scale_fill_party_c(name="Democratic\nshare") +
        geom_sf(data=map, size=0.6, fill=NA, color="white") +
        geom_sf(aes(color=dem, shape=factor(circle+2*harm, levels=0:3)),
                data=people_noharm, size=ppl_size, alpha = ifelse(grey_out, 0.2, 1)) +
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

add_voter_harm <- function(people, map, dem_harm, rep_harm, .cols = c('row', 'col')) {
    id_map <- map %>% as_tibble() %>% select(all_of(.cols)) %>% unite(id, sep = '-') %>% pull(id)
    id_people <- people %>% as_tibble() %>% select(all_of(.cols)) %>% unite(id, sep = '-') %>% pull(id)
    m <- match(id_people, id_map)

    people %>%
        mutate(harm = as.integer((dem_harm[m] & dem)| (rep_harm[m] & !dem)))
}

# make figs ----

# Minissouri

pl_fair = c(3, 3, 3, 3, 3, 3,
            2, 2, 3, 3, 3, 3,
            2, 2, 3, 3, 3, 3,
            2, 2, 2, 2, 1, 1,
            2, 2, 2, 1, 1, 1,
            2, 2, 2, 1, 1, 1)
pl_gerry_dem = c(1, 2, 2, 2, 2, 2,
                 1, 2, 3, 3, 3, 2,
                 1, 3, 3, 3, 3, 2,
                 1, 3, 3, 3, 3, 2,
                 1, 3, 3, 3, 1, 2,
                 1, 1, 1, 1, 1, 2)

# calculate stats
dem_fair = with(minissouri, tapply(dem, pl_fair, sum)/tapply(pop, pl_fair, sum))[pl_fair]
dem_gerry_dem = with(minissouri, tapply(dem, pl_gerry_dem, sum)/tapply(pop, pl_gerry_dem, sum))[pl_gerry_dem]

dem_harm_gerry_dem = minissouri$dem * (dem_gerry_dem<0.5)*(dem_fair>0.5)
rep_harm_gerry_dem = minissouri$rep * (dem_gerry_dem>0.5)*(dem_fair<0.5)


diffharm_gerry_dem = sum(dem_harm_gerry_dem)/sum(minissouri$dem) -
    sum(rep_harm_gerry_dem)/sum(minissouri$rep)
cat("Minissouri D gerrymander diff. harm: ", round(diffharm_gerry_dem, 3), "\n")

# plotting
coords_fair = list(x=c(3/4, 1/4, 1/2),
                   y=c(1.04, 1.04, -0.04))
coords_gerry_dem = list(x=c(1/2, 1/2, 1/2),
                        y=c(1.04, -0.04, 1/2))

suppressMessages({
    p1 = plot_state(minissouri, minissouri_people,
                    pl_gerry_dem, dem_gerry_dem) +
        labs(title="(a) Democratic gerrymander")
    for (i in 1:3) {
        p1 = add_lab(p1, fmt_lean(dem_gerry_dem[as.character(i)]),
                     coords_gerry_dem$x[i], coords_gerry_dem$y[i])
    }


    p2 = plot_state(minissouri, minissouri_people,
                    pl_fair, dem_fair) +
        labs(title="(b) Proportional plan")
    for (i in 1:3) {
        p2 = add_lab(p2, fmt_lean(dem_fair[as.character(i)]),
                     coords_fair$x[i], coords_fair$y[i])
    }

    p3 <- plot_state(map = minissouri,
                     people = add_voter_harm(minissouri_people, minissouri, dem_harm_gerry_dem, rep_harm_gerry_dem),
                     plan = pl_gerry_dem, qty = dem_gerry_dem, grey_out = TRUE) +
        labs(title = '(c) Individual Harm')

})

d_harm = tibble(
    party=c("Dem.", "Rep."),
    gerry=c("Dem.", "Dem."),
    harm = c(sum(dem_harm_gerry_dem)/sum(minissouri$dem),
             sum(rep_harm_gerry_dem)/sum(minissouri$rep))
)

p4 = ggplot(d_harm, aes(x=paste(gerry, "gerrymander"), y=harm, fill=party)) +
    geom_col(position="dodge") +
    scale_fill_manual(values=c(DEM, GOP), guide="none") +
    scale_y_continuous("Average harm", expand=expansion(mult=c(0, 0.05))) +
    annotate("segment", x=0.9, xend=0.9, y=d_harm$harm[1], yend=d_harm$harm[2],
             arrow = arrow(ends="both", angle=90, length=unit(.2, "cm")), size=1) +
    annotate("text", x=0.9, y=0.25, angle=90, label="Differential harm",
             vjust=-0.5, family="Times", size=3) +
    labs(title="(d) Voter harm by party", x=NULL) +
    coord_fixed(ratio=4.2) +
    theme_repr() +
    theme(plot.title=element_text(hjust=0.5))




p1 + p2 + p3 + p4 +
    plot_layout(guides="collect" , nrow = 2)

ggsave(here("paper/figures/diffharm_minissouri.pdf"), width=8, height=5.1)
