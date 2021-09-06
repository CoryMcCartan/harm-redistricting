nj = read_rds(here("data/NJ_cd_final_vtd_20.rds")) %>%
    redist_map(pop_tol=0.01, ndists=12, adj=.$adj)

if (!file.exists(sim_path <- here("data/nj_sims.rds"))) {
    N_sim = 1000
    plans = redist_smc(nj, N_sim, counties=county, pop_temper=0.01, verbose=TRUE)#FALSE)
    write_rds(plans, sim_path, compress="xz")
} else {
    plans = read_rds(sim_path)
}

statewide = with(nj, sum(ndv)/sum(ndv+nrv))
prop_seats = round(attr(nj, "ndists") * statewide)

pl = calc_plans_stats(plans, nj, ndv, nrv)

pdf("out/nj_vars.pdf", 9, 9)
expl_vars(pl, e_dem, u_glb, u_loc, f, h_dem, h, egap, pbias, mean_med, competitive)
dev.off()

select(pl$plan, e_dem, u_glb, u_loc, f, egap, pbias, mean_med, competitive) %>%
    #pairs(cex=0.2)
    pairsD3::pairsD3(cex=1.5, opacity=0.5, group=pl$plan$n_dem)


plot(nj, rowMeans(pl$mat > 0.5)) + scale_fill_party_c()


idx_loc = as.integer(as.character(filter(pl$plan, u_loc == max(u_loc))$draw[1]))
idx_glb = as.integer(as.character(filter(pl$plan, u_glb == max(u_glb))$draw[1]))
idx_both = mutate(pl$plan, score = scale(u_loc)[,1] +scale(u_glb)[,1]) %>%
    filter(score == max(score)) %>%
    pull(draw) %>%
    `[`(1) %>%
    as.character() %>%
    as.integer()

p1 = plot_cds(nj, as.matrix(pl$distr)[,idx_loc], county, "NJ") + labs(title="Best Local")
p2 = plot_cds(nj, as.matrix(pl$distr)[,idx_glb], county, "NJ") + labs(title="Best Global")
p3 = plot_cds(nj, as.matrix(pl$distr)[,idx_both], county, "NJ") + labs(title="Best Overall")
p3 + {if (idx_both != idx_loc) p1 } + {if (idx_both != idx_glb) p2 } +
    plot_layout(guides="collect") & theme(legend.position="bottom")


ggplot(pl$plan, aes(f, u_loc, color=as.factor(round(egap, 1)))) +
    geom_point() +
    scale_color_wa_d("sea_star")
ggplot(pl$plan, aes(u_glb, u_loc, color=h)) +
    geom_point()

filter(pl_sum, u_glb >= 1.77, u_loc >=0.6135)


ex1 = filter(pl_sum, f < -0.05, abs(mean_med) <= 0.005) %>%
    arrange(abs(mean_med)) %>%
    filter(row_number() <= 1) #%>%
    as.matrix() %>%
    as.integer()

plot(plans, dem, size=0.2)
hist(plans_diversity(plans)*log(12))
redist.plot.plans(plans, 1:6, nj)


plans %>%
    select(draw:total_pop) %>%
    add_reference(as.matrix(pl_sum)[,999], "ex1")  %>%
    mutate(dem = group_frac(nj, ndv, ndv + nrv)) %>%
plot(dem, size=0.2)

arrange(x, pop) %>%
    mutate(pct = cumsum(pop)/sum(pop),
           pctg = cut(pct, seq(0, 1, 1/6), ordered_result=T),
           dens = cume_dist(pop / as.numeric(units::set_units(st_area(geometry), "mi^2"))),
           densg = cut(dens, 5, ordered_result=T)) %>%
ggplot(aes(fill=pctg)) +
    geom_sf(size=0.01, color="black") +
    theme_void() +
    scale_fill_wa_d("sound_sunset", which=2:14,
                    name="Cumulative\npopulation") +
    labs(title="Cumulative County Population",
         caption="Half the U.S. population lives in the brighter half of the scale.") +
    theme(plot.background=element_rect(fill="black"),
          plot.title=element_text(hjust=0.5, face="bold", size=32, vjust=0.0),
          plot.caption=element_text(vjust=1.5, hjust=0.9),
          text=element_text(color="white", family="Palatino", size=10),
          legend.position=c(0.72, 0.15))
ggsave("~/Desktop/usa_pop.pdf", width=11, height=8.5)
