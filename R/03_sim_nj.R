nj = read_rds(here("data/NJ_cd_final_vtd_20.rds")) %>%
    redist_map(pop_tol=0.01, ndists=12, adj=.$adj)

if (!file.exists(sim_path <- here("data/nj_sims.rds"))) {
    N_sim = 1000
    plans = redist_smc(nj, N_sim, counties=county, pop_temper=0.01, verbose=TRUE)#FALSE)

    # gerrymanders
    set.seed(5118)
    opt_dem = redist_shortburst(nj, scorer_group_pct(nj, ndv, ndv+nrv, 11))
    set.seed(5118)
    opt_rep = redist_shortburst(nj, scorer_group_pct(nj, nrv, ndv+nrv, 6))

    plans %>%
        add_reference(last_plan(opt_dem), "dem_gerry") %>%
        add_reference(last_plan(opt_rep), "rep_gerry") %>%
        write_rds(sim_path, compress="xz")
} else {
    plans = read_rds(sim_path)
}

statewide = with(nj, sum(ndv)/sum(ndv+nrv))
prop_seats = round(attr(nj, "ndists") * statewide)

pl = calc_plans_stats(plans, nj, ndv, nrv)

# Initial plots
p1 = ggplot(nj, aes(fill=ndv/(ndv+nrv))) +
    geom_sf(size=0, color="#00000000") +
    scale_fill_party_c(limits=c(0.15, 0.85)) +
    labs(title="(a) Partisan patterns") +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p2 = plot_cds(nj, as.matrix(plans)[,"dem_gerry"], county, "NJ") +
    labs(title="(b) Democratic gerrymander") +
    scale_fill_party_c(limits=c(0.15, 0.85)) +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p3 = plot_cds(nj, as.matrix(plans)[,"rep_gerry"], county, "NJ") +
    labs(title="(c) Republican gerrymander") +
    scale_fill_party_c(limits=c(0.15, 0.85)) +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p = p1 + p2 + p3 + plot_layout(guides="collect")
ggsave(here("paper/figures/nj_maps.pdf"), plot=p, width=8, height=4.5)




pdf("out/nj_vars.pdf", 9, 9)
expl_vars(pl, e_dem, u_glb, u_loc, f, f_glb, h, egap, pbias, mean_med, competitive)
dev.off()

pdf("out/nj_vars2.pdf", 9, 9)
subset_sampled(pl$plan) %>%
    select(n_dem, e_dem, u_glb, u_loc, f, h, egap, pbias, mean_med) %>%
    expl_vars(labels=c("Expected\nDem. seats", expression(U^G), expression(U^L),
                       "F(q)", "H", "Efficiency\ngap", "Partisan\nbias", "Mean-median"))
dev.off()

select(pl$plan, e_dem, u_glb, u_loc, f, egap, pbias, mean_med, competitive) %>%
    #pairs(cex=0.2)
    pairsD3::pairsD3(cex=1.5, opacity=0.5, group=pl$plan$n_dem)


d_plot = pl$plan %>%
    mutate(`terc_Local Utility` = ntile(u_loc, 3),
           `terc_Global Utility` = ntile(u_glb, 3),
           terc_Fairness = ntile(f, 3)) %>%
    select(draw, n_dem, e_dem, u_loc, u_glb, f, starts_with("terc_")) %>%
    as_tibble()
d_plot = number_by(pl$distr, dem) %>%
    left_join(d_plot, by="draw") %>%
    pivot_longer(starts_with("terc_"), names_to="qty",
                 values_to="tercile", names_prefix="terc_")

ggplot(d_plot, aes(as.factor(district), dem, fill=as.factor(tercile))) +
    facet_grid(qty ~ .) +
    geom_hline(yintercept=0.5, lty="dashed") +
    geom_boxplot(size=0.3, outlier.size=0.1) +
    scale_y_continuous("Democratic two-party share", labels=percent) +
    labs(x="Districts, ordered by Democratic share",
         fill="Tercile\nof measure") +
    scale_fill_wa_d("sound_sunset", which=c(1, 8, 13)) +
    theme_repr()
ggsave("paper/figures/nj_terciles.pdf", width=6.5, height=6)



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
