nj = read_rds(here("data/NJ_cd_final_vtd_20.rds")) %>%
    redist_map(pop_tol=0.01, ndists=12, adj=.$adj)

N_sim = 10000
if (!file.exists(sim_path <- here("data/nj_sims.rds"))) {
    plans = redist_smc(nj, N_sim, counties=county, pop_temper=0.005, verbose=TRUE)#FALSE)

    # gerrymanders
    set.seed(5118)
    opt_dem = redist_shortburst(nj, scorer_group_pct(nj, ndv, ndv+nrv, 11), max_bursts=800)
    set.seed(5118)
    opt_rep = redist_shortburst(nj, scorer_group_pct(nj, nrv, ndv+nrv, 6), max_bursts=800)

    plans = plans %>%
        add_reference(last_plan(opt_dem), "dem_gerry") %>%
        add_reference(last_plan(opt_rep), "rep_gerry")
    write_rds(plans, sim_path, compress="xz")
} else {
    plans = read_rds(sim_path)
    attr(plans, "ndists") = 12L
}

statewide = with(nj, sum(ndv)/sum(ndv+nrv))
prop_seats = round(attr(nj, "ndists") * statewide)

pl = calc_plans_stats(plans, nj, ndv, nrv)

# get 100% accurate harm for 2 gerrymanders
hh = harm(pl$distr, dem, nj$ndv, nj$nrv, idx_1=1:2, idx_2=(2+1:N_sim), ker=k_t())
pl$plan$h_dem[1:2] = hh[1, ]
pl$plan$h_rep[1:2] = hh[2, ]
pl$plan$dh[1:2] = hh[1, ] - hh[2, ]
pl$plan$h[1:2] = hh[3, ]

# Initial plots ----
map_scale = scale_fill_party_c(name="Democratic\nshare", limits=c(0.19, 0.81))
geom_simp = rmapshaper::ms_simplify(nj$geometry, keep_shapes=TRUE)
p1 = ggplot(nj, aes(fill=ndv/(ndv+nrv))) +
    geom_sf(aes(geometry=geom_simp), size=0, color="#00000000") +
    map_scale +
    labs(title="(a) Partisan patterns") +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p2 = plot_cds(nj, as.matrix(plans)[,"dem_gerry"], county, "NJ") +
    labs(title="(b) Democratic\ngerrymander") +
    map_scale +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p3 = plot_cds(nj, as.matrix(plans)[,"rep_gerry"], county, "NJ") +
    labs(title="(c) Republican\ngerrymander") +
    map_scale +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p4 = plot_cds(nj, as.matrix(plans)[,425], county, "NJ") +
    labs(title="(d) Sample plan") +
    map_scale +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p = p1 + p2 + p3 + p4 + plot_layout(guides="collect", nrow=1)
if (!file.exists(path <- here("paper/figures/nj_maps.pdf")))
    ggsave(path, plot=p, width=8, height=3.75)


# variables pairs plot -----
meas_labels = c(e_dem="Expected\nDem. seats", dh="Differential harm", h="Average harm",
                egap="Efficiency gap", pbias="Partisan bias", mean_med="Mean-median", decl="Declination")

if (!file.exists(path <- here("paper/figures/nj_pairs.pdf"))) {
    pl_plot = pl$plan %>%
        as_tibble() %>%
        select(n_dem, e_dem, dh, h, egap, pbias, mean_med, decl)
    pl_plot = bind_rows(head(pl_plot, 2), slice_sample(head(pl_plot, -2), n=2000))

    expl_vars(pl_plot, labels=meas_labels, refs=GOP_DEM[c(2, 14)], rasterize=TRUE)
    dev.copy2pdf(file=path, width=8.5, height=8.5)
}

# p-values
cat("p-values:")
pl$plan %>%
    as_tibble() %>%
    mutate(across(n_dem:h, pval)) %>%
    print()

if (FALSE) {
pl_best = pl$plan %>%
    subset_sampled() %>%
    as_tibble() %>%
    mutate(across(c(pbias, mean_med, egap, decl), ~ scale(.)[, 1]),
           score = sqrt(pbias^2 + mean_med^2 + egap^2 + decl^2))
ggplot(pl_best, aes(score, dh, color=e_dem)) +
    geom_point(size=0.4) +
    geom_smooth(method="lm", color="black")

with(pl_best, cor(score, h, method="pearson"))
}

rm(nj, plans, pl_best, pl_plot, pl_sum, p, p1, p2, p3, p4, pl,
   m_dem, statewide, prop_setas, meas_labels)
