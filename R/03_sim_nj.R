nj = read_rds(here("data/NJ_cd_final_vtd_20.rds")) %>%
    redist_map(pop_tol=0.01, ndists=12, adj=.$adj)

if (!'dem_comm' %in% names(nj)) {
    nj_enacted <- read_sf('https://redistricting.lls.edu/wp-content/uploads/nj_2020_congress_2021-12-22_2031-06-30.json')
    nj_rep_comm <- read_sf('data-raw/NJ CD 2021 GOP submission/NJ CD 2021 GOP V5_Hospital_shoreline.shp')
    nj$dem_comm <- as.integer(nj_enacted$District[geomander::geo_match(from = nj, to = nj_enacted, method = 'area')])
    nj$rep_comm <- as.integer(nj_rep_comm$DISTRICT[geomander::geo_match(from = nj, to = nj_rep_comm, method = 'area')])
}

N_sim = 10e3
n_runs = 4
if (!file.exists(sim_path <- here("data/nj_sims.rds"))) {
    plans = redist_smc(nj, N_sim/n_runs, counties=county, pop_temper=0.005,
                       runs=n_runs, verbose=TRUE)

    # gerrymanders
    set.seed(5118)
    opt_dem = redist_shortburst(nj, scorer_group_pct(nj, ndv, ndv+nrv, 11),
                                max_bursts=1000, burst_size=15, return_all=FALSE)
    set.seed(5118)
    opt_rep = redist_shortburst(nj, scorer_group_pct(nj, nrv, ndv+nrv, 6),
                                max_bursts=1000, burst_size=15, return_all=FALSE)

    plans = plans %>%
        subset_sampled() %>%
        add_reference(last_plan(opt_dem), "dem_gerry") %>%
        add_reference(last_plan(opt_rep), "rep_gerry")
    write_rds(plans, sim_path, compress="xz")
} else {
    plans = read_rds(sim_path)
}

plans <- plans %>%
    subset_sampled() %>%
    add_reference(ref_plan = nj$dem_comm, name = 'dem_comm') %>%
    add_reference(ref_plan = nj$rep_comm, name = 'rep_comm')

statewide = with(nj, sum(ndv)/sum(ndv+nrv))
prop_seats = round(attr(nj, "ndists") * statewide)

pl = calc_plans_stats(plans, nj, ndv, nrv, elec_model_spec)
pl$plan$chain = by_plan(plans$chain, ndists=12)

# get 100% accurate harm for 2 gerrymanders
hh = partisan_harm(pl$distr, dem, nj$ndv, nj$nrv, elec_model_spec, idx_1=1:2, idx_2=(2+1:N_sim))
pl$plan$h_dem[1:2] = hh[1, ]
pl$plan$h_rep[1:2] = hh[2, ]
pl$plan$dh[1:2] = hh[1, ] - hh[2, ]
pl$plan$h[1:2] = hh[3, ]

# Initial plots ----
map_scale = scale_fill_party_c(name="Democratic\nshare", limits=c(0.3, 0.7))
geom_simp = rmapshaper::ms_simplify(nj$geometry, keep=0.02, keep_shapes=TRUE)
p1 = ggplot(nj, aes(fill=ndv/(ndv+nrv))) +
    geom_sf(aes(geometry=geom_simp), size=0, color="#00000000") +
    map_scale +
    labs(title="(a) Partisan patterns") +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p2 = plot_cds(nj, as.matrix(plans)[,"dem_comm"], county, "NJ") +
    labs(title="(b) Democratic\nproposal") +
    map_scale +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p3 = plot_cds(nj, as.matrix(plans)[,"rep_comm"], county, "NJ") +
    labs(title="(c) Republican\nproposal") +
    map_scale +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p4 = plot_cds(nj, as.matrix(plans)[,425], county, "NJ") +
    labs(title="(d) Sample plan") +
    map_scale +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p = p1 + p2 + p3 + p4 + plot_layout(guides="collect", nrow=1) &
    theme(plot.margin = unit(rep(0, 4), "cm"))
if (!file.exists(path <- here("paper/figures/nj_maps.pdf")))
    ggsave(path, plot=p, width=7.5, height=3.5)


# variables pairs plot -----
meas_labels = c(e_dem="Expected\nDem. seats", dh="Differential harm", h="Average harm",
                egap="Efficiency gap", pbias="Partisan bias", mean_med="Mean-median", decl="Declination")

if (!file.exists(path <- here("paper/figures/nj_pairs.pdf"))) {
    pl_plot = pl$plan %>%
        as_tibble() %>%
        select(draw, e_dem, dh, h, egap, pbias, mean_med, decl)
    pl_plot = bind_rows(head(pl_plot, 2), slice_sample(head(pl_plot, -2), n=2000))

    expl_vars(pl_plot, labels=meas_labels, refs=c(GOP, DEM), rasterize=TRUE)
    dev.copy2pdf(file=path, width=8.5, height=8.5)
}

if (!file.exists(path <- here("paper/figures/nj_meas.pdf"))) {
    make_d_hist = function(x) {
        as_tibble(x) %>%
            select(draw, e_dem, dh, h, egap, pbias, mean_med, decl) %>%
            pivot_longer(-draw, names_to="var") %>%
            mutate(var = fct_inorder(str_squish(meas_labels[var])))
    }
    d_hist_samp = make_d_hist(subset_sampled(pl$plan))
    d_hist_ref = make_d_hist(subset_ref(pl$plan)) %>%
        drop_na() %>%
        filter(draw != 'cd_2020')
    d_refline = tibble(var=d_hist_ref$var[c(2, 4:7)], value=0)

    p = ggplot(d_hist_samp, aes(value)) +
        facet_wrap(~ var, scales="free", nrow=2) +
        geom_histogram(fill="#888888", bins=48) +
        geom_vline(aes(xintercept=value), data=d_refline, lty="dashed", lwd=0.45) +
        geom_vline(aes(xintercept=value, color=draw), data=d_hist_ref, lwd=1.25) +
        scale_color_manual(values=c(rep_comm=GOP, dem_comm=DEM),
                           labels=c(
                           rep_comm = 'Republican proposal',
                           dem_comm = 'Democratic proposal')) +
        scale_x_continuous(NULL, labels=function(x) number(x, 0.01)) +
        scale_y_continuous("Number of plans", expand=expansion(mult=c(0, 0.05))) +
        labs(x=NULL, color="Plan") +
        theme_repr() +
        theme(legend.position=c(0.875, 0.25))

    ggsave(path, plot=p, width=7.25, height=3)
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

rm(nj, plans, pl_best, pl_plot, p, p1, p2, p3, p4, pl,
   m_dem, statewide, prop_setas, meas_labels, geom_simp)
