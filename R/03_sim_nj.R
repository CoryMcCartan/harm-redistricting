nj = read_rds(here("data/NJ_cd_final_vtd_20.rds")) %>%
    redist_map(pop_tol=0.01, ndists=12, adj=.$adj)

N_sim = 10e3
n_runs = 4
if (!file.exists(sim_path <- here("data/nj_sims.rds"))) {
    plans = redist_smc(nj, N_sim/n_runs, counties=county, pop_temper=0.005,
                       runs=n_runs, verbose=TRUE) %>%
        add_reference(nj$dem_comm) %>%
        add_reference(nj$rep_comm)

    write_rds(plans, sim_path, compress="xz")
} else {
    plans = read_rds(sim_path)
}


statewide = with(nj, sum(ndv)/sum(ndv+nrv))
prop_seats = round(attr(nj, "ndists") * statewide)

pl = calc_plans_stats(plans, nj, ndv, nrv, elec_model_spec)
pl$plan$chain = by_plan(plans$chain, ndists=12)
pl$plan$e_rep <- attr(plans, "ndists") - pl$plan$e_dem

# get 100% accurate harm for 2 gerrymanders
hh = partisan_harm(pl$distr, dem, nj$ndv, nj$nrv, elec_model_spec, idx_1=1:2, idx_2=(2+1:N_sim))
pl$plan$h_dem[1:2] = hh[1, ]
pl$plan$h_rep[1:2] = hh[2, ]
pl$plan$dh[1:2] = hh[1, ] - hh[2, ]
pl$plan$h[1:2] = hh[3, ]

# Initial plots ----
map_scale = ggredist::scale_fill_party_c(name="Two-party\nvote share", limits=c(0.3, 0.7))
geom_simp = rmapshaper::ms_simplify(nj$geometry, keep=0.02, keep_shapes=TRUE)
p1 = ggplot(nj, aes(fill=ndv/(ndv+nrv))) +
    geom_sf(aes(geometry=geom_simp), size=0, color="#00000000") +
    map_scale +
    labs(title="(a) Partisan patterns") +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p2 = plot_cds(nj, as.matrix(plans)[,"dem_comm"], county, "NJ") +
    labs(title="(b) Democratic proposal\n(enacted)") +
    map_scale +
    theme_repr_map() +
    theme(plot.title = element_text(hjust = 0.5))
p3 = plot_cds(nj, as.matrix(plans)[,"rep_comm"], county, "NJ") +
    labs(title="(c) Republican proposal") +
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

# Harm plots ----
hh_prec_dem = prec_harm(pl$distr, dem, nj$ndv, rep(0, nrow(nj)), elec_model_spec,
                        idx_1=1:2, idx_2=(2+1:N_sim)) / nj$ndv
hh_prec = prec_harm(pl$distr, dem, nj$ndv, nj$nrv, elec_model_spec,
                    idx_1=1:2, idx_2=(2+1:N_sim)) /
    (nj$ndv + nj$nrv)
p1 = plot(nj, hh_prec[,1]) + scale_fill_wa_c("forest_fire") + labs(title="GOP plan harm")
p2 = plot(nj, hh_prec[,2]) + scale_fill_wa_c("forest_fire") + labs(title="Dem plan harm")
p2 + p1
plot(nj, hh_prec[,2] - hh_prec[,1]) + scale_fill_party_c(midpoint=0, limits=c(-0.3, 0.3))

hh_prec_dem = prec_harm(pl$distr, dem, nj$ndv, 0*nj$nrv, elec_model_spec,
                        idx_1=3:102, idx_2=103:1102) / nj$ndv
hh_prec_rep = prec_harm(pl$distr, dem, 0*nj$ndv, nj$nrv, elec_model_spec,
                        idx_1=3:102, idx_2=103:1102) / nj$nrv
plot(nj, rowMeans(hh_prec_dem)) + scale_fill_wa_c("forest_fire")
plot(nj, rowMeans(hh_prec_rep)) + scale_fill_wa_c("forest_fire")


# variables pairs plot -----
meas_labels = c(e_rep="Expected\nRep. seats", dh="Differential harm", h="Average harm",
                egap="Efficiency gap", pbias="Partisan bias", mean_med="Mean-median", decl="Declination", disloc = 'Dislocation', gi = 'Ranked Marginal\nDeviation')

path <- here("paper/figures/nj_pairs.pdf")
if (!file.exists(path)) {
    pl_plot = pl$plan %>%
        as_tibble() %>%
        select(e_rep, dh, h, egap, pbias, mean_med, decl, disloc, gi)
    pl_plot = bind_rows(head(pl_plot, 2), slice_sample(head(pl_plot, -2), n=2000))

    expl_vars(pl_plot, labels=meas_labels, refs=c(GOP, DEM), rasterize=TRUE)
    dev.copy2pdf(file=path, width=8.5, height=8.5)
}

path <- here("paper/figures/nj_meas.pdf")
if (!file.exists(path)) {
    make_d_hist = function(x) {
        as_tibble(x) %>%
            select(draw, e_rep, dh, h, egap, pbias, mean_med, decl,
                   disloc, gi) %>%
            pivot_longer(-draw, names_to="var") %>%
            mutate(var = fct_inorder(str_squish(meas_labels[var])))
    }
    d_hist_samp = make_d_hist(subset_sampled(pl$plan))
    d_hist_ref = make_d_hist(subset_ref(pl$plan)) %>%
        drop_na()
    d_refline = tibble(var=d_hist_ref$var[c(2, 4:9)], value=0)

    p = ggplot(d_hist_samp, aes(value)) +
        facet_wrap(~ var, scales="free", nrow=2) +
        geom_histogram(fill="#888888", bins=48) +
        geom_vline(aes(xintercept=value), data=d_refline, lty="dashed", lwd=0.45) +
        geom_vline(aes(xintercept=value, color=draw), data=d_hist_ref, lwd=1.25) +
        scale_color_manual(values=c(rep_comm=GOP, dem_comm=DEM),
                           labels=c(
                           rep_comm = 'Republican proposal',
                           dem_comm = 'Democratic proposal')) +
        scale_x_continuous(NULL, labels=function(x) ifelse(x > 10 | x == 0, number(x, 1), number(x, 0.01))) +
        scale_y_continuous("Number of plans", expand=expansion(mult=c(0, 0.05))) +
        labs(x=NULL, color="Plan") +
        theme_repr() +
        theme(legend.position = 'inside',
              legend.position.inside = c(0.915, 0.25))
    ggsave(path, plot=p, width=8.5, height=3)

    r1 <- c('Expected Rep. seats', 'Differential harm', 'Average harm')
    ggplot(d_hist_samp %>% mutate(row = (!var %in% r1) + 1) , aes(value)) +
        facet_grid(row ~ var, scales="free", ) +
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
ggplot(pl_best, aes(score, dh, color=e_rep)) +
    geom_point(size=0.4) +
    geom_smooth(method="lm", color="black")

with(pl_best, cor(score, h, method="pearson"))
}

rm(nj, plans, pl_best, pl_plot, p, p1, p2, p3, p4, pl,
   m_dem, statewide, prop_setas, meas_labels, geom_simp)
