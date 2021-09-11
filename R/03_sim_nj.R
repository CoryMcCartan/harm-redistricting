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

# Initial plots ----
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


# variables plot -----
meas_labels = c(e_dem="Expected\nDem. seats", u_glb=expression(U^G), u_loc=expression(U^L),
                f="F(q)", h="H", egap="Efficiency\ngap", pbias="Partisan\nbias", mean_med="Mean-median")
plot_var = function(nm, i) { suppressMessages({
    lab = meas_labels[nm]
    if (str_detect(as.character(lab), "\n"))
        lab = str_replace(lab, "\n", " ")
    p = hist(pl$plan, !!ensym(nm), fill="#aaaaaa", bins=32) +
        scale_color_party_d(guide="none") +
        labs(x=NULL, title=lab) +
        scale_y_continuous(name = if (i %% 4 == 1) "Fraction of plans" else NULL,
                           labels=function(x) percent(x, 1),
                           limits=c(0, 0.405), expand=expansion(mult=c(0, 0.05))) +
        theme_repr() +
        theme(plot.title=element_text(size=10),
              panel.grid.minor=element_blank(),
              panel.grid.major.x=element_blank())
    if (min(pl$plan[[nm]]) < 0 & max(pl$plan[[nm]]) > 0)
        p = p + geom_vline(xintercept=0, lty="dashed", size=0.25)
    if (nm == "e_dem")
        p = p + geom_vline(xintercept=statewide*12, lty="dashed", size=0.25) +
            annotate("label", x=statewide*12, y=0.35, label="Proportional share",
                     family="Times", size=2.5, hjust=-0.05, label.size=0,
                     label.r=unit(0, "pt"), label.padding=unit(1, "pt"))
    if (i %% 4 != 1)
        p = p + theme(axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())
    p
})}
p = imap(names(meas_labels), plot_var) %>%
    wrap_plots(nrow=2, ncol=4)
ggsave(here("paper/figures/nj_vars.pdf"), plot=p, width=8, height=4)



# terciles plot -----

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


# appendix pairs plot ----
pdf("paper/figures/nj_pairs.pdf", 9, 9)
subset_sampled(pl$plan) %>%
    select(n_dem, e_dem, u_glb, u_loc, f, h, egap, pbias, mean_med) %>%
    expl_vars(labels=meas_labels)
dev.off()
