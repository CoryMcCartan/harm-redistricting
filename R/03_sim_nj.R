nj = read_rds(here("data/NJ_cd_final_vtd_20.rds")) %>%
    redist_map(pop_tol=0.01, ndists=12, adj=.$adj)

if (!file.exists(sim_path <- here("data/nj_sims.rds"))) {
    N_sim = 10000
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
if (!file.exists(path <- here("paper/figures/nj_maps.pdf")))
    ggsave(path, plot=p, width=8, height=4.5)


# variables plot -----
meas_labels = c(e_dem="Expected\nDem. seats", u_glb=expression(U^G), u_loc=expression(U^L),
                f="ΔH(q)", h="H", egap="Efficiency\ngap", pbias="Partisan\nbias", mean_med="Mean-median")
plot_var = function(nm, i) { suppressMessages({
    lab = meas_labels[nm]
    if (str_detect(as.character(lab), "\n"))
        lab = str_replace(lab, "\n", " ")
    p = hist(pl$plan, !!ensym(nm), fill="#aaaaaa", bins=48) +
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
if (!file.exists(path <- here("paper/figures/nj_vars.pdf")))
    ggsave(path, plot=p, width=8, height=4)



# best / worst
find_best = function(qty1, qty2=NULL) {
    p = subset_sampled(pl$plan)
    x1 = eval_tidy(enquo(qty1), p)
    if (missing(qty2)) {
        p$draw[which.max(x1)]
    } else {
        x2 = eval_tidy(enquo(qty2), p)
        x = as.numeric(scale(x1) + scale(x2))
        p$draw[which.max(x)]
    }
}

comp_pl = list(`Smallest differential harm` = find_best(-abs(f)),
               `Best overall utility` = find_best(u_loc, u_glb),
               `High global, low local utility` = find_best(-u_loc, u_glb),
               `Low global, high local utility` = find_best(u_loc, -u_glb),
               `Fair but low local utility` = find_best(-abs(f), -u_loc),
               `Unfair but high local utility` = find_best(abs(f), u_loc)) %>%
    imap(function(x, nm) {
        pl$distr %>%
            filter(as.character(draw) == as.character(x)) %>%
            mutate(draw = nm)
    }) %>%
    do.call(rbind, .) %>%
    mutate(draw = fct_inorder(draw)) %>%
    number_by(dem)

p = plot(subset_sampled(pl$distr), dem, coef=100, size=0, geom="boxplot") +
    geom_hline(yintercept=0.5, lty="dashed") +
    geom_boxplot(fill="#eeeeee", color="#777777", size=0.25, coef=100) +
    geom_point(aes(district, dem, color=draw, shape=draw), position=position_dodge(0.8),
                 data=comp_pl, inherit.aes=F, size=3.0) +
    scale_shape_manual(values=c(18, 16, 15, 8, 7, 17)) +
    scale_y_continuous("Democratic two-party share", labels=\(x) percent(x, 1)) +
    labs(x="Districts, ordered by Democratic share",
         color="Districting plan", shape="Districting plan") +
    theme_repr() +
    theme(legend.position=c(0.2, 0.8),
          legend.background=element_blank(),
          panel.grid.major.x=element_blank())
if (!file.exists(path <- here("paper/figures/nj_district_shares.pdf")))
    ggsave(path, plot=p, width=8, height=5)


ex_plots = map(seq_len(n_distinct(comp_pl$draw)), function(i) { suppressMessages({
    plot_cds(nj, as.matrix(comp_pl)[, i], county, "NJ") +
        labs(title=levels(comp_pl$draw)[i]) +
        scale_fill_party_c(limits=c(0.15, 0.85)) +
        theme_repr_map() +
        theme(plot.title = element_text(hjust = 0.5))
})})
p = wrap_plots(ex_plots, nrow=2) + plot_layout(guides="collect")
if (!file.exists(path <- here("paper/figures/nj_ex_maps.pdf")))
    ggsave(path, plot=p, width=8, height=8)


# appendix pairs plot ----
if (!file.exists(path <- here("paper/figures/nj_pairs.pdf"))) {
    meas_labels["f"] = expression(f = Delta~"H(q)")
    pdf(path, 9, 9)
    subset_sampled(pl$plan) %>%
        slice_sample(n=1200) %>%
        select(n_dem, e_dem, u_glb, u_loc, f, h, egap, pbias, mean_med) %>%
        expl_vars(labels=meas_labels)
    dev.off()
}
