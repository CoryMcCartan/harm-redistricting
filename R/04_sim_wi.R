wi = read_rds(here("data/WI_cd_final_vtd_20.rds")) %>%
    redist_map(pop_tol=0.01, ndists=8, adj=.$adj) %>%
    mutate(across(contains("dv"), ~ coalesce(., 0)),
           across(contains("rv"), ~ coalesce(., 0)))

if (!file.exists(sim_path <- here("data/wi_sims.rds"))) {
    N_sim = 1000
    plans = redist_smc(wi, N_sim, counties=county, pop_temper=0.01, verbose=TRUE)
    write_rds(plans, sim_path, compress="xz")
} else {
    plans = read_rds(sim_path)
}

statewide = with(wi, sum(ndv)/sum(ndv+nrv))
prop_seats = round(attr(wi, "ndists") * statewide)

pl = calc_plans_stats(plans, wi, ndv, nrv)

pdf("out/wi_vars.pdf", 9, 9)
expl_vars(pl, e_dem, u_glb, u_loc, f, f_glb, h, egap, pbias, mean_med, competitive)
dev.off()


d_plot = pl$plan %>%
    mutate(`terc_Local Utility` = ntile(u_loc, 10),
           `terc_Global Utility` = ntile(u_glb, 10),
           terc_Fairness = ntile(f, 10)) %>%
    mutate(across(starts_with("terc_"), ~ if_else(. %in% c(1L, 10L), ., 5L))) %>%
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
ggsave("paper/figures/wi_terciles2.pdf", width=6.5, height=6)


plot(wi, rowMeans(pl$mat > 0.5)) + scale_fill_party_c()
plot(wi, rowMeans(pl$mat)) + scale_fill_party_c(limits=c(0.3, 0.7))

idx_loc = as.integer(as.character(filter(pl$plan, u_loc == max(u_loc))$draw[1]))
idx_glb = as.integer(as.character(filter(pl$plan, u_glb == max(u_glb))$draw[1]))
idx_both = mutate(pl$plan, score = scale(u_loc)[,1] +scale(u_glb)[,1]) %>%
    filter(score == max(score)) %>%
    pull(draw) %>%
    `[`(1) %>%
    as.character() %>%
    as.integer()

p1 = plot_cds(wi, as.matrix(pl$distr)[,idx_loc], county, "WI") + labs(title="Best Local")
p2 = plot_cds(wi, as.matrix(pl$distr)[,idx_glb], county, "WI") + labs(title="Best Global")
p3 = plot_cds(wi, as.matrix(pl$distr)[,idx_both], county, "WI") + labs(title="Best Overall")
p3 + {if (idx_both != idx_loc) p1 } + {if (idx_both != idx_glb) p2 } +
    plot_layout(guides="collect") & theme(legend.position="bottom")


pl$distr %>%
    add_reference(as.matrix(pl$distr)[, 615], "glb") %>%
    add_reference(as.matrix(pl$distr)[, 81], "loc") %>%
    add_reference(as.matrix(pl$distr)[, 457], "comp") %>%
    mutate(dem = group_frac(wi, ndv, ndv+nrv)) %>%
plot(dem, color_thresh=0.5, size=0.1) +
    scale_color_party_d() +
    theme_repr()
