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
expl_vars(pl, e_dem, u_glb, u_loc, f, h_dem, h, egap, pbias, mean_med, competitive)
dev.off()

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
