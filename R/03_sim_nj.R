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

plot(nj, rowMeans(m_dem > 0.5)) + scale_fill_party_c()

redist.plot.scatter(pl_sum, u_glb, h_dem - h_rep)
redist.plot.scatter(pl_sum, u_loc, h_dem - h_rep)
redist.plot.scatter(pl_sum, u_loc, u_glb)
redist.plot.scatter(pl_sum, h_rep, h2_rep)
redist.plot.scatter(pl_sum, h_dem - h_rep, h2_dem - h2_rep)
redist.plot.scatter(pl_sum, h_dem - h_rep, h)
redist.plot.scatter(pl_sum, h_dem - h_rep, n_dem_smooth)
redist.plot.scatter(pl_sum, h, n_dem_smooth)
redist.plot.scatter(pl_sum, h, u_loc)
redist.plot.scatter(pl_sum, h, u_glb)
redist.plot.scatter(pl_sum, u_glb*0.5 + u_loc*0.5, n_dem_smooth)
redist.plot.scatter(pl_sum, u_loc, egap)
redist.plot.scatter(pl_sum, u_loc, mean_med)
redist.plot.scatter(pl_sum, h_dem - h_rep, egap)

ggplot(pl_sum, aes(h_dem - h_rep, u_loc, color=as.factor(round(egap, 1)))) +
    geom_point() +
    scale_color_wa_d("sea_star")
ggplot(pl_sum, aes(u_glb, u_loc, color=h)) +
    geom_point()

filter(pl_sum, u_glb >= 1.77, u_loc >=0.6135)


ex1 = filter(pl_sum, h_dem - h_rep < -0.05, abs(mean_med) <= 0.005) %>%
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
