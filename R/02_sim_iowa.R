iowa = read_rds(here("data/IA_cd_final_vtd_20.rds")) %>%
    redist_map(pop_tol=0.001, ndists=4, adj=.$adj)

N_sim = 5e3
plans = redist_smc(iowa, N_sim, verbose=FALSE)
#pl_opt_dem = redist_shortburst(iowa, scorer_group_pct(iowa, ndv, ndv+nrv, 2))
opt_dem = c(4, 4, 2, 4, 4, 4, 2, 2, 2, 3, 2, 2, 2, 2, 4, 3, 2, 4, 2, 4, 2, 2, 3, 2, 1, 4, 4, 2, 3, 2, 2, 2, 2, 2, 2, 4, 2, 2, 1, 2, 2, 2, 4, 3, 2, 2, 4, 4, 3, 1, 4, 3, 3, 4, 2, 4, 3, 3, 4, 2, 4, 4, 4, 1, 4, 2, 4, 4, 4, 3, 4, 2, 4, 2, 4, 2, 1, 4, 1, 4, 2, 3, 4, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 4, 2, 2)
#pl_opt_rep = redist_shortburst(iowa, scorer_group_pct(iowa, nrv, ndv+nrv, 4))
opt_rep = c(4, 1, 3, 1, 4, 3, 2, 4, 2, 3, 2, 2, 4, 4, 4, 3, 2, 4, 3, 1, 2, 3, 3, 4, 4, 1, 1, 3, 2, 2, 3, 2, 3, 3, 2, 1, 4, 2, 4, 4, 4, 2, 4, 2, 3, 4, 4, 3, 3, 4, 1, 2, 3, 2, 2, 2, 3, 2, 1, 2, 1, 1, 1, 4, 1, 2, 4, 1, 1, 2, 2, 2, 1, 4, 4, 4, 1, 4, 2, 1, 4, 3, 4, 2, 4, 2, 1, 4, 1, 1, 1, 2, 1, 4, 2, 3, 4, 2, 4)

plans = plans %>%
    subset_sampled() %>%
    add_reference(opt_dem, "dem_gerry") %>%
    add_reference(opt_rep, "rep_gerry") %>%
    mutate(dev = plan_parity(iowa),
           comp = distr_compactness(iowa),
           dem = group_frac(iowa, ndv, ndv + nrv),
           competitive = competitiveness(iowa, nrv, ndv, ),
           mean_med = partisan_metrics(iowa, "MeanMedian", nrv, ndv),
           pbias = partisan_metrics(iowa, "Bias", nrv, ndv))
m_dem = district_group(plans, dem)

pl_sum = plans %>%
    group_by(draw) %>%
    summarize(n_dem = sum(dem > 0.5),
              across(c(dev, comp, competitive:pbias), ~ .[1])) %>%
    mutate(u_loc_dem = utility_local(iowa, ndv, m_dem),
           u_loc_rep = utility_local(iowa, ndv, m_dem, invert=TRUE),
           h1_dem = harm_v1(iowa, ndv, m_dem, idx_2=-1:-2),
           h1_rep = harm_v1(iowa, ndv, m_dem, idx_2=-1:-2, invert=TRUE),
           h2_dem = harm_v2(iowa, ndv, m_dem, idx_2=-1:-2),
           h2_rep = harm_v2(iowa, ndv, m_dem, idx_2=-1:-2, invert=TRUE),
           u_loc = total(u_loc_dem, u_loc_rep, iowa, ndv, nrv),
           h1 = total(h1_dem, h1_rep, iowa, ndv, nrv),
           h2 = total(h2_dem, h2_rep, iowa, ndv, nrv))

hist(pl_sum, mean_med, bins=100)
hist(pl_sum, competitive, bins=100)
hist(pl_sum, h1, bins=100)
hist(pl_sum, h2, bins=100)
hist(pl_sum, u_loc, bins=100)
hist(pl_sum, h1_dem - h1_rep, bins=100)
hist(pl_sum, h2_dem - h2_rep, bins=100)
redist.plot.scatter(pl_sum, h1, h2)
redist.plot.scatter(pl_sum, h1_dem - h1_rep, h2_dem - h2_rep)
redist.plot.scatter(pl_sum, h1_dem, h2_dem)
redist.plot.scatter(pl_sum, h1_rep, h2_rep)

