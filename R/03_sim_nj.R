nj = read_rds(here("data/NJ_cd_final_vtd_20.rds")) %>%
    redist_map(pop_tol=0.01, ndists=12, adj=.$adj)

N_sim = 4e2
plans = redist_smc(nj, N_sim, counties=county, pop_temper=0.01, verbose=FALSE)

plans = plans %>%
    mutate(dev = plan_parity(nj),
           comp = distr_compactness(nj),
           dem = group_frac(nj, ndv, ndv + nrv),
           competitive = competitiveness(nj, nrv, ndv, ),
           mean_med = partisan_metrics(nj, "MeanMedian", nrv, ndv),
           pbias = partisan_metrics(nj, "Bias", nrv, ndv))

m_dem = district_group(plans, dem)

ker = k_t()
pl_sum = plans %>%
    group_by(draw) %>%
    summarize(n_dem = sum(dem > 0.5),
              n_dem_smooth = sum(ker(dem)),
              across(c(dev, comp, competitive:pbias), ~ .[1])) %>%
    mutate(u_loc_dem = utility_local(nj, ndv, m_dem, ker),
           u_loc_rep = utility_local(nj, ndv, m_dem, ker, invert=TRUE),
           u_glb_dem = utility_global(plans, dem, ker),
           u_glb_rep = utility_global(plans, dem, ker, invert=TRUE),
           h_dem = harm_v1(nj, ndv, m_dem, idx_2=-1:-2, kernel=ker),
           h_rep = harm_v1(nj, ndv, m_dem, idx_2=-1:-2, kernel=ker, invert=TRUE),
           u_loc_log = total(log(u_loc_dem), log(u_loc_rep), nj, ndv, nrv),
           u_loc = total(u_loc_dem, u_loc_rep, nj, ndv, nrv),
           u_glb = total(u_glb_dem, u_glb_rep, nj, ndv, nrv),
           h = total(h_dem, h_rep, nj, ndv, nrv))

plot(nj, rowMeans(m_Dem > 0.5)) + scale_fill_party_c()

redist.plot.scatter(pl_sum, u_glb, h_dem - h_rep)
redist.plot.scatter(pl_sum, u_loc, h_dem - h_rep)

plot(plans, dem, size=0.2)
hist(plans_diversity(plans)*log(12))
redist.plot.plans(plans, 1:6, nj)
