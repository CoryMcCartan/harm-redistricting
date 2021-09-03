pl_mininois = redist_smc(mininois, 2000, verbose=F)

opt = redist_shortburst(mininois, scorer_group_pct(mininois, dem, pop, 3))

pl_minissouri = redist_smc(minissouri, 2000, verbose=F) %>%
    add_reference(pl_opt$minissouri, "opt") %>%
    calc_plans_stats(minissouri, dem, rep)


redist.plot.scatter(pl_sum_mini, u_glb, h_dem - h_rep)
redist.plot.scatter(pl_sum_mini, u_loc, h_dem - h_rep)
redist.plot.scatter(pl_sum_mini, u_loc, u_glb)
redist.plot.scatter(pl_sum_mini, h_dem - h_rep, h)
redist.plot.scatter(pl_sum_mini, h_dem - h_rep, n_dem_smooth)
redist.plot.scatter(pl_sum_mini, h, n_dem_smooth)
redist.plot.scatter(pl_sum_mini, u_glb*0.15 + u_loc*0.85, n_dem_smooth)
redist.plot.scatter(pl_sum_mini, h_dem - h_rep, egap)
