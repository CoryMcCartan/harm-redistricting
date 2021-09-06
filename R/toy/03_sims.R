pl_mininois = redist_smc(mininois, 2000, verbose=F) %>%
    add_reference(pl_opt$mininois, "opt") %>%
    calc_plans_stats(mininois, dem, rep, ker=k_step)

pl_minissouri = redist_smc(minissouri, 2000, verbose=F) %>%
    add_reference(pl_opt$minissouri, "opt") %>%
    calc_plans_stats(minissouri, dem, rep, ker=k_step)

redist.plot.scatter(pl_mininois$plan, u_glb, u_loc)
redist.plot.scatter(pl_mininois$plan, n_dem, u_glb)

idxs = c(1, with(pl_mininois$plan, which(u_loc==max(u_loc))))
dm = plan_distances(pl_mininois$distr)[idxs, idxs]

as.matrix(pl_mininois$plan)[,110]
