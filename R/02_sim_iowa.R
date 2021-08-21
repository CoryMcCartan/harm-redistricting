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
pl_sum = plans %>%
    group_by(draw) %>%
    summarize(n_dem = sum(dem > 0.5),
              across(c(dev, comp, competitive:pbias), ~ .[1]))

m_dem = arrange(plans, as.integer(draw), district)$dem %>%
    matrix(nrow=attr(iowa, "ndists"))
m_prec = matrix(nrow=nrow(iowa), ncol=nrow(pl_sum))
for (i in seq_len(ncol(m_prec))) m_prec[, i] = m_dem[, i][as.matrix(plans)[, i]]

flog = \(x) if_else(x <= 0, -1, suppressWarnings(log(x)))
voters = sum(iowa$ndv) + sum(iowa$nrv)
pl_sum$h_dem = as.numeric(iowa$ndv %*% (m_prec > 0.5) - sum(iowa$ndv * rowMeans(m_prec > 0.5))) / sum(iowa$ndv)
pl_sum$h_rep = as.numeric(iowa$nrv %*% (m_prec < 0.5) - sum(iowa$nrv * rowMeans(m_prec < 0.5))) / sum(iowa$nrv)
pl_sum$u_indiv = as.numeric(iowa$ndv %*% (m_prec > 0.5) + iowa$nrv %*% (m_prec < 0.5)) / voters
pl_sum$u_tot = (sum(iowa$ndv)*flog(pl_sum$n_dem) + sum(iowa$ndv)*flog(pl_sum$n_dem)) / voters

hist(pl_sum, mean_med, bins=30)
hist(pl_sum, competitive, bins=30)
hist(pl_sum, u_indiv, bins=30)
hist(pl_sum, u_tot)
qplot(u_tot, u_indiv, data=pl_sum, geom="jitter", size=I(0.5))
qplot(h_dem, h_rep, data=pl_sum, geom="jitter", size=I(0.5))

u_avg = mean(pl_sum$u_indiv[-1:-2])
hist(pl_sum, u_indiv - u_avg, bins=60)

