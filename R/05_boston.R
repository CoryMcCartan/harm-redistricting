if (!exists("fit_pcei")) source(here("R/04_fit_boston.R"))

d = read_rds(here("data/boston_elec.rds")) %>%
    mutate(ward = as.integer(str_split(precinct, "-", n=2, simplify=T)[,1]))
map = redist_map(d, existing_plan=ccd_2010, adj=d$adj)

d_votes = make_votes_long(d)
tbls = make_tables(d_votes, cand_thresh=0.02)

# priors for identification
id = tribble(~cand, ~loc, ~scale,
             "wu", c(-1, 1.0, 0.0), c(0.1, 0.1, 1),
             "essaibi_george", c(1, 1.0, 0.0), c(0.1, 0.2, 1))

fit = fit_pcei(tbls, n_comp=2L, id=id, algorithm="vb", eta=0.2, adapt_engaged=F)
init_l = lapply(fit, median)
fit = fit_pcei(tbls, n_comp=2L, id=id, algorithm="hmc")

library(bayesplot)
hist(unlist(lapply(fit, posterior::rhat)))
mcmc_intervals(as_draws_matrix(fit$support))
mcmc_intervals(as_draws_matrix(fit$alpha))
mcmc_intervals(as_draws_matrix(fit$loading[, 1]))
mcmc_intervals(as_draws_matrix(fit$loading[, 2]))
median(fit$L_t %**% t(fit$L_t))
median(fit$L_p %**% t(fit$L_p))
mcmc_trace(as_draws_matrix(fit$loading[1:2, ]))
mcmc_trace(as_draws_matrix(fit$support[1:4]))
mcmc_trace(as_draws_matrix(fit$turnout_overall))
mcmc_trace(as_draws_matrix(fit$scale))
mcmc_pairs(as_draws_matrix(fit$turnout_overall))
mcmc_pairs(as_draws_matrix(fit$scale))
mcmc_pairs(as_draws_matrix(fit$loading[1:2, ]))
mcmc_pairs(as_draws_matrix(fit$pref_z[1:2, , 1]))

median(fit$loading) %>%
    as.data.frame() %>%
    rownames_to_column("cand") %>%
ggplot(aes(V1, V2, label=cand)) +
    geom_text()

plot(map, median(fit$pref_geo[,2,"black"]))
plot(map, 100*median(fit$turnout[, "black"]))
plot(map, rowSums(median(fit$pref_geo)[,1,] * tbls$race))
plot(map, rowSums(median(fit$pref_geo)[,2,] * tbls$race))


est_total = function(race) {
    vap_race = m_race[, paste0("vap_", race)]
    pref_race = fit$pref_geo[, , race, drop=T] * fit$turnout[, race] * vap_race
    cand_load = fit$loading %**% (rev(fit$scale) * t(pref_race))
    #vs_est = median(t(draws$support * (1 + alpha * cand_load)) * m_race[, paste0("vap_", race)])
    #return(vs_est)
    pr_mayor = (fit$support * (1 + alpha * cand_load))[1:6,] %**%
        (m_prec[, 1] * vap_race)
    pr_pres = (fit$support * (1 + alpha * cand_load))[7:11,] %**%
        (m_prec[, 2] * vap_race)
    #out = rbind(pr_mayor / rvar_sum(pr_mayor), pr_pres / rvar_sum(pr_pres))
    out = rbind(pr_mayor, pr_pres)
    colnames(out) = race
    out
}

est_joint = cbind(
    est_total("white"),
    est_total("black"),
    est_total("hisp"),
    est_total("other")
)

t(t(est_joint[1:6,]) / do.call(c, apply(est_joint[1:6, ], 2, rvar_sum)))
t(t(est_joint[7:11,]) / do.call(c, apply(est_joint[7:11, ], 2, rvar_sum)))




