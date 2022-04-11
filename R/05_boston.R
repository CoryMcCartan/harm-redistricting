if (!exists("fit_pcei")) source(here("R/04_fit_boston.R"))

d = read_rds(here("data/boston_elec.rds")) %>%
    mutate(ward = as.integer(str_split(precinct, "-", n=2, simplify=T)[,1]))
map = redist_map(d, existing_plan=ccd_2010, adj=d$adj)

d_votes = make_votes_long(d)
tbls = make_tables(d_votes, cand_thresh=0.02)

if (!file.exists(fit_path <- here("data-raw/boston_fit_2.rds"))) {
    id = list(
        loc = list(sanders=c(-1, 0), biden=c(1, 0), janey=c(-0.25, -1)),
        scale = list(sanders=c(0.1, 0.25), biden=c(1, 0.5), janey=c(0.5, 2)),
        corr = list(
            list("sanders", "biden", c(-0.5, 0.8))
        )
    )

    fit = fit_pcei(tbls, n_comp=2L, id=id, algorithm="hmc", step_size=0.03)
    save_fit(fit, fit_path)
} else {
    fit = read_rds(fit_path)
}


library(bayesplot)
rhats = unlist(lapply(fit, posterior::rhat))
qplot(rhats, bins=50)
mcmc_intervals(as_draws_matrix(fit$support))
mcmc_intervals(as_draws_matrix(fit$alpha))
mcmc_intervals(as_draws_matrix(fit$loading[, 1]))
mcmc_intervals(as_draws_matrix(fit$loading[, 2]))
median(fit$L_t %**% t(fit$L_t))
median(fit$L_p %**% t(fit$L_p))

mcmc_trace(as_draws_matrix(fit$loading[1:3, ]))
mcmc_trace(as_draws_matrix(fit$loading[7:8, ]))
mcmc_trace(as_draws_matrix(fit$support[1:4]))
mcmc_trace(as_draws_matrix(fit$turnout_overall))
mcmc_trace(as_draws_matrix(fit$pref[103, ,]))
mcmc_trace(as_draws_matrix(fit$scale))
mcmc_trace(as_draws_matrix(fit$sigma_t))
mcmc_trace(as_draws_matrix(fit$L_p))

mcmc_pairs(as_draws_matrix(fit$turnout_overall))
mcmc_pairs(as_draws_matrix(fit$scale))
mcmc_pairs(as_draws_matrix(fit$loading[1:2, ]))
mcmc_pairs(as_draws_matrix(fit$loading[c(1,2,7,8), 2]))
mcmc_pairs(as_draws_matrix(fit$pref[1:2, 1,]))

median(fit$loading) %>%
    `rownames<-`(rownames(fit$loading)) %>%
    as.data.frame() %>%
    rownames_to_column("cand") %>%
ggplot(aes(V1, V2, label=cand)) +
    geom_text()

plot(map, median(fit$pref[,1,"white"])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, median(fit$pref[,1,"black"])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, median(fit$pref[,2,"white"])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, median(fit$pref[,2,"black"])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, 100*median(fit$turnout[, "white"]))
plot(map, 100*median(fit$turnout[, "black"]))
plot(map, median(fit$post_factor[,1])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, median(fit$post_factor[,2])) + scale_fill_wa_c("stuart", midpoint=0)

map_dbl(tbls$cands, function(cnd) {
    cor(tbls$vote_share[, cnd], mean(fit$post_share_prec[, cnd]), method="spearman")
}) %>%
    `names<-`(tbls$cands)



