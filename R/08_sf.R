if (!exists("fit_lfei")) source(here("R/05_fit_ei.R"))

# Load and fit -------------

d = read_rds(here("data/sf_elec.rds"))
map = redist_map(d, existing_plan=sup_2020, pop_tol=0.05, adj=d$adj)

d_votes =  d %>%
    as_tibble() %>%
    select(precinct, atty_loftus:p21_no) %>%
    pivot_longer(atty_loftus:p21_no, names_to="cand", values_to="votes") %>%
    separate(cand, c("elec", "candidate"), sep="_", extra="merge") %>%
    mutate(precinct = fct_inorder(precinct),
           elec = fct_inorder(elec),
           candidate = if_else(elec %in% c("p19", "p21"),
                               str_c(elec, "_", candidate), candidate),
           candidate = fct_inorder(candidate))
tbls = make_tables_sf(d_votes, cand_thresh=0.02)

if (!file.exists(fit_path <- here("data-raw/sf_fit.rds"))) {
    id = list(
        loc = list(p21_yes=c(-1, 0), p21_no=c(1, 0), biden=c(1, 0.5), buttigieg=c(0, 1)),
        scale = list(p21_yes=c(0.5, 0.25), p21_no=c(0.5, 0.5), biden=c(1, 1), buttigieg=c(1, 0.5))
    )
    # fit = fit_lfei(tbls, n_comp=2L, id=id, algorithm="vb", eta=0.5, adapt_engaged=F, tol_rel_obj=0.005)

    fit = fit_lfei(tbls, n_comp=2L, id=id, algorithm="hmc", step_size=0.1,
                   inv_metric=read_rds(here("data-raw/sf_metric.rds")),
                   warmup=500, iter=1000, thin=2)
    save_fit(fit, fit_path)
} else {
    fit = read_rds(fit_path)
}



# VALIDATION ---------------

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
mcmc_trace(as_draws_matrix(fit$pref[1:2, 1,]))
mcmc_trace(as_draws_matrix(fit$scale))
mcmc_trace(as_draws_matrix(fit$sigma_p))
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
    # ggplot(aes(1, V1, label=cand)) +
    geom_text()

sc_fill = scale_fill_wa_c("stuart", midpoint=0, name="Factor value", limits=c(-2, 2), oob=squish)
p1 = plot(map, median(fit$pref[,1,"rent"])) + sc_fill + labs(title="1st Dimension: Renters")
p2 = plot(map, median(fit$pref[,1,"own"])) + sc_fill + labs(title="1nd Dimension: Owners")
p3 = plot(map, median(fit$pref[,2,"rent"])) + sc_fill + labs(title="2nd Dimension: Renters")
p4 = plot(map, median(fit$pref[,2,"own"])) + sc_fill + labs(title="2nd Dimension: Owners")
p1 + p2 + p3 + p4 + plot_layout(guides="collect") & theme_repr_map()
plot(map, 100*median(fit$turnout[, "rent"]))
plot(map, 100*median(fit$turnout[, "own"]))
plot(map, median(fit$post_factor[,1])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, median(fit$post_factor[,2])) + scale_fill_wa_c("stuart", midpoint=0)

map_dbl(tbls$cands, function(cnd) {
    cor(tbls$vote_share[, cnd], mean(fit$post_share_prec[, cnd]), method="spearman")
}) %>%
    `names<-`(tbls$cands)

x = median(fit$post_share_race[1:4,])
x = median(fit$post_share_race[5:9,])
x = median(fit$post_share_race[13:16,])
round(`colnames<-`(x %*% diag(1/colSums(x)), colnames(x)), 2)



