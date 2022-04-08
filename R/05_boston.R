library(cmdstanr)
library(tidybayes)
library(posterior)
library(bayesplot)

d = read_rds(here("data/boston_elec.rds"))
map = redist_map(d, existing_plan=ccd_2010, adj=d$adj)

d_votes = d %>%
    as_tibble() %>%
    mutate(vap_other = vap - vap_white - vap_black - vap_hisp) %>%
    select(precinct, vap, mayor_gen_wu:pres_prelim_castro) %>%
    pivot_longer(mayor_gen_wu:pres_prelim_castro, names_to="cand", values_to="votes") %>%
    separate(cand, c("elec", "stage", "candidate"), sep="_", extra="merge") %>%
    filter(stage == "prelim", candidate != "other") %>%
    #filter(stage == "prelim") %>%
    #mutate(elec = paste0(elec, "_", stage)) %>%
    select(-stage) %>%
    #arrange(precinct, elec, candidate) %>%
    mutate(precinct = fct_inorder(precinct),
           elec = fct_inorder(elec),
           candidate = fct_inorder(candidate))

vs_mayor = rowwise(d) %>%
    mutate(x = mayor_gen_wu / sum(c_across(starts_with("mayor_gen")))) %>%
    pull()
vs_pres = rowwise(d) %>%
    mutate(x = (pres_prelim_sanders + pres_prelim_warren) / sum(c_across(starts_with("pres_prelim")))) %>%
    pull()

#d_abstain = d_votes %>%
#    group_by(precinct, elec) %>%
#    summarize(candidate = as.character(str_glue("<abstain {elec[1]}>")),
#              vap = vap[1],
#              votes = vap - sum(votes),
#              .groups="drop")
#d_votes = bind_rows(d_votes, d_abstain) %>%
#    arrange(precinct, elec) %>%
#    mutate(candidate = fct_inorder(candidate))


m_votes = d_votes %>%
    arrange(precinct, candidate) %>%
    group_by(precinct, elec) %>%
    mutate(vs = votes / sum(1e-6 + votes)) %>%
    ungroup() %>%
    with(., matrix(vs, nrow=n_distinct(precinct), byrow=T)) %>%
    `colnames<-`(levels(d_votes$candidate))
d_sum = d_votes %>%
    group_by(elec, candidate) %>%
    summarize(votes = sum(votes)) %>%
    group_by(elec) %>%
    mutate(vs = round(votes / sum(votes), 3)) %>%
    ungroup() %>%
    mutate(elec = as.factor(elec))
m_prec = d_votes %>%
    group_by(precinct, elec) %>%
    summarize(votes = sum(votes), .groups="drop") %>%
    with(., matrix(votes, nrow=n_distinct(precinct), byrow=T))

m_race = d %>%
    as_tibble() %>%
    mutate(vap_other = vap - vap_white - vap_black - vap_hisp,
           across(starts_with("vap_"), ~ . / vap)) %>%
    select(vap_white:vap_hisp, vap_other) %>%
    as.matrix()

big_cands = which(d_sum$vs > 0.01)
pca = prcomp(m_votes[, big_cands], center=T, scale.=T, rank.=3)

stan_d = compose_data(
    N = nrow(d),
    K = length(big_cands),
    L = nlevels(d_votes$elec),
    R = ncol(m_race),

    vap = map$vap,
    votes = m_prec,
    vote_share = m_votes[, big_cands],
    vap_race = m_race,
    elec = as.integer(d_sum$elec[big_cands]),

    idx_left = c(
        which(d_sum$candidate[big_cands] == "wu"),
        which(d_sum$candidate[big_cands] == "essaibi_george")
    ),
    Q = 3L
)

sm = cmdstan_model(here("R/pca.stan"), compile=F)

{
file.remove(here("R/pca"))
sm$compile()
}

init_l = lapply(as_draws_rvars(fit$draws()), mean)
fit_hmc = sm$sample(data=stan_d, chains=4, iter_warmup=400, iter_sampling=300,
                    init=rep(list(init_l), 4))

fit = sm$variational(data=stan_d, algorithm="meanfield", eta=0.2, adapt_engaged=F, init=0)

{
draws = as_draws_rvars(fit$draws())
names(draws$support) = levels(d_votes$candidate)[big_cands]
rownames(draws$loading) = names(draws$support)
names(draws$alpha) = names(draws$support)[-1]
names(draws$turnout_overall) = str_sub(colnames(m_race), 5)
colnames(draws$turnout) = names(draws$turnout_overall)
dimnames(draws$pref_geo) = list(NULL, NULL, names(draws$turnout_overall))
}

mcmc_intervals(as_draws_matrix(draws$support))
mcmc_intervals(as_draws_matrix(draws$alpha))
mcmc_intervals(as_draws_matrix(draws$loading[, 3]))
draws$scale/tail(draws$scale, 1)
mcmc_trace(as_draws_matrix(draws$support[1:2]))
mcmc_pairs(as_draws_matrix(draws$support[1:2]))

median(draws$loading) %>%
    as.data.frame() %>%
    rownames_to_column("cand") %>%
ggplot(aes(V1, V2, label=cand)) +
    geom_text()


plot(map, median(draws$pref_geo[,2,"white"]))
plot(map, 100*median(draws$turnout[,"other"]))
plot(map, pca$x[,3])
qplot(median(draws$pref_geo[,2,"white"]), vs_pres, size=map$vap)
qplot(median(draws$pref_geo[,2]), pca$x[,1], size=map$vap)

plot(map, rowSums(median(draws$pref_geo)[,1,] * m_race))


est_total = function(race) {
    cand_load = draws$loading %**% (rev(draws$scale) * t(draws$pref_geo[,, race, drop=T]))
    alpha = c(rvar(array(rep(1, 1200), dim=c(300,4,1)), with_chains=TRUE), draws$alpha)
    names(alpha) = rownames(cand_load)
    #vs_est = median(t(draws$support * (1 + alpha * cand_load)) * m_race[, paste0("vap_", race)])
    #return(vs_est)
    pr_mayor = (draws$support * (1 + alpha * cand_load))[1:6,] %**%
        (m_prec[, 1] * m_race[, paste0("vap_", race)])
    pr_pres = (draws$support * (1 + alpha * cand_load))[7:11,] %**%
        (m_prec[, 2] * m_race[, paste0("vap_", race)])
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




