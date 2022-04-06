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
    select(-stage) %>%
    arrange(precinct, elec, candidate) %>%
    mutate(elec = fct_inorder(elec),
           candidate = fct_inorder(candidate))

m_votes = d_votes %>%
    group_by(precinct, elec) %>%
    mutate(vs = votes / sum(1e-6 + votes)) %>%
    ungroup() %>%
    with(., matrix(vs, nrow=n_distinct(precinct), byrow=T))
d_sum = d_votes %>%
    group_by(elec, candidate) %>%
    summarize(votes = sum(votes)) %>%
    group_by(elec) %>%
    mutate(vs = round(votes / sum(votes), 3)) %>%
    ungroup() %>%
    arrange(elec, candidate) %>%
    mutate(elec = as.factor(elec))
m_prec = d_votes %>%
    group_by(precinct, elec) %>%
    summarize(votes = sum(votes), .groups="drop") %>%
    arrange(precinct, elec) %>%
    with(., matrix(votes, nrow=n_distinct(precinct), byrow=T))

big_cands = which(d_sum$vs > 0.01)

stan_d = compose_data(
    N = nrow(d),
    K = length(big_cands),
    L = nlevels(d_votes$elec),

    votes = m_prec,
    vote_share = m_votes[, big_cands],
    elec = as.integer(d_sum$elec[big_cands]),

    Q = 2L
)

sm = cmdstan_model(here("R/pca.stan"), compile=F)

{
file.remove(here("R/pca"))
sm$compile()
}

fit = sm$sample(data=stan_d, chains=2, iter_warmup=400, iter_sampling=400)

fit = sm$optimize(data=stan_d)
fit = sm$variational(data=stan_d, algorithm="meanfield", eta=0.5, adapt_engaged=F)
fit = sm$variational(data=stan_d, algorithm="fullrank")

{
draws = as_draws_rvars(fit$draws())
rownames(draws$loading) = levels(d_votes$candidate)[big_cands]
names(draws$support) = levels(d_votes$candidate)[big_cands]
}

mcmc_intervals(as_draws_matrix(draws$support))
mcmc_intervals(as_draws_matrix(draws$loading[, 2]))
mcmc_pairs(as_draws_matrix(draws$scale))
#mcmc_trace(as_draws_matrix(draws$support[1:3]))

median(draws$loading) %>%
    as.data.frame() %>%
    rownames_to_column("cand") %>%
ggplot(aes(V1, V2, label=cand)) +
    geom_text()

vs_mayor = rowwise(d) %>%
    mutate(x = mayor_gen_wu / sum(c_across(starts_with("mayor_gen")))) %>%
    pull()
vs_pres = rowwise(d) %>%
    mutate(x = (pres_prelim_sanders + pres_prelim_warren) / sum(c_across(starts_with("pres_prelim")))) %>%
    pull()

plot(map, mean(draws$pref_geo[,2]))

plot(map, 2*vs_mayor)
plot(map, 2*vs_pres)

qplot(median(draws$pref_geo[,2]), vs_pres, size=map$vap)
#qplot(median(draws$pref_geo[,1]), map$med_inc_total, size=map$vap)


d_votes %>%
    group_by(precinct, elec) %>%
    mutate(vs = votes / sum(0.0001 + votes)) %>%
ggplot(aes(sample=qlogis(vs))) +
    facet_wrap(~ candidate) +
    geom_qq_line(color="red") +
    geom_qq(size=0.1)


pca = prcomp(m_votes, center=T, scale.=F, rank.=3)
