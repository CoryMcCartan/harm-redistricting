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
    select(-stage)
d_abstain = d_votes %>%
    group_by(precinct, elec) %>%
    summarize(candidate="<abstain/other>",
              votes = vap[1] - sum(votes),
              .groups="drop")
d_votes = bind_rows(d_votes, d_abstain) %>%
    select(-vap) %>%
    arrange(precinct, elec, candidate) %>%
    mutate(precinct = as.factor(precinct),
           elec = as.factor(elec),
           candidate = as.factor(candidate)) %>%
    arrange(elec, precinct)
# indices of presidential candidates
pres = which(levels(d_votes$candidate) %in% unique(filter(d_votes, elec=="pres")$candidate)[-1]) - 1L

d_race = d %>%
    as_tibble() %>%
    mutate(vap_other = vap - vap_white - vap_black - vap_hisp,
           across(starts_with("vap_"), ~ . / vap)) %>%
    select(vap_white:vap_hisp, vap_other)
    #select(vap_white, vap_other)

d_race = matrix(1, nrow=nrow(d))

m_votes = d_votes %>%
    #filter(candidate != "<abstain/other>") %>%
    arrange(precinct, elec, candidate) %>%
    group_by(precinct, elec) %>%
    mutate(vs = votes / sum(1e-6 + votes)) %>%
    ungroup() %>%
    with(., matrix(vs, nrow=nlevels(precinct), byrow=T))
cands = d_votes %>%
    #filter(candidate != "<abstain/other>", precinct == "1-1") %>%
    filter(precinct == "1-1") %>%
    arrange(elec, candidate) %>%
    pull(candidate) %>%
    as.character()

if (F) {
stan_d = compose_data(
    N = nrow(d),
    R = ncol(d_race),
    K = nlevels(d_votes$candidate),
    L = nlevels(d_votes$elec),
    M = N*L,
    obs_votes = nrow(d_votes),

    vap_race = as.matrix(d_race),

    n_cand = count(distinct(d_votes, elec, candidate), elec)$n,
    cand = as.integer(d_votes$candidate) - 1L,
    votes = as.integer(d_votes$votes),

    idx_left = which(levels(d_votes$candidate) == "sanders") - 1L,
    prior_race_scale = 0.25,
    prior_geo_scale = 2.0
)
}

stan_d = compose_data(
    N = nrow(d),
    R = ncol(d_race),
    K = ncol(m_votes),
    L = nlevels(d_votes$elec),

    vap = d$vap,
    #vap_race = as.matrix(d_race),
    vap_race = as.matrix(d_race),
    vote_share = m_votes,

    idx_cand = cumsum(c(1, count(distinct(d_votes, elec, candidate), elec)$n)),

    prior_only = FALSE,
    prior_pca = prcomp(m_votes, center=T, scale.=F, rank.=2)$rotation[, 2],
)
stan_d$idx_cand=c(1,9,24)

sm = cmdstan_model(here("R/pcei_mvn.stan"), compile=F)

{
file.remove(here("R/pcei_mvn"))
sm$compile()
}

fit = sm$sample(data=stan_d, chains=2, iter_warmup=300, iter_sampling=400)

fit = sm$optimize(data=stan_d)
fit = sm$variational(data=stan_d, algorithm="meanfield", eta=0.2, adapt_engaged=F)
fit = sm$variational(data=stan_d, algorithm="fullrank")

if (F) {
draws = as_draws_rvars(fit$draws())
names(draws$loading) = levels(d_votes$candidate)[-1]
names(draws$support) = levels(d_votes$candidate)[-1]
#names(draws$pref_race) = str_sub(colnames(d_race), 5)
#colnames(draws$pref) = str_sub(colnames(d_race), 5)
colnames(draws$turnout) = str_sub(colnames(d_race), 5)
rownames(draws$turnout) = levels(d_votes$elec)
#names(draws$elec_effect) = levels(d_votes$elec)
}

{
draws = as_draws_rvars(fit$draws())
names(draws$loading) = arrange(d_votes, precinct, elec, candidate)$candidate[1:25]
names(draws$support) = names(draws$loading)
names(draws$pref) = str_sub(colnames(d_race), 5)
}

mcmc_intervals(as_draws_matrix(draws$support[1:9]))
mcmc_intervals(as_draws_matrix(draws$support[10:25]))
mcmc_intervals(as_draws_matrix(draws$loading[1:9]))
mcmc_intervals(as_draws_matrix(draws$loading[10:25]))

mcmc_intervals(as_draws_matrix(draws$support[pres]))
mcmc_intervals(as_draws_matrix(draws$support[-pres]))
mcmc_intervals(as_draws_matrix(draws$loading[pres]))
mcmc_intervals(as_draws_matrix(draws$loading[-pres]))
mcmc_pairs(as_draws_matrix(draws$pref_race))
mcmc_pairs(as_draws_matrix(draws$turnout))


d_votes %>%
    filter(candidate != "<abstain/other>") %>%
    group_by(elec, candidate) %>%
    summarize(votes = sum(votes)) %>%
    group_by(elec) %>%
    mutate(vs = round(votes / sum(votes), 3)) %>%
    arrange(elec, desc(vs)) %>%
    as.data.frame()

vs_mayor = rowwise(d) %>%
    mutate(x = mayor_gen_wu / sum(c_across(starts_with("mayor_gen")))) %>%
    pull()
vs_pres = rowwise(d) %>%
    mutate(x = (pres_prelim_sanders + pres_prelim_warren) / sum(c_across(starts_with("pres_prelim")))) %>%
    pull()

plot(map, mean(draws$pref))
plot(map, mean(draws$pref_geo + draws$pref_race["white"])) +
    wacolors::scale_fill_wa_c(limits=c(-3, 3))

plot(map, 2*vs_mayor)
plot(map, 2*vs_pres)

qplot(mean(draws$pref_geo), vs_pres, size=map$vap)


d_votes %>%
    filter(candidate != "<abstain/other>") %>%
    group_by(precinct, elec) %>%
    mutate(vs = votes / sum(0.0001 + votes)) %>%
ggplot(aes(sample=qlogis(vs))) +
    facet_wrap(~ candidate) +
    geom_qq_line(color="red") +
    geom_qq(size=0.1)





x=c(8.23257e-05,-7.14394e-06,-4.62546e-07,-0,-2.54027e-05,-0.000331302,-2.74118e-05,-0,-4.0149e-07,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
-7.14394e-06,8.82676e-06,-1.03533e-08,-0,-5.68596e-07,-7.41564e-06,-6.13566e-07,-0,-8.98666e-09,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
-4.62546e-07,-1.03533e-08,5.81185e-07,-0,-3.68146e-08,-4.80137e-07,-3.97263e-08,-0,-5.81855e-10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
-0,-0,-0,0,-0,-0,-0,-0,-0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
-2.54027e-05,-5.68596e-07,-3.68146e-08,-0,2.99333e-05,-2.63688e-05,-2.18174e-06,-0,-3.19551e-08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
-0.000331302,-7.41564e-06,-4.80137e-07,-0,-2.63688e-05,7.28567e-05,-2.84543e-05,-0,-4.16759e-07,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-2.74118e-05,-6.13566e-07,-3.97263e-08,-0,-2.18174e-06,-2.84543e-05,3.21281e-05,-0,-3.44824e-08,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0,-0,-0,-0,-0,-0,-0,0,-0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-4.0149e-07,-8.98666e-09,-5.81855e-10,-0,-3.19551e-08,-4.16759e-07,-3.44824e-08,-0,5.04545e-07,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.69371e-05,-3.09742e-05,-3.06141e-06,-2.12077e-08,-0,-3.30581e-06,-2.32454e-05,-8.78419e-07,-0,-0,-3.74366e-06,-3.23582e-05,-5.78586e-07,-4.3397e-09,-0,-1.97617e-06,0,0,0,0,0,0,0,0,0,-3.09742e-05,8.89126e-05,-2.9746e-05,-2.06063e-07,-0,-3.21207e-05,-0.000225862,-8.53508e-06,-0,-0,-3.6375e-05,-0.000314406,-5.62178e-06,-4.21664e-08,-0,-1.92013e-05,0,0,0,0,0,0,0,0,0,-3.06141e-06,-2.9746e-05,3.55938e-05,-2.03667e-08,-0,-3.17473e-06,-2.23237e-05,-8.43587e-07,-0,-0,-3.59522e-06,-3.10751e-05,-5.55643e-07,-4.16763e-09,-0,-1.89781e-06,0,0,0,0,0,0,0,0,0,-2.12077e-08,-2.06063e-07,-2.03667e-08,2.66799e-07,-0,-2.19927e-08,-1.54645e-07,-5.84388e-09,-0,-0,-2.49056e-08,-2.1527e-07,-3.84917e-09,-2.88709e-11,-0,-1.31469e-08,0,0,0,0,0,0,0,0,0,-0,-0,-0,-0,0,-0,-0,-0,-0,-0,-0,-0,-0,-0,-0,-0,0,0,0,0,0,0,0,0,0,-3.30581e-06,-3.21207e-05,-3.17473e-06,-2.19927e-08,-0,3.8182e-05,-2.41058e-05,-9.10933e-07,-0,-0,-3.88224e-06,-3.35559e-05,-6.00002e-07,-4.50034e-09,-0,-2.04932e-06,0,0,0,0,0,0,0,0,0,-2.32454e-05,-0.000225862,-2.23237e-05,-1.54645e-07,-0,-2.41058e-05,0.000123085,-6.40538e-06,-0,-0,-2.72986e-05,-0.000235954,-4.21902e-06,-3.16449e-08,-0,-1.44102e-05,0,0,0,0,0,0,0,0,0,-8.78419e-07,-8.53508e-06,-8.43587e-07,-5.84388e-09,-0,-9.10933e-07,-6.40538e-06,1.08146e-05,-0,-0,-1.03159e-06,-8.91646e-06,-1.59432e-07,-1.19583e-09,-0,-5.44544e-07,0,0,0,0,0,0,0,0,0,-0,-0,-0,-0,-0,-0,-0,-0,0,-0,-0,-0,-0,-0,-0,-0,0,0,0,0,0,0,0,0,0,-0,-0,-0,-0,-0,-0,-0,-0,-0,0,-0,-0,-0,-0,-0,-0,0,0,0,0,0,0,0,0,0,-3.74366e-06,-3.6375e-05,-3.59522e-06,-2.49056e-08,-0,-3.88224e-06,-2.72986e-05,-1.03159e-06,-0,-0,4.27249e-05,-3.80003e-05,-6.79471e-07,-5.0964e-09,-0,-2.32075e-06,0,0,0,0,0,0,0,0,0,-3.23582e-05,-0.000314406,-3.10751e-05,-2.1527e-07,-0,-3.35559e-05,-0.000235954,-8.91646e-06,-0,-0,-3.80003e-05,7.88368e-05,-5.87298e-06,-4.40505e-08,-0,-2.00593e-05,0,0,0,0,0,0,0,0,0,-5.78586e-07,-5.62178e-06,-5.55643e-07,-3.84917e-09,-0,-6.00002e-07,-4.21902e-06,-1.59432e-07,-0,-0,-6.79471e-07,-5.87298e-06,7.17762e-06,-7.87652e-10,-0,-3.58674e-07,0,0,0,0,0,0,0,0,0,-4.3397e-09,-4.21664e-08,-4.16763e-09,-2.88709e-11,-0,-4.50034e-09,-3.16449e-08,-1.19583e-09,-0,-0,-5.0964e-09,-4.40505e-08,-7.87652e-10,5.46178e-08,-0,-2.69025e-09,0,0,0,0,0,0,0,0,0,-0,-0,-0,-0,-0,-0,-0,-0,-0,-0,-0,-0,-0,-0,0,-0,0,0,0,0,0,0,0,0,0,-1.97617e-06,-1.92013e-05,-1.89781e-06,-1.31469e-08,-0,-2.04932e-06,-1.44102e-05,-5.44544e-07,-0,-0,-2.32075e-06,-2.00593e-05,-3.58674e-07,-2.69025e-09,-0,2.36489e-05)
