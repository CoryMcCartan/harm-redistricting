library(cmdstanr)
library(posterior)
library(tidybayes)


fit_ei = function(tbls, recompile=FALSE, algorithm="vb",
                  chains=4, warmup=1000, iter=500, adapt_delta=0.8, init=0, ...) {
    stan_d = list(
        N = nrow(tbls$votes),
        L = ncol(tbls$votes),
        R = ncol(tbls$race),

        vap = tbls$vap,
        votes = tbls$votes,
        dem_votes = tbls$dem_votes,
        vap_race = tbls$race
    )

    path_model = here("R/ei.stan")
    path_exc = here("R/ei")

    sm = cmdstan_model(path_model, compile=F)
    if (file.exists(path_exc)) {
        if (isTRUE(recompile) || file.info(path_exc)["mtime"] < file.info(path_model)["mtime"]) {
            file.remove(path_exc)
        }
    }
    sm$compile()

    if (algorithm == "hmc") {
        fit = sm$sample(data=stan_d, chains=chains,
                        iter_warmup=warmup, iter_sampling=iter,
                        init=init, refresh=100, adapt_delta=adapt_delta,...)
    } else if (algorithm == "vb") {
        fit = sm$variational(data=stan_d, algorithm="meanfield", init=init, ...)
    } else {
        stop("Algorithm should be `hmc` or `vb`.")
    }

    vars = c("lp__", "turnout_overall", "turnout_elec", "turnout", "L_t", "sigma_t",
             "support_overall", "support_elec", "support", "L_s", "sigma_s")
    draws = as_draws_rvars(fit$draws(variables=vars))

    draws_dim = dim(draws_of(draws$lp__, with_chains=TRUE))
    races = colnames(tbls$race)

    names(draws$turnout_overall) = races
    colnames(draws$turnout) = races
    names(draws$turnout_elec) = tbls$elecs
    rownames(draws$L_t) = races
    names(draws$sigma_t) = races

    names(draws$support_overall) = races
    colnames(draws$support) = races
    names(draws$support_elec) = tbls$elecs
    rownames(draws$L_s) = races
    names(draws$sigma_s) = races

    attr(draws, "sm") = fit
    draws
}



# Data prep functions ----

make_votes_long = function(d) {
    d %>%
        as_tibble() %>%
        mutate(vap_other = vap - vap_white - vap_black) %>%
        select(GEOID20, pre_16_rep_tru:uss_20_dem_jon) %>%
        pivot_longer(pre_16_rep_tru:uss_20_dem_jon, names_to="cand", values_to="votes") %>%
        separate(cand, c("elec", "year", "party", "candidate"), sep="_", extra="merge") %>%
        mutate(elec = str_c(elec, "_", year)) %>%
        select(-candidate, -year) %>%
        mutate(GEOID20 = fct_inorder(GEOID20),
               elec = fct_inorder(elec))
}

make_tables = function(d_votes, al_map) {
    m_dem = d_votes %>%
        pivot_wider(names_from=party, values_from=votes) %>%
        arrange(elec, GEOID20) %>%
        with(., matrix(dem, nrow=n_distinct(GEOID20))) %>%
        `colnames<-`(levels(d_votes$elec))
    storage.mode(m_dem) = "integer"

    m_votes = d_votes %>%
        group_by(GEOID20, elec) %>%
        summarize(votes = sum(votes), .groups="drop") %>%
        with(., matrix(votes, nrow=n_distinct(GEOID20), byrow=T))
    colnames(m_votes) = levels(d_votes$elec)
    storage.mode(m_votes) = "integer"

    m_race = al_map %>%
        as_tibble() %>%
        mutate(vap_other = vap - vap_white - vap_black,
               across(starts_with("vap_"), ~ . / vap)) %>%
        select(vap_white, vap_black, vap_other) %>%
        as.matrix()
    colnames(m_race) = str_sub(colnames(m_race), 5)

    list(vap = as.integer(pmax(al_map$vap, apply(m_votes, 1, max))),
         votes = m_votes,
         dem_votes = m_dem,
         race = m_race,
         elecs = colnames(m_votes))
}
