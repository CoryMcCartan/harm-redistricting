library(cmdstanr)
library(posterior)
library(tidybayes)

load_538_al = function(pl) {
    url = str_glue("https://projects.fivethirtyeight.com/redistricting-2022-maps/alabama/plaintiffs_plan_{pl}-districts.json")
    x = suppressWarnings(read_sf(url)$geometry)
    # MAGIC NUMBERS determined after much painful analysis
    x = x * matrix(c(39746.27033, 3498.95882, 3477.34807, -39993.28232), nrow=2) + c(-19134951.8329, 9134711.70241)
    st_crs(x) = 5070
    st_sf(geometry=st_transform(x, 2759))
}

make_al_map = function() {
    al_shp = geomander::get_alarm("AL", geometry=TRUE, epsg=2759)
    al_shp$geometry = rmapshaper::ms_simplify(al_shp$geometry, keep=0.005, keep_shapes=TRUE)
    al_cd = read_sf(here("data-raw/al_2020_congress_2021-11-04_2031-06-30/al_2020_congress_2021-11-04_2031-06-30.shp")) %>%
        st_transform(4269)
    # add plans
    al_shp$cd_2020 = geomander::geo_match(al_shp, al_cd, method="area", epsg=2759)
    al_shp$cd_pet_a = geomander::geo_match(al_shp, load_538_al("a"), method="area")
    al_shp$cd_pet_b = geomander::geo_match(al_shp, load_538_al("b"), method="area")
    al_shp$cd_pet_c = geomander::geo_match(al_shp, load_538_al("c"), method="area")
    al_shp$cd_pet_d = geomander::geo_match(al_shp, load_538_al("d"), method="area")

    al_shp = relocate(al_shp, starts_with("cd_"), .after=vtd)
    redist_map(al_shp, existing_plan=cd_2020, pop_tol=0.005)
}


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
