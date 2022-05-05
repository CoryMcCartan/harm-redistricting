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

make_tables_alabama = function(d_votes, al_map) {
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

make_tables_boston = function(d_votes, cand_thresh=0.01) {
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
    colnames(m_prec) = levels(d_votes$elec)

    m_race = d %>%
        as_tibble() %>%
        mutate(vap_other = vap - vap_white - vap_black - vap_hisp,
               across(starts_with("vap_"), ~ . / vap)) %>%
        select(vap_white:vap_hisp, vap_other) %>%
        as.matrix()
    colnames(m_race) = str_sub(colnames(m_race), 5)

    big_cands = which(d_sum$vs > cand_thresh)

    list(vap = d$vap,
         votes = m_prec,
         vote_share = m_votes[, big_cands],
         race = m_race,
         summary = d_sum[big_cands, ],
         cands = colnames(m_votes)[big_cands],
         elecs = d_sum$elec[big_cands])
}


make_tables_sf = function(d_votes, cand_thresh=0.02) {
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
    colnames(m_prec) = levels(d_votes$elec)

    m_race = cbind(d$rent_frac, 1 - d$rent_frac)
    colnames(m_race) = c("rent", "own")

    big_cands = which(d_sum$vs > cand_thresh)

    list(vap = pmax(d$vap, apply(m_prec, 1, max)),
         votes = m_prec,
         vote_share = m_votes[, big_cands],
         race = m_race,
         summary = d_sum[big_cands, ],
         cands = colnames(m_votes)[big_cands],
         elecs = d_sum$elec[big_cands])
}
