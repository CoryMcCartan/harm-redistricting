if (!exists("fit_ei")) source(here("R/06_fit_alabama.R"))

if (!file.exists(path <- here("data/AL_cd_final_vtd_20.rds"))) {
    al_shp = geomander::get_alarm("AL", geometry=TRUE, epsg=2759)
    al_shp$geometry = rmapshaper::ms_simplify(al_shp$geometry, keep=0.03, keep_shapes=TRUE)
    al_cd = read_sf(here("data-raw/al_2020_congress_2021-11-04_2031-06-30/al_2020_congress_2021-11-04_2031-06-30.shp")) %>%
        st_transform(4269)
    al_shp$cd_2020 = geomander::geo_match(al_shp, al_cd, method="area", epsg=2759)
    al_shp = relocate(al_shp, cd_2020, .after=vtd)
    al_map = redist_map(al_shp, existing_plan=cd_2020, pop_tol=0.005)

    write_rds(al_map, path, compress="xz")
} else {
    al_map = read_rds(path)
}

d_votes = make_votes_long(al_map)
tbls = make_tables(d_votes, al_map)

fit = fit_ei(tbls, algorithm="hmc", init=0, chains=4, adapt_delta=0.99)
fit = fit_ei(tbls, algorithm="vb", init=0, eta=0.1, adapt_engaged=F, tol_rel_obj=0.005)

median(fit$L_t %**% t(fit$L_t))
median(fit$L_s %**% t(fit$L_s))

p1 = plot(al_map, median(fit$turnout)[, "white"]) + scale_fill_wa_c(name="Turnout", labels=percent, limits=c(0, 1)) + labs(title="White turnout")
p2 = plot(al_map, median(fit$turnout)[, "black"]) + scale_fill_wa_c(name="Turnout", labels=percent, limits=c(0, 1)) + labs(title="Black turnout")
p3 = plot(al_map, median(fit$support)[, "white"]) + scale_fill_party_c() + labs(title="White Dem. support")
p4 = plot(al_map, median(fit$support)[, "black"]) + scale_fill_party_c() + labs(title="Black Dem. support")
p1 + p2 + p3 + p4 + plot_layout(nrow=1, guides="collect") & theme_repr_map() & theme(legend.position="bottom")


est_ndv = median(fit$support * fit$turnout) * with(al_map, cbind(vap_white, vap_black, vap_other))
colnames(est_ndv) = paste0("ndv_", colnames(est_ndv))
est_nrv = median((1 - fit$support) * fit$turnout) * with(al_map, cbind(vap_white, vap_black, vap_other))
colnames(est_nrv) = paste0("nrv_", colnames(est_nrv))

al_map = select(al_map, GEOID20: adj) %>%
    bind_cols(est_ndv, est_nrv) %>%
    st_as_sf() %>%
    as_redist_map()

constr = redist_constr(al_map) %>%
    add_constr_grp_hinge(10.0, group_pop=vap_black, total_pop=vap,
                         tgts_group=c(0.53, 0.3, 0.15, 0.07))
plans = redist_smc(al_map, 2000, counties=county, constraints=constr, runs=3, ncores=3)

m = mgcv::gam(I(vap_black/vap) ~ s(I(ndv/(ndv+nrv))), data=al_map)
plot(al_map, as.numeric(resid(m))) + scale_fill_wa_c("lopez", midpoint=0.0)

plans = plans %>%
    mutate(black = group_frac(al_map, vap_black, vap),
           dem = group_frac(al_map, ndv, ndv + nrv),
           resid = group_frac(al_map, resid(m)*vap, vap),
           comp = distr_compactness(al_map),
           splits = county_splits(al_map, county))

summary(number_by(plans, black, desc=T))
plot(plans, dem, geom="boxplot")
plot(plans, black, geom="boxplot")
plot(plans, resid, geom="boxplot")
plot(number_by(plans, dem), resid, geom="boxplot", sort=F)
plot(number_by(plans, black), resid, geom="boxplot", sort=F)
