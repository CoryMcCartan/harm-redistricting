if (!requireNamespace("alarmdata", quietly=TRUE)) {
    remotes::install_github("alarm-redist/alarmdata")
}
library(alarmdata)
library(kableExtra)

states = c("IA", "NC", "WI", "MA", "UT", "NJ", "MS", "CA")

ker_t = k_t(sd=with(elec_model_spec, sqrt(year^2 + resid^2)))
corr_state = function(abbr) {
    st_map = alarm_50state_map(abbr)
    dvote = st_map$ndv
    rvote = st_map$nrv
    statewide = sum(dvote) / sum(dvote + rvote)
    nd = attr(st_map, "ndists")

    st_plans = alarm_50state_plans(abbr, stats=TRUE)
    attr(st_plans, "ndists") = nd
    cat("Downloaded", abbr, "\n")

    idx_2 = sample(seq_len(ncol(as.matrix(st_plans)))[-1], 250)
    hh = partisan_harm(st_plans, ndshare, st_map$ndv, st_map$nrv, elec_model_spec, idx_2=idx_2)

    pl_corr = st_plans %>%
        subset_sampled() %>%
        mutate(decl = part_decl(., st_map, dvote, rvote)) %>%
        group_by(draw) %>%
        summarize(e_dem = sum(ker_t(ndshare)),
                  pbias = pbias[1],
                  egap = egap[1],
                  decl = decl[1],
                  mean_med = median(ndshare) - mean(ndshare)) %>%
        mutate(h_dem = hh[1, -1],
               h_rep = hh[2, -1],
               dh = h_dem - h_rep,
               h = hh[3, -1]) %>%
        as_tibble() %>%
        select(dh, e_dem, egap, pbias, mean_med, decl) %>%
        as.matrix() %>%
        cor(use="pairwise.complete.obs")

    tibble(state=censable::match_name(abbr),
           statewide=statewide,
           ndists=nd,
           stat=c("Exp. Dem. seats", "Efficiency gap", "Partisan bias",
                  "Mean-median", "Declination"),
           corr_dh=pl_corr[-1, "dh"])

}

res = map_dfr(states, corr_state)

# make table
tbl_body = res %>%
    # mutate(state = str_glue("{state} ({round(100*statewide)}% D, {ndists} districts)")) %>%
    arrange(statewide) %>%
    select(-statewide, -ndists) %>%
    pivot_wider(names_from=stat, values_from=corr_dh) %>%
    rename(State=state)

col_scale = scales::col_numeric(rev(wacolors$vantage)[3:13], c(-1, 1))
{
    tbl = kbl(tbl_body, booktabs=TRUE, digits=3, linesep="", format="latex") %>%
        add_header_above(c(" ", "Correlation with differential harm and:"=5))# %>%
        # column_spec(1, width="2.25in")
    for (i in 2:6) {
        tbl = column_spec(tbl, i, background=col_scale(tbl_body[[i]]))
    }
    write_lines(tbl, here("paper/figures/corr_table.tex"))
}


distinct(res, state, ndists, statewide) %>%
    arrange(statewide)
