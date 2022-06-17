library(alarmdata)
library(lme4)

d_house = read_csv(here("data-raw/1976-2020-house.csv"), show_col_types=FALSE) |>
    group_by(year, state, district) |>
    filter(sum(str_starts(party, "DEMOCRAT")) == 1,
           sum(str_starts(party, "REPUBLICAN")) == 1) |>
    ungroup() |>
    filter(str_starts(party, "(DEMOCRAT|REPUBLICAN)")) |>
    transmute(year = year,
              state = state_po,
              district = str_c(state, "-", district, "-", floor(year/10)*10),
              party = str_to_lower(str_sub(party, 1, 3)),
              votes = candidatevotes) |>
    pivot_wider(names_from=party, values_from=votes) |>
    mutate(dshare = dem / (dem + rep))

m_distr = lmer(dshare ~ (1 | district) + (1 | year), data=d_house)

ranef_distr = ranef(m_distr)$district |>
    rownames_to_column("dist") |>
    as_tibble() |>
    rename(eff_dist = `(Intercept)`)
ranef_year = ranef(m_distr)$year |>
    rownames_to_column("year") |>
    as_tibble() |>
    rename(eff_year = `(Intercept)`)


nj = alarm_50state_map("NJ")

d = as_tibble(nj) |>
    transmute(prec = GEOID,
              dist = cd_2010,
              turn_16 = pre_16_dem_cli + pre_16_rep_tru,
              turn_18 = uss_18_dem_men + uss_18_rep_hug,
              dshare_16 = pre_16_dem_cli / turn_16,
              dshare_18 = uss_18_dem_men / turn_18) |>
    pivot_longer(-prec:-dist, names_to=c("var", "year"), names_sep="_") |>
    pivot_wider(names_from=var) |>
    drop_na() |>
    mutate(dvote = as.integer(round(dshare * turn)),
           year = str_c("20", year),
           dist = str_c("NJ-", str_pad(2, 3, pad="0"), "-2010")) |>
    left_join(ranef_distr, by="dist") |>
    left_join(ranef_year, by="year") |>
    mutate(dshare_adj = dshare - eff_dist - eff_year)


m_prec = lmer(dshare ~ (1 | prec) + (1|dist:year), data=d)
