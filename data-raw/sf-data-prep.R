library(tidyverse)
library(sf)
library(readxl)
library(censable)
library(geomander)
library(easycensus)
library(janitor)
library(redist)
library(here)

# Shapefiles ------
d_prec = read_sf(here("data-raw/sf_prec/SF_DOE_PREC_2019.shp")) %>%
    transmute(precinct = coalesce(PREC_2019, sprintf("U%03d", 1:n())),
              nbhd = NeighRep,
              sup_2010 = as.integer(SupDist),
              geometry = geometry)
d_bos = read_sf(here("data-raw/sf_bos/SF_BOS_FinalMap.shp")) %>%
    st_transform(st_crs(d_prec))

d_prec = d_prec %>%
    mutate(sup_2020 = coalesce(as.integer(d_bos$DISTRICT)[
        geo_match(d_prec, d_bos, method="area")], 5), # extra bit in the Bay
        .after=sup_2010)

d_cens = build_dec("block", "CA", county="San Francisco", geometry=TRUE, year=2020) %>%
    select(-NAME) %>%
    mutate(blockgroup = str_sub(GEOID, 1, 12),
           .after = GEOID)
crosswalk_block = geo_match(d_cens, d_prec, method="area")

# ACS data -----
d_acs = get_acs_table("block group", "B25008", year=2020,
                      state="CA", county="San Francisco", drop_total=TRUE) %>%
    select(GEOID, estimate, tenure) %>%
    mutate(tenure = str_c(word(tenure, 1, 1), "s")) %>%
    pivot_wider(names_from=tenure, values_from=estimate)
d_cens$renters = estimate_down(d_cens$pop, d_acs$renters, match(d_cens$blockgroup, d_acs$GEOID))
d_cens$owners = estimate_down(d_cens$pop, d_acs$owners, match(d_cens$blockgroup, d_acs$GEOID))

d_cens_prec = d_cens %>%
    st_drop_geometry() %>%
    group_by(precinct = d_prec$precinct[crosswalk_block]) %>%
    summarize(across(c(pop:vap_two, renters, owners), sum)) %>%
    mutate(rent_frac = renters / (1e-3 + renters + owners),
           vap_rent = rent_frac * vap,
           vap_own = (1 - rent_frac) * vap)
d_prec = left_join(d_prec, d_cens_prec, by="precinct") %>%
    relocate(geometry, .after=everything())


# Election returns ------
get_sheet_names = function(path) {
    n_sheets = length(excel_sheets(path))
    map_chr(seq_len(n_sheets), function(i) {
        read_xlsx(path, sheet=i, col_names=F, n_max=2, .name_repair="minimal")[[1]][2]
    })
}
parse_elec = function(path, sheet=2, prefix="") {
    raw = read_xlsx(path, sheet=sheet, skip=3, .name_repair="unique") %>%
        suppressMessages()
    idx_prec2 = which(str_starts(names(raw), "Precinct"))[2]
    d = select(raw, -seq_len(idx_prec2-1), -starts_with("...")) %>%
        janitor::clean_names() %>%
        select(-starts_with("total_votes"), -contains("write_in")) %>%
        rename(left=1) %>%
        mutate(precinct = if_else(str_starts(left, "(PCT|Pct|Electionwide)"),
                                 str_extract_all(left, "\\d\\d\\d\\d"), list(NULL)),
               .after=left)
    # chop off tail
    d = head(d, nrow(d) - which.max(rev(lengths(d$precinct) > 0)) + 4)
    d %>%
        fill(precinct, .direction="down") %>%
        filter(lengths(precinct) > 0, left=="Total") %>%
        mutate(group = 1:n()) %>%
        select(-left) %>%
        unnest(precinct) %>%
        right_join(select(as_tibble(d_prec), precinct, vap), by="precinct") %>%
        group_by(group) %>%
        mutate(across(c(-precinct, -vap), ~ coalesce(., 0) * vap/sum(vap))) %>%
        ungroup() %>%
        select(-group, -vap) %>%
        mutate(across(-precinct, as.integer)) %>%
        rename_with(~ str_c(prefix, .), .cols=-precinct)
}

tidy_names = function(d) {
    names(d)[-1] = str_split(names(d)[-1], "_") %>%
        map_chr(function(x) {
            str_c(x[1], "_", tail(x[nchar(x)>3], 1))
        })
    d
}

d_elec = list(
    # Nov 2019: want sheet 5 (district atty)
    d_elec_atty = parse_elec(here("data-raw/sf_returns/20191125_sov.xlsx"),
                             sheet=5, prefix="atty_") %>%
        tidy_names(),

    # Mar 2020: want sheet 2 (president) and 14 (state senator)
    d_elec_pres = parse_elec(here("data-raw/sf_returns/20200326_sov.xlsx"),
                             sheet=2, prefix="pres_") %>%
        tidy_names(),
    d_elec_ssen = parse_elec(here("data-raw/sf_returns/20200326_sov.xlsx"),
                             sheet=14, prefix="ssen_") %>%
        tidy_names(),

    # Nov 2020: want sheet 24 (Prop 19) and 26 (Prop 21)
    d_elec_p19 = parse_elec(here("data-raw/sf_returns/20201201_sov.xlsx"),
                            sheet=24, prefix="p19_"),
    d_elec_p21 = parse_elec(here("data-raw/sf_returns/20201201_sov.xlsx"),
                            sheet=26, prefix="p21_")
) %>%
    reduce(left_join, by="precinct") %>%
    mutate(across(-precinct, ~ coalesce(., 0)))

d_prec = d_prec %>%
    mutate(across(pop:vap_two, as.integer)) %>%
    left_join(d_elec, by="precinct") %>%
    relocate(geometry, .after=everything()) %>%
    filter(precinct != "9900")

idx_yerbabuena = which(d_prec$precinct == "7648")
idx_landconn = which(d_prec$precinct == "7638")
d_prec$adj = suppressWarnings(redist.adjacency(d_prec))
d_prec$adj = add_edge(d_prec$adj, idx_yerbabuena, idx_landconn)

d_prec$geometry = rmapshaper::ms_simplify(d_prec$geometry, keep=0.05, keep_shapes=TRUE)
d_prec$geometry = st_make_valid(d_prec$geometry)

# save
write_rds(d_prec, here("data/sf_elec.rds"), compress="xz")
