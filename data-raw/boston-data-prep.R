library(tidyverse)
library(sf)
library(censable)
library(geomander)
library(easycensus)
library(redist)
library(here)

# functions -----

# read from Tabula output
parse_mayor_data = function(data_dir) {
    map_dfr(list.files(data_dir), function(fname) {
        ward_no = as.integer(str_extract(fname, "(?<=-)\\d+(?=.csv)")) + 1L
        d = read_lines(here(data_dir, fname)) %>%
            str_replace_all(",{2,}", ",") %>%
            str_c(collapse="\n") %>%
            read_csv(show_col_types=F) %>%
            select(-TOTAL)

        d = d[1:(which(d$CANDIDATES == "VOTES CAST") - 1),]

        d %>%
            pivot_longer(-CANDIDATES, names_to="precinct", values_to="votes") %>%
            rename(candidate=CANDIDATES) %>%
            mutate(precinct = str_c(ward_no, "-", precinct)) %>%
            select(precinct, candidate, votes)
    })
}

parse_name = function(x) {
    case_when(
        x %in% c("ALL OTHERS", "NO PREFERENCE") ~ "other",
        x == "ANNISSA ESSAIBI GEORGE" ~ "essaibi_george",
        x == "JOSEPH R. BIDEN, JR." ~ "biden",
        TRUE ~ str_to_lower(word(x, -1))
    )
}

# precinct rows, candidate columns
pivot_name = function(d, prefix="") {
    d %>%
        mutate(candidate = parse_name(candidate)) %>%
        group_by(precinct, candidate) %>%
        summarize(votes = sum(votes), .groups="drop") %>%
        arrange(desc(votes)) %>%
        pivot_wider(names_from = candidate,
                    values_from = votes) %>%
        slice(str_order(precinct, numeric=TRUE)) %>%
        rename_with(function(x) str_c(prefix, x), .cols=-precinct)
}


# read in and parse  ------

d_gen = parse_mayor_data("data-raw/boston_returns/tabula-gen_mayor") %>%
    pivot_name("mayor_gen_")
d_prelim = parse_mayor_data("data-raw/boston_returns/tabula-prelim_mayor") %>%
    pivot_name("mayor_prelim_")

d_pres = read_csv(here("data-raw/ma_pres_primary_2020_prec.csv"),
                  show_col_types=F) %>%
    suppressWarnings() %>%
    filter(`City/Town` == "Boston") %>%
    mutate(precinct = str_c(Ward, "-", Pct)) %>%
    select(precinct, `Joseph R. Biden, Jr.`:`No Preference`) %>%
    pivot_longer(-precinct, names_to="candidate", values_to="votes") %>%
    mutate(candidate = str_to_upper(candidate)) %>%
    pivot_name("pres_prelim_")

# additional datasets ------
## ward geometry ----
d_geom = read_sf(here("data-raw/ma_wardsprecincts/WARDSPRECINCTS_POLY.shp")) %>%
    filter(TOWN == "BOSTON") %>%
    select(precinct=DISTRICT, pop_2010=POP_2010, geometry)


## census data ----
d_cens = build_dec("voting district", "MA", county="Suffolk", geometry=TRUE, year=2020)
idx_match = geo_match(st_as_sf(d), d_cens, method="area")
d_cens = as_tibble(d_cens) %>%
    slice(idx_match) %>%
    select(pop:vap_two)

d_acs = get_acs_table("tract", "B19301", year=2020, state="MA", county="Suffolk") %>%
    transmute(GEOID=GEOID, med_inc=estimate, moe_inc=moe, race=tidy_race(race_ethnicity)) %>%
    filter(race %in% c("total", "white_nh", "black", "hisp")) %>%
    mutate(race = if_else(race == "white_nh", "white", as.character(race))) %>%
    distinct() %>%
    pivot_wider(names_from=race, values_from=c(med_inc, moe_inc))

d_tract = tigris::tracts("MA", "Suffolk", cb=T, year=2020)
idx_match = geo_match(st_as_sf(d), d_tract, method="area")
d_tract = as_tibble(d_tract) %>%
    select(GEOID) %>%
    slice(idx_match)

## district geometry ----
d_distr = read_sf(here("data-raw/City_Council_Districts_View/city_council_districts.shp")) %>%
    arrange(DISTRICT)

## neighborhood geometry ----
d_nbhd = read_sf(here("data-raw/Boston_Neighborhoods/Boston_Neighborhoods.shp"))

d = d_geom %>%
    slice(str_order(precinct, numeric=TRUE)) %>%
    left_join(d_gen, by="precinct") %>%
    left_join(d_prelim, by="precinct") %>%
    left_join(d_pres, by="precinct") %>%
    relocate(geometry, .after=everything()) %>%
    bind_cols(d_cens, .) %>%
    select(precinct, pop_2010, everything(), geometry)
d = left_join(d_tract, d_acs, by="GEOID") %>%
    select(-GEOID) %>%
    bind_cols(d) %>%
    relocate(med_inc_total:moe_inc_hisp, .after=vap_two) %>%
    rows_update(tibble(precinct="1-15", pop=3, pop_white=3, vap=3, vap_white=3),
                by="precinct") %>% # PATCH Harbor Islands population
    st_as_sf()
    mutate(ccd_2010 = geo_match(., d_distr, method="area"),
           nbhd = d_nbhd$Name[geo_match(., d_nbhd, method="area")],
           .after=precinct)

# adjacency graph
d$adj = redist.adjacency(d) %>% suppressWarnings()
d$adj = with(d, add_edge(adj, which(precinct == "2-1"),
                              which(precinct == "3-5")))
d$adj = with(d, add_edge(adj, which(precinct == "2-1"),
                              which(precinct == "3-2")))
d$adj = with(d, add_edge(adj, which(precinct == "1-4"),
                              which(precinct == "3-1")))
d$adj = with(d, add_edge(adj, which(precinct == "3-6"),
                              which(precinct == "6-1")))
d$adj = with(d, add_edge(adj, which(precinct == "1-1"),
                              which(precinct == "6-1")))
d$adj = with(d, add_edge(adj, which(precinct == "6-1"),
                              which(precinct == "6-5")))
d$adj = with(d, add_edge(adj, which(precinct == "1-15"),
                              which(precinct == "13-10")))

# save
write_rds(d, here("data/boston_elec.rds"), compress="xz")

