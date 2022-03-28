library(tidyverse)
library(sf)
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


# geometry ----
d_geom = read_sf(here("data-raw/ma_wardsprecincts/WARDSPRECINCTS_POLY.shp")) %>%
    filter(TOWN == "BOSTON") %>%
    select(precinct=DISTRICT, pop_2010=POP_2010, geometry)

d = d_geom %>%
    slice(str_order(precinct, numeric=TRUE)) %>%
    left_join(d_gen, by="precinct") %>%
    left_join(d_prelim, by="precinct") %>%
    left_join(d_pres, by="precinct") %>%
    relocate(geometry, .after=everything())

# Add Census data ----
d_cens = censable::build_dec("voting district", "MA", county="Suffolk", geometry=TRUE, year=2020)
idx_match = geomander::geo_match(d_idx, d_cens, method="area")

d = as_tibble(d_cens) %>%
    slice(idx_match) %>%
    select(pop:vap_two) %>%
    bind_cols(d) %>%
    select(precinct, everything(), geometry) %>%
    select(-pop_2010)

write_rds(d, here("data/boston_elec.rds"), compress="xz")


# Plotting checks ----

ggplot(d, aes(fill=mayor_gen_wu/(mayor_gen_wu+mayor_gen_essaibi_george))) +
    geom_sf(size=0) +
    wacolors::scale_fill_wa_c("lopez", name="Wu", labels=scales::percent, midpoint=0.5) +
    theme_void()

norm_votes = function(d, elec="mayor_gen") {
    m = as.data.frame(d) %>%
        select(starts_with(elec))
    m / c(1e-7 + rowSums(m))
}

pca = cbind(norm_votes(d, "mayor_gen"),
            norm_votes(d, "mayor_prelim"),
            norm_votes(d, "pres_prelim")) %>%
    select(-mayor_prelim_other) %>%
    prcomp(rank=3, scale.=T)

d_sum = d %>%
    group_by(precinct) %>%
    mutate(mayor_gen = mayor_gen_wu / sum(c_across(starts_with("mayor_gen"))),
           mayor_prelim = (mayor_prelim_wu + mayor_prelim_campbell + mayor_prelim_janey) /
               sum(c_across(starts_with("mayor_prelim"))),
           pres_prelim = (pres_prelim_warren + pres_prelim_sanders) /
               sum(c_across(starts_with("pres_prelim")))) %>%
    ungroup() %>%
    mutate(pca1 = pca$x[, 1],
           pca2 = pca$x[, 2],
           pca3 = pca$x[, 3],
           white = vap_white / vap,
           black = vap_black / vap,
           hisp = vap_hisp / vap,
           other = 1 - white - black - hisp) %>%
    select(precinct, mayor_gen, mayor_prelim, pres_prelim, pca1:pca3, vap, white:other, geometry) %>%
    st_as_sf()

ggplot(d_sum, aes(pca1, pres_prelim, color=black+hisp, size=vap)) +
    geom_point()

ggplot(d_sum, aes(fill=pca2)) +
    geom_sf(size=0) +
    wacolors::scale_fill_wa_c("stuart", name="Progressive\nIndex (PCA)",
                              midpoint=0.0, limits=c(-5, 5), oob=scales::squish) +
    theme_void()

ggplot(d_sum, aes(fill=black+hisp)) +
    geom_sf(size=0) +
    wacolors::scale_fill_wa_b() +
    theme_void()



