library(rlang)
library(tidyverse)
library(geomander)
library(divseg)
library(redist)
library(sf)
library(scales)
library(patchwork)
library(wacolors)
library(here)

theme_repr = function() theme_bw(base_family="Times", base_size=10)
theme_repr_map = function() theme_void(base_family="Times", base_size=10)

PAL_COAST = c("#7BAEA0", "#386276", "#3A4332", "#7A7D6F", "#D9B96E", "#BED4F0")
PAL_LARCH = c("#D2A554", "#626B5D", "#8C8F9E", "#858753", "#A4BADF", "#D3BEAF")
PAL = PAL_COAST[c(5, 1, 2, 4, 3, 6)]
GOP_DEM = c("#A0442C", "#B25D4C", "#C27568", "#D18E84", "#DFA8A0",
            "#EBC2BC",  "#F6DCD9", "#F9F9F9", "#DAE2F4", "#BDCCEA",
            "#9FB6DE", "#82A0D2", "#638BC6", "#3D77BB", "#0063B1")
scale_fill_party_c = function(name="Democratic share", midpoint=0.5, limits=0:1,
                              labels=percent, oob=squish, ...) {
    scale_fill_gradient2(name=name, ..., low = GOP_DEM[1], high = GOP_DEM[15],
                         midpoint=midpoint, limits=limits, labels=labels, oob=oob)
}
scale_color_party_d = function(...) {
    scale_color_manual(..., values=c(GOP_DEM[2], GOP_DEM[14]),
                       labels=c("Rep.", "Dem."))
}

# 'fixed' log that handles 0 and negative numbers
flog = function(x) if_else(x <= 0, -1, suppressWarnings(log(x)))
k_step = function(x) x > 0.5
# historical congressional national shifts
d_hist = local({
    dem_hist = c(0.575, 0.559, 0.537, 0.505, 0.552, 0.521, 0.543, 0.533, 0.521, 0.501, 0.447, 0.482, 0.473, 0.471, 0.452, 0.468, 0.523, 0.532, 0.449, 0.488, 0.455, 0.48, 0.534)
    gop_hist = c(0.407, 0.423, 0.448, 0.479, 0.434, 0.47, 0.444, 0.456, 0.443, 0.451, 0.515, 0.482, 0.484, 0.476, 0.5, 0.494, 0.443, 0.426, 0.517, 0.476, 0.512, 0.491, 0.448)
    dem_tpp = dem_hist / (dem_hist + gop_hist)
    list(sd=sd(dem_tpp), df=length(dem_hist)-1)
})
k_t = function(sd=d_hist$sd) {
    function(x) pt((x-0.5)/sd, df=d_hist$df)
}

plot_cds = function(map, pl, county, abbr) {
    plan = as.factor(redist:::color_graph(get_adj(map), as.integer(pl)))
    places = suppressMessages(tigris::places(abbr, cb=TRUE))

    counties = map %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by({{ county }}) %>%
        summarize(is_coverage=TRUE)
    map %>%
        mutate(.plan = plan,
               .distr = pl) %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by(.distr) %>%
        summarize(.plan = .plan[1], is_coverage=TRUE) %>%
        ggplot(aes(fill=.plan)) +
        geom_sf(size=0.0) +
        geom_sf(data=places, inherit.aes=FALSE, fill="#0000002A", color=NA) +
        geom_sf(data=counties, inherit.aes=FALSE, fill=NA, size=0.25, color="#ffffff1D") +
        geom_sf(fill=NA, size=0.4, color="black") +
        scale_fill_manual(values=PAL, guide="none") +
        theme_void()
}


# downloads data for state `abbr` to `folder/{abbr}_2020_*.csv` and returns path to file
download_redistricting_file = function(abbr, folder) {
    abbr = tolower(abbr)
    url_vtd = paste0("https://raw.githubusercontent.com/alarm-redist/census-2020/",
                     "main/census-vest-2020/", abbr, "_2020_vtd.csv")
    url_block = paste0("https://raw.githubusercontent.com/alarm-redist/census-2020/",
                       "main/census-vest-2020/", abbr, "_2020_block.csv")

    path = paste0(folder, "/", basename(url_vtd))
    resp = download.file(url_vtd, path)
    if (resp != 0) {
        path = paste0(folder, "/", basename(url_block))
        resp = download.file(url_block, path)
        if (resp != 0)  {
            stop("No files available for ", abbr)
        }
    }
    path
}

# adds precinct shapefile geometry to downloaded data
join_shapefile = function(data) {
    geom_d = PL94171::pl_get_vtd(data$state[1]) %>%
        select(GEOID20, area_land=ALAND20, area_water=AWATER20, geometry)
    left_join(data, geom_d, by="GEOID20") %>%
        sf::st_as_sf()
}
