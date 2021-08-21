library(tidyverse)
library(geomander)
library(redist)
library(sf)
library(scales)
library(wacolors)
library(here)

theme_repr = function() theme_bw(base_family="Times New Roman", base_size=11)

GOP_DEM = c("#A0442C", "#B25D4C", "#C27568", "#D18E84", "#DFA8A0",
            "#EBC2BC",  "#F6DCD9", "#F9F9F9", "#DAE2F4", "#BDCCEA",
            "#9FB6DE", "#82A0D2", "#638BC6", "#3D77BB", "#0063B1")
scale_fill_party_c = function(...) {
    scale_fill_gradient2(..., low = GOP_DEM[1], high = GOP_DEM[15],
                         midpoint=0.5, limits=0:1,
                         labels=percent, name="Democratic share")
}
scale_color_party_d = function(...) {
    scale_color_manual(..., values=c(GOP_DEM[2], GOP_DEM[14]),
                       labels=c("Rep.", "Dem."))
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
