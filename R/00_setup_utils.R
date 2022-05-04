library(rlang)
library(tidyverse)
library(sf)
library(redist)
library(scales)
library(patchwork)
library(wacolors)
library(here)
library(Rcpp)

theme_repr = function() theme_bw(base_family="Times", base_size=10)
theme_repr_map = function() theme_void(base_family="Times", base_size=10)

PAL = wa_pal("coast", which=c(5, 1, 2, 4, 3, 6))
PAL_RACE = wa_pal("skagit", which=c(1, 4, 6, 3))
names(PAL_RACE) = c("white", "black", "hisp", "other")
NAMES_RACE = c("White", "Black", "Hispanic", "other")

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

scale_fill_ideo = \(...) scale_fill_wa_c(palette="stuart", ...)

# 'fixed' log that handles 0 and negative numbers
flog = function(x) if_else(x <= 0, -1, suppressWarnings(log(x)))
plapl = function(x) 0.5 * (1 + sign(x) * (1 - exp(-2*abs(x))))
qlapl = function(p) -0.5 * sign(p - 0.5) * log(1 - 2*abs(p - 0.5))
k_step = function(x) x > 0.5
pos_part = function(x) {
    x[x < 0] = 0
    x
}
# historical congressional national shifts
d_hist = local({
    dem_hist = c(0.575, 0.559, 0.537, 0.505, 0.552, 0.521, 0.543, 0.533, 0.521, 0.501, 0.447, 0.482, 0.473, 0.471, 0.452, 0.468, 0.523, 0.532, 0.449, 0.488, 0.455, 0.48, 0.534)
    gop_hist = c(0.407, 0.423, 0.448, 0.479, 0.434, 0.47, 0.444, 0.456, 0.443, 0.451, 0.515, 0.482, 0.484, 0.476, 0.5, 0.494, 0.443, 0.426, 0.517, 0.476, 0.512, 0.491, 0.448)
    dem_tpp = qlapl(dem_hist / (dem_hist + gop_hist))
    list(sd=sd(dem_tpp), df=length(dem_hist)-1)
})
sd_app = d_hist$sd * sd(rt(1e5, df=d_hist$df))
k_t = function(sd=d_hist$sd) {
    function(x) pt(qlapl(x)/sd, df=d_hist$df)
}
e_approx = function(a, b) {
    y = sd_app * (dnorm(a/sd_app) - dnorm(b/sd_app)) / (pnorm(b/sd_app) - pnorm(a/sd_app))
    if_else(is.finite(y), y,
            if_else(is.finite(a),
                    if_else(is.finite(b), 0.5*(a+b), a), b))
}


plot_cds = function(map, pl, county, abbr, qty="ndv") {
    plan = as.factor(redist:::color_graph(get_adj(map), as.integer(pl)))
    # places = suppressMessages(tigris::places(abbr, cb=TRUE))

    if (qty == "dem") {
        dvote = map$dem
        rvote = map$rep
        qty = expr(dem)
        scale = scale_fill_party_c(limits=c(0.3, 0.7))
    } else if (qty == "ndv") {
        dvote = map$ndv
        rvote = map$nrv
        qty = expr(dem)
        scale = scale_fill_party_c(limits=c(0.3, 0.7))
    } else {
        qty = expr(.plan)
        scale = scale_fill_manual(values=PAL, guide="none")
    }

    counties = map %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by({{ county }}) %>%
        summarize(is_coverage=TRUE)
    map %>%
        mutate(.plan = plan,
               .distr = pl,
               dvote = dvote,
               rvote = rvote) %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by(.distr) %>%
        summarize(.plan = .plan[1],
                  dem = 1 / (1 + sum(rvote) / sum(dvote)),
                  is_coverage=TRUE) %>%
        ggplot(aes(fill={{ qty }})) +
        geom_sf(size=0.0) +
        # geom_sf(data=places, inherit.aes=FALSE, fill="#0000003A", color=NA) +
        geom_sf(fill=NA, size=0.4, color="black") +
        geom_sf(data=counties, inherit.aes=FALSE, fill=NA, size=0.4, color="#ffffff33") +
        scale +
        theme_void()
}

expl_vars = function(pl, labels, refs=character(0), rasterize=TRUE,...) {
    n_ref = length(refs)
    idx = if (n_ref == 0L) seq_len(nrow(pl)) else -seq_len(n_ref)
    tmp <- tempfile()

    panel.hist <- function(x, ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x[idx], breaks=24, plot=FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, family="Times",
             col="#888888", border="#88888800", lwd=0)
        for (i in seq_len(n_ref)) {
            abline(v=x[i], col=refs[i], lwd=3.0)
        }
    }
    panel.text <- function(x, y, labels, cex, font, ...) {
        text(x, y, labels=labels, family="Times", font=2)
    }
    panel.cor <- function(x, y, digits = 2, prefix = "", ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x[idx], y[idx], use="complete.obs"))
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste0(prefix, txt)
        text(0.5, 0.5, txt, cex=2.5*sqrt(r), family="Times", offset=0, adj=c(0.5, 0.5))
    }
    panel.points = function(x, y, ...) {
        coords <- par("usr")
        gx <- grconvertX(coords[1:2], "user", "inches")
        gy <- grconvertY(coords[3:4], "user", "inches")
        width <- max(gx) - min(gx)
        height <- max(gy) - min(gy)

        if (rasterize) {
            ragg::agg_png(tmp, width = width, height = height, units = "in", res = 300, bg = "transparent")
            par(mar = c(0,0,0,0))
            plot.new()
            plot.window(coords[1:2], coords[3:4], mar = c(0,0,0,0), xaxs = "i", yaxs = "i")
            points(x[idx], y[idx], family="Times", ...)
            dev.off()

            panel <- png::readPNG(tmp)
            rasterImage(panel, coords[1], coords[3], coords[2], coords[4])
        } else {
            points(x[idx], y[idx], family="Times", ...)
        }

        abline(h=0, v=0, lty="dashed")
        for (i in seq_len(n_ref)) {
            points(x[i], y[i], col=refs[i], pch=3, cex=1.2)
        }
    }

    pairs(pl, labels=labels, oma=rep(1.75, 4),
          cex=0.10, gap=0.5, family="Times", cex.labels=0.9, ...,
          lower.panel=panel.cor, upper.panel=panel.points,
          diag.panel=panel.hist, text.panel=panel.text, col="#22222255")
}


# get a two-sided p-value
pval = function(x, n_ref=redist:::get_n_ref(plans)) {
    p = numeric(length(x))
    idx = seq_len(n_ref)
    n = length(x[-idx])
    p[-idx] = cume_dist(x[-idx]) * n/(n+1)
    p[-idx] = 2*pmin(p[-idx], 1-p[-idx])
    for (i in idx) {
        p[i] = 2*(1 + sum(x[-idx] <= x[i])) / (n+1)
        if (p[i] > 1) {
            p[i] = 2*(1 + sum(x[-idx] >= x[i])) / (n+1)
        }
    }
    p
}


Rcpp::sourceCpp("src/harm.cpp", cacheDir="src")

if (!exists("harm")) source(here("R/01_metrics.R"))
if (!exists("mininois")) source(here("R/toy/00_toy_setup.R"))
