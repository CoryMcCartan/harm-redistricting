if (!exists("fit_pcei")) source(here("R/04_fit_boston.R"))

# Load and fit -------------

d = read_rds(here("data/boston_elec.rds")) %>%
    mutate(ward = as.integer(str_split(precinct, "-", n=2, simplify=T)[,1]))
map = redist_map(d, existing_plan=ccd_2010, adj=d$adj)

d_votes = make_votes_long(d)
tbls = make_tables(d_votes, cand_thresh=0.02)

if (!file.exists(fit_path <- here("data-raw/boston_fit_2.rds"))) {
    id = list(
        loc = list(sanders=c(-1, 0), biden=c(1, 0), janey=c(-0.25, -1)),
        scale = list(sanders=c(0.1, 0.25), biden=c(1, 0.5), janey=c(0.5, 2)),
        corr = list(
            list("sanders", "biden", c(-0.5, 0.8))
        )
    )

    fit = fit_pcei(tbls, n_comp=2L, id=id, algorithm="hmc", step_size=0.03)
    save_fit(fit, fit_path)
} else {
    fit = read_rds(fit_path)
}


# Plots --------
## map -----
map$geometry = rmapshaper::ms_simplify(map$geometry, 0.1, keep_shapes=TRUE)
map$geometry2 = rmapshaper::ms_simplify(d$geometry, 0.04, keep_shapes=TRUE)
geom_dist = summarize(group_by(map, ccd_2010))
geom_nbhd = summarize(group_by(map, nbhd)) %>%
    as_tibble() %>%
    st_as_sf() %>%
    mutate(cuml_area = cume_dist(st_area(geometry)))
nbhd_show = with(geom_nbhd,
                 (cuml_area >= 0.4 | nbhd %in% c("Downtown", "Back Bay")) &
                 (!nbhd %in% c("Harbor Islands", "South Boston Waterfront")))
geom_ward = summarize(group_by(map, ward))


map = map %>%
    mutate(maj_grp = colnames(tbls$race)[apply(tbls$race, 1, which.max)],
           maj_share = apply(tbls$race, 1, max))

p1 = ggplot(map, aes(fill=maj_grp, alpha=maj_share)) +
    geom_sf(size=0, color=NA) +
    geom_sf(data=geom_nbhd, inherit.aes=F, fill=NA, size=0.25, color="black") +
    geom_sf_text(aes(label=str_wrap(str_to_upper(nbhd), 15)), data=geom_nbhd[nbhd_show,],
                 inherit.aes=F, size=2.0, color="#ffffffaa", nudge_y=100,
                 family="DIN Condensed", fontface="bold") +
    scale_fill_manual(name="Majority racial group",
                      values=PAL_RACE, labels=NAMES_RACE) +
    scale_alpha_binned(name="Share of precinct", breaks=c(0.5, 0.7, 0.9),
                       labels=percent, range=c(0.3, 1), oob=squish) +
    coord_sf(expand=F) +
    theme_repr_map() +
    guides(fill=guide_legend(order=1, direction="vertical", title.position="top"),
           alpha=guide_bins(order=2, override.aes=list(fill="#888888"), title.position="top")) +
    theme(legend.position=c(1.0, 0.05),
          legend.spacing=unit(0.1, "cm"),
          legend.key.width=unit(0.75, "cm"),
          legend.key.height=unit(0.4, "cm"),
          legend.justification=c(1, 0),
          legend.direction="horizontal")

set.seed(123)
p2 = ggplot(map, aes(fill=sample(1:9)[ccd_2010])) +
    geom_sf(size=0, color=NA, alpha=0.7) +
    geom_sf(data=geom_dist, inherit.aes=F, fill=NA, size=0.5, color="black") +
    geom_sf(data=geom_nbhd, inherit.aes=F, fill=NA, size=0.1, color="#ffffff88") +
    geom_sf_text(aes(label=ccd_2010), data=geom_dist,
                 inherit.aes=F, size=3.0, color="#000000aa", nudge_y=-50,
                 family="DIN Condensed", fontface="bold") +
    scale_fill_wa_c("sea_star", guide="none") +
    coord_sf(expand=F) +
    theme_repr_map()
p1 + p2 & theme(plot.margin=rep(unit(0.1, "mm"), 4))

ggsave(here("paper/figures/boston_map.pdf"), width=8, height=4, device=cairo_pdf)

## loadings -----

d_load = gather_draws(fit$loading, x[cand, dim]) %>%
    pivot_wider(names_from=dim, names_prefix="load_", values_from=.value) %>%
    left_join(tbls$summary, by=c("cand"="candidate"))
d_load_exp = group_by(d_load, cand, elec, vs) %>%
    summarize(across(load_1:load_2, median)) %>%
    mutate(load_2 = if_else(cand=="campbell", load_2-0.1, load_2))

library(ggrepel)
nice_name = function(x) str_to_title(str_replace(x, "_", "-"))
nice_elec = function(x) c(mayor="Mayoral", pres="Presidential")[x]

p1 = ggplot(NULL, aes(load_1, load_2)) +
    geom_hline(yintercept=0, color="#888888") +
    geom_vline(xintercept=0, color="#888888") +
    stat_ellipse(aes(group=cand, color=vs, lty=nice_elec(elec), size=vs),
                 data=d_load, level=0.8, alpha=0.6) +
    geom_text(aes(label=nice_name(cand), color=vs), data=d_load_exp,
              family="DIN Condensed", fontface="bold", size=2.8, show.legend=FALSE) +
    scale_color_wa_b("sea_star", which=1:12, labels=percent, reverse=TRUE) +
    scale_size_continuous(range=c(0.3, 0.6), guide="none") +
    labs(x="First dimension", y="Second dimension", lty="Election") +
    theme_repr() +
    guides(lty=guide_legend(order=1),
           color=guide_bins(title="Vote share", order=2, direction="horizontal",
                            title.position="top", override.aes=list(size=4))) +
    theme(legend.position=c(1.0, -0.05),
          legend.justification=c(1, 0),
          legend.background=element_blank(),
          legend.key.height=unit(0.25, "cm"),
          legend.spacing=unit(0, "cm"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_line(size=0.3),
          plot.margin=unit(c(0, 0.1, 0, 0), "cm"))

map_fact = map %>%
    as_tibble() %>%
    select(-geometry) %>%
    mutate(white_1 = median(fit$pref[, 1, "white", drop=T]),
           black_1 = median(fit$pref[, 1, "black", drop=T]),
           white_2 = median(fit$pref[, 2, "white", drop=T]),
           black_2 = median(fit$pref[, 2, "black", drop=T])) %>%
    pivot_longer(white_1:black_2, names_to=c("race", "factor"), names_sep="_") %>%
    mutate(factor = paste(c("First", "Second")[as.integer(factor)], "dimension"),
           race = factor(str_to_title(race), levels=c("White", "Black"))) %>%
    st_as_sf()
geom_nbhd = summarize(group_by(map_fact, nbhd))

p2 = ggplot(map_fact, aes(fill=value)) +
    facet_grid(race ~ factor, switch="y") +
    geom_sf(size=0, color=NA) +
    geom_sf(data=geom_nbhd, inherit.aes=F, fill=NA, size=0.1, color="#000000aa") +
    scale_fill_wa_c("stuart", midpoint=0.0, name="Factor value\n",
                    limits=c(-1.0, 1.0), oob=squish) +
    coord_sf(expand=F, clip="off") +
    theme_repr_map() +
    theme(strip.text=element_text(face="bold"),
          legend.direction="horizontal",
          legend.position=c(0.5, -0.08),
          legend.key.height=unit(0.4, "cm"),
          legend.key.width=unit(0.8, "cm"),
          plot.margin=unit(rep(0, 4), "cm"),
          panel.spacing=unit(0, "cm"))

p1 + p2

ggsave(here("paper/figures/boston_factors.pdf"), height=3.5, width=7, device=cairo_pdf)


# VALIDATION ---------------

library(bayesplot)
rhats = unlist(lapply(fit, posterior::rhat))
qplot(rhats, bins=50)
mcmc_intervals(as_draws_matrix(fit$support))
mcmc_intervals(as_draws_matrix(fit$alpha))
mcmc_intervals(as_draws_matrix(fit$loading[, 1]))
mcmc_intervals(as_draws_matrix(fit$loading[, 2]))
median(fit$L_t %**% t(fit$L_t))
median(fit$L_p %**% t(fit$L_p))

mcmc_trace(as_draws_matrix(fit$loading[1:3, ]))
mcmc_trace(as_draws_matrix(fit$loading[7:8, ]))
mcmc_trace(as_draws_matrix(fit$support[1:4]))
mcmc_trace(as_draws_matrix(fit$turnout_overall))
mcmc_trace(as_draws_matrix(fit$scale))
mcmc_trace(as_draws_matrix(fit$sigma_p))
mcmc_trace(as_draws_matrix(fit$L_p))

mcmc_pairs(as_draws_matrix(fit$turnout_overall))
mcmc_pairs(as_draws_matrix(fit$scale))
mcmc_pairs(as_draws_matrix(fit$loading[1:2, ]))
mcmc_pairs(as_draws_matrix(fit$loading[c(1,2,7,8), 2]))
mcmc_pairs(as_draws_matrix(fit$pref[1:2, 1,]))

median(fit$loading) %>%
    `rownames<-`(rownames(fit$loading)) %>%
    as.data.frame() %>%
    rownames_to_column("cand") %>%
ggplot(aes(V1, V2, label=cand)) +
    geom_text()

plot(map, median(fit$pref[,1,"white"])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, median(fit$pref[,1,"black"])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, median(fit$pref[,2,"white"])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, median(fit$pref[,2,"black"])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, 100*median(fit$turnout[, "white"]))
plot(map, 100*median(fit$turnout[, "black"]))
plot(map, median(fit$post_factor[,1])) + scale_fill_wa_c("stuart", midpoint=0)
plot(map, median(fit$post_factor[,2])) + scale_fill_wa_c("stuart", midpoint=0)

map_dbl(tbls$cands, function(cnd) {
    cor(tbls$vote_share[, cnd], mean(fit$post_share_prec[, cnd]), method="spearman")
}) %>%
    `names<-`(tbls$cands)

x = median(fit$post_share_race[7:11,])
x = median(fit$post_share_race[1:6,])
round(`colnames<-`(x %*% diag(1/colSums(x)), colnames(x)), 2)



