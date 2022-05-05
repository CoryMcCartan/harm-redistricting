if (!exists("fit_lfei")) source(here("R/05_fit_ei.R"))

# Load and fit -------------

d = read_rds(here("data/boston_elec.rds")) %>%
    mutate(ward = as.integer(str_split(precinct, "-", n=2, simplify=T)[,1]))
map = redist_map(d, existing_plan=ccd_2010, pop_tol=0.05, adj=d$adj)

d_votes =  d %>%
    as_tibble() %>%
    mutate(vap_other = vap - vap_white - vap_black - vap_hisp) %>%
    select(precinct, mayor_gen_wu:pres_prelim_castro) %>%
    pivot_longer(mayor_gen_wu:pres_prelim_castro, names_to="cand", values_to="votes") %>%
    separate(cand, c("elec", "stage", "candidate"), sep="_", extra="merge") %>%
    filter(stage == "prelim", candidate != "other") %>%
    select(-stage) %>%
    mutate(precinct = fct_inorder(precinct),
           elec = fct_inorder(elec),
           candidate = fct_inorder(candidate))
tbls = make_tables_boston(d_votes, cand_thresh=0.02)

if (!file.exists(fit_path <- here("data-raw/boston_fit_2.rds"))) {
    id = list(
        loc = list(sanders=c(-1, 0), biden=c(1, 0), janey=c(-0.25, -1)),
        scale = list(sanders=c(0.1, 0.25), biden=c(1, 0.5), janey=c(0.5, 2)),
        corr = list(
            list("sanders", "biden", c(-0.5, 0.8))
        )
    )

    fit = fit_lfei(tbls, n_comp=2L, id=id, algorithm="hmc", step_size=0.03)
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
nice_name = function(x) str_to_title(str_replace(x, "_", " "))
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



# Predictions -------

## Mayoral General Election -------
wu_gen_total = with(d, sum(mayor_gen_wu)/(sum(mayor_gen_wu) + sum(mayor_gen_essaibi_george)))
wu_gen = with(d, (mayor_gen_wu)/((mayor_gen_wu) + (mayor_gen_essaibi_george)))

prior_gen = tbls$summary$vs[1:2] / sum(tbls$summary$vs[1:2])
loading = mean(fit$loading)[1:2,]

est_votes = predict(fit, prior_gen, loading, tbls)
est_turn_r = 1e-6 + rvar_apply(est_votes, 1:2, rvar_sum)
est_turn = rvar_apply(est_turn_r, 1, rvar_sum)
est_vs = est_votes / est_turn_r
est_votes_total = rvar_apply(est_votes, c(1, 3), rvar_sum)
est_vs_total = as_tibble(est_votes_total / est_turn)

act_turn = with(d, mayor_gen_wu + mayor_gen_essaibi_george)
p1 = ggplot(d, aes(x=wu_gen, y=median(est_vs_total$wu))) +
    geom_point(size=0.6) +
    geom_abline(slope=1, color="red") +
    scale_x_continuous("Actual Wu vote share", labels=percent) +
    scale_y_continuous("Estimated Wu vote share", labels=percent) +
    theme_repr()
p2 = ggplot(d, aes(x=act_turn, y=median(est_turn))) +
    geom_point(size=0.6) +
    geom_abline(slope=1, color="red") +
    coord_trans("sqrt", "sqrt") +
    labs(x="Actual total votes", y="Estimated total votes") +
    theme_repr()
p1 + p2 & theme(plot.margin=unit(c(0, 0.2, 0, 0), "cm"))
ggsave(here("paper/figures/boston_gen_pred.pdf"), width=6.5, height=3.25)

cat("Wu vote share Spearman correlation:",
    cor(median(est_vs_total$wu), wu_gen, method="spearman"),
    "\nTotal votes Spearman correlation:",
    cor(median(est_turn), act_turn, method="spearman"))


## Counterfactual -------

n_draws = length(draws_of(fit$lp__))
draws_dim = dim(draws_of(fit$lp__, with_chains=TRUE))

support = rvar(array(rbeta(n_draws, 20, 20),, dim=draws_dim), with_chains=TRUE)
support = c(support, 1 - support)
loading = tribble(~cand, ~f1, ~f2,
                  "Left", -0.0, -1.0,
                  "Right", 0.0, 1.0) %>%
    column_to_rownames("cand") %>%
    as.matrix()

est_votes = predict(fit, support, loading, tbls)
est_turn_r = 1e-6 + rvar(rowSums(draws_of(est_votes), dims=3))
est_turn = 4e-6 + rvar(rowSums(draws_of(est_votes), dims=2))
est_vs = est_votes / est_turn_r
est_votes_total = rvar(rowSums(aperm(draws_of(est_votes), c(1, 2, 4, 3)), dims=3))
est_vs_total = as_tibble(est_votes_total / est_turn)

plot(map, median(est_vs_total$Left)) + scale_fill_wa_c("lopez")

# Simulate
# map13 = redist_map(d, ndists=13, pop_tol=0.05, adj=d$adj)

constr = redist_constr(map) %>%
    add_constr_grp_hinge(15.0, group_pop=vap-vap_white, total_pop=vap,
                         tgts_group=c(0.1, 0.5))
plans = redist_smc(map, n_draws/2, runs=2, counties=nbhd, seq_alpha=0.5)
plans = plans %>%
    mutate(left = group_frac(map, median(est_votes_total[,1]), median(est_turn)))
m_pl = as.matrix(plans)


pr_left_e = pnorm(avg_by_prec(plans, left, draws="ccd_2010"), 0.5, 0.065)
pr_left_s = pnorm(avg_by_prec(plans, left), 0.5, 0.065)
plot(map, pr_left_s) + scale_fill_wa_c(name="Pr(left)", "stuart", midpoint=0.5)
plot(map, pr_left_s - pr_left_e) +
#plot(map, avg_by_prec(plans, left) - avg_by_prec(plans, left, draws=7)) +
    scale_fill_wa_c(name="Ideological displacement", "stuart", midpoint=0.0)

pl_e = map$ccd_2010
n_race = ncol(tbls$race)
harm_prec = array(dim=c(n_draws, nrow(map), n_race),
                  dimnames=list(NULL, NULL, colnames(tbls$race)))
for (i in seq_len(n_draws)) {
    votes_i = draws_of(est_votes_total)[i, , ]
    pl_i = m_pl[, i+1]
    tv_i1 = tapply(votes_i[, 1], pl_i, sum)
    tv_i2 = tapply(votes_i[, 2], pl_i, sum)
    tv_e1 = tapply(votes_i[, 1], pl_e, sum)
    tv_e2 = tapply(votes_i[, 2], pl_e, sum)
    harm_1 = (tv_i1 > tv_i2)[pl_i] & (tv_e1 < tv_e2)[pl_e]
    harm_2 = (tv_i1 < tv_i2)[pl_i] & (tv_e1 > tv_e2)[pl_e]

    votes_ir = draws_of(est_votes)[i, , , ]
    # turn_i = 1e-6 + rowSums(votes_ir, dims=2)

    harm_prec[i,,] = rep(harm_1, n_race)*votes_ir[,,1] + rep(harm_2, n_race)*votes_ir[,,2]
}
harm_prec = rvar(harm_prec)

#plot(map, median(harm_prec / est_turn_r)[, ""])
plot(map, median(harm_prec/est_turn_r)[, 1])
plot(map, median(harm_prec/est_turn_r)[, 2])
plot(map, median(harm_prec/est_turn_r)[, 1] - median(harm_prec/est_turn_r)[, 2]) + scale_fill_wa_c("lopez", midpoint=0.0)
harm_race = rvar_apply(harm_prec, 2, rvar_sum)/rvar_apply(est_turn_r, 2, rvar_sum)
hist(draws_of(harm_race[2] - harm_race[1]))
Pr(harm_race[2] < harm_race[1])
E(harm_race[2] - harm_race[1])





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



