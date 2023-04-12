if (!exists("fit_ei")) source(here("R/05_fit_ei.R"))

if (!file.exists(path <- here("data/AL_cd_final_vtd_20.rds"))) {
    al_map = make_al_map()
    write_rds(al_map, path, compress="xz")
} else {
    al_map = read_rds(path)
}

d_votes = al_map %>%
    as_tibble() %>%
    mutate(vap_other = vap - vap_white - vap_black) %>%
    select(GEOID20, pre_16_rep_tru:uss_20_dem_jon) %>%
    pivot_longer(pre_16_rep_tru:uss_20_dem_jon, names_to="cand", values_to="votes") %>%
    separate(cand, c("elec", "year", "party", "candidate"), sep="_", extra="merge") %>%
    mutate(elec = str_c(elec, "_", year)) %>%
    select(-candidate, -year) %>%
    mutate(GEOID20 = fct_inorder(GEOID20),
           elec = fct_inorder(elec))
tbls = make_tables_alabama(d_votes, al_map)

prior = list(loc=qlogis(c(0.18, 0.90, 0.35)), # Kuriwaki et al. RPV estimates
             scale=c(0.03, 0.03, 0.2))
fit = fit_ei(tbls, algorithm="vb", prior=prior,
             init=0, eta=0.15, adapt_engaged=FALSE, tol_rel_obj=0.0002)

#  EI outputs--------
cat("Turnout correlation matrix:\n")
print(median(fit$L_t %**% t(fit$L_t)))
cat("Support correlation matrix:\n")
print(median(fit$L_s %**% t(fit$L_s)))

## Turnout and support maps by race ------
p1 = plot(al_map, median(fit$turnout)[, "white"]) + scale_fill_wa_c(name="Turnout", labels=percent, limits=c(0, 1)) + labs(title="White turnout")
p2 = plot(al_map, median(fit$turnout)[, "black"]) + scale_fill_wa_c(name="Turnout", labels=percent, limits=c(0, 1)) + labs(title="Black turnout")
p3 = plot(al_map, median(fit$support)[, "white"]) + scale_fill_party_c() + labs(title="White Dem. support")
p4 = plot(al_map, median(fit$support)[, "black"]) + scale_fill_party_c() + labs(title="Black Dem. support")
p1 + p2 + p3 + p4 + plot_layout(nrow=1, guides="collect") & theme_repr_map() & theme(legend.position="bottom")


# appendix output
d_est = bind_cols(
    rename_with(as_tibble(median(fit$turnout)), ~ str_c("turn_", .)),
    rename_with(as_tibble(median(fit$support)), ~ str_c("supp_", .)),
    as_tibble(tbls$race)
) %>%
    mutate(vap = tbls$vap)


p1 = ggplot(d_est, aes(turn_white, turn_black, size=vap, color=black)) +
    geom_point() +
    scale_x_continuous("White turnout", trans="logit", labels=percent,
                       breaks=c(0.1, 0.2, 0.4, 0.6, 0.8, 0.9)) +
    scale_y_continuous("Black turnout", trans="logit", labels=percent,
                       breaks=c(0.1, 0.2, 0.4, 0.6, 0.8, 0.9)) +
    scale_color_wa_c("sea", name="BVAP", labels=percent, limits=c(0, 1)) +
    scale_size_area(max_size=1.6, guide="none") +
    labs(title="(a) Turnout estimates") +
    theme_repr()
p2 = ggplot(d_est, aes(supp_white, supp_black, size=vap, color=black)) +
    geom_point() +
    scale_x_continuous("White Democratic support", trans="logit", labels=percent,
                       breaks=c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99)) +
    scale_y_continuous("Black Democratic support", trans="logit", labels=percent,
                       breaks=c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99)) +
    scale_color_wa_c("sea", name="BVAP", labels=percent, limits=c(0, 1)) +
    scale_size_area(max_size=1.6, guide="none") +
    labs(title="(b) Support estimates") +
    theme_repr()
p1 + p2 + plot_layout(guides="collect")
if (!file.exists(path <- here("paper/figures/al_ei.pdf"))) {
    ggsave(path, width=8, height=3.5)
}

## Partisan votes by race -----
est_ndv = median(fit$support * fit$turnout) * with(al_map, cbind(vap_white, vap_black, vap_other))
colnames(est_ndv) = paste0("ndv_", colnames(est_ndv))
est_nrv = median((1 - fit$support) * fit$turnout) * with(al_map, cbind(vap_white, vap_black, vap_other))
colnames(est_nrv) = paste0("nrv_", colnames(est_nrv))

al_map = select(al_map, GEOID20:adj) %>%
    bind_cols(est_ndv, est_nrv) %>%
    st_as_sf() %>%
    as_redist_map() %>%
    mutate(ndv = ndv_black + ndv_white + ndv_other,
           nrv = nrv_black + nrv_white + nrv_other)


# Analysis --------

N_sim = 2000

m_plans = as_tibble(al_map) %>%
    select(starts_with("cd_pet")) %>%
    as.matrix() %>%
    `colnames<-`(NULL)
idx = rep(1:4, each=N_sim/4)
m_plans = m_plans[, idx]

ker_t = k_t(sd=with(elec_model_spec, sqrt(year^2 + resid^2)))
plans = redist_plans(m_plans, al_map, algorithm="none", wgt=NULL) %>%
    add_reference(al_map$cd_2020) %>%
    mutate(total_vap = tally_var(al_map, vap),
           vap_white = tally_var(al_map, vap_white),
           vap_black = tally_var(al_map, vap_black),
           dem = group_frac(al_map, ndv, ndv + nrv),
           pr_dem = ker_t(dem))

hh_white = partisan_harm(plans, dem, al_map$ndv_white, al_map$nrv_white,
                         elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))
hh_black = partisan_harm(plans, dem, al_map$ndv_black, al_map$nrv_black,
                         elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))
hh_other = partisan_harm(plans, dem, al_map$ndv_other, al_map$nrv_other,
                         elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))

m_harm = t(cbind(hh_white, hh_black, hh_other)[-3, ]) %>%
    `rownames<-`(c("White", "Black", "Other")) %>%
    `colnames<-`(c("Dem.", "Rep."))
m_grps = with(al_map, matrix(c(
    sum(ndv_white),
    sum(ndv_black),
    sum(ndv_other),
    sum(nrv_white),
    sum(nrv_black),
    sum(nrv_other)
), nrow=3)) %>%
    `rownames<-`(c("White", "Black", "Other")) %>%
    `colnames<-`(c("Dem.", "Rep."))

{
cat("Average total harmed by group:\n")
print(round(m_harm*m_grps, 1))
print(round(m_harm, 4))

cat("\nDifferential racial harm:", diff((rowSums(m_harm*m_grps)/rowSums(m_grps))[-3]), "\n")
cat("Differential partisan harm:", abs(diff(colSums(m_harm*m_grps)/colSums(m_grps))), "\n")
cat("Total harm:", sum(m_harm*m_grps), "\n")
cat("Average total harmed:", sum(m_harm*m_grps) / sum(m_grps), "\n")
}

hh_prec_white = prec_harm(plans, dem, al_map$ndv_white, al_map$nrv_white,
                          elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))
hh_prec_black = prec_harm(plans, dem, al_map$ndv_black, al_map$nrv_black,
                          elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))
hh_prec_other = prec_harm(plans, dem, al_map$ndv_other, al_map$nrv_other,
                          elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))
harm_prec = (hh_prec_white[, 1] + hh_prec_black[, 1] + hh_prec_other[, 1]) /
    (al_map$ndv + al_map$nrv)


# plots out
al_sum = al_map %>%
    group_by(cd_2020) %>%
    summarize(is_coverage=TRUE) %>%
    mutate(dem = plans$dem[1:7])
d_harm = tibble(race = rep(rownames(m_harm), 2),
                party = rep(colnames(m_harm), each=3),
                Harmed = as.numeric(m_harm*m_grps),
                Total = as.numeric(m_grps))

p1 = plot(al_map, ndv / (ndv + nrv)) +
    geom_sf(data=al_sum, fill=NA, color="black", size=0.2, inherit.aes= FALSE) +
    scale_fill_party_c("Democratic\nshare", limits=c(0.3, 0.7)) +
    theme_repr_map() +
    labs(title="(a) Partisan patterns")
p2 = plot(al_map, vap_black / vap) +
    geom_sf(data=al_sum, fill=NA, color="white", size=0.15, inherit.aes= FALSE) +
    scale_fill_wa_c("sea", name="BVAP", labels=percent, limits=c(0, 1)) +
    theme_repr_map() +
    labs(title="(b) Racial demographics")
p3 = plot(al_map, harm_prec) +
    geom_sf(data=al_sum, fill=NA, color="white", size=0.15, inherit.aes= FALSE) +
    scale_fill_wa_c("forest_fire", name="Fraction of\nvoters harmed",
                    labels=percent, limits=c(0, 1)) +
    theme_repr_map() +
    theme(legend.title=element_text(vjust=1)) +
    labs(title="(c) Distribution of harm")
p4 = plot_cds(al_map, al_map$cd_2020, qty="bvap") +
    labs(title="(d) Challenged plan") +
    coord_sf(expand= FALSE) +
    theme_repr_map()
p5 = plot_cds(al_map, al_map$cd_pet_a, qty="bvap") +
    labs(title="(e) Remedial plan A") +
    guides(fill="none") +
    coord_sf(expand= FALSE) +
    theme_repr_map()
p6 = d_harm %>%
    filter(race != "Other") %>%
    pivot_longer(Harmed:Total, names_to="grp", values_to="radius") %>%
    arrange(desc(grp)) %>%
ggplot(aes(party, race, size=radius, color=grp)) +
    geom_point()  +
    scale_color_manual(values=wacolors$forest_fire[c(12, 1)], name=NULL) +
    scale_size_area(max_size=28, guide="none") +
    guides(color=guide_legend(override.aes=list(size=4))) +
    labs(x=NULL, y=NULL, title="(f) Distribution of harm") +
    theme_repr()

p = p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(heights=c(3, 2.2), widths=c(1, 1, 1.1)) &
    theme(plot.title = element_text(hjust = 0.5),
          plot.margin = unit(rep(0, 4), "cm"))
if (!file.exists(path <- here("paper/figures/al_maps.pdf"))) {
    ggsave(path, plot=p, width=7.5, height=4.5)
}


# Appendix ------
plans = alarmdata::alarm_50state_plans("AL", stats=FALSE, year=2020) %>%
    mutate(total_vap = tally_var(al_map, vap),
           vap_white = tally_var(al_map, vap_white),
           vap_black = tally_var(al_map, vap_black),
           dem = group_frac(al_map, ndv, ndv + nrv),
           pr_dem = ker_t(dem))

hh_white = partisan_harm(plans, dem, al_map$ndv_white, al_map$nrv_white,
                         elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))
hh_black = partisan_harm(plans, dem, al_map$ndv_black, al_map$nrv_black,
                         elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))
hh_other = partisan_harm(plans, dem, al_map$ndv_other, al_map$nrv_other,
                         elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))

m_harm = t(cbind(hh_white, hh_black, hh_other)[-3, ]) %>%
    `rownames<-`(c("White", "Black", "Other")) %>%
    `colnames<-`(c("Dem.", "Rep."))
m_grps = with(al_map, matrix(c(
    sum(ndv_white),
    sum(ndv_black),
    sum(ndv_other),
    sum(nrv_white),
    sum(nrv_black),
    sum(nrv_other)
), nrow=3)) %>%
    `rownames<-`(c("White", "Black", "Other")) %>%
    `colnames<-`(c("Dem.", "Rep."))

{
    cat("Average total harmed by group:\n")
    print(round(m_harm*m_grps, 1))
    print(round(m_harm, 4))

    cat("\nDifferential racial harm:", diff((rowSums(m_harm*m_grps)/rowSums(m_grps))[-3]), "\n")
    cat("Differential partisan harm:", abs(diff(colSums(m_harm*m_grps)/colSums(m_grps))), "\n")
    cat("Total harm:", sum(m_harm*m_grps), "\n")
    cat("Average total harmed:", sum(m_harm*m_grps) / sum(m_grps), "\n")
}

hh_prec_white = prec_harm(plans, dem, al_map$ndv_white, al_map$nrv_white,
                          elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))
hh_prec_black = prec_harm(plans, dem, al_map$ndv_black, al_map$nrv_black,
                          elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))
hh_prec_other = prec_harm(plans, dem, al_map$ndv_other, al_map$nrv_other,
                          elec_model_spec, idx_1=1, idx_2=1+seq_len(N_sim))
harm_prec = (hh_prec_white[, 1] + hh_prec_black[, 1] + hh_prec_other[, 1]) /
    (al_map$ndv + al_map$nrv)


# plots out
al_sum = al_map %>%
    group_by(cd_2020) %>%
    summarize(is_coverage=TRUE) %>%
    mutate(dem = plans$dem[1:7])
d_harm = tibble(race = rep(rownames(m_harm), 2),
                party = rep(colnames(m_harm), each=3),
                Harmed = as.numeric(m_harm*m_grps),
                Total = as.numeric(m_grps))

p3 = plot(al_map, harm_prec) +
    geom_sf(data=al_sum, fill=NA, color="white", size=0.15, inherit.aes= FALSE) +
    scale_fill_wa_c("forest_fire", name="Fraction of\nvoters harmed",
                    labels=percent, limits=c(0, 1)) +
    theme_repr_map() +
    theme(legend.title=element_text(vjust=1)) +
    labs(title="(a) Distribution of harm")
p6 = d_harm %>%
    filter(race != "Other") %>%
    pivot_longer(Harmed:Total, names_to="grp", values_to="radius") %>%
    arrange(desc(grp)) %>%
    ggplot(aes(party, race, size=radius, color=grp)) +
    geom_point()  +
    scale_color_manual(values=wacolors$forest_fire[c(12, 1)], name=NULL) +
    scale_size_area(max_size=36, guide="none") +
    guides(color=guide_legend(override.aes=list(size=4))) +
    labs(x=NULL, y=NULL, title="(b) Distribution of harm") +
    theme_repr()

p = p3 + p6 +  theme(plot.title = element_text(hjust = 0.5))
if (!file.exists(path <- here("paper/figures/al_harm_sample.pdf"))) {
    ggsave(path, plot=p, width=7.5, height=3.75)
}


# extra figure for presentation
if (FALSE) {
    d_harm %>%
        filter(race != "Other") %>%
        mutate(lab = fct_rev(paste(race, party)),
               n = Harmed / Total) %>%
        # pivot_longer(Harmed:Total, names_to="grp", values_to="n") %>%
        # mutate(lab = fct_rev(paste(race, party)),
        #        grp = fct_rev(grp)) %>%
    ggplot(aes(lab, n)) +
        geom_col(width=0.8, fill=wacolors$forest_fire[2]) +
        # geom_col(position="fill", width=0.8) +
        # scale_fill_manual(values=wacolors$forest_fire[c(1, 12)], name=NULL) +
        scale_y_continuous("Proportion harmed", labels=percent, expand=expansion(c(0, 0.05))) +
        # guides(fill=guide_legend(override.aes=list(size=4))) +
        labs(x=NULL) +
        theme_repr() +
        # coord_cartesian(ylim=c(0, 0.25)) +
        theme(legend.position="bottom",
              axis.text.x=element_text(face="bold", color="black"))
    ggsave(here("pres/AL_50statesims_harm.pdf"), width=3.25, height=3)


    p = plot_cds(al_map, as.matrix(plans)[, 2], county, qty="bvap") +
        plot_cds(al_map, as.matrix(plans)[, 1000], county, qty="bvap") +
        plot_cds(al_map, as.matrix(plans)[, 2000], county, qty="bvap") +
        plot_cds(al_map, as.matrix(plans)[, 3000], county, qty="bvap") +
        plot_cds(al_map, as.matrix(plans)[, 4000], county, qty="bvap") +
        plot_cds(al_map, as.matrix(plans)[, 5000], county, qty="bvap") +
        plot_layout(nrow=2, guides="collect")
    ggsave(here("pres/AL_50statesims.pdf"), plot=p, width=6, height=5)
}

# additional extra figures for presentations ----
if (FALSE) {
  cd_a <- plot_cds(al_map, al_map$cd_pet_a, qty = 'bvap') +
    labs(title = 'Remedial plan A') +
    coord_sf(expand = FALSE) +
    theme_repr_map()

  cd_b <- plot_cds(al_map, al_map$cd_pet_b, qty = 'bvap') +
    labs(title = 'Remedial plan B') +
    coord_sf(expand = FALSE) +
    theme_repr_map()

  cd_c <- plot_cds(al_map, al_map$cd_pet_c, qty = 'bvap') +
    labs(title = 'Remedial plan C') +
    coord_sf(expand = FALSE) +
    theme_repr_map()

  cd_d <- plot_cds(al_map, al_map$cd_pet_d, qty = 'bvap') +
    labs(title = 'Remedial plan D') +
    coord_sf(expand = FALSE) +
    theme_repr_map()

  cd_a + cd_b + cd_c + cd_d +
    plot_layout(heights = c(3, 3), widths = c(1.75, 1.75), guides = 'collect') &
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.margin = unit(rep(0, 4), 'cm')
    )

  ggsave(here('pres/al_remedial_maps.png'), width = 4, height = 5)
}
