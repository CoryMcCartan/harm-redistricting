#' Democratic share of district
#'
#' Returns a matrix of precincts by plans, where each entry is the Democratic
#' share in the district the precinct belongs to in that plan.
#'
#' @param plans a `redist_plans` object.
#' @param group column of `plans` containing the group share of each district.
#'
#' @returns a matrix
district_group = function(plans, group) {
    m_grp = matrix(eval_tidy(enquo(group), plans),
                   nrow=attr(plans, "ndists"))
    mat_by_prec(as.matrix(plans), m_grp)
}

#' Partisan harm
#'
#' @param plans a `redist_plans` object.
#' @param dem_share column of `map` containing the Democratic share of each precinct.
#' @param dvote the Democratic vote in each precinct
#' @param rvote the Republican vote in each precinct
#' @param elec_model_spec A list with elements `year` and `resid` containing the
#'   standard deviation for year effects (constant across districts) and
#'   residual effects (vary by district)
#' @param idx_1 indices of the district to evaluate the harm form
#' @param idx_2 indices of the counterfactual districts
#'
#' @returns a numeric matrix matching `idx_1`: first row is harm to in-group,
#'   second row is out-group, third row is total harm.
partisan_harm = function(plans, dem_share, dvote, rvote,
                         elec_model_spec=list(distr=0.1586, year=0.02659, resid=0.079858),
                         idx_1=seq_len(ncol(as.matrix(plans))),
                         idx_2=seq_len(ncol(as.matrix(plans)))) {
    ndists = attr(plans, "ndists")
    m_dem = matrix(eval_tidy(enquo(dem_share), plans), nrow=ndists)
    shift_elec = rnorm(length(idx_2), sd=elec_model_spec$year)
    shift_distr_1 = matrix(rnorm(length(idx_2)*ndists, sd=elec_model_spec$resid), nrow=ndists)
    shift_distr_2 = matrix(rnorm(length(idx_2)*ndists, sd=elec_model_spec$resid), nrow=ndists)

    harm_helper(as.matrix(plans), m_dem, dvote, rvote, shift_elec,
                shift_distr_1, shift_distr_2, idx_1, idx_2)
}

#' Voter harm for local representation, by precinct
#'
#' @param map a `redist_map` object.
#' @param group_pop column of `map` containing the group population of each precinct.
#' @param distr_grp a matrix from `district_group()`
#' @param idx_1,idx_2 indices for each of the comparison groups
#' @param kernel a vectorized function mapping [0, 1] to [0, 1]
#' @param invert if `TRUE`, calculate utility for the out-group
#'
#' @returns a numeric vector matching `map`, showing harm averaged over `idx_1`
prec_harm = function(map, group_pop, distr_grp,
                     idx_1=seq_len(ncol(distr_grp)), idx_2=seq_len(ncol(distr_grp)),
                     ker=k_step, invert=FALSE) {
    m = district_group(plans, ker(!!group_share))
    voters = eval_tidy(enquo(group_pop), map)
    invert = 1L - invert*2L
    rowMeans(apply(m, 2, function(m_pl) {
        harm_benefit = m[, idx_2] - m_pl
        voters * pos_part(invert * harm_benefit)
    }))
}


# plans calculator
calc_plans_stats = function(plans, map, dem, gop, elec_model_spec=elec_model_spec, max_harm=500L) {
    dvote = eval_tidy(enquo(dem), map)
    rvote = eval_tidy(enquo(gop), map)
    statewide = sum(dvote) / (sum(dvote) + sum(rvote))
    ndists = attr(map, "ndists")
    n_ref = redist:::get_n_ref(plans)
    idx_2 = n_ref + seq_len(min(ncol(as.matrix(plans)) - n_ref, max_harm))

    plans = plans %>%
        mutate(dev = plan_parity(map),
               comp = distr_compactness(map),
               dem = group_frac(map, dvote, dvote+rvote),
               decl = part_decl(., map, dvote, rvote),
               egap = part_egap(., map, dvote, rvote))

    ker_t = k_t(sd=with(elec_model_spec, sqrt(year^2 + resid^2)))
    pl_sum = plans %>%
        group_by(draw) %>%
        summarize(n_dem = sum(dem > 0.5),
                  e_dem = sum(ker_t(dem)),
                  pbias = mean(ker_t(pos_part(pmin(dem - (statewide - 0.5), 1)))) - 0.5,
                  mean_med = median(dem) - mean(dem),
                  across(c(dev, comp, decl, egap), ~ .[1]))
    hh = partisan_harm(plans, dem, dvote, rvote, elec_model_spec, idx_2=idx_2)
    pl_sum$h_dem = hh[1, ]
    pl_sum$h_rep = hh[2, ]
    pl_sum$dh = hh[1, ] - hh[2, ]
    pl_sum$h = hh[3, ]

    list(distr=select(plans, draw:total_pop, dem), plan=pl_sum)
}
