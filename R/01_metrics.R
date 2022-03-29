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
    m = as.matrix(plans)
    m_grp = arrange(plans, as.integer(draw), district) %>%
        pull({{ group }}) %>%
        matrix(nrow=max(plans$district))
    m_prec = matrix(nrow=nrow(m), ncol=ncol(m))
    for (i in seq_len(ncol(m))) {
        m_prec[, i] = m_grp[, i][m[, i]]
    }
    m_prec
}

#' Voter harm for local representation
#'
#' @param map a `redist_map` object.
#' @param group_pop column of `map` containing the group population of each precinct.
#' @param distr_grp a matrix from `district_group()`
#' @param idx_1,idx_2 indices for each of the comparison groups
#' @param kernel a vectorized function mapping [0, 1] to [0, 1]
#' @param invert if `TRUE`, calculate utility for the out-group
#'
#' @returns a numeric vector matching `idx_1`
harm = function(map, group_pop, distr_grp,
                   idx_1=seq_len(ncol(distr_grp)), idx_2=seq_len(ncol(distr_grp)),
                   kernel=k_step, invert=FALSE) {
    m = kernel(distr_grp[, idx_1, drop=FALSE])
    counterfactual = rowMeans(kernel(distr_grp[, idx_2, drop=FALSE]))
    harm_benefit = counterfactual - m
    invert = 1L - invert*2L
    voters = eval_tidy(enquo(group_pop), map)
    as.numeric(voters %*% pos_part(invert * harm_benefit)) / sum(voters)
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
                     kernel=k_step, invert=FALSE) {
    m = kernel(distr_grp[, idx_1, drop=FALSE])
    counterfactual = rowMeans(kernel(distr_grp[, idx_2, drop=FALSE]))
    harm_benefit = counterfactual - m
    invert = 1L - invert*2L
    voters = eval_tidy(enquo(group_pop), map)
    voters * rowMeans(pos_part(invert * harm_benefit))
}


# plans calculator
calc_plans_stats = function(plans, map, dem, gop, ker=k_t()) {
    dvote = eval_tidy(enquo(dem), map)
    rvote = eval_tidy(enquo(gop), map)
    statewide = sum(dvote) / (sum(dvote) + sum(rvote))
    ndists = attr(map, "ndists")
    n_ref = redist:::get_n_ref(plans)
    idx_2 = if (n_ref > 1) -seq_len(n_ref) else seq_len(ncol(as.matrix(plans)))

    plans = plans %>%
        mutate(dev = plan_parity(map),
               comp = distr_compactness(map),
               dem = group_frac(map, dvote, dvote+rvote),
               egap = partisan_metrics(map, "EffGap", rvote, dvote))

    m_dem = district_group(plans, dem)

    pl_sum = plans %>%
        group_by(draw) %>%
        summarize(n_dem = sum(dem > 0.5),
                  e_dem = sum(k_t()(dem)),
                  pbias = mean(ker(pos_part(pmin(dem - (statewide - 0.5), 1)))) - 0.5,
                  pbias_sw = e_dem/ndists - statewide,
                  mean_med = median(dem) - mean(dem),
                  across(c(dev, comp, competitive:pbias), ~ .[1])) %>%
        mutate(h_dem = harm(map, dvote, m_dem, idx_2=idx_2, kernel=ker),
               h_rep = harm(map, rvote, m_dem, idx_2=idx_2, kernel=ker, invert=TRUE),
               dh = h_dem - h_rep,
               h = total(h_dem, h_rep, map, dvote, rvote))

    list(distr=select(plans, draw:total_pop, dem), plan=pl_sum, mat=m_dem)
}
