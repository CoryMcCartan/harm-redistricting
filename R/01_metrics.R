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

#' Voter utility for local representation
#'
#' @param map a `redist_map` object.
#' @param group_pop column of `map` containing the group population of each precinct.
#' @param distr_grp a matrix from `district_group()`
#' @param kernel a vectorized function mapping [0, 1] to [0, 1]
#' @param invert if `TRUE`, calculate utility for the out-group
#'
#' @returns a numeric vector
utility_local = function(map, group_pop, distr_grp, kernel=k_step, invert=FALSE) {
    m = kernel(distr_grp)
    if (invert) m = 1 - m
    voters = eval_tidy(enquo(group_pop), map)
    as.numeric(voters %*% m) / sum(voters)
}

#' Voter utility for global representation
#'
#' @param plans a `redist_plans` object.
#' @param group column of `plans` containing the group share of each district.
#' @param invert if `TRUE`, calculate utility for the out-group
#' @param fn_util the utility function. Defaults to a logarithm
#'
#' @returns a numeric vector
utility_global = function(plans, group, kernel=k_step, invert=FALSE, fn_util=flog) {
    n_dists = max(plans$district)
    pl = plans %>%
        group_by(draw) %>%
        summarize(n_grp = sum(kernel({{ group }})))
    if (invert)
        pl$n_grp = n_dists - pl$n_grp
    fn_util(pl$n_grp)
}

#' Voter harm for local representation, version 1 (nonlinear)
#'
#' @param map a `redist_map` object.
#' @param group_pop column of `map` containing the group population of each precinct.
#' @param distr_grp a matrix from `district_group()`
#' @param idx_1,idx_2 indices for each of the comparison groups
#' @param kernel a vectorized function mapping [0, 1] to [0, 1]
#' @param invert if `TRUE`, calculate utility for the out-group
#'
#' @returns a numeric vector matching `idx_1`
harm_v1 = function(map, group_pop, distr_grp,
                   idx_1=seq_len(ncol(distr_grp)), idx_2=seq_len(ncol(distr_grp)),
                   kernel=k_step, invert=FALSE) {
    m = 1 - kernel(distr_grp[, idx_1, drop=FALSE])
    counterfactual = rowMeans(kernel(distr_grp[, idx_2, drop=FALSE]))
    if (invert) {
        m = 1 - m
        counterfactual = 1 - counterfactual
    }
    voters = eval_tidy(enquo(group_pop), map)
    as.numeric(voters %*% (m * counterfactual)) / sum(voters)
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
    m = 1 - kernel(distr_grp[, idx_1, drop=FALSE])
    counterfactual = rowMeans(kernel(distr_grp[, idx_2, drop=FALSE]))
    if (invert) {
        m = 1 - m
        counterfactual = 1 - counterfactual
    }
    rowMeans(m * counterfactual)
}

#' Voter harm for local representation, version 2 (linear)
#'
#' @param map a `redist_map` object.
#' @param group_pop column of `map` containing the group population of each precinct.
#' @param distr_grp a matrix from `district_group()`
#' @param idx_1,idx_2 indices for each of the comparison groups
#' @param kernel a vectorized function mapping [0, 1] to [0, 1]
#' @param invert if `TRUE`, calculate utility for the out-group
#'
#' @returns a numeric vector matching `idx_1`
harm_v2 = function(map, group_pop, distr_grp,
                   idx_1=seq_len(ncol(distr_grp)), idx_2=seq_len(ncol(distr_grp)),
                   kernel=k_step, invert=FALSE) {
    util_1 = utility_local(map, !!enquo(group_pop),
                           distr_grp[, idx_1, drop=FALSE], kernel, invert)
    util_2 = utility_local(map, !!enquo(group_pop),
                           distr_grp[, idx_2, drop=FALSE], kernel, invert)
    mean(util_2) - util_1
}

#' Calculate total utility from by-group utilities
#'
#' @param util_1,util_2 numeric vectors of group utilities
#' @param map a `redist_map` object.
#' @param group_1,group_2 column of `map` containing the group populations of each precinct.
#'
#' @returns a numeric vector
total = function(util_1, util_2, map, group_1, group_2) {
    voters_1 = sum(eval_tidy(enquo(group_1), map))
    voters_2 = sum(eval_tidy(enquo(group_2), map))
    (util_1 * voters_1 + util_2 * voters_2) / (voters_1 + voters_2)
}
