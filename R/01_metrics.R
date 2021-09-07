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
#' @param ker_mean a vectorized function returning the mean x value under the kernel distribution on the interval [a, b]
#'
#' @returns a numeric vector
utility_local = function(map, group_pop, other_pop, distr_grp, kernel=k_step, ker_mean=e_approx) {
    m = kernel(distr_grp)
    vote_a = eval_tidy(enquo(group_pop), map)
    vote_b = eval_tidy(enquo(other_pop), map)
    voters = vote_a + vote_b
    share = vote_a / (voters + 1e-6)

    if (identical(kernel, k_step)) {
        as.numeric(vote_a %*% m + vote_b %*% (1 - m)) / sum(voters)
    } else {
        u_a = as.numeric(vote_a %*% m)
        u_b = as.numeric(vote_b %*% (1 - m))
        # compute adjustment for vote shifts
        rdgrp = round(distr_grp, 3)
        vals = unique(as.numeric(rdgrp))
        lookup = match(rdgrp, vals)

        adj_a = ker_mean(-qlapl(vals), rep(Inf, length(vals))) * kernel(vals)
        adj_b = ker_mean(qlapl(vals), rep(Inf, length(vals))) * kernel(1-vals)
        slope = 1 - 2*abs(share - 0.5)
        adj_a = as.numeric((voters * slope) %*% matrix(adj_a[lookup], nrow=nrow(distr_grp)))
        adj_b = as.numeric((voters * slope) %*% matrix(adj_b[lookup], nrow=nrow(distr_grp)))

        (u_a + u_b + adj_a + adj_b) / sum(voters)
    }
}

#' Voter utility for global representation
#'
#' @param plans a `redist_plans` object.
#' @param group column of `plans` containing the group share of each district.
#' @param statewide the statewide vote
#' @param fn_util the utility function. Defaults to a logarithm
#' @param ker_mean a vectorized function returning the mean x value under the kernel distribution on the interval [a, b]
#'
#' @returns a numeric vector
utility_global = function(plans, group, kernel=k_step, statewide, fn_util=flog, ker_mean=e_approx) {
    n_dists = max(plans$district)
    distr_dem = matrix(eval_tidy(enquo(group), plans), nrow=n_dists)
    dist_seq_a = fn_util(seq(n_dists, 0L, -1L))
    dist_seq_b = fn_util(seq(0L, n_dists))
    apply(distr_dem, 2, function(x) {
        x = sort(x)
        pr_bins = diff(c(0, kernel(x), 1)) # pr of each seat combo
        # avg d vote share for each seat combo
        if (identical(kernel, k_step)) {
            share_a = rep(statewide, n_dists+1L)
        } else {
            y = c(-Inf, qlapl(x - statewide + 0.5), Inf)
            share_a = plapl(ker_mean(y[-n_dists-2L], y[-1])) - 0.5 + statewide
        }
        sum(dist_seq_a * pr_bins * share_a + dist_seq_b * pr_bins * (1 - share_a))
    })
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
    as.numeric(voters %*% pmax(invert * harm_benefit, 0)) / sum(voters)
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
    voters * rowMeans(pmax(invert * harm_benefit, 0))
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


# plans calculator
calc_plans_stats = function(plans, map, dem, gop, ker=k_t()) {
    dvote = eval_tidy(enquo(dem), map)
    rvote = eval_tidy(enquo(gop), map)
    statewide = sum(dvote) / (sum(dvote) + sum(rvote))
    ndists = attr(map, "ndists")

    plans = plans %>%
        mutate(dev = plan_parity(map),
               comp = distr_compactness(map),
               dem = group_frac(map, dvote, dvote+rvote),
               competitive = rep(redist:::talisman(matrix(dem, nrow=ndists), ndists), each=ndists),
               egap = partisan_metrics(map, "EffGap", rvote, dvote))

    m_dem = district_group(plans, dem)

    pl_sum = plans %>%
        group_by(draw) %>%
        summarize(n_dem = sum(dem > 0.5),
                  e_dem = sum(k_t()(dem)),
                  pbias = mean(ker(pmax(pmin(dem - (statewide - 0.5), 1), 0))) - 0.5,
                  pbias_sw = e_dem/ndists - statewide,
                  mean_med = median(dem) - mean(dem),
                  across(c(dev, comp, competitive:pbias), ~ .[1])) %>%
        mutate(u_loc = utility_local(map, dvote, rvote, m_dem, ker),
               u_glb = utility_global(plans, dem, ker, statewide),
               h_dem = harm(map, dvote, m_dem, kernel=ker),
               h_rep = harm(map, rvote, m_dem, kernel=ker, invert=TRUE),
               f = h_dem - h_rep,
               h = total(h_dem, h_rep, map, dvote, rvote))

    list(distr=select(plans, draw:total_pop, dem), plan=pl_sum, mat=m_dem)
}
