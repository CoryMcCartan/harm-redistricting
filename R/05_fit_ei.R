library(cmdstanr)
library(posterior)
library(tidybayes)

fit_ei = function(tbls, recompile=FALSE, algorithm="vb",
                  chains=4, warmup=1000, iter=500, adapt_delta=0.8, init=0, refresh=100, ...) {
    stan_d = list(
        N = nrow(tbls$votes),
        L = ncol(tbls$votes),
        R = ncol(tbls$race),

        vap = tbls$vap,
        votes = tbls$votes,
        dem_votes = tbls$dem_votes,
        vap_race = tbls$race
    )

    path_model = here("R/ei.stan")
    path_exc = here("R/ei")

    sm = cmdstan_model(path_model, compile=F)
    if (file.exists(path_exc)) {
        if (isTRUE(recompile) || file.info(path_exc)["mtime"] < file.info(path_model)["mtime"]) {
            file.remove(path_exc)
        }
    }
    sm$compile()

    if (algorithm == "hmc") {
        fit = sm$sample(data=stan_d, chains=chains,
                        iter_warmup=warmup, iter_sampling=iter,
                        init=init, refresh=refresh, adapt_delta=adapt_delta,...)
    } else if (algorithm == "vb") {
        fit = sm$variational(data=stan_d, algorithm="meanfield", init=init, ...)
    } else {
        stop("Algorithm should be `hmc` or `vb`.")
    }

    vars = c("lp__", "turnout_overall", "turnout_elec", "turnout", "L_t", "sigma_t",
             "support_overall", "support_elec", "support", "L_s", "sigma_s")
    draws = as_draws_rvars(fit$draws(variables=vars))

    draws_dim = dim(draws_of(draws$lp__, with_chains=TRUE))
    races = colnames(tbls$race)

    names(draws$turnout_overall) = races
    colnames(draws$turnout) = races
    names(draws$turnout_elec) = tbls$elecs
    rownames(draws$L_t) = races
    names(draws$sigma_t) = races

    names(draws$support_overall) = races
    colnames(draws$support) = races
    names(draws$support_elec) = tbls$elecs
    rownames(draws$L_s) = races
    names(draws$sigma_s) = races

    attr(draws, "sm") = fit
    draws
}


fit_lfei = function(tbls, n_comp=2L, id=NULL, recompile=FALSE, algorithm="vb",
                    chains=4, warmup=1000, iter=500, adapt_delta=0.8, init=0, ...) {
    n_comp = as.integer(n_comp)
    K = length(tbls$cands)

    # convert ID conditions
    loading_loc = rep(0, K*n_comp)
    loading_cov = diag(K*n_comp)
    get_idx = function(nm, q=seq_len(n_comp)) {
        match(nm, tbls$cands) + (q-1)*K
    }
    for (i in seq_along(id$loc)) {
        idx = get_idx(names(id$loc)[i])
        loading_loc[idx] = id$loc[[i]]
    }
    for (i in seq_along(id$scale)) {
        idx = get_idx(names(id$scale)[i])
        diag(loading_cov)[idx] = id$scale[[i]]^2
    }
    for (i in seq_along(id$corr)) {
        i1 = get_idx(id$corr[[i]][[1]])
        i2 = get_idx(id$corr[[i]][[2]])
        new_cor = sqrt(diag(loading_cov)[i1] * diag(loading_cov)[i2]) * id$corr[[i]][[3]]
        for (j in seq_len(n_comp)) {
            loading_cov[i1[j], i2[j]] = new_cor[j]
            loading_cov[i2[j], i1[j]] = new_cor[j]
        }
    }

    eig = eigen(loading_cov)
    if (any(eig$values <= 0)) {
        message("Provided identification prior is not positive semidefinite; taking nearest approximation.")
        D = diag(pmax(eig$values, 1e-2))
        loading_cov = eig$vectors %*% D %*% t(eig$vectors)
    }

    stan_d = list(
        Q = n_comp,
        N = nrow(tbls$votes),
        K = K,
        L = ncol(tbls$votes),
        R = ncol(tbls$race),

        vap = tbls$vap,
        votes = tbls$votes,
        vote_share = tbls$vote_share,
        vap_race = tbls$race,
        elec = as.integer(tbls$elecs),

        loading_loc = loading_loc,
        loading_cov = loading_cov
    )

    path_model = here("R/lfei.stan")
    path_exc = here("R/lfei")

    sm = cmdstan_model(path_model, compile=F)
    if (file.exists(path_exc)) {
        if (isTRUE(recompile) || file.info(path_exc)["mtime"] < file.info(path_model)["mtime"]) {
            file.remove(path_exc)
        }
    }
    sm$compile()

    if (algorithm == "hmc") {
        fit = sm$sample(data=stan_d, chains=chains,
                        iter_warmup=warmup, iter_sampling=iter,
                        init=init, refresh=100, adapt_delta=adapt_delta,...)
    } else if (algorithm == "vb") {
        fit = sm$variational(data=stan_d, algorithm="meanfield", init=init, ...)
    } else {
        stop("Algorithm should be `hmc` or `vb`.")
    }

    vars = c("lp__", "turnout_overall", "turnout_elec", "turnout", "L_t", "sigma_t",
             "support", "alpha", "loading", "pref", "scale", "L_p", "sigma_p", "err_mult",
             "post_share_prec", "post_share_race", "post_turn", "post_factor")
    draws = as_draws_rvars(fit$draws(variables=vars))

    # fix identification issues
    load_m = matrix(loading_loc, ncol=n_comp)
    id_sign = rfun(function(l) {
        agree = colSums(l * load_m) / colSums(abs(load_m))
        sign(agree)
    })
    signs = id_sign(draws$loading)

    draws_dim = dim(draws_of(draws$err_mult, with_chains=TRUE))
    races = colnames(tbls$race)
    names(draws$support) = tbls$cands

    dim(signs) = c(1, n_comp)
    draws$loading = draws$loading * signs
    rownames(draws$loading) = tbls$cands

    draws$alpha = c(rvar(array(1, dim=draws_dim), with_chains=TRUE), draws$alpha)
    names(draws$alpha) = tbls$cands
    names(draws$turnout_overall) = races
    colnames(draws$turnout) = races

    dim(signs) = c(1, n_comp, 1)
    draws$pref = draws$pref * signs
    dimnames(draws$pref) = list(NULL, NULL, races)

    dim(signs) = c(1, n_comp)
    draws$post_factor = draws$post_factor * signs

    rownames(draws$L_p) = races
    rownames(draws$L_t) = races
    names(draws$sigma_p) = races
    names(draws$sigma_t) = races

    colnames(draws$post_share_race) = races
    rownames(draws$post_share_race) = tbls$cands
    colnames(draws$post_share_prec) = tbls$cands

    class(draws) = c("fit_lfei", class(draws))
    attr(draws, "sm") = fit
    attr(draws, "loading_loc") = loading_loc
    attr(draws, "loading_cov") = loading_cov
    draws
}

# generics ----
print.fit_lfei = function(x, load_len=NULL, corr=FALSE) {
    n_comp = dim(x$scale)
    cat("A latent factor ecological inference model fit with",
        n_comp, "components.\n\n")

    comp_scale = rev(median(x$scale^2))
    cat("Component variance explained:\n")
    cat(percent(comp_scale/sum(comp_scale), 0.1), "\n", sep="  ")
    cat(percent(cumsum(comp_scale)/sum(comp_scale), 0.1), "\n\n", sep="  ")

    cat("Turnout estimates: ")
    print(x$turnout_overall)

    cat("\nCandidate ordering on components:\n")
    m = median(x$loading)
    rownames(m) = abbreviate(str_to_title(str_replace(rownames(m), "_", " ")), 6)
    if (is.null(load_len)) load_len = nrow(m) + 3L
    breaks = seq(min(m), max(m), length.out=load_len)
    m_bins = apply(m, 2, function(x) cut(x, breaks, labels=FALSE, include.lowest=TRUE))
    bin_0 = cut(0, breaks, labels=F)
    m_out = matrix("", nrow=length(breaks), ncol=n_comp)
    for (i in seq_len(n_comp)) {
        placements = tapply(rownames(m), m_bins[, i], paste, collapse=" ")
        for (j in seq_along(placements)) {
            m_out[as.integer(names(placements)[j]), i] = placements[j]
        }
    }
    widths = apply(m_out, 2, \(x) max(nchar(x)))
    for (i in seq_len(nrow(m_out))) {
        for (j in seq_len(n_comp)) {
            if (i == bin_0) cat("0|") else cat(" |")
            cat(str_pad(m_out[i, j], width=widths[j]+1L, side="right",
                        pad = if (i == bin_0) "-" else " "))
        }
        cat("\n")
    }

    if (isTRUE(corr)) {
        cat("\nRace correlation:\n")
        m_t = median(x$L_t %**% t(x$L_t))
        m_p = median(x$L_p %**% t(x$L_p))
        colnames(m_t) = c("turnout", rep("", ncol(m_t)-1))
        colnames(m_p) = c("loading", rep("", ncol(m_p)-1))
        print(round(cbind(m_t, m_p), n_comp))
    }
}


predict.fit_lfei = function(object, support, loading, tbls, recompile=FALSE) {
    n_comp = length(fit$scale)
    stopifnot(ncol(loading) == n_comp)

    lsupport = rdo(qlogis(support))

    stan_d = list(
        Q = n_comp,
        N = nrow(tbls$votes),
        K = nrow(loading),
        R = ncol(tbls$race),

        vap = tbls$vap,
        vap_race = tbls$race,

        loading = loading
    )

    path_model = here("R/lfei_gq.stan")
    path_exc = here("R/lfei_gq")

    sm = cmdstan_model(path_model, compile=F)
    if (file.exists(path_exc)) {
        if (isTRUE(recompile) || file.info(path_exc)["mtime"] < file.info(path_model)["mtime"]) {
            file.remove(path_exc)
        }
    }
    sm$compile()

    draws_obj = c(list(support=lsupport),
                  object[c("turnout", "scale", "pref", "err_mult")])
    colnames(draws_obj$turnout) = NULL
    dimnames(draws_obj$pref) = list(NULL, NULL, NULL)

    gqs = sm$generate_quantities(posterior::as_draws_array(draws_obj),
                                 data=stan_d, sig_figs=4)

    votes = as_draws_rvars(gqs$draws())$votes
    dimnames(votes) = list(NULL, colnames(tbls$race), rownames(loading))

    votes
}



save_fit = function(fit, path, compress="xz") {
    attr(fit, "sm") = NULL
    write_rds(fit, path, compress=compress)
}



