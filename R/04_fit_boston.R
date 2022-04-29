library(cmdstanr)
library(posterior)
library(tidybayes)


fit_pcei = function(tbls, n_comp=2L, id=NULL, recompile=FALSE, algorithm="vb",
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

    path_model = here("R/pcei.stan")
    path_exc = here("R/pcei")

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

    class(draws) = c("fit_pcei", class(draws))
    attr(draws, "sm") = fit
    attr(draws, "loading_loc") = loading_loc
    attr(draws, "loading_cov") = loading_cov
    draws
}

# generics ----
print.fit_pcei = function(x, load_len=NULL, corr=FALSE) {
    n_comp = dim(x$scale)
    cat("A principal components ecological inference model fit with",
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
        print(round(cbind(m_t, m_p), 2))
    }
}


predict.fit_pcei = function(object, support, loading, tbls, recompile=FALSE) {
    n_comp = length(fit$scale)
    stopifnot(ncol(loading) == n_comp)


    lsupport = qlogis(support)

    stan_d = list(
        Q = n_comp,
        N = nrow(tbls$votes),
        K = nrow(loading),
        R = ncol(tbls$race),

        vap = tbls$vap,
        vap_race = tbls$race,

        loading = loading,
        support = lsupport
    )

    path_model = here("R/pcei_gq.stan")
    path_exc = here("R/pcei_gq")

    sm = cmdstan_model(path_model, compile=F)
    if (file.exists(path_exc)) {
        if (isTRUE(recompile) || file.info(path_exc)["mtime"] < file.info(path_model)["mtime"]) {
            file.remove(path_exc)
        }
    }
    sm$compile()

    draws_obj = object[c("turnout", "scale", "pref", "err_mult")]
    colnames(draws_obj$turnout) = NULL
    dimnames(draws_obj$pref) = list(NULL, NULL, NULL)

    gqs = sm$generate_quantities(posterior::as_draws_array(draws_obj),
                                 data=stan_d, sig_figs=4)

    votes = as_draws_rvars(gqs$draws())$votes
    dimnames(votes) = list(NULL, colnames(tbls$race), rownames(loading))

    votes
}


rot_mat = function(deg) {
    deg = deg * pi/180
    matrix(c(cos(deg), sin(deg), -sin(deg), cos(deg)), 2, 2)
}

save_fit = function(fit, path, compress="xz") {
    attr(fit, "sm") = NULL
    write_rds(fit, path, compress=compress)
}


# Data prep functions ----

make_votes_long = function(d) {
    d %>%
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
}

make_tables = function(d_votes, cand_thresh=0.01) {
    m_votes = d_votes %>%
        arrange(precinct, candidate) %>%
        group_by(precinct, elec) %>%
        mutate(vs = votes / sum(1e-6 + votes)) %>%
        ungroup() %>%
        with(., matrix(vs, nrow=n_distinct(precinct), byrow=T)) %>%
        `colnames<-`(levels(d_votes$candidate))
    d_sum = d_votes %>%
        group_by(elec, candidate) %>%
        summarize(votes = sum(votes)) %>%
        group_by(elec) %>%
        mutate(vs = round(votes / sum(votes), 3)) %>%
        ungroup() %>%
        mutate(elec = as.factor(elec))

    m_prec = d_votes %>%
        group_by(precinct, elec) %>%
        summarize(votes = sum(votes), .groups="drop") %>%
        with(., matrix(votes, nrow=n_distinct(precinct), byrow=T))
    colnames(m_prec) = levels(d_votes$elec)

    m_race = d %>%
        as_tibble() %>%
        mutate(vap_other = vap - vap_white - vap_black - vap_hisp,
               across(starts_with("vap_"), ~ . / vap)) %>%
        select(vap_white:vap_hisp, vap_other) %>%
        as.matrix()
    colnames(m_race) = str_sub(colnames(m_race), 5)

    big_cands = which(d_sum$vs > 0.01)

    list(vap = d$vap,
         votes = m_prec,
         vote_share = m_votes[, big_cands],
         race = m_race,
         summary = d_sum[big_cands, ],
         cands = colnames(m_votes)[big_cands],
         elecs = d_sum$elec[big_cands])
}
