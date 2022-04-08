library(cmdstanr)
library(posterior)

fit_pcei = function(tbls, n_comp=2L, id=NULL, recompile=FALSE, algorithm="vb",
                    chains=4, warmup=500, iter=250, adapt_delta=0.95, ...) {
    stan_d = list(
        N = nrow(tbls$votes),
        K = length(tbls$cands),
        L = ncol(tbls$votes),
        R = ncol(tbls$race),

        vap = tbls$vap,
        votes = tbls$votes,
        vote_share = tbls$vote_share,
        vap_race = tbls$race,
        elec = as.integer(tbls$elecs),

        Q = as.integer(n_comp),
        n_id = nrow(id),
        id_idx = match(id$cand, tbls$cands),
        id_loc = do.call(rbind, id$loc)[, seq_len(nrow(id)), drop=F],
        id_scale = do.call(rbind, id$scale)[, seq_len(nrow(id)), drop=F]
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
                        init=0, refresh=50, adapt_delta=adapt_delta, ...)
    } else if (algorithm == "vb") {
        fit = sm$variational(data=stan_d, algorithm="meanfield", init=0, ...)
    } else {
        stop("Algorithm should be `hmc` or `vb`.")
    }

    draws = as_draws_rvars(fit$draws())

    draws_dim = dim(draws_of(draws$err_mult))
    races = colnames(tbls$race)
    names(draws$support) = tbls$cands
    rownames(draws$loading) = tbls$cands
    draws$alpha = c(rvar(array(1, dim=draws_dim), with_chains=TRUE), draws$alpha)
    names(draws$alpha) = tbls$cands
    names(draws$turnout_overall) = races
    colnames(draws$turnout) = races
    dimnames(draws$pref_geo) = list(NULL, NULL, races)
    rownames(draws$L_p) = races
    rownames(draws$L_t) = races
    names(draws$sigma_p) = races
    names(draws$sigma_t) = races

    class(draws) = c("fit_pcei", class(draws))
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
        m_t = median(draws$L_t %**% t(draws$L_t))
        m_p = median(draws$L_p %**% t(draws$L_p))
        colnames(m_t) = c("turnout", rep("", ncol(m_t)-1))
        colnames(m_p) = c("loading", rep("", ncol(m_p)-1))
        print(round(cbind(m_t, m_p), 2))
    }
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
