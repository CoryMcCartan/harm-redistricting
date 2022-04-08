/***************************************************
* Principal Components Ecological Inference Model *
* (c) 2022 Cory McCartan and Christopher T. Kenny *
***************************************************/

data {
    int<lower=1> N; // precincts
    int<lower=2> K; // candidates.
    int<lower=1> R; // races
    int<lower=1> L; // elections

    array[N] int<lower=0> vap; // total votes
    array[N, L] int<lower=0> votes; // total votes
    array[N] vector<lower=0, upper=1>[K] vote_share; // per candidate
    array[N] vector<lower=0, upper=1>[R] vap_race; // share of VAP for each race
    array[K] int<lower=1, upper=L> elec;

    // priors
    int<lower=1> Q; // number of principal components

    // for identification
    int<lower=1> n_id;
    array[n_id] int<lower=1, upper=K> id_idx;
    matrix[n_id, Q] id_loc;
    matrix<lower=0>[n_id, Q] id_scale;
}

transformed data {
    vector[R] zero_r = rep_vector(0.0, R);
    array[N] vector<lower=0>[K] sqrt_inv_votes;
    for (i in 1:N) {
        for (k in 1:K) {
            sqrt_inv_votes[i, k] = inv_sqrt(1.0 + votes[i, elec[k]]);
        }
    }
}

parameters {
    vector<lower=0, upper=1>[R] turnout_overall; // overall turnout by race
    vector<lower=0>[L-1] turnout_elec; // election shift
    array[N] vector[R] turnout; // turnout by ward and race
    // Cholesky parametrization of turnout correlation between races within precinct
    cholesky_factor_corr[R] L_t;
    row_vector<lower=0>[R] sigma_t;

    vector<lower=0, upper=1>[K] support; // overall support for each candidate
    vector<lower=0>[K-1] alpha; // fudge factor
    matrix[K, Q] loading; // loading of each candidate onto principal component
    positive_ordered[Q] scale; // scale of each component
    array[N] matrix[Q, R] pref_geo; // latent components
    // Cholesky parametrization of preference correlation between races within precinct
    cholesky_factor_corr[R] L_p;
    row_vector<lower=0>[R] sigma_p;
    real<lower=0> err_mult;
}

model {
    // likelihood -----------------------------------------
    for (i in 1:N) {
        // turnout
        vector[R] l_turn_r = inv_logit(turnout[i]) .* vap_race[i];
        vector[L] turn = append_row(0, turnout_elec) + logit(sum(l_turn_r));
        votes[i] ~ binomial_logit(vap[i], turn);

        // support
        vector[K] p = support .* (1 + append_row(1.0, alpha) .* (
            loading * (reverse(scale) .* (pref_geo[i] * l_turn_r))
            ));
        vote_share[i] ~ normal(p, err_mult * sqrt_inv_votes[i]);
    }

    // priors ----------------------------------------------
    support ~ beta(L/2.0, K/2.0);

    // loadings -- with soft identification priors
    to_vector(loading) ~ std_normal();
    for (i in 1:n_id) {
        for (q in 1:Q) {
            target += normal_lpdf(loading[id_idx[i], q] | id_loc[i, q], id_scale[i, q]) -
                std_normal_lpdf(loading[id_idx[i], q]);
        }
    }

    L_p ~ lkj_corr_cholesky(1.0);
    L_t ~ lkj_corr_cholesky(2.0);
    sigma_p ~ exponential(1/0.5);
    sigma_t ~ exponential(1/0.25);
    for (i in 1:N) {
        turnout[i] ~ multi_normal_cholesky(logit(turnout_overall), diag_pre_multiply(sigma_t, L_t));
        for (q in 1:Q) {
            pref_geo[i, q] ~ multi_normal_cholesky(zero_r, diag_pre_multiply(sigma_p, L_p));
        }
    }
    turnout_overall ~ beta(15, 50);
    turnout_elec ~ normal(0, 0.5);

    scale ~ cauchy(0.0, 0.1);
    err_mult ~ exponential(1.0/1.00);
    alpha ~ gamma(3.0, 3.0/1.0);
}
