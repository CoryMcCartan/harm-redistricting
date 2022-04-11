/***************************************************
* Principal Components Ecological Inference Model *
* (c) 2022 Cory McCartan and Christopher T. Kenny *
***************************************************/

data {
    int<lower=1> Q; // number of principal components
    int<lower=1> N; // precincts
    int<lower=Q> K; // candidates.
    int<lower=1> R; // races
    int<lower=1> L; // elections

    array[N] int<lower=0> vap; // total votes
    array[N, L] int<lower=0> votes; // total votes
    array[N] vector<lower=0, upper=1>[K] vote_share; // per candidate
    array[N] vector<lower=0, upper=1>[R] vap_race; // share of VAP for each race
    array[K] int<lower=1, upper=L> elec;

    // priors for identification
    int<lower=1> n_id; // how many identifying conditions
    int<lower=1> n_drop; // how many normal priors to drop
    array[n_id] vector[K] id_contrast;
    matrix[n_id, Q] id_loc;
    matrix<lower=0>[n_id, Q] id_scale;
    array[n_id] int<lower=1, upper=K> id_idx;
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
    array[N] vector[R] turnout_z; // turnout by ward and race
    // Cholesky parametrization of turnout correlation between races within precinct
    cholesky_factor_corr[R] L_t;
    row_vector<lower=0>[R] sigma_t;

    vector<lower=0, upper=1>[K] support; // overall support for each candidate
    vector<lower=0>[K-1] alpha; // fudge factor
    matrix[K, Q] loading; // loading of each candidate onto principal component
    positive_ordered[Q] scale; // scale of each component
    // vector<lower=0>[Q] scale; // scale of each component
    array[N] matrix[Q, R] pref_z; // latent components
    // Cholesky parametrization of preference correlation between races within precinct
    cholesky_factor_corr[R] L_p;
    row_vector<lower=0>[R] sigma_p;
    real<lower=0> err_mult;
}

transformed parameters {
    // un-Cholesky transform & save for output
    array[N] vector[R] turnout;
    array[N] matrix[Q, R] pref;
    {
        matrix[R, R] Sigma_t = diag_pre_multiply(sigma_t, L_t);
        matrix[R, R] Sigma_p = diag_post_multiply(L_p', sigma_p);
        vector[R] l_turn_over = logit(turnout_overall);
        for (i in 1:N) {
            turnout[i] = inv_logit(l_turn_over + Sigma_t * turnout_z[i]);
            pref[i] = pref_z[i] * Sigma_p;
        }
    }
}

model {
    // likelihood -----------------------------------------
    vector[L] a_turn_elec = append_row(0.0, turnout_elec);
    vector[K] a_alpha = append_row(1.0, alpha);
    vector[Q] r_scale = reverse(scale);
    for (i in 1:N) {
        // turnout
        vector[R] turn_r = turnout[i] .* vap_race[i];
        votes[i] ~ binomial_logit(vap[i], a_turn_elec + logit(sum(turn_r)));

        // support
        vector[Q] pref_marg = r_scale .* (pref[i] * turn_r);
        vector[K] p = support .* (1 + a_alpha .* (loading * pref_marg));
        vote_share[i] ~ normal(p, err_mult * sqrt_inv_votes[i]);
    }

    // priors ----------------------------------------------
    support ~ beta(1.0, (1.0*K)/L);

    // loadings -- with soft identification priors
    to_vector(loading) ~ std_normal();
    for (i in 1:n_id) {
        row_vector[Q] id_vec = id_contrast[i] * loading;
        for (q in 1:Q) {
            target += -std_normal_lpdf(loading[id_idx[i], q]);
            // target += normal_lpdf(loading[id_idx[i], q] | id_loc[i, q], id_scale[i, q]) -
            //     std_normal_lpdf(loading[id_idx[i], q]);
        }
    }
    row_vector[2] id_diff = loading[id_idx[2]] - loading[id_idx[1]];
    target += normal_lpdf(loading[id_idx[1]] | -1.0, 1.0);
    target += normal_lpdf(id_diff[1] | 2.0, 0.5);
    target += normal_lpdf(id_diff[2] | 0.0, 0.05);
    target += normal_lpdf(dot_product(loading[id_idx[3]], id_diff) | 0.0, 0.1);

    L_p ~ lkj_corr_cholesky(1.0);
    L_t ~ lkj_corr_cholesky(4.0);
    sigma_p ~ gamma(2.0, 2.0/0.5);
    sigma_t ~ gamma(2.0, 2.0/0.25);
    for (i in 1:N) {
        turnout_z[i] ~ std_normal();
        to_vector(pref_z[i]) ~ std_normal();
    }
    turnout_overall ~ beta(10, 30);
    turnout_elec ~ normal(0, 0.5);

    scale ~ gamma(1.5, 1.5);
    err_mult ~ exponential(1.0/1.00);
    alpha ~ gamma(20.0, 20.0/1.0);
}

generated quantities {
}
