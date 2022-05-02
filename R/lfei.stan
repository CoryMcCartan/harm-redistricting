/***************************************************
* Latent Factor Ecological Inference Model        *
* (c) 2022 Cory McCartan and Christopher T. Kenny *
***************************************************/

data {
    int<lower=1> Q; // number of principal components
    int<lower=1> N; // precincts
    int<lower=Q> K; // candidates.
    int<lower=1> R; // races
    int<lower=1> L; // elections

    array[N] int<lower=0> vap; // voting-age population
    array[N, L] int<lower=0> votes; // total votes
    array[N] vector<lower=0, upper=1>[K] vote_share; // per candidate
    array[N] vector<lower=0, upper=1>[R] vap_race; // share of VAP for each race
    array[K] int<lower=1, upper=L> elec;

    // priors for identification
    vector[K*Q] loading_loc;
    cov_matrix[K*Q] loading_cov;
}

transformed data {
    array[N] vector<lower=0>[K] sqrt_inv_votes;
    cholesky_factor_cov[K*Q] loading_chol = cholesky_decompose(loading_cov);
    array[N] vector[K] l_vote_share;

    for (i in 1:N) {
        for (k in 1:K) {
            sqrt_inv_votes[i, k] = inv_sqrt(1.0 + votes[i, elec[k]]);
        }
        l_vote_share[i] = logit(1e-3 + vote_share[i]);
    }
}

parameters {
    vector<lower=0, upper=1>[R] turnout_overall; // overall turnout by race
    vector[L-1] turnout_elec; // election shift
    array[N] vector[R] turnout_z; // turnout by precinct and race
    // Cholesky parametrization of turnout correlation between races within precinct
    cholesky_factor_corr[R] L_t;
    row_vector<lower=0>[R] sigma_t;

    vector[K] support; // overall support for each candidate
    vector<lower=0>[K-1] alpha; // fudge factor
    matrix[K, Q] loading; // loading of each candidate onto principal component
    vector<lower=0>[Q] scale; // scale of each component
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
    array[N] vector[Q] post_factor;
    array[N] vector[R] turn_r;
    {
        matrix[R, R] Sigma_t = diag_pre_multiply(sigma_t, L_t);
        matrix[R, R] Sigma_p = diag_post_multiply(L_p', sigma_p);
        vector[R] l_turn_over = logit(turnout_overall);
        vector[Q] r_scale = reverse(scale);
        for (i in 1:N) {
            turnout[i] = inv_logit(l_turn_over + Sigma_t * turnout_z[i]);
            turn_r[i] = turnout[i] .* vap_race[i];
            pref[i] = pref_z[i] * Sigma_p;
            post_factor[i] = r_scale .* (pref[i] * turn_r[i]);
        }
    }
}

model {
    // likelihood -----------------------------------------
    vector[L] a_turn_elec = append_row(0.0, turnout_elec);
    vector[K] a_alpha = append_row(1.0, alpha);
    for (i in 1:N) {
        // turnout
        votes[i] ~ binomial_logit(vap[i], a_turn_elec + logit(sum(turn_r[i])));

        // support
        vector[K] p = support + a_alpha .* (loading * post_factor[i]);
        l_vote_share[i] ~ normal(p, err_mult * sqrt_inv_votes[i]);
    }

    // priors ----------------------------------------------
    // support ~ beta(1.0, (1.0*K)/L);
    support ~ normal(0.0, 4.0);

    // loadings -- with soft identification priors
    to_vector(loading) ~ multi_normal_cholesky(loading_loc, loading_chol);

    L_p ~ lkj_corr_cholesky(20.0);
    L_t ~ lkj_corr_cholesky(4.0);
    sigma_p ~ gamma(1.5, 1.5/1.0);
    sigma_t ~ gamma(1.5, 1.5/0.25);
    for (i in 1:N) {
        turnout_z[i] ~ std_normal();
        to_vector(pref_z[i]) ~ std_normal();
    }
    turnout_overall ~ beta(10, 30);
    turnout_elec ~ normal(0, 0.5);

    scale ~ gamma(1.5, 1.5/4.0);
    err_mult ~ exponential(1.0/4.00);
    alpha ~ gamma(5.0, 5.0/1.0);
}

generated quantities {
    array[N, K] real post_share_prec;
    matrix[K, R] post_share_race = rep_matrix(0.0, K, R);
    array[N] int post_turn;

    {
        vector[K] a_alpha = append_row(1.0, alpha);
        vector[Q] r_scale = reverse(scale);

        for (i in 1:N) {
            post_turn[i] = binomial_rng(vap[i], sum(turn_r[i]));

            // support
            vector[K] p = support + a_alpha .* (loading * post_factor[i]);
            post_share_prec[i] = inv_logit(normal_rng(p, err_mult * sqrt_inv_votes[i]));
            for (r in 1:R) {
                vector[Q] pref_r = r_scale .* pref[i, :, r];
                vector[K] p_r = support + a_alpha .* (loading * pref_r);
                post_share_race[:, r] += inv_logit(p_r) * turn_r[i, r] * vap[i];
            }
        }
    }
}
