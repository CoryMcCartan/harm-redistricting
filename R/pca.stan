/***************************************************
* Principal Components Model                      *
* (c) 2022 Cory McCartan and Christopher T. Kenny *
***************************************************/

data {
    int<lower=1> N; // precincts
    int<lower=2> K; // candidates.
    int<lower=1> R; // races
    int<lower=1> L; // elections
    int<lower=1> W; // wards

    array[N] int<lower=0> vap; // total votes
    array[N, L] int<lower=0> votes; // total votes
    array[N] vector<lower=0, upper=1>[K] vote_share; // per candidate
    array[N] vector<lower=0, upper=1>[R] vap_race; // share of VAP for each race
    array[N] int<lower=1, upper=W> ward;
    array[K] int<lower=1, upper=L> elec;

    // priors
    int<lower=1> Q; // number of principal components
    array[2] int<lower=1, upper=K> idx_left; // for identification
}

transformed data {
    // covariance matrices for MVN
    array[N] matrix[K, K] multinom_cov;
    for (i in 1:N) {
        multinom_cov[i] = rep_matrix(0.0, K, K);
        for (k in 1:K) {
            real votes_i = 1e-4 + votes[i, elec[k]];
            multinom_cov[i][k, k] = vote_share[i, k] * (1 - vote_share[i, k]) / votes_i;
            if (k > 1) {
                for (k2 in 1:(k-1)) {
                    if (elec[k] != elec[k2]) continue;
                    multinom_cov[i][k, k2] = -vote_share[i, k] * vote_share[i, k2] / votes_i;
                    multinom_cov[i][k2, k] = multinom_cov[i][k, k2];
                }
            }
        }
    }
}

parameters {
    vector<lower=0, upper=1>[R] turnout_overall;
    vector<lower=0>[L-1] turnout_elec;
    array[W] vector<lower=0, upper=1>[R] turnout;
    real<lower=0> kappa;

    vector<lower=0>[K] support; // overall support for each candidate
    matrix[K, Q] loading; // loading of each candidate onto principal component
    positive_ordered[Q] scale;
    array[N] matrix[Q, R] pref_geo; // latent components
    vector<lower=0>[K-1] alpha;
    real<lower=0> sigma_add;
    real<lower=0> sigma_mult;
}

model {
    // likelihood
    for (i in 1:N) {
        for (l in 1:L) {
            votes[i, l] ~ binomial_logit(
                vap[i],
                append_row(0, turnout_elec)[l] +
                logit(dot_product(turnout[ward[i]], vap_race[i]))
                );
        }

        //vote_share[i] ~ multi_normal(
        //    support .* (1 + append_row(1, alpha) .* (
        //        loading * (reverse(scale) .* (pref_geo[i] * (turnout[i] .* vap_race[i])))
        //        )),
        //    diag_post_multiply(add_diag(multinom_cov[i], square(sigma_add)), rep_vector(sigma_mult, K))
        //    );
        vote_share[i] ~ multi_normal(
            support .* (1 + append_row(1, alpha) .* (
                loading * (reverse(scale) .* (pref_geo[i] * (turnout[i] .* vap_race[i])))
                )),
            diag_post_multiply(add_diag(multinom_cov[i], square(sigma_add)), rep_vector(sigma_mult, K))
            );
    }

    // prior
    support ~ std_normal();
    to_vector(loading) ~ std_normal();
    target += normal_lpdf(loading[idx_left[1], 1] | -1.0, 0.2) - std_normal_lpdf(loading[idx_left[1], 1]);
    target += normal_lpdf(loading[idx_left[1], 2] |  0.0, 0.5) - std_normal_lpdf(loading[idx_left[1], 2]);
    target += normal_lpdf(loading[idx_left[2], 1] |  1.0, 0.2) - std_normal_lpdf(loading[idx_left[2], 1]);
    //target += normal_lpdf(loading[idx_left[2], 2] |  0.0, 0.2) - std_normal_lpdf(loading[idx_left[2], 2]);

    for (i in 1:N) {
        to_vector(pref_geo[i]) ~ std_normal();
    }
    for (w in 1:W) {
        turnout[w] ~ beta_proportion(turnout_overall, kappa);
    }
    turnout_overall ~ beta(5, 10);
    turnout_elec ~ normal(0, 0.5);

    scale ~ cauchy(0.0, 0.1);
    sigma_add ~ gamma(2.0, 2.0/0.1);
    sigma_mult ~ exponential(1.0);
    kappa ~ exponential(0.1);
    alpha ~ gamma(3.0, 3.0/1.0);
}
