/***************************************************
* Principal Components Ecological Inference Model *
* (c) 2022 Cory McCartan and Christopher T. Kenny *
***************************************************/

data {
    int<lower=1> Q; // number of principal components
    int<lower=1> N; // precincts
    int<lower=Q> K; // candidates.
    int<lower=1> R; // races

    array[N] int<lower=0> vap; // voting-age population
    array[N] vector<lower=0, upper=1>[R] vap_race; // share of VAP for each race
    matrix[K, Q] loading; // loading of each candidate onto principal component
    vector[K] support; // overall support for each candidate
}

parameters {
    array[N] vector[R] turnout; // turnout by ward and race

    vector<lower=0>[Q] scale; // scale of each component
    array[N] matrix[Q, R] pref; // latent components
    real<lower=0> err_mult;
}

generated quantities {
    array[N, R] vector[K] votes;

    {
        vector[Q] r_scale = reverse(scale);

        for (i in 1:N) {
            matrix[Q, R] pref_r = diag_pre_multiply(r_scale, pref[i]);
            matrix[K, R] p_r = rep_matrix(support, R) + loading * pref_r;

            for (r in 1:R) {
                int votes_ir = binomial_rng(vap[i], turnout[i, r] * vap_race[i, r]);
                votes[i, r] = votes_ir * inv_logit(to_vector(normal_rng(
                    p_r[:, r],
                    1e-6 + err_mult * inv_sqrt(1.0 + votes_ir) * vap_race[i, r]
                    )));
            }
        }
    }
}
