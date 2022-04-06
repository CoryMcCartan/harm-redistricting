/***************************************************
* Principal Components Model                      *
* (c) 2022 Cory McCartan and Christopher T. Kenny *
***************************************************/

data {
    int<lower=1> N; // precincts
    int<lower=2> K; // candidates.
    int<lower=1> L; // elections

    array[N, L] real<lower=0> votes; // total votes
    array[N] vector<lower=0, upper=1>[K] vote_share; // per candidate
    array[K] int<lower=1, upper=L> elec; // which election

    int<lower=1> Q; // number of principal components
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
    vector[K] support; // overall support for each candidate
    matrix[K, Q] loading; // loading of each candidate onto principal component
    positive_ordered[Q] scale;
    array[N] vector[Q] pref_geo; // latent components
    real<lower=0> sigma;
}

model {
    // likelihood
    for (i in 1:N) {
        vote_share[i] ~ multi_normal(
            support + loading * (reverse(scale) .* pref_geo[i]),
            add_diag(multinom_cov[i], square(sigma))
            );
    }

    // prior
    support ~ std_normal();
    to_vector(loading) ~ std_normal();

    for (i in 1:N)
        pref_geo[i] ~ std_normal();

    sigma ~ gamma(2.0, 2.0/0.1);
}
