/***************************************************
* Principal Components Ecological Inference Model *
* (c) 2022 Cory McCartan and Christopher T. Kenny *
***************************************************/

data {
    int<lower=1> N; // precincts
    int<lower=1> R; // race/ethnicity
    int<lower=2> K; // candidates.
    int<lower=1> L; // elections

    array[N] int<lower=0> vap; // voting age pop.
    array[N] vector<lower=0, upper=1>[R] vap_race; // share of VAP for each race
    array[N] vector<lower=0, upper=1>[K] vote_share; // vote share for each candidate

    array[L+1] int<lower=1, upper=K+1> idx_cand; // index of first cand in each elec

    int<lower=0, upper=1> prior_only;
    vector[K] prior_pca;
}

transformed data {
    // covariance matrices for MVN
    array[N] matrix[K, K] multinom_cov;
    for (i in 1:N) {
        //multinom_cov[i] = -vote_share[i] * (vote_share[i]' / vap[i]);
        multinom_cov[i] = rep_matrix(0.0, K, K);
        for (k in 1:K) {
            multinom_cov[i][k, k] = vote_share[i, k] * (1 - vote_share[i, k]) / vap[i];
        }
        // zero the off-diagonal for different elections
        /*
        for (l1 in 1:L) {
            int i1a = idx_cand[l1];
            int i1b = idx_cand[l1+1] - 1;
            for (l2 in 1:(l1-1)) {
                int i2a = idx_cand[l2];
                int i2b = idx_cand[l2+1] - 1;
                multinom_cov[i][i1a:i1b, i2a:i2b] = rep_matrix(0.0, i1b-i1a+1, i2b-i2a+1);
                multinom_cov[i][i2a:i2b, i1a:i1b] = rep_matrix(0.0, i2b-i2a+1, i1b-i1a+1);
            }
        }
        */
    }
}

parameters {
    vector<lower=0>[K-L] gammas;
    vector[K] loading; // loading of each candidate onto principal component
    array[N] row_vector[R] pref;
    real<lower=0.01> sigma;
}

transformed parameters {
    vector<lower=0, upper=1>[K] support; // overall support for each candidate
    vector[L] sum_gamma;
    for (l in 1:L) {
        int i1 = idx_cand[l];
        int i2 = idx_cand[l+1] - 1;
        sum_gamma[l] = 1 + sum(gammas[(i1 + 1 - l):(i2 - l)]);
        support[i1:i2] = append_row(1, gammas[(i1 + 1 - l):(i2 - l)]) / sum_gamma[l];
    }
}

model {
    // likelihood
    if (!prior_only) {
        for (i in 1:N) {
            vote_share[i] ~ multi_normal(
                loading * pref[i] * vap_race[i] + support,
                add_diag(multinom_cov[i], square(sigma))
                );
        }
    }

    // prior
    // dirichlet(0.5) on `support` w/in elections
    for (l in 1:L) {
        int i1 = idx_cand[l];
        int i2 = idx_cand[l+1] - 1;
        support[i1:i2] ~ dirichlet(rep_vector(0.5, i2-i1+1));
        // Jacobian
        target += -(i2-i1+1) * log(sum_gamma[l]);
    }
    loading ~ normal(prior_pca, 0.1);
    for (i in 1:N) {
        pref[i] ~ std_normal();
    }
    sigma ~ gamma(2.0, 2.0/1.0);
}
