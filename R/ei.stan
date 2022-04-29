/***************************************************
* Principal Components Ecological Inference Model *
* (c) 2022 Cory McCartan and Christopher T. Kenny *
***************************************************/

data {
    int<lower=1> N; // precincts
    int<lower=1> L; // elections
    int<lower=1> R; // races

    array[N] int<lower=0> vap; // voting-age population
    array[N, L] int<lower=0> votes; // total votes
    array[N, L] int<lower=0> dem_votes; // dem votes by prec.
    array[N] vector<lower=0, upper=1>[R] vap_race; // share of VAP for each race
}

transformed data {
    vector[N] l_turn_share = rep_vector(0.0, N); // turnout pct by prec.
    vector[N] l_dem_share = rep_vector(0.0, N); // dem share by prec.
    for (i in 1:N) {
        for (l in 1:L) {
            l_turn_share[i] += (1.0 * votes[i, l]) / vap[i] / L;
            l_dem_share[i] += (1.0 * dem_votes[i, l]) / votes[i, l] / L;
        }
        l_turn_share[i] = logit(l_turn_share[i]);
        l_dem_share[i] = logit(l_dem_share[i]);
    }
}

parameters {
    vector<lower=0, upper=1>[R] turnout_overall; // overall turnout by race
    vector[L] turnout_elec; // election shift
    array[N] vector[R] turnout_z; // turnout by precinct and race
    // Cholesky parametrization of turnout correlation between races within precinct
    cholesky_factor_corr[R] L_t;
    row_vector<lower=0>[R] sigma_t;

    vector[L] support_elec; // election shift
    vector<lower=0, upper=1>[R] support_overall; // overall turnout by race
    array[N] vector[R] support_z; // dem. support by race for each election
    // Cholesky parametrization of turnout correlation between races within precinct
    cholesky_factor_corr[R] L_s;
    row_vector<lower=0>[R] sigma_s;
}

transformed parameters {
    // un-Cholesky transform & save for output
    array[N] vector[R] turnout;
    array[N] vector[R] support;
    array[N] vector[R] turn_r;
    array[N] vector[R] supp_r;
    {
        matrix[R, R] Sigma_t = diag_pre_multiply(sigma_t, L_t);
        matrix[R, R] Sigma_s = diag_pre_multiply(sigma_s, L_s);
        // matrix[R, R] Sigma_s = diag_post_multiply(L_s', sigma_s);
        vector[R] l_turn_over = logit(turnout_overall);
        vector[R] l_supp_over = logit(support_overall);
        for (i in 1:N) {
            turnout[i] = inv_logit(l_turn_over + Sigma_t * turnout_z[i]);
            support[i] = inv_logit(l_supp_over + Sigma_s * support_z[i]);
            // turnout[i, 1] = inv_logit(
            //     (l_turn_share[i] - logit(1e-3 + 0.998 * dot_product(turnout[i, 2:R], vap_race[i, 2:R])))
            //     / (1e-4 + vap_race[i, 1])
            //     + Sigma_t[1, 1:R] * turnout_z[i]);

            turn_r[i] = turnout[i] .* vap_race[i];
            supp_r[i] = support[i] .* vap_race[i];
        }
    }
}

model {
    // likelihood -----------------------------------------
    for (i in 1:N) {
        // turnout
        votes[i] ~ binomial_logit(vap[i], turnout_elec + logit(sum(turn_r[i])));

        // support
        dem_votes[i] ~ binomial_logit(votes[i], support_elec + logit(sum(supp_r[i])));
    }

    // priors ----------------------------------------------
    L_s ~ lkj_corr_cholesky(5.0);
    L_t ~ lkj_corr_cholesky(5.0);
    sigma_s ~ gamma(1.5, 1.5/1.0);
    sigma_t ~ gamma(1.5, 1.5/0.25);
    for (i in 1:N) {
        turnout_z[i] ~ std_normal();
        support_z[i] ~ std_normal();
    }
    turnout_overall ~ beta(20, 20);
    turnout_elec ~ normal(0, 0.6);
    support_overall ~ beta(20, 20);
    support_elec ~ normal(0, 0.4);
}
