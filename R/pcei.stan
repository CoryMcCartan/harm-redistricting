/***************************************************
* Principal Components Ecological Inference Model *
* (c) 2022 Cory McCartan and Christopher T. Kenny *
***************************************************/

data {
    int<lower=1> N; // precincts
    int<lower=2> R; // race/ethnicity
    int<lower=2> K; // candidates. ENSURE 1="<abstain/other>"
    int<lower=1> L; // elections
    int<lower=1> M; // = N*L
    int<lower=1> obs_votes; // prec/elec/cand

    array[N] vector<lower=0, upper=1>[R] vap_race; // share of VAP for each race

    array[L] int<lower=2, upper=K> n_cand; // n cand per prec/elec
    // ragged structure; both these should be sorted by elec then prec
    array[obs_votes] int<lower=0, upper=K-1> cand; // ZERO-indexed candidate
    array[obs_votes] int<lower=0> votes; // votes per candidate

    // priors
    int<lower=1, upper=K-1> idx_left;
    real<lower=0> prior_race_scale;
    real<lower=0> prior_geo_scale;
}

parameters {
    vector[K-1] support; // overall support for each candidate
    vector[K-1] loading; // loading of each candidate onto principal component
    //row_vector[L] elec_effect; // election-level shifts on PC scale
    // racial voting preferences for PC dimension (non-centered param).
    vector[N] pref_geo;
    //row_vector[R] pref_race;
    //real<lower=0> race_scale;
    real<lower=0> geo_scale;
    array[L] vector<lower=0, upper=1>[R] turnout; // turnout rates by race and election
}

model {
    int pos;
    // likelihood
    pos = 1;
    for (l in 1:L) {
        int k_l = n_cand[l];
        for (i in 1:N) {
            // marginalized propensity to support PC dimension
            vector[R] marg_prop = turnout[l] .* vap_race[i];
            vector[k_l] cand_prop = rep_vector(0.0, k_l); // marginalized candidate support

            cand_prop[1] = 1 - sum(marg_prop);
            for (r in 1:R) {
                cand_prop[2:k_l] += softmax(
                    support[segment(cand, pos+1, k_l-1)] +
                    loading[segment(cand, pos+1, k_l-1)] *
                    (pref_geo[i]*geo_scale)
                    //(pref_geo[i]*geo_scale + pref_race[r]*race_scale)
                    //(pref[r] + elec_effect[l])
                ) * marg_prop[r];
            }

            segment(votes, pos, k_l) ~ multinomial(cand_prop);
            pos += k_l;
        }
    }

    // prior
    support ~ std_normal();
    for (k in 1:(K-1)) {
        if (k == idx_left)
            loading[k] ~ normal(-1, 0.001);
        else
            loading[k] ~ std_normal();
    }
    //elec_effect ~ std_normal();
    for (l in 1:L)
        turnout[l] ~ beta(2, 2);

    pref_geo ~ std_normal();
    //pref_race ~ std_normal();
    geo_scale ~ exponential(1.0/prior_geo_scale);
    //race_scale ~ exponential(1.0/prior_race_scale);
}
