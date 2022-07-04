#include <RcppArmadillo.h>
#include <RcppThread.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppThread)]]
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export(rng = false)]]
NumericMatrix mat_by_prec(IntegerMatrix pl, NumericMatrix x) {
    int row = pl.nrow();
    int col = pl.ncol();
    NumericMatrix m_prec(row, col);

    for (int i = 0; i < col; i++) {
        for (int j = 0; j < row; j++) {
            m_prec(j, i) = x(pl(j, i) - 1, i);
        }
    }

    return m_prec;
}

// [[Rcpp::export(rng = false)]]
arma::mat harm_helper(const arma::imat pl, const arma::mat vs,
                      const arma::vec wt_a, const arma::vec wt_b,
                      const arma::vec shift_elec,
                      const arma::mat shift_distr_1,
                      const arma::mat shift_distr_2,
                      const arma::ivec idx_1, const arma::ivec idx_2) {
    int n1 = idx_1.n_elem;
    int n2 = idx_2.n_elem;
    int prec = pl.n_rows;

    arma::mat out(3, n1);

    double tot_a = sum(wt_a);
    double tot_b = sum(wt_b);

    RcppThread::ProgressBar bar(n1, 1);
    RcppThread::parallelFor(0, n1, [&] (int i) {
        int i1 = idx_1(i) - 1;
        double accuml_a = 0.0;
        double accuml_b = 0.0;
        for (int k = 0; k < prec; k++) {
            int pl_ki = pl(k, i1) - 1;
            double vs_base_ik = vs(pl_ki, i1);
            double wt_a_k = wt_a(k);
            double wt_b_k = wt_b(k);
            for (int j = 0; j < n2; j++) {
                int j2 = idx_2(j) - 1;
                int pl_kj = pl(k, j2) - 1;
                double vs_ik = vs_base_ik + shift_elec(j) + shift_distr_1(pl_ki, j);
                double vs_jk = vs(pl_kj, j2) + shift_elec(j) + shift_distr_2(pl_kj, j);
                if (vs_jk > 0.5) {
                    if (vs_ik <= 0.5) { // A win under counterfactual but not original
                        accuml_a += wt_a_k;
                    }
                } else if (vs_ik > 0.5) { // B win under counterfactual but not original
                    accuml_b += wt_b_k;
                }
            }
        }
        out(0, i) = accuml_a / tot_a / n2;
        out(1, i) = accuml_b / tot_b / n2;
        out(2, i) = (accuml_a + accuml_b) / (tot_a + tot_b) / n2;

        RcppThread::checkUserInterrupt();
        bar++;
    });

    return out;
}


// [[Rcpp::export(rng = false)]]
arma::mat prec_harm_helper(const arma::imat pl, const arma::mat vs,
                      const arma::vec wt_a, const arma::vec wt_b,
                      const arma::vec shift_elec,
                      const arma::mat shift_distr_1,
                      const arma::mat shift_distr_2,
                      const arma::ivec idx_1, const arma::ivec idx_2) {
    int n1 = idx_1.n_elem;
    int n2 = idx_2.n_elem;
    int prec = pl.n_rows;

    arma::mat out(prec, n1);

    RcppThread::ProgressBar bar(n1, 1);
    RcppThread::parallelFor(0, n1, [&] (int i) {
        int i1 = idx_1(i) - 1;
        for (int k = 0; k < prec; k++) {
            int pl_ki = pl(k, i1) - 1;
            double vs_base_ik = vs(pl_ki, i1);
            double wt_a_k = wt_a(k) / n2;
            double wt_b_k = wt_b(k) / n2;
            out(k, i) = 0;
            for (int j = 0; j < n2; j++) {
                int j2 = idx_2(j) - 1;
                int pl_kj = pl(k, j2) - 1;
                double vs_ik = vs_base_ik + shift_elec(j) + shift_distr_1(pl_ki, j);
                double vs_jk = vs(pl_kj, j2) + shift_elec(j) + shift_distr_2(pl_kj, j);
                if (vs_jk > 0.5) {
                    if (vs_ik <= 0.5) { // A win under counterfactual but not original
                        out(k, i) += wt_a_k;
                    }
                } else if (vs_ik > 0.5) { // B win under counterfactual but not original
                    out(k, i) += wt_b_k;
                }
            }
        }

        RcppThread::checkUserInterrupt();
        bar++;
    });

    return out;
}
