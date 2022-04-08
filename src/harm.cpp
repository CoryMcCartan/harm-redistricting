#include <Rcpp.h>
using namespace Rcpp;

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
NumericMatrix harm_helper(IntegerMatrix pl, NumericMatrix pr,
                          NumericVector wt_a, NumericVector wt_b,
                          IntegerVector idx_1, IntegerVector idx_2) {
    int n1 = idx_1.size();
    int n2 = idx_2.size();
    int prec = pl.nrow();
    NumericMatrix out(3, n1);

    double tot_a = sum(wt_a);
    double tot_b = sum(wt_b);
    for (int i = 0; i < n1; i++) {
        int i1 = idx_1(i) - 1;
        double pos_accuml = 0.0;
        double neg_accuml = 0.0;
        for (int k = 0; k < prec; k++) {
            double pr_ik = pr(pl(k, i1) - 1, i1);
            double wt_a_k = wt_a(k);
            double wt_b_k = wt_b(k);
            for (int j = 0; j < n2; j++) {
                int j2 = idx_2(j) - 1;
                double harm_benefit = pr(pl(k, j2) - 1, j2) - pr_ik;
                if (harm_benefit >= 0.0) {
                    pos_accuml += harm_benefit * wt_a_k;
                } else {
                    neg_accuml -= harm_benefit * wt_b_k;
                }
            }
        }
        out(0, i) = pos_accuml / tot_a / n2;
        out(1, i) = neg_accuml / tot_b / n2;
        out(2, i) = (pos_accuml + neg_accuml) / (tot_a + tot_b) / n2;
        Rcpp::checkUserInterrupt();
    }

    return out;
}
