d_raw = read_csv("data-raw/schedule_a-2022-04-05T16_47_45.csv")

candidates = c(
    C00703975="biden",
    #C00695510="booker",
    C00694455="harris",
    #C00704510="moulton",
    C00693234="warren",
    C00659938="yang",
    #C00694018="gillibrand",
    #C00696054="williamson",
    C00696419="klobuchar",
    C00696948="sanders",
    C00697441="buttigieg"
    #C00698050="inslee",
    #C00698258="hickenlooper",
    #C00508416="delaney",
    #C00700906="weld",
    #C00705186="bennet",
    #C00706416="bullock",
    #C00701698="swalwell",
    #C00701979="ryan",
    #C00706697="de_blasio",
    #C00711614="steyer",
    #C00717033="walsh",
    #C00728154="bloomberg"
)


d_fec = d_raw %>%
    transmute(cand=candidates[committee_id],
              last_name=contributor_last_name,
              zip=str_sub(contributor_zip, 1, 5)) %>%
    drop_na(cand) %>%
    distinct()


library(raceproxy)

r_probs = predict_race_sgz_me(last_name, zip, data=d_fec)
#r_probs = predict_race_sgz(last_name, zip, data=d_fec, iterate=3, regularize=T)
p_r_est = colSums(r_probs)/sum(r_probs)

fit = model_race(r_probs, cand, zip, data=d_fec, config=list(lr=0.15))
est = colMeans(fit$p_xr)
colnames(est) = str_sub(names(r_probs), 4)
rownames(est) = levels(factor(d_fec$cand))
tot_row = matrix(colSums(100*est), nrow=1)
rownames(tot_row) = "TOTAL"
rbind(round(100*est, 1), tot_row)

(calc_joint_bisgz(r_probs, d_fec$cand) %*% diag(1/p_r_est)) %>%
    `colnames<-`(colnames(est)) %>%
    (\(x) round(x*100, 1)) %>%
    rbind(tot_row)






