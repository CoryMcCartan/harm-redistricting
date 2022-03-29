# Minissouri

pl_fair = c(3, 3, 3, 3, 3, 3,
              2, 2, 3, 3, 3, 3,
              2, 2, 3, 3, 3, 3,
              2, 2, 2, 2, 1, 1,
              2, 2, 2, 1, 1, 1,
              2, 2, 2, 1, 1, 1)
pl_gerry_dem = c(1, 2, 2, 2, 2, 2,
                 1, 2, 3, 3, 3, 2,
                 1, 3, 3, 3, 3, 2,
                 1, 3, 3, 3, 3, 2,
                 1, 3, 3, 3, 1, 2,
                 1, 1, 1, 1, 1, 2)
pl_gerry_rep = c(3, 2, 2, 2, 2, 2,
                 3, 1, 1, 1, 2, 2,
                 3, 1, 1, 1, 2, 2,
                 3, 3, 1, 1, 1, 2,
                 3, 3, 3, 1, 1, 2,
                 3, 3, 3, 3, 1, 2)


dem_gerry = with(minissouri, tapply(dem, pl_gerry, sum)/tapply(pop, pl_gerry, sum))[pl_gerry]
harm_gerry = minissouri$dem*(dem_gerry<0.5)*(dem_opt$minissouri>0.5) +
    minissouri$rep*(dem_gerry>0.5)*(dem_opt$minissouri<0.5)
dem_harm = weighted.mean((dem_gerry<0.5)*(dem_opt$minissouri>0.5), minissouri$dem)
rep_harm = weighted.mean((dem_gerry>0.5)*(dem_opt$minissouri<0.5), minissouri$rep)
fairness_gerry = dem_harm - rep_harm
cat("Minissouri D gerrymander fairness: ", round(fairness_gerry, 3), "\n")

coords_fair = list(x=c(3/4, 1/4, 1/2),
                   y=c(1.04, 1.04, -0.04))
coords_gerry_dem = list(x=c(1/2, 1/2, 1/2),
                        y=c(1.04, -0.04, 1/2))
coords_gerry_rep = list(x=c(3/4, 7/12, 1/3),
                        y=c(1.04, -0.04, 1.04))

suppressMessages({
p3 = plot_state(minissouri, minissouri_people,
           pl_gerry, dem_gerry) +
    labs(title="Democratic gerrymander\nof Minissouri")
for (i in 1:3) {
    p3 = add_lab(p3, fmt_lean(dem_gerry[as.character(i)]),
                coords$x[i], coords$y[i])
}
p4 = plot_state(minissouri, minissouri_people,
           pl_gerry, harm_gerry) +
    scale_fill_gradient(low="#e5e6e5", high="#666666", guide="none") +
    labs(title="Total voter harm vs. 2(b)")
})


rep_gerry = with(minissouri, tapply(dem, pl_gerry, sum)/tapply(pop, pl_gerry, sum))[pl_gerry]
harm_gerry = minissouri$dem*(rep_gerry<0.5)*(dem_opt$minissouri>0.5) +
    minissouri$rep*(rep_gerry>0.5)*(dem_opt$minissouri<0.5)
dem_harm = weighted.mean((rep_gerry<0.5)*(dem_opt$minissouri>0.5), minissouri$dem)
rep_harm = weighted.mean((rep_gerry>0.5)*(dem_opt$minissouri<0.5), minissouri$rep)
fairness_gerry = dem_harm - rep_harm
cat("Minissouri R gerrymander fairness: ", round(fairness_gerry, 3), "\n")

suppressMessages({
p5 = plot_state(minissouri, minissouri_people,
           pl_gerry, rep_gerry) +
    labs(title="Republican gerrymander\nof Minissouri")
for (i in 1:3) {
    p5 = add_lab(p5, fmt_lean(rep_gerry[as.character(i)]),
                coords$x[i], coords$y[i])
}
p6 = plot_state(minissouri, minissouri_people,
           pl_gerry, harm_gerry) +
    scale_fill_gradient(low="#e5e6e5", high="#666666", guide="none") +
    labs(title="Total voter harm vs. 2(b)")
})

p1 + p3 + p5 + p2 + p4 + p6 +
    plot_layout(guides="collect")
ggsave(here("paper/figures/mini_harm.pdf"), width=7, height=4.75)


ggsave(here("paper/figures/diffharm_minissouri.pdf"), width=7, height=2.5)
