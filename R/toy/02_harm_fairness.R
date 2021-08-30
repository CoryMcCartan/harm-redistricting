# Mininois
pl_gerry = c(2, 2, 2, 3, 3, 1,
             2, 2, 2, 3, 3, 1,
             2, 2, 2, 3, 3, 1,
             2, 2, 3, 3, 3, 1,
             2, 3, 3, 3, 1, 1,
             1, 1, 1, 1, 1, 1)
dem_gerry = with(mininois, tapply(dem, pl_gerry, sum)/tapply(pop, pl_gerry, sum))[pl_gerry]
dem_harm = weighted.mean((dem_gerry<0.5)*(dem_opt$mininois>0.5), mininois$dem)
rep_harm = weighted.mean((dem_gerry>0.5)*(dem_opt$mininois<0.5), mininois$rep)
fairness_gerry = dem_harm - rep_harm
cat("Mininois D gerrymander fairness: ", round(fairness_gerry, 3), "\n")

p1 = plot_state(mininois, mininois_people,
           pl_gerry, dem_gerry) +
    labs(title="Democratic gerrymander\nof Mininois")
p2 = plot_state(mininois, mininois_people,
           pl_gerry, mininois$rep*round(1-dem_opt$mininois)) +
    scale_fill_gradient(low="#e5e6e5", high="#666666", guide="none") +
    labs(title="Total voter harm vs. 2(a)")

# Minissouri
pl_gerry = c(1, 2, 2, 2, 2, 2,
             1, 2, 3, 3, 3, 2,
             1, 3, 3, 3, 3, 2,
             1, 3, 3, 3, 3, 2,
             1, 3, 3, 3, 1, 2,
             1, 1, 1, 1, 1, 2)
dem_gerry = with(minissouri, tapply(dem, pl_gerry, sum)/tapply(pop, pl_gerry, sum))[pl_gerry]
harm_gerry = minissouri$dem*(dem_gerry<0.5)*(dem_opt$minissouri>0.5) +
    minissouri$rep*(dem_gerry>0.5)*(dem_opt$minissouri<0.5)
dem_harm = weighted.mean((dem_gerry<0.5)*(dem_opt$minissouri>0.5), minissouri$dem)
rep_harm = weighted.mean((dem_gerry>0.5)*(dem_opt$minissouri<0.5), minissouri$rep)
fairness_gerry = dem_harm - rep_harm
cat("Minissouri D gerrymander fairness: ", round(fairness_gerry, 3), "\n")

p3 = plot_state(minissouri, minissouri_people,
           pl_gerry, dem_gerry) +
    labs(title="Democratic gerrymander\nof Minissouri")
p4 = plot_state(minissouri, minissouri_people,
           pl_gerry, harm_gerry) +
    scale_fill_gradient(low="#e5e6e5", high="#666666", guide="none") +
    labs(title="Total voter harm vs. 2(b)")

pl_gerry = c(3, 2, 2, 2, 2, 2,
             3, 1, 1, 1, 2, 2,
             3, 1, 1, 1, 2, 2,
             3, 3, 1, 1, 1, 2,
             3, 3, 3, 1, 1, 2,
             3, 3, 3, 3, 1, 2)
rep_gerry = with(minissouri, tapply(dem, pl_gerry, sum)/tapply(pop, pl_gerry, sum))[pl_gerry]
harm_gerry = minissouri$dem*(rep_gerry<0.5)*(dem_opt$minissouri>0.5) +
    minissouri$rep*(dem_gerry>0.5)*(dem_opt$minissouri<0.5)
dem_harm = weighted.mean((rep_gerry<0.5)*(dem_opt$minissouri>0.5), minissouri$dem)
rep_harm = weighted.mean((rep_gerry>0.5)*(dem_opt$minissouri<0.5), minissouri$rep)
fairness_gerry = dem_harm - rep_harm
cat("Minissouri R gerrymander fairness: ", round(fairness_gerry, 3), "\n")

p5 = plot_state(minissouri, minissouri_people,
           pl_gerry, dem_gerry) +
    labs(title="Republican gerrymander\nof Minissouri")
p6 = plot_state(minissouri, minissouri_people,
           pl_gerry, harm_gerry) +
    scale_fill_gradient(low="#e5e6e5", high="#666666", guide="none") +
    labs(title="Total voter harm vs. 2(b)")

p1 + p3 + p5 + p2 + p4 + p6 +
    plot_layout(guides="collect")
ggsave(here("paper/figures/mini_harm.pdf"), width=7, height=4.75)

