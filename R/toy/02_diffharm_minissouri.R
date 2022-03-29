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


# calculate stats
dem_fair = with(minissouri, tapply(dem, pl_fair, sum)/tapply(pop, pl_fair, sum))[pl_fair]
dem_gerry_dem = with(minissouri, tapply(dem, pl_gerry_dem, sum)/tapply(pop, pl_gerry_dem, sum))[pl_gerry_dem]
dem_gerry_rep = with(minissouri, tapply(dem, pl_gerry_rep, sum)/tapply(pop, pl_gerry_rep, sum))[pl_gerry_rep]

dem_harm_gerry_dem = minissouri$dem * (dem_gerry_dem<0.5)*(dem_fair>0.5)
rep_harm_gerry_dem = minissouri$rep * (dem_gerry_dem>0.5)*(dem_fair<0.5)
dem_harm_gerry_rep = minissouri$dem * (dem_gerry_rep<0.5)*(dem_fair>0.5)
rep_harm_gerry_rep = minissouri$rep * (dem_gerry_rep>0.5)*(dem_fair<0.5)

diffharm_gerry_dem = sum(dem_harm_gerry_dem)/sum(minissouri$dem) -
    sum(rep_harm_gerry_dem)/sum(minissouri$rep)
diffharm_gerry_rep = sum(dem_harm_gerry_rep)/sum(minissouri$dem) -
    sum(rep_harm_gerry_rep)/sum(minissouri$rep)
cat("Minissouri D gerrymander diff. harm: ", round(diffharm_gerry_dem, 3), "\n")
cat("Minissouri R gerrymander diff. harm: ", round(diffharm_gerry_rep, 3), "\n")

# plotting
coords_fair = list(x=c(3/4, 1/4, 1/2),
                   y=c(1.04, 1.04, -0.04))
coords_gerry_dem = list(x=c(1/2, 1/2, 1/2),
                        y=c(1.04, -0.04, 1/2))
coords_gerry_rep = list(x=c(3/4, 7/12, 1/3),
                        y=c(1.04, -0.04, 1.04))

suppressMessages({
p1 = plot_state(minissouri, minissouri_people,
           pl_gerry_dem, dem_gerry_dem) +
    labs(title="(a) Democratic gerrymander")
for (i in 1:3) {
    p1 = add_lab(p1, fmt_lean(dem_gerry_dem[as.character(i)]),
                coords_gerry_dem$x[i], coords_gerry_dem$y[i])
}


p2 = plot_state(minissouri, minissouri_people,
           pl_fair, dem_fair) +
    labs(title="(b) Proportional plan")
for (i in 1:3) {
    p2 = add_lab(p2, fmt_lean(dem_fair[as.character(i)]),
                coords_fair$x[i], coords_fair$y[i])
}


p3 = plot_state(minissouri, minissouri_people,
           pl_gerry_rep, dem_gerry_rep) +
    labs(title="(c) Republican gerrymander")
for (i in 1:3) {
    p3 = add_lab(p3, fmt_lean(dem_gerry_rep[as.character(i)]),
                coords_gerry_rep$x[i], coords_gerry_rep$y[i])
}


p4 = plot_state(minissouri, minissouri_people,
           pl_gerry_dem, dem_harm_gerry_dem + rep_harm_gerry_dem) +
    scale_fill_gradient("Total\nharm", low="#e5e6e5", high="#666666", limits=c(0, 8)) +
    labs(title="(d) Voter harm, (a) vs. (b)")

p6 = plot_state(minissouri, minissouri_people,
           pl_gerry_rep, dem_harm_gerry_rep + rep_harm_gerry_rep) +
    scale_fill_gradient("Total\nharm", low="#e5e6e5", high="#666666", limits=c(0, 8)) +
    labs(title="(f) Voter harm, (c) vs. (b)")
})

d_harm = tibble(
    party=c("Dem.", "Dem.", "Rep.", "Rep."),
    gerry=c("Dem.", "Rep.", "Dem.", "Rep."),
    harm = c(sum(dem_harm_gerry_dem)/sum(minissouri$dem),
             sum(dem_harm_gerry_rep)/sum(minissouri$dem),
             sum(rep_harm_gerry_dem)/sum(minissouri$rep),
             sum(rep_harm_gerry_rep)/sum(minissouri$rep))
)

p5 = ggplot(d_harm, aes(x=paste(gerry, "gerrymander"), y=harm, fill=party)) +
    geom_col(position="dodge") +
    scale_fill_manual(values=c(GOP_DEM[14], GOP_DEM[2]), guide="none") +
    scale_y_continuous("Average harm", expand=expansion(mult=c(0, 0.05))) +
    annotate("segment", x=0.9, xend=0.9, y=d_harm$harm[1], yend=d_harm$harm[3],
             arrow = arrow(ends="both", angle=90, length=unit(.2, "cm")), size=1) +
    annotate("text", x=0.9, y=0.25, angle=90, label="Differential harm",
             vjust=-0.5, family="Times", size=3) +
    annotate("segment", x=2.1, xend=2.1, y=d_harm$harm[2], yend=d_harm$harm[4],
             arrow = arrow(ends="both", angle=90, length=unit(.2, "cm")), size=1) +
    annotate("text", x=2.1, y=0.25, angle=90, label="Differential harm",
             vjust=1.5, family="Times", size=3) +
    labs(title="(e) Voter harm by party", x=NULL) +
    coord_fixed(ratio=4.2) +
    theme_repr() +
    theme(plot.title=element_text(hjust=0.5))


p1 + p2 + p3 + p4 + p5 + p6 +
    plot_layout(guides="collect")

ggsave(here("paper/figures/diffharm_minissouri.pdf"), width=8, height=5.1)
