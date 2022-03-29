# Mininois
pl_fair = c(2, 2, 3, 3, 3, 3,
            2, 2, 3, 3, 3, 3,
            2, 2, 3, 3, 3, 3,
            2, 2, 2, 3, 3, 3,
            2, 1, 1, 3, 3, 3,
            1, 1, 1, 3, 3, 3)
pl_gerry = c(2, 2, 2, 3, 3, 1,
             2, 2, 2, 3, 3, 1,
             2, 2, 2, 3, 3, 1,
             2, 2, 3, 3, 3, 1,
             2, 3, 3, 3, 1, 1,
             1, 1, 1, 1, 1, 1)

dem_gerry = with(mininois, tapply(dem, pl_gerry, sum)/tapply(pop, pl_gerry, sum))[pl_gerry]
dem_fair = with(mininois, tapply(dem, pl_fair, sum)/tapply(pop, pl_fair, sum))[pl_fair]
dem_harm = weighted.mean((dem_gerry<0.5)*(dem_fair>0.5), mininois$dem)
rep_harm = weighted.mean((dem_gerry>0.5)*(dem_fair<0.5), mininois$rep)

coords_fair = list(x=c(1/4, 1/6, 2/3),
                   y=c(1.04, -0.04, -0.04))
coords_gerry = list(x=c(1/2, 2/3, 1/4),
                    y=c(1.04, -0.04, -0.04))

suppressMessages({
# Dem gerry plot
p1 = plot_state(mininois, mininois_people,
           pl_gerry, dem_gerry) +
    labs(title="(a) Democratic gerrymander")
for (i in 1:3) {
    p1 = add_lab(p1, fmt_lean(dem_gerry[as.character(i)]),
                coords_gerry$x[i], coords_gerry$y[i])
}
p1 = p1 + guides(fill="none")

# Fair plan plot
p2 = plot_state(mininois, mininois_people,
                pl_fair, dem_fair) +
    labs(title="(b) Proportional plan")
for (i in 1:3) {
    p2 = add_lab(p2, fmt_lean(dem_fair[as.character(i)]),
                 coords_fair$x[i], coords_fair$y[i])
}

# Harm plot
p3 = plot_state(mininois, mininois_people,
           pl_gerry, mininois$rep*round(1 - dem_fair)) +
    scale_fill_gradient("Total\nharm", low="#e5e6e5", high="#666666") +
    labs(title="(c), Voter harm, (a) vs. (b)") +
    theme(legend.key.width=unit(0.2, "cm"))
})

p1 + p2 + p3
ggsave(here("paper/figures/harm_mininois.pdf"), width=7, height=2.5)


