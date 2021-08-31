pl_opt = list(
    mininois = c(2, 2, 3, 3, 3, 3,
                 2, 2, 3, 3, 3, 3,
                 2, 2, 2, 3, 3, 3,
                 2, 2, 2, 3, 3, 3,
                 1, 1, 1, 3, 3, 3,
                 1, 1, 1, 3, 3, 3),
    minissouri = c(3, 3, 3, 3, 3, 3,
                   2, 2, 3, 3, 3, 3,
                   2, 2, 3, 3, 3, 3,
                   2, 2, 2, 2, 1, 1,
                   2, 2, 2, 1, 1, 1,
                   2, 2, 2, 1, 1, 1),
    minichusetts = rep(c(1, 1, 2, 2, 3, 3), 6)
)

dem_opt = imap(pl_opt, function(pl, nm) {
    with(get(nm), tapply(dem, pl, sum)/tapply(pop, pl, sum))[pl]
})

coords = list(
    mininois = list(x=c(1/4, 1/6, 2/3),
                    y=c(1.04, -0.04, -0.04)),
    minissouri = list(x=c(3/4, 1/4, 1/2),
                      y=c(1.04, 1.04, -0.04)),
    minichusetts = list(x=c(1/6, 1/2, 5/6),
                        y=c(-0.04, 1.04, -0.04))
)

plot_opt = imap(names(pl_opt), function(nm, i) {
    p = suppressMessages(plot_state(get(nm), get(paste0(nm, "_people")),
                                    pl_opt[[nm]], dem_opt[[nm]])) +
        labs(title=paste(c("(a)", "(b)", "(c)")[i], str_to_title(nm)))
    for (i in 1:3) {
        p = add_lab(p, fmt_lean(dem_opt[[nm]][as.character(i)]),
                    coords[[nm]]$x[i], coords[[nm]]$y[i])
    }
    p
})

wrap_plots(plot_opt) +
    plot_layout(guides="collect")
ggsave(here("paper/figures/minis_util_opt.pdf"), width=7, height=2.5)


