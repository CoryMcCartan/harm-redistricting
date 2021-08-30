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

plot_opt = imap(names(pl_opt), function(nm, i) {
    plot_state(get(nm), get(paste0(nm, "_people")),
               pl_opt[[nm]], dem_opt[[nm]]) +
        labs(title=paste(c("(a)", "(b)", "(c)")[i], str_to_title(nm)))
})

wrap_plots(plot_opt) +
    plot_layout(guides="collect")
ggsave(here("paper/figures/minis_util_opt.pdf"), width=7, height=2.5)


