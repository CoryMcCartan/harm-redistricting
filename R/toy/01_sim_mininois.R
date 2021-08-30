pl_mininois = sim_toy(mininois, N=2000)

pl_mininois %>%
    mutate(u_g_dem = rep(utility_global(., dem), each=3),
           u_g_rep = rep(utility_global(., dem, invert=T), each=3),
           u_g = total(u_g_dem, u_g_rep, mininois, dem, rep)) %>%
    arrange(desc(u_g)) %>%
    as.matrix() %>%
    `[`(, 1) %>%
    datapasta::dpasta()
x = c(2, 2, 2, 3, 3, 3, 2, 2, 2, 3, 3, 3, 2, 2, 2, 3, 3, 3, 2, 2, 2, 3, 3, 3, 1, 1, 3, 3, 3, 3, 1, 1, 1, 3, 3, 3)

redist.plot.plans(pl_mininois, 1, mininois, dem) +
    scale_fill_party_c() +
    geom_sf(data=mininois, size=0.8, fill=NA, color="white") +
    geom_sf(aes(color=dem), data=mininois_people, size=5) +
    scale_color_party_d(guide="none")
