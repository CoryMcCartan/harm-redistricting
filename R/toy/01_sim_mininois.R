pl_mininois = sim_toy(mininois)

redist.plot.plans(pl_mininois, 1, mininois, dem) +
    scale_fill_party_c() +
    geom_sf(data=mininois, size=0.8, fill=NA, color="white") +
    geom_sf(aes(color=dem), data=mininois_people, size=7) +
    scale_color_party_d()
