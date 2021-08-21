pl_mininois = sim_toy(mininois)

redist.plot.plans(pl_mininois, 1, mininois, dem) +
    .scale_fill_party(midpoint=0.5, limits=0:1) +
    geom_sf(aes(color=dem), data=mininois_people, size=5) +
    scale_color_manual(values=c(.GOP, .DEM))
