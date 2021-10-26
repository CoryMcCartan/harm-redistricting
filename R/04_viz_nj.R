svc::svc(pl$distr, dem) + scale_color_party_d()

ggsave('paper/figures/svc.png', dpi = 320, height = 6, width = 6)

counterfactual <- function(map, pl, r, d, lim = 1, ref_idx = 1) {
  r <- rlang::eval_tidy(rlang::enquo(r), data = map)
  d <- rlang::eval_tidy(rlang::enquo(d), data = map)
  map %>%
    mutate(cf = calc_cf(map, pl, r, d, ref_idx = ref_idx)) %>%
    ggplot() +
    geom_sf(aes(fill = cf), lwd = 0.005) +
    theme_void() +
    scale_fill_party_c('Simulated - Enacted',
                       midpoint = 0,
                       limits = c(min(lim, -lim), max(lim, -lim)),
    )
}


calc_cf <- function(map, pl, r, d, ref_idx) {
  pl <- pl %>%
    mutate(pct_dem = group_frac(map, r, r + d))

  m_pl <- as.matrix(pl)
  m_val <- pl %>%
    arrange(as.integer(draw), district) %>%
    pull(pct_dem) %>%
    matrix(nrow = attr(map, 'ndists'))

  m_prec <- matrix(nrow = nrow(m_pl), ncol = ncol(m_pl))
  for (i in seq_len(ncol(m_pl))) {
    m_prec[, i] <- m_val[, i][m_pl[, i]]
  }

  rowMeans(-m_prec[, -1:-4] + m_prec[, ref_idx])
}

counterfactual(nj, pl$distr, r = nrv, d = ndv, ref_idx = 1, lim = 0.5)
counterfactual(nj, pl$distr, r = nrv, d = ndv, ref_idx = 1, lim = 0.5)
