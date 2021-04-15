combine_cowplots <- function(g1, g2, nrow = 1, common_legend = TRUE, ...) {
  if (common_legend) {
    legend <- get_legend(g1)
    g1 <- g1 + theme(legend.position = "none")
    g2 <- g2 + theme(legend.position = "none")
  }
  prow <- cowplot::plot_grid(
    g1,
    g2,
    align = "vh",
    labels = c("A", "B"),
    hjust = -1,
    nrow = nrow
  )
  if (common_legend) {
    cowplot::plot_grid(prow, legend, rel_widths = c(3, .5), ...)
  } else {
    prow
  }
}

combine_cowplots_2 <- function(g1, g2, g3) {
  lgd <- get_legend(g1)
  bottom_row <- plot_grid(g3, labels = "C", align = "h", rel_widths = c(1, .1))
  plot_grid(combine_cowplots(g1, g2, common_legend = TRUE), bottom_row, nrow = 2)
}

simple_cowplot <- function(g1, g2) {
  leg <- get_legend(g1 + theme(legend.position = "bottom"))
  p <- plot_grid(g1 + theme(legend.position = "none"),
    g2 + theme(legend.position = "none"),
    align = "vh",
    labels = c("A", "B"),
    hjust = -1,
    nrow = 2,
    scale = .9
  )
  plot_grid(p, leg, ncol = 1, rel_heights = c(1, .2))
}

combine_cowplots_3 <- function(g1, g2, g3) {
  g1 <- g1 + theme(legend.position = "none")
  g2 <- g2 + theme(legend.position = "none")
  g3 <- g3 + theme(legend.position = "none")
  theme_set(theme_cowplot(font_size = 10))
  plot_grid(g1, g2, g3,
    labels = "AUTO",
    nrow = 3, align = "v"
  )
}
