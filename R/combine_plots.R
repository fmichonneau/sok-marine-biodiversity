combine_cowplots <- function(g1, g2, nrow = 1, common_legend = TRUE, ...) {
    if (common_legend) {
        legend <- get_legend(g1)
        g1 <- g1 + theme(legend.position = "none")
        g2 <- g2 + theme(legend.position = "none")
    }
    prow <- cowplot::plot_grid(
                         g1,
                         g2,
                 align = 'vh',
                 labels = c("A", "B"),
                 hjust = -1,
                 nrow = nrow
                 )
    if (common_legend) {
        cowplot::plot_grid(prow, legend, rel_widths = c(3, .5), ...)
    } else prow

}

combine_cowplots_2 <- function(g1, g2, g3) {
    lgd <- get_legend(g1)
    bottom_row <- plot_grid(g3, labels = "C", align = "h", rel_widths = c(1, .1))
    plot_grid(combine_cowplots(g1, g2, common_legend = TRUE), bottom_row, nrow = 2)
}
