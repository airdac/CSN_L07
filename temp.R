param_combinations <- expand.grid(p = p0, graph_name = names(list_graphs))

# Determine layout
n_plots <- nrow(param_combinations)
n_col <- 4
n_row <- ceiling(n_plots / n_col)

path.file <- file.path("combined_plots", "combined_plots2_below.pdf")
pdf(path.file, width = 10, height = 12)  # same 5:6 ratio as 1000x1200

# Adjust cex and lwd to make text, points, and lines smaller
par(mfrow = c(n_row, n_col), 
    mar = c(5,4,4,20), 
    oma = c(0,0,0,0),
    cex = 0.3,        # scale down overall text/points
    cex.axis = 0.7,
    cex.lab = 0.7,
    cex.main = 1.8,
    lwd = 0.7)         # thinner lines

invisible(
  apply(param_combinations, 1, function(params) {
    plot_threshold_combined(file.path("plots_vectorized2", "below"), params['graph_name']
                             , params['p'], beta2, gamma2_below[,params['graph_name']]
                             , t_max, list_p_inf2_below, thresholds
                             , colorRampPalette(cols)(11))
  })
)

par(xpd = NA)
dev.off()