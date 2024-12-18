# UPC. FIB. 2024-25, fall semester.
# CSN. Lab work 7. Simulation of SIS model over networks
# Adrià Casanova, Emma Meneghini

if (!dir.exists("combined_plots")) {
  dir.create("combined_plots")
}

################
# Task 1
################
param_combinations <- expand.grid(p = p0, beta_idx = seq_along(beta))

# Determine layout
n_plots <- nrow(param_combinations)
n_col <- 4
n_row <- ceiling(n_plots / n_col)

path.file <- file.path("combined_plots", "combined_plots1.pdf")
pdf(path.file, width = 10, height = 12)  # same 5:6 ratio as 1000x1200

# Adjust cex and lwd to make text, points, and lines smaller
par(mfrow = c(n_row, n_col), 
    mar = c(5,4,4,4), 
    oma = c(0,0,0,13),
    cex = 0.3,        # scale down overall text/points
    cex.axis = 0.7,
    cex.lab = 0.7,
    cex.main = 1.8,
    lwd = 0.7)         # thinner lines

invisible(
  apply(param_combinations, 1, function(params) {
    plot_infected_proportion_no_legend(params['p'], params['beta_idx'],
                                       beta[params['beta_idx']],
                                       gamma[params['beta_idx']],
                                       list_graphs, t_max, list_p_inf, cols)
  })
)

# Adjust legend text size as well if needed
par(xpd = NA)
legend("topright", legend = names(list_graphs), col = cols, pch = 19, bty = "n",
       inset = c(-0.55,-5.15), cex = 2, title = expression(bold("Network")))


dev.off()

################
# Task 2 below
################
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

################
# Task 2 above
################
param_combinations <- expand.grid(p = p0, graph_name = names(list_graphs))

# Determine layout
n_plots <- nrow(param_combinations)
n_col <- 4
n_row <- ceiling(n_plots / n_col)

path.file <- file.path("combined_plots", "combined_plots2_above.pdf")
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
    plot_threshold_combined(file.path("plots_vectorized2", "above"), params['graph_name']
                            , params['p'], beta2, gamma2_above[,params['graph_name']]
                            , t_max, list_p_inf2_above, thresholds
                            , colorRampPalette(cols)(11))
  })
)

par(xpd = NA)
dev.off()