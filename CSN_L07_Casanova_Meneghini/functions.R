# UPC. FIB. 2024-25, fall semester.
# CSN. Lab work 7. Simulation of SIS model over networks
# Adrià Casanova, Emma Meneghini

# Load libraries
library(igraph)

# Infection simulation
simulate_spread <- function(graph, p, beta, gamma, n, t_max) {
  A <- as_adjacency_matrix(graph)
  
  # Create node labels with initial proportion of infected
  infected_idx <- sample(1:n, max(1, round(n * p)))
  node_labels <- matrix(0, nrow = t_max+1, ncol = n)
  node_labels[1, infected_idx] <- 1
  
  # Run the algorithm for t_max time steps
  for (t in 1:t_max) {
    infected <- node_labels[t, ] == 1
    
    # For every infected node, recover it with probability gamma
    n_infected <- sum(infected)
    recovered <- runif(n_infected) <= gamma
    
    # For every susceptible node, compute its number of infected neighbours
    k_infected <- A[!infected,] %*% infected
    
    # For every susceptible node, compute its probability of getting infected
    infection_prob <- 1 - (1 - beta)^k_infected
    
    # For every susceptible node, infect it with probability infection_prob
    n_susceptible <- sum(!infected)
    new_infected <- runif(n_susceptible) <= infection_prob
    
    # Assign labels to time step t + 1
    node_labels[t + 1, infected] <- as.numeric(recovered)
    node_labels[t + 1, !infected] <- as.numeric(new_infected)
  }
  
  # Calculate the proportion of infected nodes at each time step
  rowMeans(node_labels)
}

# Plot proportion of infected for every combination of parameters
plot_infected_proportion <- function(directory, p, beta_idx, beta, gamma, list_graphs, t_max, list_p_inf, cols) {
  jpeg(file.path(directory, paste0(p, "-", beta_idx, ".jpg"))
       , width=500, height = 400)
  par(mar = c(5, 4, 4, 4))
  
  # Plot for each graph
  lapply(seq_along(list_graphs), function(g) {
    
    # Extract the proportion of infected nodes for the graph, p0, and beta-gamma configuration
    infected_prop <- list_p_inf[[names(list_graphs)[g]]][[as.character(p)]][[beta_idx]]
    
    if (g == 1) {
      plot(seq(0, t_max, by = 1), 
           infected_prop, 
           col = cols[g], type = "b", pch = 19, 
           ylim = c(0, 1), xlab = "t", ylab = "p"
           , main = bquote(p[0] == .(p) ~ ", "
                           ~ beta == .(beta) ~ ", "
                           ~ gamma == .(gamma) ~ ", "
                           ~ beta/gamma == .(beta/gamma)))
    } else {
      lines(seq(0, t_max, by = 1), 
            infected_prop, 
            col = cols[g], type = "b", pch = 19)
    }
  })
  
  legend("topright", names(list_graphs),
         col = cols, lty = 1, pch = 19, xpd = TRUE, inset = c(-0.5, 0))
  dev.off()
}

# To combine all plots
plot_infected_proportion_no_legend <- function(p, beta_idx, beta, gamma, list_graphs, t_max, list_p_inf, cols) {
  # Just plot the lines/points
  first_graph <- TRUE
  for (g in seq_along(list_graphs)) {
    infected_prop <- list_p_inf[[names(list_graphs)[g]]][[as.character(p)]][[beta_idx]]
    if (first_graph) {
      plot(seq(0, t_max, by = 1), infected_prop, 
           col = cols[g], type = "b", pch = 19, 
           ylim = c(0, 1), xlab = "t", ylab = "p",
           main = bquote(p[0] == .(p) ~ ", "
                         ~ beta == .(beta) ~ ", "
                         ~ gamma == .(gamma) ~ ", "
                         ~ beta/gamma == .(beta/gamma)))
      first_graph <- FALSE
    } else {
      lines(seq(0, t_max, by = 1), infected_prop, col = cols[g], type = "b", pch = 19)
    }
  }
}

# Plot proportion of infected on a single graph for every beta-gamma configuration
plot_threshold <- function(directory, graph_name, p, beta, gamma, t_max, list_p_inf, thresholds, cols) {
  jpeg(file.path(directory, paste0(p, "-", graph_name, ".jpg"))
       , width = 700, height = 500)
  par(mar = c(5, 4, 4, 18))
  
  # Plot for each beta-gamma configuration
  lapply(seq_along(beta), function(beta_idx) {
    
    # Extract the proportion of infected nodes for the graph, p0, and beta-gamma configuration
    p_str <- sub("0+$", "", as.character(p)) # This fixes a weird indexing bug
    infected_prop <- list_p_inf[[graph_name]][[p_str]][[beta_idx]]
    
    if(length(infected_prop) <= 1) return()
    
    if (beta_idx == 1) {
      plot(seq(0, t_max, by = 1), 
           infected_prop, 
           col = cols[beta_idx], type = "b", pch = 19, 
           ylim = c(0, 1), xlab = "t", ylab = "p"
           , main = bquote(p[0] == .(p) ~ ", "
                           ~ Network == .(graph_name) ~ ", "
                           ~ Threshold == .(round(thresholds[graph_name], 4))))
    } else {
      lines(seq(0, t_max, by = 1), 
            infected_prop, 
            col = cols[beta_idx], type = "b", pch = 19)
    }
  })
  beta_gamma_list <- mapply(function(b, g) bquote(beta == .(b) ~ ", "
                                                  ~ gamma == .(g) ~ ", "
                                                  ~ beta/gamma == .(round(b/g,4))), 
                            round(beta,4), round(gamma, 4), SIMPLIFY = TRUE)
  legend("topright", legend = beta_gamma_list,
         col = cols, lty = 1, pch = 19, xpd = TRUE, inset = c(-0.65, 0))
  dev.off()
}

# To combine all plots
plot_threshold_combined <- function(directory, graph_name, p, beta, gamma, t_max, list_p_inf, thresholds, cols) {
  # Plot for each beta-gamma configuration
  first_plot <- TRUE
  for (beta_idx in seq_along(beta)) {
    p_str <- sub("0+$", "", as.character(p))
    infected_prop <- list_p_inf[[graph_name]][[p_str]][[beta_idx]]
    
    if (length(infected_prop) <= 1) next
    
    if (first_plot) {
      plot(seq(0, t_max, by = 1), 
           infected_prop, 
           col = cols[beta_idx], type = "b", pch = 19, 
           ylim = c(0, 1), xlab = "t", ylab = "p",
           main = NULL)  # Remove main title from plot
      # Add title aligned to the right
      title(main = bquote(p[0] == .(p) ~ ", "
                          ~ Network == .(graph_name) ~ ", "
                          ~ Threshold == .(round(thresholds[graph_name], 4))),
            adj = 0)  # adj = 1 for right alignment
      first_plot <- FALSE
    } else {
      lines(seq(0, t_max, by = 1), 
            infected_prop, 
            col = cols[beta_idx], type = "b", pch = 19)
    }
  }
  
  # Create legend labels
  beta_gamma_list <- mapply(function(b, g) {
    if (is.na(g)) return(NULL)
    else {
      bquote(gamma == .(g) ~ ", "
             ~ beta/gamma == .(round(b/g,4)))
    }
    }, round(beta,4), round(gamma, 4), SIMPLIFY = TRUE)
  # Hide legend of missing plots
  beta_gamma_list <- beta_gamma_list[!sapply(beta_gamma_list, is.null)]
  # Number of plots
  n_plots <- length(beta_gamma_list)
  # Add legend
  legend("topright", legend = beta_gamma_list,
         col = cols[1:n_plots], lty = 1, pch = 19, xpd = TRUE, inset = c(-0.4, 0))
}



