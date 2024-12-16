# UPC. FIB. 2024-25, fall semester.
# CSN. Lab work 7. Simulation of SIS model over networks
# Adrià Casanova, Emma Meneghini

# Load libraries
library(igraph)

t0 <- Sys.time()
# Create plots folder if necessary
if (!dir.exists("plots_vectorized")) {
  dir.create("plots_vectorized")
}

# Tasks 1 and 2 ####
# Define the parameters
n <- 1000                                     # number of nodes
m_ER <- 2*n                                   # number of edges in the ER graph
m_BA <- 1                                     # number of edges to add at each step in the BA model

t_max <- 20                                   # number of time steps
p0 <- c(1/100, 5/100, 10/100, 20/100)         # fraction of nodes initially infected

gamma <- c(0.4, 0.05, 0.8, 0.9, 0.45)         # probability of recovery
beta <- c(0.0004, 0.0025, 0.16, 0.45, 0.9)    # probability of infecting

# Create graphs
ER <- sample_gnm(n, m_ER)
BA <- sample_pa(n, m_BA, directed = FALSE)
WS <- sample_smallworld(dim = 1, n, nei = 2, p = 0.2); WS <- simplify(WS)
FC <- make_full_graph(n) # Fully Connected
Tree <- make_tree(n, mode = "undirected")
Star <- make_star(n, mode = "undirected")
Lattice <- make_lattice(dimvector = c(10, 10, 10))

list_graphs <- list(ER, BA, WS, FC, Tree, Star, Lattice)
list_graphs <- setNames(list_graphs, c("ER", "BA", "WS", "Fully Connected"
                                       , "Tree", "Star", "Lattice"))

# Compute leading eigenvalue
list_eigen <- sapply(list_graphs, function(g) eigen(as_adjacency_matrix(g))$values[1])

# Infection simulation
simulate_spread <- function(graph, p, beta, gamma) {
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

# Run simulations

# Compute list of proportion of infected for each initial proportion of infected
# and each configuration of beta and gamma
list_p_inf <- lapply(list_graphs, function(graph) {
  
  cat("Processing graph: ", graph$name, "\n")
  
  # Create a list of results for each initial fraction of infected (p0)
  p_results <- lapply(p0, function(p) {
    
    # Create a list of proportions of infected for every configuration of beta and gamma
    beta_results <- sapply(seq_along(beta), function(beta_idx) {
      
      # Run the simulation
      result <- simulate_spread(graph, p, beta[beta_idx], gamma[beta_idx])
      setNames(result, seq(0, t_max))
    }, simplify = FALSE)
    
    setNames(beta_results, seq_along(beta))
  })
  
  setNames(p_results, as.character(p0))
})
list_p_inf <- setNames(list_p_inf, names(list_graphs))


# Plot proportion of infected for every combination of parameters
cols <- c("#9e0142", "#d53e4f", "#f46d43", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")
plot_infected_proportion <- function(p, beta_idx, list_graphs, t_max, list_p_inf, cols) {
  jpeg(file.path("plots_vectorized", paste0(p, "-", beta_idx, ".jpg")))
  par(mar = c(5, 4, 4, 10))
  
  # Plot for each graph
  lapply(seq_along(list_graphs), function(g) {
    
    # Extract the proportion of infected nodes for the graph, p0, and beta-gamma configuration
    infected_prop <- list_p_inf[[names(list_graphs)[g]]][[as.character(p)]][[beta_idx]]
    
    if (g == 1) {
      plot(seq(0, t_max, by = 1), 
           infected_prop, 
           col = cols[g], type = "b", pch = 19, 
           ylim = c(0, 1), xlab = "t", ylab = "p")
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

param_combinations <- expand.grid(p = p0, beta_idx = seq_along(beta))
invisible(
  apply(param_combinations, 1, function(params) {
    plot_infected_proportion(params['p'], params['beta_idx']
                             , list_graphs, t_max, list_p_inf, cols)
  })
)


# Show whether theoretical epidemic thresholds will be surpassed
thresholds <- 1/unlist(list_eigen)
beta_over_gamma <- beta / gamma
param_combinations <- expand.grid(eigen_idx = seq_along(list_eigen), beta_idx = seq_along(beta))
invisible(
  mapply(function(eigen_idx, beta_idx) {
    cat("Beta: ", beta[beta_idx], " Gamma: ", gamma[beta_idx], " Graph: ", 
        names(list_graphs)[eigen_idx], ". The epidemic will continue? ",
        beta_over_gamma[beta_idx] >= thresholds[eigen_idx],
        "\n", sep = "")
  }, param_combinations$eigen_idx, param_combinations$beta_idx)
)

tf <- Sys.time()
cat("Running time of vectorized code (s):", difftime(tf, t0, units = "secs"))
