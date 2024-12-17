# UPC. FIB. 2024-25, fall semester.
# CSN. Lab work 7. Simulation of SIS model over networks
# Adrià Casanova, Emma Meneghini

# Load libraries
library(igraph)
source("functions.R")

t0 <- Sys.time()

# Create tables and plots folders if necessary
if (!dir.exists("tables")) {
  dir.create("tables")
}
if (!dir.exists("plots_vectorized1")) {
  dir.create("plots_vectorized1")
}
if (!dir.exists("plots_vectorized2")) {
  dir.create("plots_vectorized2")
}
if (!dir.exists(file.path("plots_vectorized2", "below"))) {
  dir.create(file.path("plots_vectorized2", "below"))
}
if (!dir.exists(file.path("plots_vectorized2", "above"))) {
  dir.create(file.path("plots_vectorized2", "above"))
}

cols <- c("#9e0142", "#d53e4f", "#f46d43", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")

##############################
# Task 1
##############################

# Define the parameters
n <- 1000                                     # number of nodes
m_ER <- 2*n                                   # number of edges in the ER graph
m_BA <- 1                                     # number of edges to add at each step in the BA model

t_max <- 50                                   # number of time steps
p0 <- c(1/100, 5/100, 10/100, 20/100)         # fraction of nodes initially infected

# Beta and gamma chosen so that beta/gamma = 0.001 0.050 0.200 0.500 2.000
gamma <- c(0.4, 0.05, 0.8, 0.9, 0.45)         # probability of recovery
beta <- c(0.0004, 0.0025, 0.16, 0.45, 0.9)    # probability of infecting

# Create graphs
ER <- sample_gnm(n, m_ER)
ER <- set_graph_attr(ER, "name", "ER")

BA <- sample_pa(n, m_BA, directed = FALSE)
BA <- set_graph_attr(BA, "name", "BA")

WS <- sample_smallworld(dim = 1, n, nei = 2, p = 0.2)
WS <- simplify(WS)
WS <- set_graph_attr(WS, "name", "WS")

FC <- make_full_graph(n) 
FC <- set_graph_attr(FC, "name", "Fully Connected")

Tree <- make_tree(n, mode = "undirected")
Tree <- set_graph_attr(Tree, "name", "Tree")

Star <- make_star(n, mode = "undirected")
Star <- set_graph_attr(Star, "name", "Star")

Lattice <- make_lattice(dimvector = c(10, 10, 10))
Lattice <- set_graph_attr(Lattice, "name", "Lattice")

list_graphs <- list(ER, BA, WS, FC, Tree, Star, Lattice)
list_graphs <- setNames(list_graphs, c("ER", "BA", "WS", "Fully Connected"
                                       , "Tree", "Star", "Lattice"))

# Compute leading eigenvalue
list_eigen <- sapply(list_graphs, function(g) eigen(as_adjacency_matrix(g))$values[1])

# Run simulations

# Compute list of proportion of infected for each initial proportion of infected
# and each configuration of beta and gamma
list_p_inf <- lapply(list_graphs, function(graph) {
  
  cat("Processing graph: ", graph$name, "\n")
  
  # Create a list of results for each initial fraction of infected (p0)
  p_results <- lapply(p0, function(p) {
    
    # Create a list of proportions of infected for every configuration of beta and gamma
    beta_results <- sapply(seq_along(beta), function(beta_idx) {
      
      # Run the simulation 15 times and average results
      run_results <- replicate(15, simulate_spread(graph, p, beta[beta_idx]
                                                   , gamma[beta_idx], n, t_max))
      avg_result <- rowMeans(run_results)
      setNames(avg_result, seq(0, t_max))
    }, simplify = FALSE)
    
    setNames(beta_results, seq_along(beta))
  })
  
  setNames(p_results, as.character(p0))
})
list_p_inf <- setNames(list_p_inf, names(list_graphs))


# Plot proportion of infected for every combination of parameters
param_combinations <- expand.grid(p = p0, beta_idx = seq_along(beta))
invisible(
  apply(param_combinations, 1, function(params) {
    plot_infected_proportion("plots_vectorized1", params['p'], params['beta_idx']
                             , beta[params['beta_idx']]
                             , gamma[params['beta_idx']]
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

thresholds_surpassed <- outer(beta_over_gamma, thresholds, ">=")
rownames(thresholds_surpassed) <- beta_over_gamma
print("Table: reachability of the theoretical threshold for every beta/gamma")
print(thresholds_surpassed)
write.table(thresholds_surpassed, file.path("tables", "thresholds_surpassed.txt"), sep = "\t"
            , row.names = TRUE, col.names = TRUE, quote = FALSE)

# Analysis of the plots based on the previous table:
# beta/gamma = 0.001: correct
# beta/gamma = 0.05: correct except for Star, which has a threshold close to 0.05
# beta/gamma = 0.2: correct except for WS, which has a threshold close to 0.2
# beta/gamma = 0.5: correct
# beta/gamma = 2: correct

thresholds[c("Star", "WS")]

##############################
# Task 2
##############################

# For each beta value and graph, compute the gamma value s.t. beta/gamma = threshold

# Fully Connected and Star graphs require a very low beta to reach the threshold.
# We want to run each simulation with at least 3 beta-gamma configurations,
# so we combine reasonable configurations with 3 that have a very low beta

beta2_small <- exp(seq(log(1e-4), log(5e-4), length.out = 3))
beta2_large <- exp(seq(log(0.01), log(0.1), length.out = 3))
beta2_very_large <- exp(seq(log(0.14), log(0.3), length.out = 5))
beta2 <- c(beta2_small, beta2_large, beta2_very_large)
gamma2 <- outer(beta2, thresholds, "/")
rownames(gamma2) <- round(beta2, 4)
gamma2[gamma2 > 1] <- 1

# Results are robust. There is only a pandemic when gamma is very large
# which does not coincide with the 1/lambda threshold, but does coincide with
# the standard SIS model because gamma > beta
gamma2_below <- gamma2 * 1.5
gamma2_below[gamma2_below > 1] <- 1
gamma2_below

# Results are not very robust.
gamma2_above <- gamma2 * 0.5
gamma2_above

# Run simulations with beta-gamma configurations below threshold
list_p_inf2_below <- lapply(list_graphs, function(graph) {
  
  cat("Processing graph: ", graph$name, "\n")
  
  # Create a list of results for each initial fraction of infected (p0)
  p_results <- lapply(p0, function(p) {
    
    # Create a list of proportions of infected for every configuration of beta and gamma
    beta_results <- sapply(seq_along(beta2), function(beta_idx) {
      # If the threshold cannot be reached, return NA
      if (is.na(gamma2_below[beta_idx, graph$name])) return(NA)
      
      # Run the simulation 15 times and average results
      run_results <- replicate(15, simulate_spread(graph, p, beta2[beta_idx]
                                                   , gamma2_below[beta_idx, graph$name]
                                                   , n, t_max))
      avg_result <- rowMeans(run_results)
      setNames(avg_result, seq(0, t_max))
    }, simplify = FALSE)
    
    setNames(beta_results, seq_along(beta2))
  })
  
  setNames(p_results, p0)
})
list_p_inf2_below <- setNames(list_p_inf2_below, names(list_graphs))

# Plot proportion of infected with beta-gamma configurations below threshold
param_combinations <- expand.grid(p = p0, graph_name = names(list_graphs))
invisible(
  apply(param_combinations, 1, function(params) {
    plot_threshold(file.path("plots_vectorized2", "below"), params['graph_name']
                   , params['p'], beta2, gamma2_below[,params['graph_name']]
                   , t_max, list_p_inf2_below, thresholds
                   , colorRampPalette(cols)(11))
  })
)

# Run simulations with beta-gamma configurations above threshold
list_p_inf2_above <- lapply(list_graphs, function(graph) {
  
  cat("Processing graph: ", graph$name, "\n")
  
  # Create a list of results for each initial fraction of infected (p0)
  p_results <- lapply(p0, function(p) {
    
    # Create a list of proportions of infected for every configuration of beta and gamma
    beta_results <- sapply(seq_along(beta2), function(beta_idx) {
      # If the threshold cannot be reached, return NA
      if (is.na(gamma2_above[beta_idx, graph$name])) return(NA)
      
      # Run the simulation 15 times and average results
      run_results <- replicate(15, simulate_spread(graph, p, beta2[beta_idx]
                                                   , gamma2_above[beta_idx, graph$name]
                                                   , n, t_max))
      avg_result <- rowMeans(run_results)
      setNames(avg_result, seq(0, t_max))
    }, simplify = FALSE)
    
    setNames(beta_results, seq_along(beta2))
  })
  
  setNames(p_results, p0)
})
list_p_inf2_above <- setNames(list_p_inf2_above, names(list_graphs))

# Plot proportion of infected with beta-gamma configurations above threshold
param_combinations <- expand.grid(p = p0, graph_name = names(list_graphs))
invisible(
  apply(param_combinations, 1, function(params) {
    plot_threshold(file.path("plots_vectorized2", "above"), params['graph_name']
                   , params['p'], beta2, gamma2_above[,params['graph_name']]
                   , t_max, list_p_inf2_above, thresholds
                   , colorRampPalette(cols)(11))
  })
)









tf <- Sys.time()
cat("Running time of vectorized code (s):", difftime(tf, t0, units = "secs"))
