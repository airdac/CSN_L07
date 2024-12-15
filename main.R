# UPC. FIB. 2024-25, fall semester.
# CSN. Lab work 7. Simulation of SIS model over networks
# Adrià Casanova, Emma Meneghini

# Load the libraries
library(igraph)

# Tasks 1 and 2 ####
# Define the parameters
n <- 1000                                     # number of nodes
m_ER <- 2*n                                   # number of edges in the ER graph
m_BA <- 1                                     # number of edges to add at each step in the BA model

t_max <- 20                                   # number of time steps
p0 <- c(1/100, 5/100, 10/100, 20/100)         # fraction of nodes initially infected

gamma <- c(0.4, 0.05, 0.8, 0.9, 0.45)         # probability of recovery
beta <- c(0.0004, 0.0025, 0.16, 0.45, 0.9)    # probability of infecting

# Create ER graph
ER <- sample_gnm(n, m_ER)
ER$name <- "ER"
# Create BA graph
BA <- sample_pa(n, m_BA, directed = FALSE)
BA$name <- "BA"
# Create a WS model
WS <- sample_smallworld(dim = 1, n, nei = 2, p = 0.2)
WS <- simplify(WS)
WS$name <- "WS"
# Create a fully connected network
FC <- make_full_graph(n)
FC$name <- "Fully Connected"
# Create a tree
Tree <- make_tree(n, mode = "undirected")
Tree$name <- "Tree"
# Create a star network
Star <- make_star(n, mode = "undirected")
Star$name <- "Star"
# Create a lattice
Lattice <- make_lattice(dimvector = c(10, 10, 10))
Lattice$name <- "Lattice"

list_graphs <- list(ER, BA, WS, FC, Tree, Star, Lattice)
list_eigen <- list()
list_p_inf <- list()

for (graph in list_graphs) { # for every graph type
  
  cat("Processing graph: ", graph$name, "\n")
  
  # Adjacency matrix
  A <- as_adjacency_matrix(graph)
  
  # Eigenvector
  list_eigen[[graph$name]] <- eigen(A)$values[[1]]
  
  # Initialize list of proportion of infected for each initial proportion of infected
  # and each configuration of beta and gamma
  list_p_inf[[graph$name]] <- list()
  
  for (p in p0) { # for every initial fraction of infected nodes
    
    # Initialize sub-list
    list_p_inf[[graph$name]][[as.character(p)]] <- list()
    
    for (j in 1:length(beta)) {# for every configuration of beta and gamma
      
      # Create node labels with initial proportion of infected
      infected_idx <- sample(1:n, n*p)
      init_node_labels <- rep(0, n)
      init_node_labels[infected_idx] <- 1
      
      list_p_inf[[graph$name]][[as.character(p)]][[j]] <- vector(mode = "numeric", length = t_max+1)
      list_p_inf[[graph$name]][[as.character(p)]][[j]][1] <- p
      
      # Initialize the vector of node labels for the next time step
      final_node_labels <- init_node_labels
      
      for (t in 1:(t_max)) { # run the algorithm for t_max time steps
        
        for (i in 1:n) { # for all the nodes in the graph
          
          if (init_node_labels[i] == 1) { # if the node is infected
            
            if (runif(1) <= gamma[j]) { # with probability gamma it recovers in the next time step
              final_node_labels[i] <- 0
            }
            
          } else { # if the node is susceptible 
            
            # Calculate the number of infected neighbours
            k_infected <- sum(init_node_labels[which(A[i,] == 1)]) 
            
            if (runif(1) <= 1-((1-beta[j])^k_infected)) { # with this probability the node is infected
              final_node_labels[i] <- 1
            }
            
          }
          
        }
        
        # Calculate the proportion of infected nodes in the graph at time step t+1
        list_p_inf[[graph$name]][[as.character(p)]][[j]][[t+1]] <- sum(final_node_labels) / length(final_node_labels)
        
        # Update the vector of initial node labels
        init_node_labels <- final_node_labels
      }
      
    }
  }
}
    
    

cols <- c("#9e0142", "#d53e4f", "#f46d43", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")
for (p in p0) { # for every initial fraction of infected nodes
  for (j in 1:length(beta)) { # for every configuration of beta and gamma
    
    jpeg(paste0(p, "-", j, ".jpg"))
    par(mar = c(5, 4, 4, 10))
    
    for (g in 1:length(list_graphs)) { # for every graph type
      
      if (g == 1) {
        plot(seq(0, t_max, by = 1), 
             list_p_inf[[list_graphs[[g]]$name]][[as.character(p)]][[j]], 
             col = cols[g], type = "b", pch = 19, 
             ylim = c(0, 1), xlab = "t", ylab = "p")
      } else {
        lines(seq(0, t_max, by = 1), 
              list_p_inf[[list_graphs[[g]]$name]][[as.character(p)]][[j]], 
              col = cols[g], type = "b", pch = 19)
      }
    }
    
    legend("topright", c("ER", "BA", "WS", "Fully Connected", "Tree", "Star", "Lattice"),
           col = cols, lty = 1, pch = 19, xpd = TRUE, inset = c(-0.5, 0))
    dev.off()
    
  }
  
}


thresholds <- 1/unlist(list_eigen)
beta_over_gamma <- beta / gamma
for (i in 1:length(beta)) {
  for (j in 1:length(list_eigen)) {
    cat("Beta: ", beta[i], " Gamma: ", gamma[i], " Graph: ", list_graphs[[j]]$name, 
        ". The epidemic will continue?", (beta[i] / gamma[i] * list_eigen[[j]]) >= 1, "\n",
        sep = "")
  }
}
