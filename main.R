# UPC. FIB. 2024-25, fall semester.
# CSN. Lab work 7. Simulation of SIS model over networks
# Adrià Casanova, Emma Meneghini

library(igraph)

# Functions we might need to use
# adjacency_matrix()
# eigen

n <- 1000
m <- 2*n
t_max <- 20
p0 <- 1/100
g <- sample_gnm(n, m)
A <- as_adjacency_matrix(g)

infected_idx <- sample(1:n, n*p0)
infected <- rep(0, n)
infected[infected_idx] <- 1

