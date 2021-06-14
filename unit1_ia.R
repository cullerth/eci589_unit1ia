##1. PREPARE
#1a. review the reserach

#1b. identify a question: 
# How does the DLT 2 dataset compare to the results of the question we we asked about the DLT 1 dataset?: "To what extent did educators engage with other participants in the discussion forums?"

#1c. load packages: 
library(igraph)
library(tidyverse)

##2. WRANGEL
#2a. import data: 
#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ZZH3UB
ties <- read_csv("data/dlt2edgelist.csv", 
                 col_types = cols(Sender = col_character(), 
                                  Receiver = col_character(), 
                                  `Category Text` = col_skip(), 
                                  `Comment ID` = col_character(), 
                                  `Discussion ID` = col_character()))

ties

actors <- read_csv("data/dlt2nodes.csv", 
                   col_types = cols(UID = col_character(), 
                                    Facilitator = col_character(), 
                                    expert = col_character(), 
                                    connect = col_character()))

#actors


#2b. Create a network object
network <- graph_from_data_frame(d = ties, 
                                 vertices = actors, 
                                 directed = T) 

#network

simple_network <- simplify(network, remove.loops = TRUE) 
# > simple_network <- simplify(network, remove.loops = TRUE) 
# Error in simplify(network, remove.loops = TRUE) : 
#   unused argument (remove.loops = TRUE)

# simple_network

edge_weights <- count(ties, Sender, Receiver)

edge_weights

E(network)$weight <- 1  

network

weighted_network <- simplify(network,
                             edge.attr.comb = list(weight="sum")
)
# > weighted_network <- simplify(network,
#                                +                              edge.attr.comb = list(weight="sum")
#                                + )
# Error in simplify(network, edge.attr.comb = list(weight = "sum")) : 
#   unused argument (edge.attr.comb = list(weight = "sum"))

weighted_network

##3. EXPLORE
#3a. examine basic descriptives
node_degree <- degree(weighted_network, mode = "all")


