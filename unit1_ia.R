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


actors


#2b. Create a network object
network <- graph_from_data_frame(d = ties, 
                                 vertices = actors, 
                                 directed = T) 

network

simple_network <- igraph::simplify(network, remove.loops = TRUE) 

simple_network

edge_weights <- count(ties, Sender, Receiver)

edge_weights

E(network)$weight <- 1  

network

weighted_network <- igraph::simplify(network,
                             edge.attr.comb = list(weight="sum")
)

weighted_network

##3. EXPLORE
#3a. examine basic descriptives
node_degree <- degree(weighted_network, mode = "all")

hist(node_degree, breaks = 30)

mean(node_degree)

median(node_degree)

in_degree <- degree(weighted_network, mode="in")

hist(in_degree, breaks = 30)

mean(in_degree)

median(in_degree)

out_degree <- degree(weighted_network, mode="out")

hist(out_degree, breaks = 30)

mean(out_degree)

median(out_degree)

weights <- E(weighted_network)$weight

hist(weights, breaks = 10)

mean(weights)

median(weights)

# e_weights <- E(edge_weights)$weight
# 
# hist(e_weights, breaks = 10)
# mean(e_weights)
# median(e_weights)
#getting this error: Error in E(edge_weights) : Not a graph object 
#also just not super sure what is being asked here...

#3b. Make a Sociogram
plot(weighted_network)

plot(weighted_network,
     vertex.label = NA)

plot(weighted_network,
     vertex.label = NA,
     vertex.size = 1)

plot(weighted_network,
     vertex.label = NA,
     vertex.size = node_degree)

plot(weighted_network,
     vertex.label = NA,
     vertex.size = node_degree/10)

plot(weighted_network,
     vertex.label = NA,
     vertex.size = node_degree*.1,
     edge.arrow.size = .04)

plot(weighted_network,
     vertex.label = NA,
     vertex.size = node_degree*.05,
     edge.arrow.size = .04,
     edge.width = .2)

plot(weighted_network,
     vertex.label = NA,
     vertex.size = node_degree*.1,
     edge.arrow.size = .04,
     edge.width = E(weighted_network)$weight)

plot(weighted_network,
     vertex.label = NA,
     vertex.size = node_degree*.05,
     edge.arrow.size = .05,
     edge.width = E(weighted_network)$weight/5)

plot(weighted_network,
     vertex.label = NA,
     vertex.size = node_degree*.05,
     edge.arrow.size = .05,
     edge.width = E(weighted_network)$weight/5,
     layout = layout_with_fr)

plot(weighted_network,
     vertex.label = NA,
     vertex.size = node_degree*.05,
     edge.arrow.size = .05,
     edge.width = E(weighted_network)$weight/5,
     layout = layout_in_circle)

##4. MODEL