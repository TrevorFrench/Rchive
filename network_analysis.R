library(network)
library(tidyverse)

source <- c('seth', 'trevor', 'susan', 'reagan', 'jordan', 'jordan', 'mike', 'trevor')
destination <- c('trevor', 'susan', 'alaka', 'jordan', 'mike', 'isaac', 'steve', 'zach')

sme <- data.frame(source = source, destination = destination)

sources <- sme %>%
  distinct(source) %>%
  rename(label = source)

destinations <- sme %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")
nodes

nodes <- rowid_to_column(nodes, "id")
nodes

per_route <- sme %>%  
  group_by(source, destination) %>%
  summarise(weight = n()) %>% 
  ungroup()

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges

routes_network <- network(edges,
                          vertex.attr = nodes,
                          matrix.type = "edgelist",
                          ignore.eval = FALSE)

plot(routes_network, vertex.cex = 3)

library(visNetwork)

groups <- c('BOSS', 'DATA', 'GOV', 'CAS', 'CAS', 'DATA', 'DATA', 'CAS', 'GOV', 'TAX')
nodes$group <- groups

colors <- c('pink', '#182C6A', '#095AD1', '#00A4AC', '#5BCFF7FF')

legendNodes <- data.frame(
  label = c("BOSS","DATA", "GOV", "CAS", "TAX"),
  color.background = colors,
  #color.border = c("black", "white"),
  color.border = c('black','black','black','black','black'),
  #shape = c("dot", "dot")
  shape = c('dot','dot','dot','dot','dot')
  #color.label = 'white'
)

visNetwork(nodes
           , edges
           # , width = "100%"
           # , height = "100%"
           , main = list(text = "SME GRATITUDE", style = "font-family:'Courier New',monospace;font-size:25px;text-align:center;font-weight:bold")
           , submain = list(text = "A Network Analysis"
                            , style = "font-family:'Courier New',monospace;font-size:15px;text-align:center;"
                            )
           ) %>% 
  visGroups(groupname = "BOSS", color = "pink") %>% 
  visGroups(groupname = "DATA", color = "#182C6A")  %>%
  visGroups(groupname = "GOV", color = "#095AD1")  %>%
  visGroups(groupname = "CAS", color = "#00A4AC")  %>%
  visGroups(groupname = "TAX", color = "#5BCFF7FF") %>%
  visLegend(useGroups = FALSE,
            addNodes = legendNodes)
