library(tidyverse)
library(igraph)
library(ggraph)

cran_20230905 <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_20230905.csv")
dep_el <- cran_20230905 |>
    dplyr::filter(!is.na(Imports)) |>
    mutate(import = str_split(Imports, ",")) |>
    unnest(import) |>
    select(Package, import) |>
    mutate(import = str_remove_all(str_squish(import), "\\(.*\\)")) |>
    mutate(import = str_trim(import)) |>
    dplyr::filter(import != "")

g <- graph_from_data_frame(dep_el)
g1 <- netUtils::biggest_component(g)
saveRDS(g1, "processed_data/depend-biggest_comp.RDS")


xy <- graphlayouts::layout_with_stress(g1)

ggraph(g1, "manual", x = xy[, 1], y = xy[, 2]) +
    geom_edge_link0(edge_alpha = 0.2) +
    geom_node_point() +
    theme_graph()

nodes <- as_data_frame(g1, "vertices")
nodes$x <- xy[, 1]
nodes$y <- xy[, 2]

edges <- as_data_frame(g1, "edges")
edges$from <- match(edges$from, nodes$name)
edges$to <- match(edges$to, nodes$name)
names(edges)[1:2] <- c("source", "target")
nodes$id <- seq_len(nrow(nodes))
rownames(nodes) <- NULL
nodes <- nodes[, c(4, 2, 3, 1)]
readr::write_csv(tibble::as_tibble(edges), "~/Documents/playground/edges.csv")
readr::write_csv(tibble::as_tibble(nodes), "~/Documents/playground/nodes.csv")
