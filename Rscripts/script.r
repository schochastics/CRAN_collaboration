source("Rscripts/helpers.R")
library(igraph)
library(ggraph)
new <- FALSE

if (new) {
    # get data ----
    description <- sprintf(
        "%s/web/packages/packages.rds",
        getOption("repos")["CRAN"]
    )
    con <- if (substring(description, 1L, 7L) == "file://") {
        file(description, "rb")
    } else {
        url(description, "rb")
    }
    db <- as.data.frame(readRDS(gzcon(con)), stringsAsFactors = FALSE)
    close(con)
    rownames(db) <- NULL
    saveRDS(db, paste0("data/", Sys.Date(), "_database.RDS"))
} else {
    # read data ----
    db <- readRDS("data/2023-09-05_database.RDS")
}

bip <- author_cleaner(db)
readr::write_csv(bip, "processed_data/package_authors.csv")
bip <- readr::read_csv("processed_data/package_authors.csv")

l <- netUtils::bipartite_from_data_frame(bip, "authorsR", "Package")
A <- as_incidence_matrix(l, sparse = TRUE)
A <- as(A, "sparseMatrix")
B <- Matrix::t(A) %*% A
g <- graph_from_adjacency_matrix(B, "undirected", diag = FALSE, weighted = TRUE)
g1 <- netUtils::biggest_component(g)
xy <- graphlayouts::layout_with_sparse_stress(g1, pivots = 200)
V(g1)$x <- xy[, 1]
V(g1)$y <- xy[, 2]
idx <- which(V(g1)$name == "Hadley Wickham")
dist2HW <- distances(g1, to = idx, weights = NA)
cc <- 1 / closeness(g1, weights = NA)
V(g1)$dist2HW <- dist2HW
V(g1)$cc <- cc
saveRDS(g1, "processed_data/coauthor-biggest_comp.RDS")

p1 <- ggraph(g1, "manual", x = V(g1)$x, y = V(g1)$y) +
    geom_edge_link0(edge_width = 0.2, edge_colour = "grey66", alpha = 0.75) +
    geom_node_point(aes(fill = "grey25", filter = name != "Hadley Wickham"),
        shape = 21, stroke = 0.3, size = 2, show.legend = FALSE
    ) +
    theme_graph(background = "white") +
    coord_fixed(expand = FALSE, clip = "off")
ggsave("figures/network.png", p1, width = 10, height = 10)
