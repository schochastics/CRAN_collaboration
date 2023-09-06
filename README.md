
## CRAN collaboration graph

The CRAN collaboration graph consists of R package developers who are
connected if they appear together as authors of an R package in the
DESCRIPTION file.

![](figures/network.png)

The graph consists of 15419 R developers and 126,988 collaborative ties.

## Six Degrees of Hadley Wickham

If you are familiar with the [Erdős
number](https://en.wikipedia.org/wiki/Erd%C5%91s_number) number and/or
the [Bacon
number](https://en.wikipedia.org/wiki/Six_Degrees_of_Kevin_Bacon#Bacon_numbers)
then you know where this is going. The “Hadley number” is defined as the
distance of R developers to Hadley Wickham in the collaboration graph.
Someone (“A”) who contributed to a package that Hadley has contributed
to has a Hadley number of 1. Someone who contributed to a package that A
has contributed to but not Hadley has Hadley number 2, and so on. Hadley
himself is the only person with Hadley number 0

The distribution of Hadley numbres is shown below.
![](figures/README-hadley_distribution-1.png)<!-- -->

The average Hadley number is 2.982. There is no easy way of checking
your own Hadley number yet. For now, you can download
`processed_data/coauthor-biggest_comp.RDS` and do

``` r
library(igraph)
g <- readRDS("coauthor-biggest_comp.RDS")
me <- "David Schoch"
idx <- which(V(g)$name==me)
V(g)$dist2HW[idx]
```

## The center of the collaboration network

The center of the collaboration network is defined as the developer
who’s average distance to all other developers is the lowest. The top
ten developers in that regard are shown below.

| name              | central |
|:------------------|--------:|
| Hadley Wickham    | 2.98178 |
| Ben Bolker        | 3.10481 |
| Dirk Eddelbuettel | 3.13269 |
| Martin Maechler   | 3.17355 |
| Romain Francois   | 3.17375 |
| Michael Friendly  | 3.18030 |
| R Core Team       | 3.19534 |
| Jim Hester        | 3.20585 |
| Posit Software    | 3.21376 |
| Kevin Ushey       | 3.23419 |

## Disclaimer

The repository only includes the “largest connected component” of the
collaboration graph. Developers who have single authored one package do
not appear in the graph.

The author field in the DESCRIPTION file can be very messy. I have a
very lengthy cleaning script (see `Rscripts/helpers.R` and
`data/delete_authors.txt`) but the final data is for sure not yet free
of errors.
