### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### Dependency graph
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2016-07-02
### ===============================================================================================
## This script should be run manually after the package has been built
## The script based on:
## https://cran.r-project.org/web/packages/miniCRAN/vignettes/miniCRAN-dependency-graph.html

require(miniCRAN)

Rversion <- "3.4.0"

graph <- miniCRAN::makeDepGraph(
  pkg = "Luminescence",
  availPkgs = miniCRAN::pkgAvail(
    repos = getOption("repos"),
    type = "source",
    Rversion = Rversion
  ),
  enhances = TRUE,
  suggests = TRUE,
  includeBasePkgs = FALSE
)

png(filename = "RLum.Images/Package_DependencyGraph.png", width = 1200, height = 1000, res = 150)
plot(graph, legendPosition = c(-1, 1), cex = 0.7,  vertex.size=10)
dev.off()


