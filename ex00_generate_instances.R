library(grapherator)
#library(mcMST)

# INSTANCE GENERATION
# ===
#
# Script for the generation of the benchmark instances.
#

# considered instance sizes
ns = c(10L, 25L, 50L, 100L, 250L)

# number of clusters for clustered instances
n.clusters = 5L

# number of instances per type
k = 10L

unlink("instances", recursive = TRUE)
dir.create("instances")

# upper bound for node coordinates

saveBenchmarkInstance = function(g, repl, class, folder = "instances") {
  checkmate::assertClass(g, "grapherator")
  repl = checkmate::asInt(repl, lower = 1L)
  meta = as.character(g)
  filename = sprintf("%s_%s_%i.graph", class, meta, repl)
  file = file.path(folder, filename)
  grapherator::writeGP(g, file)
}

# reproducability
set.seed(1)

# CEG_RWG
# as in Zhou & Gen, Knowles et al.
# CONVEX FRONT
for (n in ns) {
  for (r in seq_len(k)) {
    cat(".")
    g = graph(0, 100)
    g = addNodes(g, n = n, generator = addNodesUniform)
    g = addEdges(g, generator = addEdgesComplete)
    g = addWeights(g, generator = addWeightsRandom, method = runif, min = 10, max = 100, to.int = TRUE)
    g = addWeights(g, generator = addWeightsRandom, method = runif, min = 10, max = 50, to.int = TRUE)
    saveBenchmarkInstance(g, class = "CLASS1", repl = r)
  }
}

# DEG_CWG
# CONCAVE FRONT (SEE KNOWLES ET AL. FOR EXPLANATION)
for (n in ns) {
  for (r in seq_len(k)) {
    cat(".")
    g = graph(0, 100)
    g = addNodes(g, n = n, generator = addNodesUniform)
    g = addEdges(g, generator = addEdgesComplete)
    g = addWeights(g, generator = addWeightsConcave, to.int = TRUE)
    saveBenchmarkInstance(g, class = "CLASS2", repl = r)
  }
}

# CEG_CORR
# Negative correlation -> many efficient solutions
for (n in ns) {
  for (r in seq_len(k)) {
    cat(".")
    g = graph(0, 100)
    g = addNodes(g, n = n, generator = addNodesUniform)
    g = addEdges(g, generator = addEdgesComplete)
    g = addWeights(g, generator = addWeightsCorrelated, rho = -0.95, to.int = TRUE)
    saveBenchmarkInstance(g, class = "CLASS3", repl = r)
  }
}

# CEG_CORR
# Positive correlation -> few efficient solutions
for (n in ns) {
  for (r in seq_len(k)) {
    cat(".")
    g = graph(0, 100)
    g = addNodes(g, n = n, generator = addNodesUniform)
    g = addEdges(g, generator = addEdgesComplete)
    g = addWeights(g, generator = addWeightsCorrelated, rho = 0.95, to.int = TRUE)
    saveBenchmarkInstance(g, class = "CLASS4", repl = r)
  }
}

# sanity check
instances = list.files("instances", pattern = ".graph$")
is = length(instances)
shouldBe = 4 * length(ns) * k
if (is != shouldBe) {
  BBmisc::stopf("[ERROR] There should be %i instances, but found %i.", shouldBe, is)
}
