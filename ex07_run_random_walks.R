library(ecr)
library(mcMST)
library(batchtools)

# RANDOM WALKS
# ===
#
# This script runs a simple algorithm (see src/algorithms.R) which performs
# a random walk given a sub-graph mutation operator.
#

source("src/algorithms.R")

unlink("random_walks", recursive = TRUE)

# considered instances
instances = c(
  "instances/CLASS1_100_4950_0_2_UNG_CEG_RWG-RWG_1.graph",
  "instances/CLASS2_100_4950_0_2_UNG_CEG_CONCWG_1.graph",
  "instances/CLASS3_100_4950_0_2_UNG_CEG_-0.95--CORWG_1.graph",
  "instances/CLASS4_100_4950_0_2_UNG_CEG_0.95--CORWG_1.graph")
print(instances)

# parameters
mu = 1L
REPLS = 5L
n = 100L # instance size
max.sigmas = c(ceiling(log(n)), 7, 10, 25, 50)#, floor(500/2), floor(500/3), floor(1000/2), floor(1000/3), 1000)
max.iter = 100L

# Debug
# instance = readGP(instances[1L])
# output = evolAnalyse(instance, mu = 1L,
#     max.iter = 100L, mut = mutSubgraphMST,
#     mut.args = list(sigma = 50, scalarize = FALSE),
#     no.survival.selection = TRUE)$storage
#stop()

reg = batchtools::makeExperimentRegistry("random_walks", packages = c("ecr", "mcMST"), source = c("src/algorithms.R"))

for (instance in instances) {
  batchtools::addProblem(gsub(".graph", "", basename(instance)), data = list(graph = instance))
}

batchtools::addAlgorithm("EMOA", fun = function(job, data, mut, max.sigma, scalarize, ...) {
  #devtools::load_all("~/repos/software/r/mcMST")

  muts = list("SG" = mutSubgraphMST, "USG" = mutSubforestMST)

  instance = readGP(data$graph)
  output = evolAnalyse(instance, mu = 1L,
    max.iter = 100L, mut = muts[[mut]],
    mut.args = list(sigma = max.sigma, scalarize = scalarize),
    no.survival.selection = TRUE)$storage

  output$repl = job$repl
  output$n    = as.integer(strsplit(data$graph, "_")[[1L]][2L])
  output$max.sigma = max.sigma
  output$scalarize = scalarize
  output$instance = basename(data$graph)
  output$mut  = mut
  return(output)
})

design.emoa = data.table::CJ(
  max.sigma = max.sigmas,
  mut = c("SG", "USG"),
  scalarize = c(TRUE, FALSE)
)

batchtools::addExperiments(algo.designs = list("EMOA" = design.emoa), repls = REPLS)

stop()
submitJobs()

waitForJobs()

res = batchtools::reduceResults(init = data.frame(), fun = function(aggr, res) rbind(aggr, res))

write.table(res, file = "results/random_walks/random_walks.csv", col.names = TRUE, row.names = FALSE, quote = FALSE)
