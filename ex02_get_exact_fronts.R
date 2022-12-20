library(grapherator)
library(ecr)
library(batchtools)
library(mcMST)

# A-PRIORI ANALYSIS
# ===
#
# This script calcualtes the exact Pareto-front for instances with n=10 nodes
# via exhaustive enumeration.
#

source("src/utils.R")
source("src/defs.R")

unlink("exactFronts", recursive = TRUE)

# enumerate exact front for small instances
small.graphs = list.files("instances", pattern = "CLASS1_10_.*", full.names = TRUE)
large.graphs =  c(
  "instances/CLASS1_100_4950_0_2_UNG_CEG_RWG-RWG_1.graph",
  "instances/CLASS1_100_4950_0_2_UNG_CEG_RWG-RWG_2.graph",
  "instances/CLASS1_50_1225_0_2_UNG_CEG_RWG-RWG_1.graph",
  "instances/CLASS1_50_1225_0_2_UNG_CEG_RWG-RWG_2.graph",

  "instances/CLASS2_100_4950_0_2_UNG_CEG_CONCWG_1.graph",
  "instances/CLASS2_100_4950_0_2_UNG_CEG_CONCWG_2.graph",
  "instances/CLASS2_50_1225_0_2_UNG_CEG_CONCWG_1.graph",
  "instances/CLASS2_50_1225_0_2_UNG_CEG_CONCWG_2.graph",

  "instances/CLASS3_100_4950_0_2_UNG_CEG_-0.95--CORWG_1.graph",
  "instances/CLASS3_100_4950_0_2_UNG_CEG_-0.95--CORWG_2.graph",
  "instances/CLASS3_50_1225_0_2_UNG_CEG_-0.95--CORWG_1.graph",
  "instances/CLASS3_50_1225_0_2_UNG_CEG_-0.95--CORWG_2.graph",

  "instances/CLASS4_100_4950_0_2_UNG_CEG_0.95--CORWG_1.graph",
  "instances/CLASS4_100_4950_0_2_UNG_CEG_0.95--CORWG_2.graph",
  "instances/CLASS4_50_1225_0_2_UNG_CEG_0.95--CORWG_1.graph",
  "instances/CLASS4_50_1225_0_2_UNG_CEG_0.95--CORWG_2.graph"
)
graphs = c(small.graphs, large.graphs)
graphs = large.graphs

BBmisc::catf("Processing %i graphs ...", length(small.graphs))

reg = makeRegistry(
  "exactFronts",
  source = "src/utils.R",
  packages = c("grapherator")
)

# get exact front by exhaustive enumeration for n = 10
batchtools::batchMap(function(x) {
  devtools::load_all("~/repos/software/r/mcMST")
  importer = new(GraphImporter)
  g = importer$importFromGrapheratorFile(x)
  filename = basename(x)
  # get exact Pareto-front/set
  # res = if (grepl("CLASS1_10", basename(x))) {
  #   # exhaustive for small instance
  #   getExactFrontMCMST(g)
  # } else {
    # approximate for large instances
  res = mcMSTWeightedSum(g, n.lambdas = 1000L)
  #}
  res = filterDuplicated(res)
  path = sprintf("results/exact_fronts/%s", filename)
  dir.create(path, recursive = TRUE)
  writeResult(res, path = path, time.passed = 1)
  },
  graphs
)
