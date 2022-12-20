library(BBmisc)
library(batchtools)
library(experimentR)
library(ecr)

source("src/utils.R")
source("src/emoa.helpers.R")
source("src/defs.R")

# RUN BENCHMARK
# ===
#
# This script defines the experiments: run a portfolio of algorithms
# (EMOA, WeightedSum, Corley's algorithm) with different parameters each
# multiple times (for stochastic solvers) on all benchmark instances.
#
# The experiments are set up via the powerful batchtools R package for
# HPC experimentation in R.
#
# Note: these experiments take some time.
#

# BE CAREFUL!
unlink("runEMOAs", recursive = TRUE)

# prepare registry
reg = makeExperimentRegistry(
  "runEMOAs",
  seed = 123L,
  source = c("src/emoa.helpers.R", "src/utils.R", "src/defs.R"),
  packages = c("BBmisc", "ecr", "grapherator", "mcMST"))

# Use this for parallelisation on a multi-core system
# Note: this will also take some time.
# reg$cluster.functions = batchtools::makeClusterFunctionsMulticore(ncpus = parallel::detectCores() - 1)

# get all mcMST instances
graphs = list.files("instances", pattern = ".graph$", full.names = TRUE)
# except for the 10 nodes instances
graphs = graphs[!grepl("_10_", graphs, fixed = TRUE)]
BBmisc::catf("Adding %i graphs to registry.", length(graphs))

# add all graphs
for (graph in graphs) {
  instance.name = dropExtension(basename(graph))
  addProblem(name = instance.name, data = list(graph = graph))
}

# add mcPRIM
addAlgorithm(reg, name = "PRIM", fun = function(data, job, instance, ...) {
  #devtools::load_all("~/repositories/software/mcMST/")
  g = readGP(data$graph)
  st = proc.time()
  res = mcMSTWeightedSum(g, n.lambdas = N.LAMBDAS)
  time.passed = as.numeric((proc.time() - st)[3L])

  output.path = experimentR:::buildOutputPath(root = RESULT.ROOT, format.string = "%s/PRIM/%i", basename(data$graph), job$repl)
  writeResult(res, NULL, time.passed, output.path)

  return(NA)
})

# add EMOA based on Pruefer representation
addAlgorithm(reg, name = "EMOA.PRUEFER", fun = function(data, job, instance, mutator, ...) {
  #devtools::load_all("~/repositories/software/mcMST/")
  g = readGP(data$graph)
  #gc = grapheratorToGraph(g)
  gc = g

  n.evals = getNEvals(gc$getV())

  st = proc.time()
  res = mcMSTEmoaZhou(
    instance = gc,
    mu = MU, lambda = LAMBDA,
    selSurvival = ecr::selNondom,
    max.iter = (n.evals - MU) / LAMBDA,
    log.pop = TRUE
  )
  time.passed = as.numeric((proc.time() - st)[3L])
  res.detailed = getFrontsAfterAsDataframe(res$log, g, at = SAVE.POP.AT)

  output.path = experimentR:::buildOutputPath(root = RESULT.ROOT, format.string = "%s/EMOA___%s/%i", basename(data$graph), mutator, job$repl)
  writeResult(res, res.detailed, time.passed, output.path)

  return(NA)
})

# add EMOA based on edge list representation
addAlgorithm(reg, name = "EMOA.EDGELIST", fun = function(data, job, instance, mutator, ...) {
  #devtools::load_all("~/repositories/software/mcMST/")
  g = readGP(data$graph)
  gc = g

  mutator.fun = getMutator(mutator, n = gc$getV())
  n.evals = getNEvals(gc$getV())

  print(n.evals)

  g$setEdgeProbabilities(rep(1, gc$getE()))
  if (mutator == "1BEX") {
    # set biased edge exchange probabilities
    dc = ecr::doNondominatedSorting(gc$getWeightsAsMatrix())$dom.counter
    g$setEdgeProbabilities((max(dc) - dc) + 1)
  }

  st = proc.time()
  res = mcMSTEmoaBG(
    instance = gc,
    mu = MU, lambda = LAMBDA,
    selSurvival = ecr::selNondom,
    mut = mutator.fun,
    max.iter = (n.evals - MU) / LAMBDA,
    log.pop = TRUE
  )
  time.passed = as.numeric((proc.time() - st)[3L])
  res.detailed = getFrontsAfterAsDataframe(res$log, g, at = SAVE.POP.AT)

  #print(res.detailed)
  output.path = experimentR:::buildOutputPath(root = RESULT.ROOT, format.string = "%s/EMOA___%s/%i", basename(data$graph), mutator, job$repl)
  writeResult(res, res.detailed, time.passed, output.path)

  return(NA)
})

addAlgorithm(reg, name = "CORLEY", fun = function(data, job, instance, ...) {
  g = readGP(data$graph)
  st = proc.time()
  res = mcMST::mcMSTPrim(g)
  time.passed = as.numeric((proc.time() - st)[3L])

  output.path = experimentR:::buildOutputPath(root = RESULT.ROOT, format.string = "%s/CORLEY/%i", basename(data$graph), job$repl)
  writeResult(res, NULL, time.passed, output.path)

  return(NA)
})

# algorithm designs
design.prim = data.table()
design.emoa.edgelist = CJ(
  mutator = c("1EX", "1BEX", "SG", "SGS", "USG", "USGS", "SGSlog", "USGSlog")
)
design.emoa.pruefer = CJ(
  mutator = c("UNIFORM")
)
design.corley = data.table()

# experiments (deterministic algorithm run just once on each instance)
experiments = addExperiments(algo.designs = list(PRIM = design.prim), repls = 1L)
experiments = addExperiments(algo.designs = list(EMOA.EDGELIST = design.emoa.edgelist), repls = N.REPLS)
experiments = addExperiments(algo.designs = list(EMOA.PRUEFER = design.emoa.pruefer), repls = N.REPLS)
experiments = addExperiments(algo.designs = list(CORLEY = design.corley), repls = 1L)

#res = testJob(reg, 1L)

stop("Build registry successfully!")

# submit Corley jobs
# ids = findExperiments(algo.name = "CORLEY")
# submitJobs(ids, resources = list(walltime = 60 * 60 * 48, mem = 16000))

# submit all experiments
ids = findNotDone()
#ids = findExpired()
submitJobs(ids, resources = list(walltime = 60 * 60 * 10, memory = 4000))
