library(BBmisc)
library(batchtools)
library(experimentR)
library(ecr)

# TRAJECTORIES
# ===
#
# Run the EMOA algorithms on different instances and store the development
# of the performance indicators.
#

source("src/utils.R")
source("src/emoa.helpers.R")
source("src/defs.R")


# BE CAREFUL!
unlink("bt_trajectories", recursive = TRUE)

# prepare registry
reg = makeExperimentRegistry(
  file.dir = "bt_trajectories",
  seed = 123L,
  source = c("src/emoa.helpers.R", "src/utils.R", "src/defs.R"),
  packages = c("BBmisc", "ecr", "grapherator", "mcMST"))
reg$cluster.functions = batchtools::makeClusterFunctionsMulticore(ncpus = parallel::detectCores())

# get all mcMST instances
graphs = file.path("instances", c("CLASS1_50_1225_0_2_UNG_CEG_RWG-RWG_1.graph", "CLASS2_50_1225_0_2_UNG_CEG_CONCWG_1.graph", "CLASS3_50_1225_0_2_UNG_CEG_-0.95--CORWG_1.graph", "CLASS4_50_1225_0_2_UNG_CEG_0.95--CORWG_1.graph"))
BBmisc::catf("Adding %i graphs to registry.", length(graphs))

# add all graphs
for (graph in graphs) {
  instance.name = dropExtension(basename(graph))
  addProblem(name = instance.name, data = list(graph = graph))
}

# add EMOA based on edge list representation
addAlgorithm(reg, name = "EMOA.EDGELIST", fun = function(data, job, instance, mutator, ...) {
  #devtools::load_all("~/repositories/software/mcMST/")
  g = readGP(data$graph)
  gc = g

  mutator.fun = getMutator(mutator, n = gc$getV())
  n.evals = getNEvals(gc$getV())

  g$setEdgeProbabilities(rep(1, gc$getE()))
  if (mutator == "1BEX") {
    # set biased edge exchange probabilities
    dc = ecr::doNondominatedSorting(gc$getWeightsAsMatrix())$dom.counter
    g$setEdgeProbabilities((max(dc) - dc) + 1)
  }

  max.iter = (n.evals - MU) / LAMBDA

  set.seed(123)
  # run of best solver
  USGS.ref.points = t(as.matrix(mcMSTEmoaBG(
    instance = gc,
    mu = MU, lambda = LAMBDA,
    selSurvival = ecr::selNondom,
    mut = getMutator("USGS", n = gc$getV()),
    max.iter = max.iter)$pareto.front))

  set.seed(123)
  st = proc.time()
  res = mcMSTEmoaBG(
    instance = gc,
    mu = MU, lambda = LAMBDA,
    selSurvival = ecr::selNondom,
    mut = mutator.fun,
    max.iter = max.iter,
    log.pop = FALSE,
    log.stats = list(fitness =
      list("HV" = list(
        fun = ecr::emoaIndHV, pars = list(ref.points = USGS.ref.points)),
      "EPS" = list(
        fun = ecr::emoaIndEps, pars = list(ref.points = USGS.ref.points)),
      "DeltaP" = list(
        fun = ecr::emoaIndDeltap, pars = list(ref.points = USGS.ref.points))))
  )
  time.passed = as.numeric((proc.time() - st)[3L])
  stats = getStatistics(res$log, trim = TRUE)
  stats$job.id = job$job.id

  return(stats)
})

# algorithm designs
design.prim = data.table()
design.emoa.edgelist = CJ(
  mutator = c("1BEX", "SG", "SGS", "USG", "USGS", "SGSlog", "USGSlog")
)

# experiments (mcPRIM just once on each instance)
experiments = addExperiments(algo.designs = list(EMOA.EDGELIST = design.emoa.edgelist), repls = 1L)

stop("Build registry successfully!")

submitJobs(ids, resources = list(walltime = 60 * 60 * 10, memory = 4000))
waitForJobs()


jt = unwrap(getJobTable()[, c("job.id", "problem", "algo.pars")])
res = reduceResultsDataTable(findDone())
res = do.call(rbind, lapply(seq_len(28), function(i) {
  tmp = res$result[i][[1]]
  class(tmp) = c("data.frame")
  tmp$job.id = i
  return(tmp)
}))

res = dplyr::left_join(res, jt, by = "job.id")
write.table(res, file = "results/trajectories.csv", quote = TRUE, col.names = TRUE, row.names = FALSE)
