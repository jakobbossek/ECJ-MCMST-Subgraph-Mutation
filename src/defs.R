# EXPERIMENTAL SETUP
# ==================

# population size
MU = 100L
LAMBDA = 10L

# mcPRIM
N.LAMBDAS = 5000L

# number of independent repetitions
N.REPLS = 30L

# where to save population
SAVE.POP.AT = c(0.1, 0.5, 1.0)

# maximal number of function evaluations
# Note: we need to compute the number of function evaluations for NSGA-II
# and SMS-EMOA respectively
getNEvals = function(n) {
  1000L * n
}

getMutator = function(mut.string, n) {
  sigma = ceiling(log(n))^2
  if (mut.string == "SG")
    return(ecr::setup(mcMST::mutSubgraphMST, scalarize = FALSE))
  if (mut.string == "SGS")
    return(ecr::setup(mcMST::mutSubgraphMST, scalarize = TRUE))
  if (mut.string == "USG")
    return(ecr::setup(mcMST::mutSubforestMST, scalarize = FALSE))
  if (mut.string == "USGS")
    return(ecr::setup(mcMST::mutSubforestMST, scalarize = TRUE))
  if (mut.string == "SGSlog")
    return(ecr::setup(mcMST::mutSubgraphMST, scalarize = TRUE, sigma = sigma))
  if (mut.string == "USGSlog")
    return(ecr::setup(mcMST::mutSubforestMST, scalarize = TRUE, sigma = sigma))
  if (mut.string == "1EX" || mut.string == "1BEX")
    return(ecr::setup(mcMST::mutKEdgeExchange, k = 1L))
  stopf("initMutator: should not happen! Unknown mut.string passed")
}

# Path to result files
PARAM.SEP = "___"
RESULT.ROOT = normalizePath("/scratch/tmp/bossek/mcmst_subgraph/")
