library(grapherator)
library(ggplot2)
library(gridExtra)
library(mcMST)
#devtools::load_all("~/repos/software/r/mcMST")

# PLOT INSTANCES
# ===
#
# Script for plotting the edge weights and Pareto-front approximations
#

# what instances do we want to show?
instances = list.files("instances", recursive = TRUE, full.names = TRUE)

# sample each one instance of each class
instances.selected = sapply(1:4, function(cl) {
  tmp = instances[grepl(sprintf("CLASS%i_25_", cl), instances)]
  sample(tmp, size = 1L)
})

print(instances.selected)

# get approximation of pareto fronts
approx = lapply(instances.selected, function(instance) {
  instance = grapherator::readGP(instance)
  instance.cpp = mcMST:::grapheratorToGraph(instance)

  res.ws = mcMST::mcMSTWeightedSum(instance, n.lambdas = 1000L)
  res.ea = mcMST::mcMSTEmoaBG(instance, mu = 200L, lambda = 10L, max.iter = 50 * instance.cpp$getV(), mut = ecr::setup(mcMST::mutSubgraphMST, scalarize = TRUE, instance = instance.cpp))

  res = rbind(res.ws$pareto.front, res.ea$pareto.front)
  res.ps = c(res.ws$pareto.set, res.ea$pareto.set)

  idx.nondom = which.nondominated(t(res))
  res = res[idx.nondom, , drop = FALSE]
  res.ps = res.ps[idx.nondom]

  list(instance = instance, pareto.front = res, pareto.set = res.ps)
})

pls.weights = lapply(approx, function(elem) {
  mcMST::plotEdgeFrequency(list(elem$instance), approx.sets = list(elem$pareto.set)) + theme_minimal() + theme(legend.position = "none")# + scale_fill_brewer(palette = "Dark2")
})

pls.fronts = lapply(approx, function(elem) {
  ecr::plotScatter2d(elem$pareto.front, obj.cols = c("y1", "y2")) + labs(x = expression(c[1](T)), y = expression(c[2](T))) + theme_minimal() + theme(legend.position = "none")
})


pls = c(pls.weights, pls.fronts)
pls$nrow = 2L

pl.final = do.call(gridExtra::grid.arrange, pls)
ggsave("figures/benchmark/examplary_instances.pdf", plot = pl.final, width = 12, height = 6, limitsize = FALSE)
