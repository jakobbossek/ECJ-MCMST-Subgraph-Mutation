library(grapherator)
library(ggplot2)

#devtools::load_all("~/repos/software/r/mcMST")
library(mcMST)

# A-PRIORI ANALYSIS
# ===
#
# This script calcualtes (N)NCE scores and plots heatmaps with the pairwise
# similarity of solutions.
#

source("src/utils.R")

graphs = list.dirs("results/exact", full.names = TRUE)
graphs = paste0("instances/", basename(graphs))[-c(1L, 6L)]
print(graphs)

results = lapply(graphs, function(f) readResult2(sprintf("results/exact/%s", basename(f))))

importer = new(GraphImporter)
converter = new(RepresentationConverter)

for (i in 1:length(graphs)) {
  graph = graphs[i]
  catf("Working on graph %s.", basename(graph))

  i = 1L
  res = results[[i]]

  gr = grapherator::readGP(graph)
  gcpp = importer$importFromGrapheratorFile(graph)

  basename = basename(graph)

  pf = res$pf
  ps = res$ps

  # order by first objective
  ps = ps[order(pf[, 1L, drop = TRUE])]
  pscpp = lapply(ps, function(el) {
    converter$edgeListToGraph(gcpp, el)
  })

  nce.mat = computeSimilarityMatrix(pscpp, sim.fun = getNumberOfCommonEdges)
  pl.nce = ecr::plotHeatmap(nce.mat)# + theme(axis.ticks.x = element_blank(), axis.ticks.y =  element_blank())
  filename = sprintf("figures/apriori_analysis/NNCE_%s.pdf", gsub(".graph", "", basename))
  ggsave(filename, plot = pl.nce)

  # sls.mat = computeSimilarityMatrix(pscpp, sim.fun = getSizeOfLargestCommonComponent)
  # pl.sls = ecr::plotHeatmap(sls.mat) + theme(axis.ticks.x = element_blank())
  # filename = sprintf("figures/apriori_analysis/NSLCS_%s.pdf", gsub(".graph", "", basename))
  # ggsave(filename, plot = pl.sls)

  # pl.both = ecr::plotHeatmap(list("NNCE" = nce.mat, "NSLCS" = sls.mat))
  # pl.both = pl.both + theme(axis.text.x = element_blank(), axis.text.y = element_blank())
  # pl.both = pl.both + labs(fill = "Similarity")
  # filename = sprintf("figures/apriori_analysis/NCE_NSLCS_%s.pdf", gsub(".graph", "", basename))
  # ggsave(filename, plot = pl.both)

  # pl.freq = plotEdgeFrequency(graphs = list(gr), approx.sets = list(ps))
  # filename = sprintf("figures/apriori_analysis/EDGE_FREQUENCY_%s.pdf", gsub(".graph", "", basename))
  # ggsave(filename, plot = pl.freq, width = 4.5, height = 4.5)

  # filename = sprintf("figures/apriori_analysis/EMBEDDING_%s.pdf", gsub(".graph", "", basename))
  # pdf(filename)
  # cvs = lapply(ps, edgeListToCharVec, n = getNumberOfNodes(gr))
  # pl.embed = plotEdges(cvs, n = getNumberOfNodes(gr))
  # dev.off()
}
