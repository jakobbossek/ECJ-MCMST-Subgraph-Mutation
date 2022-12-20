library(grapherator)
library(re)
library(parallelMap)
library(mcMST)

# PURE OPERATOR RUNTIMES
# ===
#
# Get an impression of the actual runtimes of the proposed operators (without
# encapsulating meta-heuristic) for different graphs sizes and parameters.
#

set.seed(123)

half = function(n) {
  n/2
}

# considered instance sizes
ns = seq(100, 1000, by = 100)

# graphs
Gs = lapply(ns, function(n) mcMST::genRandomMCGP(n))
names(Gs) = as.character(ns)

# considered sigma-scaling
funs = c("log", "sqrt", "half")
# number of independent runs
R = 30

output.path = "results/operator_runtimes/"
dir.create(output.path)

# experimental design
exp = expand.grid(n = ns, sigma.fun = funs, repl = seq_len(R))
exp = re::df_rows_to_list(exp)

# calculate results
res = re::rbind_lapply(exp, FUN = function(e) {
  # create complete graph with n nodes and two weights per edge
  catf("Generating spanning tree")
  G = Gs[[as.character(e$n)]]
  G = mcMST:::grapheratorToGraph(G)
  MST = G$getRandomMST()

  # calculate s = sigma
  print(e)
  sigma.fun = match.fun(as.character(e$sigma.fun))
  sigma = ceiling(sigma.fun(e$n))

  catf("Mutating")
  # run both mutation operators
  sgs.time = system.time({ mutSubgraphMST(MST, sigma = sigma, sample.sigma = FALSE, scalarize = TRUE, instance = G) })
  usgs.time = system.time({ mutSubforestMST(MST, sigma = sigma, sample.sigma = FALSE, scalarize = TRUE, instance = G) })

  # return results
  return(data.frame(
    n = e$n,
    sigma.fun = e$sigma.fun,
    sigma = sigma,
    repl = e$repl,
    SGS = sgs.time[3L],
    USGS = usgs.time[3L]
  ))
})
write.table(res, file = file.path(output.path, "sgs_vs_usgs_runtimes.csv"), row.names = FALSE, quote = TRUE)

stop()

# ANALYSIS
# ===

library(ggplot2)
library(tidyverse)
library(latex2exp)

res = readr::read_delim(file.path(output.path, "sgs_vs_usgs_runtimes.csv"), delim = " ")
res$sigma.fun = factor(res$sigma.fun, levels = c("log", "sqrt", "half"), ordered = TRUE)

# transform to long format
res = reshape2::melt(res, id.vars = c("n", "sigma.fun", "sigma", "repl"), variable.name = "Operator", value.name = "runtime")
# nice LaTeX facet labels
levels(res$sigma.fun) = c(log = TeX("$s = \\sigma = \\log(n)$"), sqrt = TeX("$s = \\sigma = \\sqrt{n}$"), half = TeX("$s = \\sigma = n/2$"))

g = ggplot(res, aes(x = as.factor(n), y = runtime, color = Operator))
g = g + geom_boxplot()
g = g + facet_wrap(. ~ sigma.fun, labeller=label_parsed)
g = g + theme_minimal()
g = g + scale_color_brewer(palette = "Dark2")
g = g + theme(legend.position = "top")
g = g + labs(x = "n", y = "Runtime [in seconds]", color = "")
g

