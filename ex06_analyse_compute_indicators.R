library(tidyverse)
library(ecr2)

# CALCULATE EMOA INDICATORS
# ===
#
# This script imports the results of the benchmark study and calcualtes
# some EMOA performance indicators via the ecr package and again stores
# the results.
#


source("src/defs.R")

res.all = readr::read_delim("results/algoruns_all_pf_only.csv", delim = " ", col_types = "ddcciic")

## PERFORMANCE INDICATORS
## =======

unary.inds = list(
  HV = list(fun = ecr::emoaIndHV),
  EPS = list(fun = ecr::emoaIndEps),
  IGD = list(fun = ecr::emoaIndIGD),
  DELTAP = list(fun = ecr::emoaIndDeltap),
  DELTA = list(fun = ecr:::emoaIndDelta)
)

res.all = filter(res.all, !grepl("50", algorithm))

parallelMap::parallelStartMulticore(cpus = 7L)
inds = ecr::computeIndicators(res.all, obj.cols = c("y1", "y2"), unary.inds = unary.inds)
parallelMap::parallelStop()
stop()

inds.unary = as_tibble(inds$unary)
meta = filter(res.all[, c("algorithm", "prob", "generation", "class")]) %>%
  group_by(algorithm, prob) %>%
  dplyr::summarize(generation = generation[1L], class = class[1]) %>%
  ungroup()

inds.unary = left_join(inds.unary, meta, by = c("prob", "algorithm"))
write.table(inds.unary, file = "results/unary_indicators.csv", row.names = FALSE, col.names = TRUE, quote = TRUE)
