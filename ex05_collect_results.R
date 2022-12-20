#library(experimentR)
devtools::load_all("~/repos/software/r/experimentR")

# COLLECT BENCHMARK RESULTS
# ===
#
# This script collects all results and stores them in CSV files.
#


source("src/defs.R")

RESULT.ROOT = "results/raw/mcmst_subgraph/"

files = list.files(RESULT.ROOT, pattern = "pfps_detailed.csv$", recursive = TRUE, full.names = TRUE, all.files = TRUE)
files.prim = files[grepl("PRIM", files)]
files.corley = files[grepl("CORLEY", files)]
files.emoa = files[grepl("EMOA", files)]

res.prim = experimentR::import(files.prim, param.sep = PARAM.SEP,
  param.format.string = paste0(RESULT.ROOT, "/prob{c}/algorithm{c}/repl{i}/pfps_detailed.csv"))

res.corley = experimentR::import(files.corley, param.sep = PARAM.SEP,
  param.format.string = paste0(RESULT.ROOT, "/prob{c}/algorithm{c}/repl{i}/pfps_detailed.csv"))

res.emoa = experimentR::import(files.emoa, param.sep = PARAM.SEP,
  param.format.string = paste0(RESULT.ROOT, "/prob{c}/EMOA___algorithm{c}/repl{i}/pfps_detailed.csv"))

res.emoa$algorithm = sprintf("%s (%3i%%)", res.emoa$algorithm, res.emoa$generation)
res.prim$generation = 100
res.prim$algorithm = "PRIM"
res.corley$generation = 100
res.corley$algorithm = "CORLEY"

res = rbind(res.prim, res.corley, res.emoa)
res$class = gsub("CLASS", "C", substr(res$prob, 1L, 6L))

write.table(res, file = "results/algoruns_all.csv", col.names = TRUE, row.names = FALSE, quote = TRUE)

res$edgelist = NULL
res = ecr::normalize(res, obj.cols = c("y1", "y2"), offset = 1)

write.table(res, file = "results/algoruns_all_pf_only.csv", col.names = TRUE, row.names = FALSE, quote = TRUE)


## Runtimes
## ===
files = list.files(RESULT.ROOT, pattern = "time_detailed.csv$", recursive = TRUE, full.names = TRUE, all.files = TRUE)
files.emoa = files[!grepl("PRIM", files)]

res.emoa = experimentR::import(files.emoa, param.sep = PARAM.SEP,
  param.format.string = paste0(RESULT.ROOT, "/prob{c}/EMOA___algorithm{c}/repl{i}/time_detailed.csv"))
res.emoa$algorithm = sprintf("%s (%3i%%)", res.emoa$algorithm, res.emoa$generation)
res.emoa$class = gsub("CLASS", "C", substr(res.emoa$prob, 1L, 6L))

write.table(res.emoa, file = "results/algotimes.csv", col.names = TRUE, row.names = FALSE, quote = TRUE)
