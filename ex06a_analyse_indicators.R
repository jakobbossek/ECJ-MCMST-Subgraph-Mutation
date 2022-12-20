library(tidyverse)
library(ggplot2)
library(gridExtra)

# ANALYSE EMOA INDICATORS
# ===
#
# This script imports the performance data, calculates ranks and pairwise comparisons.
#


devtools::load_all("~/repos/software/r/ecr2")
devtools::load_all("~/repos/software/r/ggheatmap")

source("src/defs.R")

res.all = readr::read_delim("results/unary_indicators.csv", delim = " ", col_types = "ccidddddic")

# get all instances
probs = unique(res.all$prob)

# Filter relevant data:
res.all$algorithm[res.all$algorithm == "PRIM"] = "WEIGHTED SUM"

res.final = filter(res.all, algorithm %in% c("WEIGHTED SUM", "UNIFORM (100%)", "1EX (100%)", "1BEX (100%)", "SG (100%)", "USG (100%)", "SGS ( 10%)", "SGS (100%)", "USGS ( 10%)", "USGS (100%)", "SGSlog ( 10%)", "SGSlog (100%)", "USGSlog ( 10%)", "USGSlog (100%)"))
res.final.long = reshape2::melt(res.final, id.vars = c("algorithm", "prob", "repl", "class", "generation"), value.name = "value", variable.name = "indicator")

## NOW COMPUTE RANKING
## ===

# first aggregate (summary statistics for each algorithm on each instance)
res.final.long.aggr = res.final.long %>%
  group_by(algorithm, prob, indicator, class) %>%
  dplyr::summarize(mean = mean(value), median = median(value), sd = sd(value)) %>%
  ungroup()

#FIXME: show large table? Maybe in appendix?

# now add ranking
res.final.long.aggr = res.final.long.aggr %>%
  group_by(prob, indicator, class) %>%
  dplyr::mutate(
    rank.mean = rank(mean, ties.method = "average"),
    rank.median = rank(median, ties.method = "average")) %>%
  ungroup()

# now get aggregated ranks per indicator and class
ranking = res.final.long.aggr %>%
  group_by(algorithm, class, indicator) %>%
  dplyr::summarize(rank.mean = mean(rank.mean), sd.mean = sd(rank.mean, na.rm = TRUE), rank.median = mean(rank.median)) %>%
  ungroup()

pl = ggplot(filter(ranking, indicator %in% c("HV", "EPS", "DeltaP")) , aes(x = class, y = rank.mean, group = algorithm, color = algorithm, shape = algorithm))#, linetype = algorithm))
pl = pl + geom_line()
pl = pl + geom_point()
pl = pl + facet_wrap(. ~ indicator)
pl = pl + theme_minimal()
pl = pl + viridis::scale_colour_viridis(discrete = TRUE, end = 0.85)
pl = pl + scale_shape_manual(values = c(15, 16, 17, 18, 19, 6, 7, 8, 9, 10, 11, 12, 13, 14))
pl = pl + scale_y_discrete(limits = 1:14)
pl = pl + theme(legend.position = "top")
pl = pl + guides(color = guide_legend(nrow = 3L), shape = guide_legend(nrow = 2L), linetype = guide_legend(nrow = 2L))
pl = pl + labs(x = NULL, y = "Average ranking", color = "Algorithm", shape = "Algorithm", linetype = "Algorithm")
pl
ggsave("figures/benchmark/indicator_mean_ranks.pdf", plot = pl, width = 8, height = 4, limitsize = FALSE, device = cairo_pdf)

#stop()

## PAIRWISE COMPARISSONS
## ==

probs = unique(res.final$prob)
algos = unique(res.final$algorithm)

if (FALSE) {
pwcomp = data.frame()
alpha = 0.01
for(prob2 in probs) {
  tmp = filter(res.final, prob == prob2)
  print(tmp)
  for (algo1 in algos) {
    for (algo2 in algos) {
      cat(".")
#      print(pwcomp)
      err = try({
        tmp.algo1 = filter(tmp, algorithm == algo1)
        tmp.algo2 = filter(tmp, algorithm == algo2)
        vals1 = tmp.algo1$HV; vals2 = tmp.algo2$HV
        if (algo1 == "WEIGHTED SUM") vals1 = rep(vals1, 30)
        if (algo2 == "WEIGHTED SUM") vals2 = rep(vals2, 30)
        res.HV = wilcox.test(x = vals1, y = vals2, alternative = "less")$p.value < alpha
        vals1 = tmp.algo1$EPS; vals2 = tmp.algo2$EPS
        if (algo1 == "WEIGHTED SUM") vals1 = rep(vals1, 30)
        if (algo2 == "WEIGHTED SUM") vals2 = rep(vals2, 30)
        res.EPS = wilcox.test(x = vals1, y = vals2, alternative = "less")$p.value < alpha
        vals1 = tmp.algo1$DeltaP; vals2 = tmp.algo2$DeltaP
        if (algo1 == "WEIGHTED SUM") vals1 = rep(vals1, 30)
        if (algo2 == "WEIGHTED SUM") vals2 = rep(vals2, 30)
        res.DeltaP = wilcox.test(x = vals1, y = vals2, alternative = "less")$p.value < alpha
      })
      if (algo1 == algo2 | inherits(err, "try-error")) pwcomp = rbind(pwcomp, data.frame(algo1 = algo1, algo2 = algo2, prob = prob2, class = gsub("CLASS", "C", substr(prob2, 1, 6)), HV = NA, EPS = NA, DeltaP = NA))
      else pwcomp = rbind(pwcomp, data.frame(algo1 = algo1, algo2 = algo2, prob = prob2, class = gsub("CLASS", "C", substr(prob2, 1, 6)), HV = res.HV, EPS = res.EPS, DeltaP = res.DeltaP))
    }
  }
}

pwcomp$algo = paste0(pwcomp$algo1, "...", pwcomp$algo2)

# Now go for by class aggreagation
pwcomp.aggr = pwcomp %>%
  group_by(algo, class) %>%
  dplyr::summarize(HV = mean(HV), EPS = mean(EPS), DeltaP = mean(DeltaP), algo1 = algo1[1], algo2 = algo2[1]) %>%
  ungroup()

pwcomp.aggr$algo = NULL
write.table(pwcomp.aggr, file = "results/unary_indicators_pwtests.csv", col.names = TRUE, row.names = FALSE, quote = TRUE)
} # if (FALSE)

pwcomp.aggr = readr::read_delim("results/unary_indicators_pwtests.csv", delim = " ")
pwcomp.aggr = reshape2::melt(pwcomp.aggr, id.vars = c("algo1", "algo2", "class"), variable.name = "measure", value.name = "value")
pwcomp.aggr$algo1 = factor(pwcomp.aggr$algo1, levels = c("WEIGHTED SUM", "UNIFORM (100%)", "1EX (100%)", "1BEX (100%)", "SG (100%)", "SGS ( 10%)", "SGS (100%)", "USG (100%)", "USGS ( 10%)", "USGS (100%)", "SGSlog ( 10%)", "SGSlog (100%)", "USGSlog ( 10%)", "USGSlog (100%)"))
pwcomp.aggr$algo2 = factor(pwcomp.aggr$algo2, levels = c("WEIGHTED SUM", "UNIFORM (100%)", "1EX (100%)", "1BEX (100%)", "SG (100%)", "SGS ( 10%)", "SGS (100%)", "USG (100%)", "USGS ( 10%)", "USGS (100%)", "SGSlog ( 10%)", "SGSlog (100%)", "USGSlog ( 10%)", "USGSlog (100%)"))

pl = ggheatmap(filter(pwcomp.aggr, measure == "HV"), id.vars=c("algo1", "algo2"), value.name = "value", show.values = TRUE, value.size = 2.0)
pl = pl + viridis::scale_fill_viridis(end = 0.75, alpha = 0.8)
pl = pl + theme_minimal()
pl = pl + facet_wrap(. ~ class, nrow = 2L)
pl = pl + theme(axis.text.x = element_text(hjust = 1, angle = 45), legend.position = "top")
pl = pl + labs(fill = "Fraction of signifcant tests")
pl = pl + geom_vline(xintercept = c(1.5, 4.5), color = "white", size = 0.8)
pl = pl + geom_hline(yintercept = c(1.5, 4.5), color = "white", size = 0.8)
pl
ggsave("figures/benchmark/pwtests_HV.pdf", plot = pl, width = 8, height = 8, device = cairo_pdf, limitsize = FALSE)

