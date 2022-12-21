library(tidyverse)
library(kableExtra)

devtools::load_all("~/repos/software/r/tblutils")

source("src/defs.R")

res.all = readr::read_delim("results/unary_indicators.csv", delim = " ", col_types = "ccidddddic")

# get all instances
algo.names = c("UNIFORM (100%)", "1BEX (100%)", "SGS (100%)", "USGS (100%)")
algo.colors = colors = c("violet", "brown", "teal", "purple")

# Filter relevant data:
res.all$algorithm[res.all$algorithm == "PRIM"] = "WEIGHTED SUM"

tbl = res.all %>%
  filter(algorithm %in% algo.names) %>%
  dplyr::select(-generation)

tbl$n = as.integer(sapply(strsplit(tbl$prob, "_"), function(el) {
  el[2L]
}))

tbl$prob = sapply(strsplit(tbl$prob, "_"), function(el) {
  el[length(el)]
})
tbl$prob = as.integer(gsub(".graph", "", tbl$prob))
tbl = select(tbl, class, n, prob, algorithm, HV, EPS)
ns = unique(tbl$n)

tbl$algorithm2 = factor(tbl$algorithm, levels = algo.names, ordered = TRUE)
tbl = arrange(tbl, algorithm2)

res = to_result_table(tbl,
  split.cols = c("class", "n", "prob"),
  widen.col = c("algorithm2"),
  measure.cols = c("EPS", "HV"),
  test.alternative = c(EPS = "less", HV = "less"),
  testresult.formatter.args = list(positive.only = TRUE, interval = FALSE, colors = algo.colors),
  stats.formatter.args = list(sd = list(digits = 2)),
  highlighter.args = list(order.fun = "min", bg.color = "gray", bg.saturation.max = 0, digits = 4L))

tbl.EPS = res[[1L]]
tbl.HV = res[[2L]]
tbl.merged = cbind(tbl.HV, tbl.EPS[, -(1:3)])

tbl.merged$mean.HV = sprintf("%s $\\pm$ %.2f", tbl.merged$mean.HV, tbl.merged$sd.HV)
tbl.merged$mean.HV2 = sprintf("%s $\\pm$ %.2f", tbl.merged$mean.HV2, tbl.merged$sd.HV2)
tbl.merged$mean.HV3 = sprintf("%s $\\pm$ %.2f", tbl.merged$mean.HV3, tbl.merged$sd.HV3)
tbl.merged$mean.HV4 = sprintf("%s $\\pm$ %.2f", tbl.merged$mean.HV4, tbl.merged$sd.HV4)
tbl.merged$mean.EPS = sprintf("%s $\\pm$ %.2f", tbl.merged$mean.EPS, tbl.merged$sd.EPS)
tbl.merged$mean.EPS2 = sprintf("%s $\\pm$ %.2f", tbl.merged$mean.EPS2, tbl.merged$sd.EPS2)
tbl.merged$mean.EPS3 = sprintf("%s $\\pm$ %.2f", tbl.merged$mean.EPS3, tbl.merged$sd.EPS3)
tbl.merged$mean.EPS4 = sprintf("%s $\\pm$ %.2f", tbl.merged$mean.EPS4, tbl.merged$sd.EPS4)
tbl.merged$sd.HV = tbl.merged$sd.HV2 = tbl.merged$sd.HV3 = tbl.merged$sd.HV4 = tbl.merged$sd.EPS = tbl.merged$sd.EPS2 = tbl.merged$sd.EPS3 = tbl.merged$sd.EPS4 = NULL

for (i in ns) {
  ktbl = to_latex(
    filter(tbl.merged, n == i),
    reps = 2L,
    param.col.names = c("", "$n$", "$I$"),
    measure.col.names = c("\\textbf{mean $\\pm$ sd}", "\\textbf{stat}"),
    algo.names = c("UNIF", "1BEX", "SGS", "USGS"),
    algo.colors = algo.colors,
    caption = "Mean, standard deviation~(\\textbf{sd}) and results of Wilcoxon-Mann-Whitney tests at significance level $\\alpha=0.01$ (\\textbf{stat}) with respect to HV-indicator and $\\varepsilon$-indicator respectively. Data is shown for all instances with at least 100 nodes. The \\textbf{stat}-column is to be read as follows: a value $X^{+}$ indicates that the indicator for the column algorithm (note that algorithms are numbered and color-encoded in the second row) is significantly lower than the one of algorithm $X$. Lowest indicator values are highlighted in \\textbf{bold-face}.") %>%
    kable_styling() %>%
    #row_spec(row = c(10, 20, 30, 70), extra_latex_after = "\\cmidrule{2-19}") %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
    add_header_above(c(" ", " ", " ", "HV-indicator" = 8, "$\\\\varepsilon$-indicator" = 8), bold = TRUE, escape = FALSE)
  #preview(ktbl)
  cat(ktbl, file = sprintf("tables/indicators_n_%i.tex", i))
}
