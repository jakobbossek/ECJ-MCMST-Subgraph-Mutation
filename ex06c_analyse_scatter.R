library(tidyverse)


# PARETO-FRONT APPROXIMATIONS
# ===
#
# This script draws examplary scatterplots of the Pareto-front approximations
#

res.all = readr::read_delim("results/algoruns_all_pf_only.csv", delim = " ", col_types = "ddcciic")

# get all instances
probs = unique(res.all$prob)

set.seed(123)

# Get each one random instance of each class
probs.demo = sapply(1:4, function(cl) {
  tmp = probs[grepl(paste0(cl, "_250"), basename(probs))]
  sample(tmp, size = 1L)
})

# get non-dominated points of union of all runs for each (prob, algorithm) combination
res.demo = filter(res.all, prob %in% probs.demo) %>%
  group_by(prob, class, algorithm) %>%
  dplyr::mutate(nd = ecr::nondominated(matrix(c(y1, y2), byrow = TRUE, nrow = 2L))) %>%
  filter(nd == TRUE) %>%
  ungroup()

# now show PF-approximations for sub-graph based and generation 10 and all other in generation 100
res.demo2 = filter(res.demo, is.na(generation) | (generation == 100 & grepl("SG|USG|1EX|1BEX|UNIFORM", algorithm)))
res.demo2 = filter(res.demo2, !grepl("log", algorithm))
# actually plot
res.demo2$prob = res.demo2$class
res.demo2$algorithm = factor(res.demo2$algorithm, levels = c("WEIGHTED SUM", "UNIFORM (100%)", "1EX (100%)", "1BEX (100%)", "SG (100%)", "SGS (100%)", "USG (100%)", "USGS (100%)"))
res.demo2$y1 = res.demo2$y1 - 1
res.demo2$y2 = res.demo2$y2 - 1
pl = ecr::plotScatter2d(res.demo2, obj.cols = c("y1", "y2"), colour = "algorithm", facet.args = list(nrow = 2L))
pl = pl + theme_minimal()
pl = pl + theme(legend.position = "top", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-8, -8, -8, -8))
pl = pl + guides(colour = guide_legend(nrow = 2L), shape = guide_legend(nrow = 2L))
#pl = pl + viridis::scale_color_viridis(discrete = TRUE, end = 0.8, alpha = 0.8)
pl = pl + scale_color_brewer(palette = "Dark2")
pl = pl + labs(x = expression(c[1](T)), y = expression(c[2](T)), colour = "Algorithm")
pl
ggsave("figures/benchmark/scatter.pdf", width = 7.2, height = 6.7, device = cairo_pdf, limitsize = FALSE)
