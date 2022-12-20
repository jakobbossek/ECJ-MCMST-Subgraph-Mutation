library(tidyverse)

# ANALYSE EMOA INDICATORS
# ===
#
# Plots the runtimes of the algorithms.
#


times.all = readr::read_delim("results/algotimes.csv", delim = " ")

times.all$n = as.integer(lapply(strsplit(times.all$prob, split = "_"), function(el) el[2L]))
times.all$group = "Baseline"
times.all$group[grepl("SG", times.all$algorithm)] = "Sub-graph"

# now compute statistics
times.aggr = times.all %>%
  group_by(class, n, algorithm, group) %>%
  dplyr::summarize(time.mean = mean(time.passed), time.sd = sd(time.passed)) %>%
  ungroup()

#times.aggr.ss = filter(times.aggr, grepl("100", algorithm, fixed = TRUE) | grepl("( 10%)", algorithm, fixed = TRUE) | grepl("( 10%)", algorithm, fixed = TRUE))
times.aggr.ss = filter(times.aggr, algorithm %in% c("1BEX (100%)", "1EX (100%)", "UNIFORM (100%)") | ((grepl("SG", algorithm, fixed = TRUE) & (!grepl("50%", algorithm, fixed = TRUE)))))

pl = ggplot(times.aggr.ss, aes(x = algorithm, y = time.mean, fill = class, color = class))
pl = pl + geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8)
pl = pl + geom_errorbar(aes(ymin = time.mean - time.sd, ymax = time.mean + time.sd), width = 0.7, position = position_dodge(0.9))
pl = pl + facet_grid(n~ group, scales = "free", drop = TRUE, space = "free_x")
pl = pl + theme_minimal()
pl = pl + theme(axis.text.x = element_text(hjust = 1, angle = 25), legend.position = "top", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-8, -8, -8, -8))
pl = pl + viridis::scale_fill_viridis(discrete = TRUE, end = 0.75, alpha = 0.8)
pl = pl + viridis::scale_color_viridis(discrete = TRUE, end = 0.75, alpha = 0.8)
#pl = pl + scale_color_brewer(palette = "Dark2")
#pl = pl + scale_fill_brewer(palette = "Dark2")
pl = pl + labs(x = "", y = "Running time [s]", fill = "Graph class", ymin = "Graph class", ymax = "Graph class", fill = "Graph class", color = "Graph class")
pl
ggsave("figures/benchmark/runtimes.pdf", plot = pl, width = 7.2, height = 6, device = cairo_pdf, limitsize = FALSE)
