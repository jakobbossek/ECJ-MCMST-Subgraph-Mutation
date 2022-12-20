library(BBmisc)
library(tidyverse)
library(ggplot2)
library(scales)

devtools::load_all("~/repos/software/r/tblutils")

# TRAJECTORIES
# ===
#
# Plot the performance trajectories.
#

tbl = readr::read_delim("results/trajectories.csv", delim = " ")
colnames(tbl) = gsub("fitness.", "", colnames(tbl))

tbl = tbl %>%
  dplyr::mutate(class = rep(paste0("C", 1:4), each = nrow(tbl) / 4)) %>%
  dplyr::select(-problem, -job.id, -time.passed) %>%
  dplyr::group_by(class) %>%
  dplyr::mutate(HV = HV / max(HV)) %>%
  dplyr::ungroup()

ten.percent.budget = ceiling(max(tbl$gen) * 0.1)

g = tblutils::convergence_plot(tbl,
  x = "gen", y = "HV", color = "mutator",
  hlines.at = 0.4, vlines.at = ten.percent.budget,
  x.logscale.base  = 10)
g = g + theme_minimal()
g = g + theme(legend.position = "top")
g = g + guides(colour = guide_legend(nrow = 1))
g = g + facet_wrap(. ~ class, nrow = 1L)
g = g + labs(
  y = "HV-indicator",
  x = "Iteration (log-scaled)",
  color = "Mutation operator",
  linetype = "Mutation operator")
ggsave("figures/benchmark/trajectories.pdf", width = 7.2, height = 2.5, device = cairo_pdf, limitsize = FALSE)
