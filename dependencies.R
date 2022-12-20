# install package management library
install.packages("remotes", dep = TRUE)

# install additional packages
install.packages("BBmisc")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gridExtra")
remotes::install_github("mllg/batchtools")
remotes::install_github("tidyverse/lubridate")
remotes::install_github("hadley/emo")

# install dev-versions of packages maintained by the main author of the paper
pkgs = c("grapherator", "mcMST", "ecr2", "re", "tbutils", "experimentR")
for (pkg in pkgs) {
  repo = paste0("jakobbossek/", pkg)
  remotes::install_github(repo, force = TRUE)
}
