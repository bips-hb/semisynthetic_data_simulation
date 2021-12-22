# Script containing the plotting functions
library(ggplot2)
library(readr)
library(dplyr)
library(simsham)
library(latex2exp)
library( purrrlyr)

source("exec/load-results.R")
source("exec/plot.R")

### create result plots
source("parameter-settings.R")

# create filenames
sim_param <- sim_param %>% 
  mutate(
    filename = sprintf("figures/%s-%g-%g-%g-%g.pdf", corr_level, p, s, n, snr)
  )

for (i in 1:nrow(sim_param)) { 
  sp <- sim_param[i, ] 
   p <- plot_results(results, sp$corr_type, sp$p, sp$s, sp$n, sp$snr, 
                  title = "", 
                  ylim = c(0, 1))
  ggsave(sp$filename, p, width = 5, height = 2)
}

