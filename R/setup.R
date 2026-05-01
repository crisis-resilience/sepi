# R/setup.R — Core dependencies shared across pipeline scripts (02–06)
# Sources: config, utils, load_data, normalise, compute_index
# Each script adds its own extra packages and sources after calling this file.

for (pkg in c("tidyverse", "psych", "purrr", "rlang", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(tidyverse)

source("R/config.R")
source("R/utils.R")
source("R/load_data.R")
source("R/normalise.R")
source("R/compute_index.R")
