set.seed(42)
library(tidyverse)

library(marlin)

library(here)

library(furrr)

library(ggridges)

library(googledrive)

# library(data.table)

library(gamm4)

library(sf)

library(mvtnorm)

library(patchwork)

library(progress)

library(rpart.plot)

library(rstanarm)

options(dplyr.summarise.inform = FALSE)


foos <- list.files(here("R"))

walk(foos, ~ source(here("R", .x)))

results_name <- "tester"

results_path <- here("results", results_name)

if (!dir.exists(results_path)){
  dir.create(results_path, recursive = TRUE)

  dir.create(file.path(results_path,"sims"))
}

run_experiments <- TRUE

experiment_workers <- 4

run_casestudy <- FALSE

optimize_casestudy <- FALSE

safety_stop <- FALSE

draws <- 20

resolution <- 10 # resolution is in squared patches, so 20 implies a 20X20 system, i.e. 400 patches

years <- 80

seasons <- 2

time_step <- 1 / seasons

workers <- round(parallel::detectCores() - 8)

steps <- years * seasons

time_steps <- seq(0,years - 1, by = time_step)

theme_set(marlin::theme_marlin())

tune_type <- "v1.0"

# load data ---------------------------------------------------------------

if (!file.exists(here("data","marlin-inputs.xlsx"))){

  dir.create("data")

  marlin_inputs_path <- googledrive::drive_get("https://docs.google.com/spreadsheets/d/1eiJIxiDYZLQlBuSLy-yIofhbc-KfWBR9v4FpThgXrQ0/edit#gid=0&fvid=1954704204")


  googledrive::drive_download(marlin_inputs_path, path = here("data","marlin-inputs.xlsx"), overwrite = TRUE)

}

marlin_inputs <- readxl::read_xlsx(here("data","marlin-inputs.xlsx"), sheet = "inputs",na = c("NA",""))


wcpfc_data <- read_csv(here("data","wcpfc_monthly.csv"))


top_bycatch <- wcpfc_data %>%
  filter(catch > 0) %>%
  group_by(species_commonname) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:5)

bycatch <- wcpfc_data %>%
  filter(species_commonname %in% top_bycatch$species_commonname)

marlin_inputs$bycatch <- marlin_inputs$scientific_name %in% unique(tolower(bycatch$species_sciname))

