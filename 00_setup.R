set.seed(42)
library(tidyverse)

library(marlin)

library(here)

library(furrr)

library(ggridges)

library(googledrive)

library(gamm4)

library(rgdal)

library(sf)

library(mvtnorm)

library(patchwork)

library(progress)

library(rpart.plot)

library(rstanarm)

library(ggrepel)

library(ggdist)

library(gganimate)

library(gifski)

options(dplyr.summarise.inform = FALSE)


foos <- list.files(here("functions"))

walk(foos, ~ source(here("functions", .x)))

print(Sys.getenv("RUN_NAME"))
if (Sys.getenv("RUN_NAME") == ''){
  run_name <- "v1.0"

} else {
  run_name <- Sys.getenv("RUN_NAME")
}


results_path <- here("results", run_name)

if (!dir.exists(results_path)){
  dir.create(results_path, recursive = TRUE)

  dir.create(file.path(results_path,"sims"))
}

run_coral_example <- TRUE

run_blue_water_example <- TRUE

experiment_workers <- 8

experiment_years <- 20

safety_stop <- FALSE

draws <- 20

resolution <- 20 # resolution is in squared patches, so 20 implies a 20X20 system, i.e. 400 patches

years <- 80

seasons <- 2

time_step <- 1 / seasons

max_delta <- 1

workers <- round(parallel::detectCores() - 8)
?unzip
steps <- years * seasons

time_steps <- seq(0,years - 1, by = time_step)

theme_set(marlin::theme_marlin())


# load data ---------------------------------------------------------------

if (!file.exists(here("data","marlin-inputs.xlsx"))){

  download.file("https://figshare.com/ndownloader/files/38766174", destfile = "data.zip", overwrite = TRUE,
                mode = "wb")

  unzip("data.zip")

  file.remove("data.zip")

}

marlin_inputs <- readxl::read_xlsx(here("data","marlin-inputs.xlsx"), sheet = "inputs",na = c("NA",""))

