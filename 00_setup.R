set.seed(42)
library(purrr)

library(dplyr)

library(tidyr)

library(readr)

library(forcats)

library(stringr)

library(ggplot2)

library(viridis)

library(marlin)

library(here)

library(furrr)

library(gamm4)

library(patchwork)

library(ggrepel)

library(ggdist)

library(rmarkdown)

library(quarto)

options(dplyr.summarise.inform = FALSE)


foos <- list.files(here("functions"))

walk(foos, ~ source(here("functions", .x)))

print(Sys.getenv("RUN_NAME"))
if (Sys.getenv("RUN_NAME") == ''){
  run_name <- "test"

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

theme_set(marlin::theme_marlin())


# load data ---------------------------------------------------------------

if (!file.exists(here("data","marlin-inputs.xlsx"))){

  download.file("https://figshare.com/ndownloader/files/38766174", destfile = "data.zip", overwrite = TRUE,
                mode = "wb")

  unzip("data.zip")

  file.remove("data.zip")

}

marlin_inputs <- readxl::read_xlsx(here("data","marlin-inputs.xlsx"), sheet = "inputs",na = c("NA",""))

