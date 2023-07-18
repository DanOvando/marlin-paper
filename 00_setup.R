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

library(lme4)

library(patchwork)

library(ggrepel)

library(ggdist)

library(rmarkdown)

library(extrafont)

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

theme_set(marlin::theme_marlin(base_size = 12))


# load data ---------------------------------------------------------------

if (!file.exists(here("data","marlin-inputs.xlsx"))){

  download.file("https://figshare.com/ndownloader/files/38766174", destfile = "data.zip", overwrite = TRUE,
                mode = "wb")

  unzip("data.zip")

  file.remove("data.zip")

}

marlin_inputs <- readxl::read_xlsx(here("data","marlin-inputs.xlsx"), sheet = "inputs",na = c("NA",""))

if (!file.exists(here("data","sci_to_com.csv"))){
  sci_to_com <- taxize::sci2comm(unique(marlin_inputs$scientific_name),simplify= TRUE,db='ncbi') %>%
    bind_rows(.id = "critter") %>%
    pivot_longer(everything(),names_to = "critter",values_to = "common_name") %>%
    mutate(common_name = stringr::str_to_title(common_name))

  write_csv(sci_to_com, file = here("data","sci_to_com.csv"))

} else{
  sci_to_com <-  read_csv(file = here("data","sci_to_com.csv"))

}
