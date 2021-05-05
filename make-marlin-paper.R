# Assessing Tradeoffs in the use of MPAs for Bycatch Reduction

#something about relative magnitude given finite resouces to collect data?
#
# what have the existing MPAs done for sharks
# setup -------------------------------------------------------------------

# idea: define a centroid of habitat and a CV around that for each species and use that to tune the scenarios..
# like manually creats random centroids that avoid each other... then increase the CV, such that there really is no key habitat when they... no I don't like that
#
# what if you randomly sample the centroid for each species, where when the cv of the sampling is low, most species have the same centroid, and as that icnreases you get more and more different centroids for each species.... that's a better idea.. centroid by gindinf the index of center cell as a default starting position
#
# iterate over cv of the centroid,
# and then cv around the centroid

set.seed(42)
library(tidyverse)

library(marlin)

library(here)

library(furrr)

library(ggridges)

library(googledrive)

library(data.table)

library(gamm4)

library(sf)

library(mvtnorm)

library(patchwork)

options(dplyr.summarise.inform = FALSE)


foos <- list.files(here("R"))

walk(foos, ~ source(here("R", .x)))

results_name <- "v0.5"

results_path <- here("results", results_name)

if (!dir.exists(results_path)){
  dir.create(results_path, recursive = TRUE)

  dir.create(file.path(results_path,"sims"))
}

run_experiments <- FALSE

safety_stop <- FALSE

draws <- 20

resolution <- 20 # resolution is in squared patches, so 20 implies a 20X20 system, i.e. 400 patches

years <- 40

seasons <- 2

time_step <- 1 / seasons

workers <- round(parallel::detectCores() - 8)

steps <- years * seasons

time_steps <- seq(0,years - 1, by = time_step)

theme_set(marlin::theme_marlin())

tune_type <- "explt"

plan(multicore, workers = workers)


# create distributions for species ----------------------------------------
# create an spatial abundance distribution for each of the species in play
# for now, pull in species from marlin team database
# for now, requires authentication for emlab shared marlin data drive

# this is broken for now
# a = googledrive::drive_find("wcpfc_monthly.csv",n_max = 1)
#
# a = googledrive::drive_get("emlab/projects/current-projects/blue-prosperity-coalition/broader-research/mpa-bycatch/data/wcpfc_monthly.csv")
#
# drive_download(
#   "wcpfc_monthly.csv",
#   path = here("data","wcpfc_monthly.csv"),
#   overwrite = TRUE
# )
#

# for now pulling in from dovando07@gmail.com account

if (!file.exists(here("data","marlin-inputs.xlsx"))){

  dir.create("data")

  marlin_inputs_path <- googledrive::drive_get("https://docs.google.com/spreadsheets/d/1eiJIxiDYZLQlBuSLy-yIofhbc-KfWBR9v4FpThgXrQ0/edit#gid=0&fvid=1954704204")


  googledrive::drive_download(marlin_inputs_path, path = here("data","marlin-inputs.xlsx"), overwrite = TRUE)

}

marlin_inputs <- readxl::read_xlsx(here("data","marlin-inputs.xlsx"), sheet = "inputs",na = c("NA",""))


wcpfc_data <- read_csv(here("data","wcpfc_monthly.csv"))

unique(wcpfc_data$species_commonname)

top_bycatch <- wcpfc_data %>%
  filter(catch > 0) %>%
  group_by(species_commonname) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:4)

wcpfc_data <- wcpfc_data %>%
  filter(species_commonname %in% top_bycatch$species_commonname)

marlin_inputs$bycatch <- marlin_inputs$scientific_name %in% unique(tolower(wcpfc_data$species_sciname))


bycatch_ll_cpue <- wcpfc_data %>%
  filter(gear == "longline") %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", remove = FALSE) %>%
  recenter_vector(center = -80) %>%
  mutate(cpue = catch / effort) %>%
  group_by(species_commonname,longitude,latitude, year) %>%
  summarise(cpue = sum(cpue)) %>%
  group_by(species_commonname,longitude,latitude) %>%
  summarise(cpue = mean(cpue)) %>%
  group_by(species_commonname) %>%
  mutate(scaled_cpue = scale(cpue),
       bycatch = TRUE) %>%
  ungroup()


bycatch_ll_cpue_plot <- bycatch_ll_cpue %>%
  ungroup() %>%
  ggplot(aes(color = scaled_cpue)) +
  geom_sf(size = 2) +
  facet_wrap(~species_commonname) +
  scale_color_viridis_c(trans = "log10")


target_ll_catch <- wcpfc_data %>%
  select(year, longitude, latitude, contains("target_catch")) %>%
  group_by(year, longitude, latitude) %>%
  select(contains("_longline") & contains("tonnes") | (contains("_SKIPJACK") & contains("tonnes"))) %>%
  select(contains("TUNA") | contains("MARLIN")) %>%
  pivot_longer(contains("target_"),
               names_to = "species_commonname",
               values_to = "catch") %>%
  mutate(species_commonname = tolower(as.character(
    str_extract_all(
      species_commonname,
      "((?<=catch_)(.*)(?=_tonnes))",
      simplify = TRUE
    )
  ))) %>%
  group_by(year,longitude, latitude, species_commonname) %>%
  summarise(catch = sum(catch, na.rm = TRUE))

target_ll_effort <- wcpfc_data %>%
  select(year, longitude, latitude, contains("target_effort")) %>%
  group_by(year, longitude, latitude) %>%
  select(contains("_longline")) %>%
  rename(effort = starts_with("target_")) %>%
  filter(effort > 0)

target_ll_cpue <- target_ll_catch %>%
  left_join(target_ll_effort, by = c("year","longitude","latitude")) %>%
  ungroup() %>%
  filter(effort > 0 & !is.na(effort)) %>%
  mutate(cpue = catch / effort,
         bycatch = FALSE) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", remove = FALSE) %>%
  recenter_vector(center = -80) %>%
  group_by(species_commonname,longitude,latitude, year) %>%
  summarise(cpue = sum(cpue,na.rm = TRUE)) %>%
  group_by(species_commonname,longitude,latitude) %>%
  summarise(cpue = mean(cpue, na.rm = TRUE)) %>%
  ungroup()



# convert this thing into a square usable by marlin

wcpo_bbox <- sf::st_bbox(bycatch_ll_cpue) # get a bounding box

x_binsize <- (wcpo_bbox['xmax'] - wcpo_bbox['xmin']) / (resolution - 1)

y_binsize <- (wcpo_bbox['ymax'] - wcpo_bbox['ymin']) / (resolution - 1)


wcpo_coords <- sf::st_coordinates(bycatch_ll_cpue) %>%
  as_tibble() %>%
  mutate(X = X - min(X),
         Y = Y - min(Y)) %>%
  mutate(rough_x = floor(X / x_binsize),
         rough_y = floor(Y / y_binsize)) %>%
  select(contains("rough_"))

# xx need to fix the damn banding resulting from the spatial transformation
rough_bycatch_habitat <-  bycatch_ll_cpue %>%
  bind_cols(wcpo_coords) %>%
  as_tibble() %>%
  select(cpue, rough_x, rough_y, species_commonname) %>%
  group_by(rough_x, rough_y,species_commonname) %>%
  summarise(cpue = sum(cpue)) %>%
  ungroup() %>%
  complete(rough_x = 0:(resolution - 1), rough_y = 0:(resolution - 1), species_commonname, fill = list(cpue = 0)) %>%
  group_by(species_commonname, rough_x) %>%
  fill(cpue, .direction = "downup") %>%
  group_by(species_commonname) %>%
  mutate(cpue = cpue / max(cpue, na.rm = TRUE),
         rough_x =rough_x + 1,
         rough_y = rough_y + 1,
         bycatch = TRUE) %>%
  ungroup()

rough_bycatch_habitat %>%
  group_by(species_commonname) %>%
  ggplot(aes(rough_x, rough_y, fill = cpue)) +
  geom_tile() +
  facet_wrap(~species_commonname) +
  scale_fill_viridis_c(trans = "log10")

# repeat for target species

wcpo_bbox <- sf::st_bbox(target_ll_cpue) # get a bounding box

x_binsize <- (wcpo_bbox['xmax'] - wcpo_bbox['xmin']) / resolution

y_binsize <- (wcpo_bbox['ymax'] - wcpo_bbox['ymin']) / resolution


wcpo_coords <- sf::st_coordinates(target_ll_cpue) %>%
  as_tibble() %>%
  mutate(X = X - min(X),
         Y = Y - min(Y)) %>%
  mutate(rough_x = round(X / x_binsize),
         rough_y = round(Y / y_binsize)) %>%
  select(contains("rough_"))

rough_target_habitat <-  target_ll_cpue %>%
  bind_cols(wcpo_coords) %>%
  as_tibble() %>%
  select(cpue, rough_x, rough_y, species_commonname) %>%
  group_by(rough_x, rough_y,species_commonname) %>%
  summarise(cpue = sum(cpue)) %>%
  ungroup() %>%
  complete(rough_x = 0:(resolution - 1), rough_y = 0:(resolution - 1), species_commonname, fill = list(cpue = 0)) %>%
  group_by(species_commonname, rough_x) %>%
  fill(cpue, .direction = "downup") %>%
  group_by(species_commonname) %>%
  mutate(cpue = cpue / max(cpue, na.rm = TRUE),
         rough_x =rough_x + 1,
         rough_y = rough_y + 1,
         bycatch = FALSE) %>%
  ungroup()

rough_target_habitat %>%
  group_by(species_commonname) %>%
  ggplot(aes(rough_x, rough_y, fill = cpue)) +
  geom_tile() +
  facet_wrap(~species_commonname) +
  scale_fill_viridis_c(trans = "log10")


# combine habitats

rough_habitat <- rough_target_habitat %>%
  bind_rows(rough_bycatch_habitat) %>%
  filter(between(rough_x,1,resolution) & between(rough_y,1,resolution)) %>%
  mutate(species_commonname = tolower(species_commonname))

critter_lookup <-
  wcpfc_data %>% select(species_commonname, species_sciname) %>%
  unique()

# just do it by hand for right now
targeted_lookup <- tribble(~species_commonname, ~species_sciname,
                           "yellowfin tuna","Thunnus albacares",
                           "bigeye tuna", "Thunnus obesus",
                           "albacore tuna", "Thunnus alalunga",
                           "skipjack tuna", "katsuwonus pelamis",
                           "striped marlin","Kajikia audax",
                           "black marlin","Istiompax indica",
                           "blue marlin","Makaira mazara")


critter_lookup <- critter_lookup %>%
  bind_rows(targeted_lookup) %>%
  mutate(species_sciname = tolower(species_sciname),
         species_commonname = tolower(species_commonname))

tmp <- rough_habitat %>%
  group_by(species_commonname) %>%
  nest()

interp <- function(data){

  data <- data %>%
    mutate(xy = rough_x * rough_y)

  mod <- gamm4::gamm4(cpue ~ s(rough_x) + s(rough_y) + s(xy), data = data)

  data$interp_cpue <- as.numeric(predict(mod$gam))

  data$habitat <- pmax(0,data$interp_cpue / max(data$interp_cpue))

return(data)
}

rough_habitat <- tmp %>%
  ungroup() %>%
  mutate(data = map(data, interp)) %>%
  unnest(cols = data) %>%
  mutate(species_commonname = tolower(species_commonname)) %>%
  left_join(critter_lookup, by = "species_commonname")

rough_habitat %>%
  group_by(species_commonname) %>%
  ggplot(aes(rough_x, rough_y, fill = habitat)) +
  geom_tile() +
  # geom_point(aes(rough_x, rough_y, fill = cpue),shape = 21,alpha = 0.5) +
  facet_wrap(~species_commonname) +
  scale_fill_viridis_c()


habitat <- rough_habitat %>%
  select(contains("species_"), contains("rough_"), habitat) %>%
  rename(x = rough_x, y = rough_y) %>%
  group_by(species_commonname, species_sciname) %>%
  nest()

write_rds(rough_habitat,file = file.path(results_path, "rough-habitat.rds"))

# create core species ---------------------------------------------------

# for now, generate a series of critter objects for each of the critters
fauna_frame <- tibble(
  scientific_name =  unique(rough_habitat$species_sciname),
  xid = list(1:draws)) %>%
  unnest(cols = xid)

# so I think the most logical thing here is to make this a central location
# to generate fauna objects, locking in place things that you can easily
# pull from marlin_inputs, and randomizing others. That way you can easily
# stick one "one" result, or generate a bunch of iterations varying in key unknowns

# so you need to break the experiment in two. First you generate blocks of critters, where that can be combinations of
# habitat heterogeneity, larval / adult habitat overlap, seasonal movement etc. Create that
# Then, join that to a frame of MPA simulations, where yousimulate MPA sizes with different prioritizations


# for now let's leave the life history stuff out of it... and then once you have a sense of timing for this decide on how many iterations to do of each
experiments <- expand_grid(sigma_centroid = seq(.25 * resolution,resolution^2 / 2, length.out = 5),
                           sigma_hab = c(20,5),
                              ontogenetic_shift = c(TRUE,FALSE),
                              seasonal_movement = c(TRUE,FALSE)) %>%
  mutate(xid = 1:nrow(.))


test <- habitat$data[[1]] %>%
  mutate(test = x * y)

test %>%
  ggplot(aes(x,y,fill = test)) +
  geom_tile() +
  scale_fill_viridis_c()


distance <-
  tidyr::expand_grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(c_x = 20, c_y = 6) %>%
  mutate(distance = sqrt((c_x - x)^2 + (c_y -y)^2)) %>%
  mutate(habitat = dnorm(distance,0,20))

distance %>%
  ggplot(aes(x,y,fill = habitat)) +
  geom_tile() +
  scale_fill_viridis_c()



# function to assign habitat for each species based on

critters <- rough_habitat %>%
  select(species_sciname, bycatch) %>%
  unique() %>%
  rename(scientific_name = species_sciname) %>%
  mutate(centroid =NA)


create_critter_habitats <-
  function(sigma_centroid,
           sigma_hab = 0.2,
           base_centroid = c(resolution / 2, resolution / 2) ,
           critters,
           resolution) {
    # base_centroid <- c(10,10)
    # sigma_hab = .2

    # sigma_block <- 0.1

    # bycatch_corr <- -1


    base_layer <-
      tidyr::expand_grid(x = 1:resolution, y = 1:resolution)


    centroid_index <-
      which(base_layer$x == base_centroid[1] &
              base_layer$y == base_centroid[2])

    critters <- critters %>%
      arrange((bycatch))

    critters$centroid <-
      pmin(nrow(base_layer), pmax(1, round(
        rnorm(nrow(critters), centroid_index, sigma_centroid)
      )))

    critters$habitat <- vector(mode = "list", length = nrow(critters))

    for (i in 1:nrow(critters)) {
      tmp <- base_layer

      tmp <- base_layer %>%
        mutate(c_x = base_layer$x[critters$centroid[i]],
               c_y = base_layer$y[critters$centroid[i]]) %>%
        mutate(distance = sqrt((c_x - x) ^ 2 + (c_y - y) ^ 2)) %>%
        mutate(habitat = dnorm(distance, 0, sigma_hab)) %>%
        select(x,y,habitat)




      tmp$habitat <- tmp$habitat - min(tmp$habitat)

      # tmp %>%
      #   ggplot(aes(x, y, fill = habitat)) +
      #   geom_tile() +
      #   scale_fill_viridis_c()
      #

      critters$habitat[[i]] <-  tmp


    }


    # (critters$habitat[[1]] %>%
    #     ggplot(aes(x,y,fill = habitat))+
    #     geom_tile() +
    #     scale_fill_viridis_c()) +
    #   (critters$habitat[[10]] %>%
    #      ggplot(aes(x,y,fill = habitat))+
    #      geom_tile() +
    #      scale_fill_viridis_c())

    return(critters)
  }




experiments <- experiments %>%
  mutate(
    habitats = map2(
      sigma_centroid,
      sigma_hab,
      create_critter_habitats,
      critters = critters,
      resolution = resolution
    )
  ) %>%
  unnest(cols = habitats)

experiments$habitat[[3]] %>%
  ggplot(aes(x, y, fill = habitat)) +
  geom_tile() +
  scale_fill_viridis_c()





experiments <- experiments %>%
  mutate(critter = pmap(
    list(sciname = scientific_name,
         habitat = habitat,
         ontogenetic_shift = ontogenetic_shift,
         seasonal_movement = seasonal_movement),
    create_critters,
    marlin_inputs = marlin_inputs,
    seasons = seasons))



# fauna_frame <- fauna_frame %>%
#   mutate(
#     critter = map2(
#       scientific_name,
#       traits,
#       create_random_critter,
#       seasons = seasons
#     )
#   )



# aggregate into lists of fauna

experiments <- experiments %>%
  group_by(xid) %>%
  nest() %>%
  mutate(fauna = map(data, ~.x$critter %>% set_names(.x$scientific_name)))

check_pop_sizes <- map_dbl(experiments$fauna[[1]],"ssb0")

tibble(sciname = names(check_pop_sizes), ssb0 = check_pop_sizes) %>%
  ggplot(aes(reorder(sciname, ssb0), ssb0))+
  geom_col() +
  coord_flip()



# create fleets -----------------------------------------------------------
# create fleet objects
#
compile_fleet <- function(fauna, tune_type = "explt") {

  # browser()
  # fauna <- wtf

  fleets <- list(
    "longline" = create_fleet(
      list(
        "thunnus obesus" = Metier$new(
          critter = fauna$`thunnus obesus`,
          price = 10,
          sel_form = "logistic",
          sel_start = 100,
          sel_delta = .01,
          catchability = .1,
          p_explt = .5,
          sel_unit = "length"
        ),
        "thunnus alalunga" = Metier$new(
          critter = fauna$`thunnus alalunga`,
          price = 4.57,
          sel_form = "logistic",
          sel_start = 79.9,
          sel_delta = .01,
          catchability = .1,
          p_explt = .98,
          sel_unit = "length"
        ),
        "katsuwonus pelamis" = Metier$new(
          critter = fauna$`katsuwonus pelamis`,
          price = 1.36,
          sel_form = "logistic",
          sel_start = 65,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .01,
          sel_unit = "length"

        ),
        "thunnus albacares" = Metier$new(
          critter = fauna$`thunnus albacares`,
          price = 7.52,
          sel_form = "logistic",
          sel_start = 90,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = 0.28,
          sel_unit = "length"

        ),
        "kajikia audax" = Metier$new(
          critter = fauna$`kajikia audax`,
          price = 5.16,
          sel_form = "logistic",
          sel_start = 175,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .99,
          sel_unit = "length"

        ),
        "carcharhinus longimanus" = Metier$new(
          critter = fauna$`carcharhinus longimanus`,
          price = 1.89,
          sel_form = "logistic",
          sel_start = 175,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .98,
          sel_unit = "length"

        ),
        "istiompax indica" = Metier$new(
          critter = fauna$`istiompax indica`,
          price = 2.74,
          sel_form = "logistic",
          sel_start = 100,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .99,
          sel_unit = "length"

        ),
        "makaira mazara" = Metier$new(
          critter = fauna$`makaira mazara`,
          price = 3.74,
          sel_form = "logistic",
          sel_start = 150,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .95,
          sel_unit = "length"

        ),
        "carcharhinus falciformis" = Metier$new(
          critter = fauna$`carcharhinus falciformis`,
          price = 2.16,
          sel_form = "logistic",
          sel_start = 200,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .79,
          sel_unit = "length"

        ),
        "prionace glauca" = Metier$new(
          critter = fauna$`prionace glauca`,
          price = 10,
          sel_form = "logistic",
          sel_start = 200,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .99,
          sel_unit = "length"

        ),
        "isurus oxyrinchus" = Metier$new(
          critter = fauna$`isurus oxyrinchus`,
          price = 4.11,
          sel_form = "logistic",
          sel_start = 190,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .99,
          sel_unit = "length"

        )
      ),
      base_effort = resolution
    ),
    "purseseine" = create_fleet(
      list(
        "thunnus obesus" = Metier$new(
          critter = fauna$`thunnus obesus`,
          price = 1,
          sel_form = "logistic",
          sel_start = 50,
          sel_delta = .01,
          catchability = .1,
          p_explt = .5,
          sel_unit = "length"

        ),
        "thunnus alalunga" = Metier$new(
          critter = fauna$`thunnus alalunga`,
          price = 4.57,
          sel_form = "logistic",
          sel_start = 100,
          sel_delta = .01,
          catchability = .1,
          p_explt = 0.02,
          sel_unit = "length"

        ),
        "katsuwonus pelamis" = Metier$new(
          critter = fauna$`katsuwonus pelamis`,
          price = 1.36,
          sel_form = "logistic",
          sel_start = 50,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .99,
          sel_unit = "length"

        ),
        "thunnus albacares" = Metier$new(
          critter = fauna$`thunnus albacares`,
          price = 7.52,
          sel_form = "logistic",
          sel_start = 60,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .72,
          sel_unit = "length"

        ),
        "kajikia audax" = Metier$new(
          critter = fauna$`kajikia audax`,
          price = 5.16,
          sel_form = "logistic",
          sel_start = 175,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .01,
          sel_unit = "length"

        ),
        "carcharhinus longimanus" = Metier$new(
          critter = fauna$`carcharhinus longimanus`,
          price = 1.89,
          sel_form = "dome",
          sel_start = 160,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .02,
          sel_unit = "length"

        ),
        "istiompax indica" = Metier$new(
          critter = fauna$`istiompax indica`,
          price = 2.74,
          sel_form = "logistic",
          sel_start = 50,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .01,
          sel_unit = "length"

        ),
        "makaira mazara" = Metier$new(
          critter = fauna$`makaira mazara`,
          price = 3.74,
          sel_form = "logistic",
          sel_start = 1000,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = 0.05,
          sel_unit = "length"

        ),
        "carcharhinus falciformis" = Metier$new(
          critter = fauna$`carcharhinus falciformis`,
          price = 2.16,
          sel_form = "logistic",
          sel_start = 150,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .21,
          sel_unit = "length"

        ),
        "prionace glauca" = Metier$new(
          critter = fauna$`prionace glauca`,
          price = 5,
          sel_form = "logistic",
          sel_start = 110,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .01,
          sel_unit = "length"

        ),
        "isurus oxyrinchus" = Metier$new(
          critter = fauna$`isurus oxyrinchus`,
          price = 4.11,
          sel_form = "logistic",
          sel_start = 190,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = 0.01,
          sel_unit = "length"

        )
      ),
      base_effort = resolution
    )
  )


  # a <- Sys.time()

  fleets <- tune_fleets(fauna, fleets, tune_type = tune_type)
}


experiments <- experiments %>%
  ungroup() %>%
  mutate(fleet = map(fauna, compile_fleet))

write_rds(critter_lookup, file = file.path(results_path, 'critter-lookup.rds'))

# safety stop -------------------------------------------------------------

# make some plots

if (safety_stop){
safety_sim <- marlin::simmar(fauna = experiments$fauna[[19]],
                             fleets = experiments$fleet[[19]],
                             years = years)
proc_safety <- process_marlin(safety_sim, keep_age = FALSE)

plot_marlin(proc_safety)

space <- (plot_marlin(proc_safety, plot_type = "space", steps_to_plot = 1) + labs(title = "Summer")) +
  (plot_marlin(proc_safety, plot_type = "space", steps_to_plot = 1.5) + labs(title = "Winter")) & theme(strip.text = element_text(size = 6))
space

sample_mpas <- place_mpa(
  target_fauna = "carcharhinus longimanus",
  size = 0.2,
  fauna = experiments$fauna[[1]],
  placement_error = 0,
  place_randomly = FALSE
)

# sample_mpas <- place_mpa(target_fauna = "prionace glauca",
#                          size = 0.2, fauna = fauna_frame$fauna[[1]])

sample_mpas %>%
  ggplot(aes(x,y,fill = mpa)) +
  geom_tile()

safety_mpa_sim <- marlin::simmar(
  fauna = experiments$fauna[[1]],
  fleets = experiments$fleet[[1]],
  mpas = list(locations = sample_mpas,
              mpa_year = floor(years * .5)),
  years = years
)

proc_safety_mpa <- process_marlin(safety_mpa_sim, keep_age = FALSE)

plot_marlin(no_mpa = proc_safety, with_mpa = proc_safety_mpa, plot_var = "ssb")

plot_marlin(no_mpa = proc_safety, with_mpa = proc_safety_mpa, plot_var = "c")

ggsave("test-space.pdf", plot = space, height = 20, width = 10)

}
# tune system -------------------------------------------------------------
# tune the system (initial state, reference points, etc)

# already tuned at the moment

# generate MPA outcomes ---------------------------------------------------
# this is by far the most complicated part. Factorial combinations of
# 1. objective function
#   - which species, weighting across species, fishing, etc.
# 2. design paradigm
#   - optimize
#   - total biomass
#   - biodiversity
#   - risk
#   - block vs. network
#
#   for any one run, do your MPA optimization for a specified number of cells. And if eventually you want to look at a range of sizes, then actually going through all cells makes sense and storing the marginal contribution of each cell, so that that way you have a library to create MPAs of arbitrary size off of

# a <- experiments
a <- Sys.time()
nrow(experiments)
stop()
if (run_experiments){

  experiments <- experiments %>%
    mutate(experiment = pmap(
      list(fauna = fauna, fleet = fleet,
           xid = xid),
      run_mpa_experiment,
      placement_error = 0
    ))

diff <- Sys.time() - a

write_rds(experiments, file = file.path(results_path, "experiements.rds"))
} else {

  experiments <- read_rds(file = file.path(results_path, "experiements.rds"))
}
# (diff / nrow(experiments)) * 60

# process MPA outcomes ----------------------------------------------------
# deal with output of simulations
#



# results -----------------------------------------------------------------
# assess whether
# 1. Optimized MPAs induce any tradeoffs across species
# 2. design paradigms induce any tradeoffs across species
# 3. bycatch vs. fishery tradeoffs under optimized and design paradigms



a = critter_lookup %>%
  left_join(rough_habitat %>% select(species_commonname, bycatch) %>% unique)


scenes <- experiments %>%
  select(xid, data) %>%
  unnest(cols = data) %>%
  select(xid:seasonal_movement) %>%
  unique()


mpa_results <-  experiments %>%
  select(xid, experiment) %>%
  unnest(cols = experiment) %>%
  select(xid,target_fauna, mpa_size, results) %>%
  unnest(cols = results) %>%
  mutate(target_fauna = map_chr(target_fauna, ~paste(.x, collapse = ","))) %>%
  left_join(a, by = c("critter" = "species_sciname")) %>%
  left_join(scenes, by = "xid") %>%
  mutate(bycatch = ifelse(bycatch == TRUE, "Bycatch Species","Target Species"))

mpa_strategy <- c(
  `carcharhinus longimanus` = "MPAs for OWT Shark",
  `prionace glauca,carcharhinus longimanus,isurus oxyrinchus,carcharhinus falciformis` = "MPAs for All Bycatch"
)



mpa_results %>%
  filter(variable == "ssb") %>%
  ggplot(aes(
    mpa_size,
    percent_change_ssb0,
    color = species_commonname,
    group = interaction(xid,species_commonname,bycatch)
  )) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(size = 1.5, alpha = 0.25) +
  facet_grid( bycatch~ target_fauna, labeller = labeller(target_fauna = mpa_strategy), scales = "free_y") +
  scale_x_continuous(name = "MPA Size", labels = scales::percent) +
  scale_y_continuous(name = "% of SSB0 Change", labels = scales::percent) +
  scale_color_discrete(name = "Species") +
  scale_linetype(name = '') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))



mpa_strategy <- c(
  `carcharhinus longimanus` = "MPAs for OWT Shark",
  `prionace glauca,carcharhinus longimanus,isurus oxyrinchus,carcharhinus falciformis` = "MPAs for All Bycatch"
)


mpa_results %>%
  filter(variable == "c") %>%
  ggplot(aes(
    mpa_size,
    percent_change,
    color = species_commonname,
    group = interaction(xid,species_commonname,bycatch)
  )) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(size = 1, alpha = 0.25) +
  facet_wrap(bycatch ~ target_fauna, labeller = labeller(target_fauna = mpa_strategy)) +
  scale_x_continuous(name = "MPA Size", labels = scales::percent) +
  scale_y_continuous(name = "% Change in Catch", labels = scales::percent) +
  scale_color_discrete(name = "Species") +
  scale_linetype(name = '') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))



mpa_strategy <- c("MPAs for OWT Shark" = "carcharhinus longimanus" ,
                  "MPAs for All Bycatch" = "prionace glauca,carcharhinus longimanus,isurus oxyrinchus,carcharhinus falciformis")

mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  select(mpa_size,
         percent_change,
         species_commonname,
         target_fauna,
         variable,
         xid) %>%
  filter(variable %in% c("c", "ssb")) %>%
  pivot_wider(names_from = "variable", values_from = percent_change) %>%
  ggplot(aes(ssb, c, shape = target_fauna, linetype = target_fauna)) +
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  # geom_line() +
  geom_point(aes(color = mpa_size), size = 2) +
  facet_wrap(~ species_commonname, scales = "free") +
  scale_y_continuous(name = "% Change in Catch", labels = scales::percent) +
  scale_x_continuous(name = "% of SSB0 Change", labels = scales::percent) +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top",
    legend.direction = "horizontal"
  ) +
  scale_colour_viridis_b(
    name = "MPA Size",
    labels = scales::percent,
    guide = guide_colorbar(barwidth = unit(9, "lines"))
  ) +
  scale_shape(name = "") +
  scale_linetype(name = "")





a %>% knitr::kable() %>% kableExtra::kable_paper()


# what is interesting
#
#
# proportion stocks benefiting / losing as a functino of MPA size and spatial homogeneity
#

all_species_p_ssb_effect_plot <- mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= 0),
            percent_lose = 1 - percent_improve) %>%
  ggplot(aes(mpa_size, percent_lose, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA reduces SSB") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")


mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= .1),
            percent_lose = mean(percent_change <= -.1)) %>%
  ggplot(aes(mpa_size, percent_lose, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where SSB Decreaes by more than 10%") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")

mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= .1),
            percent_lose = mean(percent_change <= -.1)) %>%
  ggplot(aes(mpa_size, percent_improve, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPAs increase by more htan 10%") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")



all_species_mean_p_ssb_plot <- mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(net_percent_change = weighted.mean(percent_change,ssb0)) %>%
  ggplot(aes(mpa_size, net_percent_change, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA reduces SSB") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")



all_species_net_ssb_effect_plot <- mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(net_change = sum(abs_change)) %>%
  ggplot(aes(mpa_size, net_change, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(name = "Net Change in SSB") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")


bycatch_ssb_effect_plot <- mpa_results %>%
  filter(bycatch == "Bycatch Species") %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= 0),
            percent_lose = 1 - percent_improve) %>%
  ggplot(aes(mpa_size, percent_lose, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA reduces SSB") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")


all_species_catch_effect_plot <- mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "c") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= 0),
            percent_lose = 1 - percent_improve) %>%
  ggplot(aes(mpa_size, percent_lose, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA reduces Catch") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")

mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "c") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= .1),
            percent_lose = mean(percent_change <= -.1)) %>%
  ggplot(aes(mpa_size, percent_lose, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA reduces Catch by more than 10%") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")


mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "c") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change > .1),
            percent_lose = mean(percent_change < -.1)) %>%
  ggplot(aes(mpa_size, percent_improve, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA incease Catch by >10%") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")


# now run WCPO-esque case study with the actual habitats ------------------


