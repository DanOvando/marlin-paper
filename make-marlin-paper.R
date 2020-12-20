# Assesing Tradeoffs in the use of MPAs for Bycatch Reduction

# setup -------------------------------------------------------------------

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

options(dplyr.summarise.inform = FALSE)


foos <- list.files(here("R"))

walk(foos, ~ source(here("R", .x)))

results_name <- "v0.5"

results_path <- here("results", results_name)

if (!dir.exists(results_path)){
  dir.create(results_path, recursive = TRUE)

  dir.create(file.path(results_path,"sims"))
}

draws <- 300

resolution <- 20 # resolution is in squared patches, so 20 implies a 20X20 system, i.e. 400 patches

years <- 40

seasons <- 1

time_step <- 1 / seasons

workers <- 3

steps <- years * seasons

time_steps <- seq(0,years - 1, by = time_step)

theme_set(marlin::theme_marlin())

tune_type <- "explt"

plan(multisession, workers = workers)


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
# create core species ---------------------------------------------------

# for now, generate a series of critter objects for each of the critters
draws <- 1
fauna_frame <- tibble(
  scientific_name =  unique(rough_habitat$species_sciname),
  xid = list(1:draws)) %>%
  unnest(cols = xid)


generate_traits <- function(scientific_name, habitat){

  # later sub in a lookup table for this
  trait_frame <- tibble(
    seasonal_habitat = list(habitat$data[[which(habitat$species_sciname == scientific_name)]]),
    adult_movement = sample(c(0, 20),1, replace = TRUE),
    adult_movement_sigma = sample(c(20), 1,replace = TRUE),
    seasons = seasons,
    init_explt = sample(c(.05,.1,.2), 1, replace = TRUE),
    rec_form = sample(c(0,1,2,3),1, replace = TRUE),
    fec_form = c("power"),
    weight_a = NA
  )

}


fauna_frame <- fauna_frame %>%
  mutate(traits = map(scientific_name,generate_traits, habitat = habitat))

create_random_critter <- function(scientific_name, traits,seasons){

  hab <- traits$seasonal_habitat[[1]] %>%
    pivot_wider(names_from = y, values_from = habitat) %>%
    select(-x) %>%
    as.matrix()

  critter <- marlin::create_critter(
    scientific_name = scientific_name,
    seasonal_habitat = hab,
    adult_movement = traits$adult_movement,
    adult_movement_sigma = traits$adult_movement_sigma,
    rec_form = traits$rec_form,
    seasons = seasons,
    init_explt = traits$init_explt
  )

}



fauna_frame <- fauna_frame %>%
  mutate(
    critter = map2(
      scientific_name,
      traits,
      create_random_critter,
      seasons = seasons
    )
  )


# aggregate into lists of fauna

fauna_frame <- fauna_frame %>%
  group_by(xid) %>%
  nest() %>%
  mutate(fauna = map(data, ~.x$critter %>% set_names(.x$scientific_name)))



# create fleets -----------------------------------------------------------
# create fleet objects

fauna_frame <- fauna_frame %>%
  ungroup() %>%
  mutate(fleet = map(fauna, compile_fleet))



# safety stop -------------------------------------------------------------

# make some plots

safety_sim <- marlin::simmar(fauna = fauna_frame$fauna[[1]],
                             fleets = fauna_frame$fleet[[1]])
proc_safety <- process_marlin(safety_sim, keep_age = FALSE)

plot_marlin(proc_safety)

space <- plot_marlin(proc_safety, plot_type = "space")

sample_mpas <- place_mpa(target_fauna = "carcharhinus longimanus",
                         size = 0.2, fauna = fauna_frame$fauna[[1]])

# sample_mpas <- place_mpa(target_fauna = "prionace glauca",
#                          size = 0.2, fauna = fauna_frame$fauna[[1]])

sample_mpas %>%
  ggplot(aes(x,y,fill = mpa)) +
  geom_tile()

safety_mpa_sim <- marlin::simmar(fauna = fauna_frame$fauna[[1]],
                             fleets = fauna_frame$fleet[[1]],
                             mpas = list(locations = sample_mpas,
                                         mpa_year = floor(years * .5)))

proc_safety_mpa <- process_marlin(safety_mpa_sim, keep_age = FALSE)

plot_marlin(no_mpa = proc_safety, with_mpa = proc_safety_mpa, plot_var = "ssb")

plot_marlin(no_mpa = proc_safety, with_mpa = proc_safety_mpa, plot_var = "c")

ggsave("test-space.pdf", plot = space, height = 20, width = 10)

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


mpa_sims <-
  expand_grid(
    target_fauna = list(
      "carcharhinus longimanus",
      unique(rough_habitat$species_sciname[rough_habitat$bycatch])
    ),
    mpa_size = seq(0, 1, by = .1),
    random_mpas = c(FALSE)
  )


mpa_sims <- mpa_sims %>%
  mutate(mpa = pmap(
    list(
      target_fauna = target_fauna,
      size = mpa_size
    ),
    place_mpa,
    fauna = fauna_frame$fauna[[1]]
  )) %>%
  mutate(id = 1:nrow(.))

#
#
i = 5
mpa_sims$mpa[[i]] %>%
  ggplot(aes(x,y,fill = mpa)) +
  geom_tile()

steps_to_keep <- c(max(time_steps))

run_and_process <-
  function(fauna,
           fleet,
           mpa,
           steps_to_keep,
           years,
           seasons,
           id,
           keep_age = FALSE,
           write_sim = FALSE,
           results_path) {


    # fauna <- sims$fauna[[1]]
    #
    # fleet <- sims$fleet[[1]]
    #
    # mpa <- sims$mpa[[1]]
    #

    sim_base <- simmar(fauna = fauna,
                       fleets = fleet,
                       years = years)

    proc_sim_base <-
      process_marlin(
        sim = sim_base,
        time_step =  fauna[[1]]$time_step,
        steps_to_keep = steps_to_keep,
        keep_age = keep_age
      )


    sim_mpa <- simmar(
      fauna = fauna,
      fleets = fleet,
      years = years,
      mpas = list(
        locations = mpa,
        mpa_year = floor(years * seasons * .5)
      )
    )
    proc_sim_mpa <-
      process_marlin(
        sim = sim_mpa,
        time_step =  fauna[[1]]$time_step,
        steps_to_keep = steps_to_keep,
        keep_age = keep_age
      )

    # plot_marlin(proc_sim_mpa, proc_sim_base) +
    #   geom_vline(aes(xintercept = floor(years * seasons * .5)))

    out <- list(with_mpa = proc_sim_mpa,
                without_mpa = proc_sim_base)

    if (write_sim){

      readr::write_rds(out, file.path(results_path,"sims", paste0("sim_",id,".rds")))

      out <- NA
    }

    return(out)

  }

a <- Sys.time()

mpa_sims <- mpa_sims %>%
  # slice(1:3) %>%
  mutate(sim = future_pmap(
    list(
      mpa = mpa,
      id = id
    ),
    run_and_process,
    year = years,
    steps_to_keep = steps_to_keep,
    fauna = fauna_frame$fauna[[1]],
    fleet = fauna_frame$fleet[[1]],
    seasons = seasons,
    write_sim = FALSE,
    results_path = results_path,
    .progress = TRUE
  ))

diff <- Sys.time() - a

(diff / nrow(sims)) * 60

# process MPA outcomes ----------------------------------------------------
# deal with output of simulations
#

assess_sim <- function(sim, fauna,thing = "fauna"){

  # thing = "fauna"
  # sim <- sims$sim[[1]]
  #

  ssb0s <- fauna %>%
    map_dfc("ssb0",.id = "critter") %>%
    pivot_longer(everything(), names_to = "critter", values_to = "ssb0")

  tmp <- sim %>% map(thing) %>%
    bind_rows(.id = "scenario") %>%
    pivot_longer(n:c, names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = "scenario", values_from = "value") %>%
    group_by(step, variable, critter) %>%
    summarise(
      with_mpa = sum(with_mpa),
      without_mpa = sum(without_mpa),
      percent_improved = mean(with_mpa > without_mpa)
    ) %>%
    ungroup() %>%
    left_join(ssb0s, by = "critter") %>%
    mutate(abs_change = with_mpa - without_mpa,
           percent_change = with_mpa / without_mpa - 1,
           percent_change_ssb0 = (with_mpa - without_mpa) / ssb0)
}


mpa_sims <- mpa_sims %>%
  mutate(results = map(sim, assess_sim, fauna = fauna_frame$fauna[[1]]))


# results -----------------------------------------------------------------
# assess whether
# 1. Optimized MPAs induce any tradeoffs across species
# 2. design paradigms induce any tradeoffs across species
# 3. bycatch vs. fishery tradeoffs under optimized and design paradigms



mpa_results <-  mpa_sims %>%
  select(target_fauna, mpa_size, results) %>%
  unnest(cols = results) %>%
  mutate(target_fauna = map_chr(target_fauna, ~paste(.x, collapse = ",")))

mpa_results %>%
  filter(variable == "ssb") %>%
  ggplot(aes(mpa_size, percent_change_ssb0, color = critter)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  facet_wrap(~target_fauna) +
  scale_x_continuous(name = "MPA Size", labels = scales::percent) +
  scale_y_continuous(name = "% of SSB0 Change", labels = scales::percent)


mpa_results %>%
  filter(variable == "c") %>%
  ggplot(aes(mpa_size, percent_change, color = critter)) +
  geom_line() +
  facet_wrap(~target_fauna) +
  scale_x_continuous(name = "MPA Size", labels = scales::percent) +
  scale_y_continuous(name = "% Change in Catch", labels = scales::percent)


mpa_results %>%
  select(mpa_size, percent_change, critter, target_fauna, variable) %>%
  filter(variable %in% c("c","ssb")) %>%
  pivot_wider(names_from = "variable", values_from = percent_change) %>%
  ggplot(aes(ssb, c, color = mpa_size, shape = target_fauna, linetype = target_fauna)) +
  geom_line() +
  geom_point() +
  facet_grid( ~ critter, scales = "free")





