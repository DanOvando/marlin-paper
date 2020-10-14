
# setup -------------------------------------------------------------------
# how sensitive is the outcome of an MPA to different uncertainties.
#
# i.e. put it somewhere, but suppose you don't know movement exactly
#
# shared, discrete, shared habitat
# price information
# ecology of the species
# mpa modeling design
# static modeling
#
#
library(tidyverse)

library(marlin)

library(here)

library(trelliscopejs)

library(furrr)



foos <- list.files(here("R"))

walk(foos, ~ source(here("R", .x)))

resolution <- 25 # resolution is in squared patches, so 20 implies a 20X20 system, i.e. 400 patches

years <- 20

seasons <- 4

time_step <- 1 / seasons

workers <- 3

steps <- years * seasons

theme_set(marlin::theme_marlin())

tune_type <- "explt"

plan(multisession, workers = workers)

# create core scenarios ---------------------------------------------------

# use this space to develop the core building block of scenarios you're going to use

hab0 <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(habitat =  dnorm(x, 5,5)) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()

hab1 <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(habitat =  dnorm((x ^ 2 + y ^ 2), 300, 100)) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()

hab2 <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(habitat =  dnorm(x, 20,5)) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()





trait_frame <- expand_grid(
  seasonal_habitat = list(hab0, hab1),
  adult_movement = c(0, 5),
  adult_movement_sigma = c(1, 10),
  seasons = seasons,
  init_explt = c(.1, .6),
  rec_form = c(0, 3),
  fec_form = c("power"),
  weight_a = NA
) %>%
  mutate(xid = 1:nrow(.))



fauna_frame <- tibble(
  scientific_name =  c(
  "Thunnus obesus",
  "Katsuwonus pelamis",
  "Thunnus albacares",
  "Kajikia audax",
  "Carcharhinus longimanus"
),
trait = list(trait_frame)) %>%
  unnest(cols = trait)


# create individual critters



fauna_frame <- fauna_frame %>%
  mutate(critter = furrr::future_pmap(
    list(
      scientific_name = scientific_name,
      seasonal_habitat = seasonal_habitat,
      adult_movement = adult_movement,
      adult_movement_sigma = adult_movement_sigma,
      rec_form = rec_form,
      seasons = seasons,
      init_explt = init_explt
    ),
    create_critter,
    season_blocks = list(),
    .progress = TRUE
  ))


# aggregate into lists of fauna

fauna_frame <- fauna_frame %>%
  group_by(xid) %>%
  nest() %>%
  mutate(fauna = map(data, ~.x$critter %>% set_names(.x$scientific_name)))

# create a fleets object, which is a list of lists (of lists). Each fleet has one element,
# with lists for each species inside there. Price specifies the price per unit weight of that
# species for that fleet
# sel_form can be one of logistic or dome


fauna_frame <- fauna_frame %>%
  ungroup() %>%
  mutate(fleet = future_map(fauna, compile_fleet, .progress = TRUE))


# tunas are mostly OK, but include one overfished billfish, follow Pons
# all bycatch species are overfished
# A few sharks to start with

# Add in MPAs -----------------------------------------------------
# generate iterations over variables of core iterations
# homogeneity in habitat
# habitat vs catchability
# MPA design strategy
# top X percent of fishery value
# top X percent of bycvatch species
# top X percent of all species
# migratory
# depensity dependent movement
#
#

# change that to proportion of k
sims <-
  expand_grid(
    target_fauna = list(c("Carcharhinus longimanus", "Kajikia audax"), c("Thunnus obesus")),
    mpa_size = c(0.1, .3, .5),
    xid = unique(fauna_frame$xid)
  ) %>%
  left_join(fauna_frame, by = "xid")



sims <- sims %>%
  mutate(mpa = pmap(
    list(
      target_fauna = target_fauna,
      size = mpa_size,
      fauna = fauna
    ),
    place_mpa
  ))

#
#
#
sims$mpa[[1]] %>%
  ggplot(aes(x,y,fill = mpa)) +
  geom_tile()


# key counterfactual: comapre to standard MPA model where everything has the same habitat, maybe some different dispersals, but all diffusize
#
#
# run simulations ---------------------------------------------------------

a <- Sys.time()

sims <- sims %>%
  slice(1:3) %>%
  mutate(sim = pmap(
    list(
      fauna = fauna,
      fleets = fleet,
      mpa = mpa
    ),
    ~  simmar(
      fauna = ..1,
      fleets = ..2,
      years = years,
      mpas = list(locations = ..3,
                  mpa_year = floor(years * .5))
    ),
    year = years
  ))

Sys.time() - a


sims <- sims %>%
  mutate(processed_marlin = map(sim, ~process_marlin(.x, time_step = time_step)))


# process_marlin(sim = sims$sim[[1]], time_step = time_step)


sims <- sims %>%
  mutate(ssb_plot = map_plot(processed_marlin, ~plot_marlin(.x, plot_var = "ssb")))

trelliscopejs::trelliscope(
  sims %>% select(-mpa, -sim, -processed_marlin, -target_fauna) %>% ungroup() %>% mutate(thing = 1:nrow(.)),
  name = "thing",
  panel_col = "ssb_plot"
)


# processed_marlin <- process_marlin(sim = sim, time_step = time_step)
#
# plot_marlin(processed_marlin)
#
# plot_marlin(processed_marlin, plot_var = "c")
#
# plot_marlin(processed_marlin, plot_var = "n", plot_type = "length", fauna = fauna)
#
# plot_marlin(processed_marlin, plot_var = "ssb", plot_type = "space")
#


# process simulations -----------------------------------------------------



