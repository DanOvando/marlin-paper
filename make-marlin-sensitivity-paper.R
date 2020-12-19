
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

set.seed(42)
library(tidyverse)

library(marlin)

library(here)

library(furrr)

library(ggridges)

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

# create core scenarios ---------------------------------------------------

# use this space to develop the core building block of scenarios you're going to use

fauna_frame <- tibble(
  scientific_name =  c(
    "Thunnus obesus",
    "Katsuwonus pelamis",
    "Thunnus albacares",
    "Kajikia audax",
    "Carcharhinus longimanus"
  ),
  xid = list(1:draws)) %>%
  unnest(cols = xid)


hab1 <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(habitat =  .2 * x) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()


hab2 <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(habitat =  dnorm(x, 9,5)) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()


generate_traits <- function(scientific_name){

  trait_frame <- tibble(
    seasonal_habitat = list(sample(list(hab1, hab2),1, replace = TRUE)),
    adult_movement = sample(c(0, 5),1, replace = TRUE),
    adult_movement_sigma = sample(c(1, 10), 1,replace = TRUE),
    seasons = seasons,
    init_explt = sample(c(.05,.1,.2), 1, replace = TRUE),
    rec_form = sample(c(0,1,2,3),1, replace = TRUE),
    fec_form = c("power"),
    weight_a = NA
  )

}


fauna_frame <- fauna_frame %>%
  mutate(traits = map(scientific_name,generate_traits))

create_random_critter <- function(scientific_name, traits,seasons){

  critter <- marlin::create_critter(
    scientific_name = scientific_name,
    seasonal_habitat = list(traits$seasonal_habitat),
    adult_movement = traits$adult_movement,
    adult_movement_sigma = traits$adult_movement_sigma,
    rec_form = traits$rec_form,
    seasons = seasons,
    init_explt = traits$init_explt
  )

}



fauna_frame <- fauna_frame %>%
  mutate(
    critter = future_map2(
      scientific_name,
      traits,
      create_random_critter,
      seasons = seasons,
      .progress = TRUE
    )
  )


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

steps_to_keep <- c(0, time_steps[round(0.75 * length(time_steps))], max(time_steps))

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

sims <- sims %>%
  # slice(1:3) %>%
  mutate(sim = future_pmap(
    list(
      fauna = fauna,
      fleet = fleet,
      mpa = mpa,
      id = xid
    ),
    run_and_process,
    year = years,
    steps_to_keep = steps_to_keep,
    seasons = seasons,
    write_sim = FALSE,
    results_path = results_path,
    .progress = TRUE
  ))

Sys.time() - a

print(object.size(sims$sim), units = "Mb")


# process simulations -----------------------------------------------------


## assess conservation impacts

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


sims <- sims %>%
  mutate(results = map2(sim, fauna, assess_sim))

tmp <- sims %>%
  select(xid, mpa_size, results) %>%
  unnest(cols = results)


tmp %>%
  filter(variable == "ssb", step > 0) %>%
  mutate(percent_change = pmin(2, percent_change)) %>%
  ggplot() +
  geom_vline(aes(xintercept = 0), linetype = 2, color = "red") +
  ggridges::geom_density_ridges(aes(percent_change, factor(step), fill = step), stat = "binline", show.legend = FALSE, alpha = 0.75) +
  facet_grid(mpa_size ~ critter, scales = "free_x") +
  scale_x_continuous(labels = scales::percent,
                     name = "Percent Change Caused by MPA",
                     guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete("Yearly Histograms") +
  labs(caption = "Rows equals percent of area in MPA")


tmp %>%
  filter(variable == "ssb", step > 0) %>%
  mutate(percent_change_ssb0 = pmin(2, percent_change_ssb0)) %>%
  ggplot() +
  geom_vline(aes(xintercept = 0), linetype = 2, color = "red") +
  ggridges::geom_density_ridges(aes(percent_change_ssb0, factor(step), fill = step), stat = "binline", show.legend = FALSE, alpha = 0.75) +
  facet_grid(mpa_size ~ critter, scales = "free_x") +
  scale_x_continuous(labels = scales::percent,
                     name = "Percent Change in SSB Caused by MPA As Fraction of SSB0",
                     guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete("Yearly Histograms") +
  labs(caption = "Rows equals percent of area in MPA")

tmp %>%
  filter(variable == "c", step > 0) %>%
  mutate(percent_change = pmin(2, percent_change)) %>%
  ggplot() +
  geom_vline(aes(xintercept = 0), linetype = 2, color = "red") +
  ggridges::geom_density_ridges(aes(percent_change, factor(step), fill = step), stat = "binline", show.legend = FALSE, alpha = 0.75) +
  facet_grid(mpa_size ~ critter, scales = "free_x") +
  scale_x_continuous(labels = scales::percent,
                     name = "Percent Change in Catch Caused by MPA",
                     guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete("Yearly Histograms") +
  labs(caption = "Rows equals percent of area in MPA")


## assess fleet impacts
