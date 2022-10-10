source(file.path("scripts", "00_setup.R"))

library(gganimate)

resolution <- 20

seasons <- 4

tune_type <- "explt"

experiment_workers <- 8

run_coral_example <- TRUE

years <- 50

# https://sustainablefish.org/roundtable/indonesian-snapper-and-grouper/

# https://www.tandfonline.com/doi/full/10.1080/23308249.2018.1542420?casa_token=oyFyrvo42J4AAAAA%3A3OklvuxmJxB8TpTVomWzRawFsNRUX4WgwnMa_mcTECTLZn-OatxfLBgU2OES-KYynzhXMyfTC9smdQ
#


#hermaphrodism in grouper...


# setup spatial things ----------------------------------------------------

reef_width <- 1

reefs <- data.frame(x = c(2,2,2,2,10,10,10,19), y = c(2,4,6,12,14,15,7,9))


long_reef_habitat <- expand.grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(habitat = 0)

long_spawning_ground <- expand.grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(habitat = dnorm(x, reefs$x[1], reef_width / 2) * dnorm(y, reefs$y[1], reef_width / 2))



for (i in 1:nrow(reefs)){

  long_reef_habitat$habitat <- long_reef_habitat$habitat + dnorm(long_reef_habitat$x, reefs$x[i], reef_width) * dnorm(long_reef_habitat$y, reefs$y[i], reef_width)

}

long_spawning_ground %>%
  ggplot(aes(x,y,fill = habitat)) +
  geom_tile()

long_reef_habitat %>%
  ggplot(aes(x,y,fill = habitat)) +
  geom_tile()

reef_habitat <- long_reef_habitat %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()


deep_reef_habitat <- long_reef_habitat %>%
  mutate(habitat = habitat * 1 / (1 + exp(-(x - resolution / 2)))) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()


shallow_reef_habitat <- long_reef_habitat %>%
  mutate(habitat = habitat * (1 - 1 / (1 + exp(-(x - resolution / 2))))) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()


spawning_ground <- long_spawning_ground %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()


ports <-  data.frame(x =  c(1,1), y = c(2,15))


# setup basline fauna -----------------------------------------------------


snapper <- create_critter(
  scientific_name = "lutjanus malabaricus",
  base_habitat = lapply(1:seasons,function(x) shallow_reef_habitat),
  recruit_habitat = shallow_reef_habitat,
  adult_diffusion = 2, # standard deviation of the number of patches moved by adults
  recruit_diffusion = 10,
  density_dependence = "post_dispersal", # recruitment form, where 1 implies local recruitment
  seasons = seasons,
  resolution = resolution,
  init_explt = 0.125,
  ssb0 = 100000
)

# deep water snapper

deep_snapper <- create_critter(
  scientific_name = "Pristipomoides filamentosus",
  base_habitat = lapply(1:seasons,function(x) deep_reef_habitat),
  recruit_habitat = deep_reef_habitat,
  adult_diffusion = 1, # standard deviation of the number of patches moved by adults
  recruit_diffusion = 5,
  density_dependence = "post_dispersal", # recruitment form, where 1 implies local recruitment
  seasons = seasons,
  resolution = resolution,
  init_explt = 0.5,
  ssb0 = 10000,
  steepness = 0.6
)


# grouper


grouper <- create_critter(
  scientific_name = "Epinephelus fuscoguttatus",
  base_habitat = list(reef_habitat, reef_habitat, reef_habitat, spawning_ground),
  recruit_habitat = spawning_ground,
  fec_expo = 1.5,
  adult_diffusion = 10,
  recruit_diffusion = 40,
  fished_depletion = .25,
  density_dependence = "pre_dispersal",
  seasons = seasons,
  resolution = resolution,
  init_explt = 0.75,
  steepness = 0.6,
  spawning_seasons = c(4),
  ssb0 = 50000,
  taxis_to_diff_ratio = 4)


# shark

reef_shark <- create_critter(
  scientific_name = "Carcharhinus amblyrhynchos",
  base_habitat = list(reef_habitat, reef_habitat, reef_habitat, spawning_ground),
  recruit_habitat = reef_habitat,
  adult_diffusion = 0.1,
  recruit_diffusion = 1,
  fished_depletion = 0.1,
  density_dependence = "local_habitat", # recruitment form, where 1 implies local recruitment
  seasons = seasons,
  fec_form = "pups",
  resolution = resolution,
  init_explt = 0.25,
  pups = 6,
  ssb0 = 1000,
  taxis_to_diff_ratio = 4)

# critters

fauna <-
  list(
    "snapper" = snapper,
    "deep_snapper" = deep_snapper,
    "grouper" = grouper,
    "reef_shark" = reef_shark
  )


fauna$deep_snapper$plot()


fauna$snapper$plot()

fauna$grouper$plot()

fauna$reef_shark$plot()


# create fleet ------------------------------------------------------------


fleet_one = create_fleet(
  list(
    snapper = Metier$new(
      critter = fauna$snapper,
      price = 1,
      sel_form = "dome",
      sel_start = 0.1,
      sel_delta = .2,
      p_explt = 1
    ),
    deep_snapper = Metier$new(
      critter = fauna$deep_snapper,
      price = 0,
      sel_form = "logistic",
      sel_start = 2,
      sel_delta = .2,
      p_explt = 1
    ),
    grouper = Metier$new(
      critter = fauna$grouper,
      price = 2,
      sel_form = "dome",
      sel_start = 0.1,
      sel_delta = .2,
      p_explt = 1
    ),
    reef_shark = Metier$new(
      critter = fauna$reef_shark,
      price = 0.5,
      sel_form = "logistic",
      sel_start = 0.25,
      sel_delta = .2,
      p_explt = 1
    )
  ),
  ports = ports[1,],
  cost_per_unit_effort = 1,
  cost_per_distance = 2,
  responsiveness = 0.4,
  cr_ratio = 1,
  resolution = resolution,
  mpa_response = "stay",
  fleet_model = "open access",
  spatial_allocation = "ppue"
)

fleet_two <- create_fleet(
  list(
    snapper = Metier$new(
      critter = fauna$snapper,
      price = 3,
      sel_form = "logistic",
      sel_start = 0.1,
      sel_delta = .2,
      p_explt = 2
    ),
    deep_snapper = Metier$new(
      critter = fauna$deep_snapper,
      price = 6,
      sel_form = "logistic",
      sel_start = 0.25,
      sel_delta = .2,
      p_explt = 1
    ),
    grouper = Metier$new(
      critter = fauna$grouper,
      price = 1,
      sel_form = "dome",
      sel_start = 0.1,
      sel_delta = .2,
      p_explt = .25
    ),
    reef_shark = Metier$new(
      critter = fauna$reef_shark,
      price = 0,
      sel_form = "logistic",
      sel_start = 0.25,
      sel_delta = .2,
      p_explt = 0
    )
  ),
  ports = ports[2,],
  cost_per_unit_effort = 1,
  cost_per_distance = 2,
  responsiveness = 0.4,
  cr_ratio = 1,
  resolution = resolution,
  mpa_response = "stay",
  fleet_model = "constant effort",
  spatial_allocation = "ppue"
)


fleets <- list(fleet_one = fleet_one,
               fleet_two = fleet_two)

fleets$fleet_one$metiers$grouper$sel_at_age %>% plot()

fleets$fleet_one$metiers$reef_shark$sel_at_age %>% plot()


fleets <- tune_fleets(fauna, fleets, tune_type = tune_type, tune_costs = TRUE) # tunes the catchability by fleet to achieve target depletion

fleets$fleet_one$cost_per_unit_effort


fleets$fleet_one$base_effort <- resolution

# run simulation ----------------------------------------------------------

mpa_locations <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(mpa = FALSE)


reef_sim <- simmar(
  fauna = fauna,
  fleets = fleets,
  manager = list(mpas = list(
    locations = mpa_locations,
    mpa_year = 30
  )),
  years = years
)


patch_effort <- tidyr::expand_grid(x = 1:resolution, y = 1:resolution) %>%
  dplyr::mutate(effort = reef_sim[[length(reef_sim)]]$grouper$e_p_fl$fleet_one)

patch_effort %>%
  ggplot() +
  geom_tile(aes(x,y, fill = effort)) +
  geom_point(data = ports, aes(x = x, y = y), color = "red", size = 4)



effort <-
  map_df(reef_sim, ~ data.frame(effort = sum(.x$grouper$e_p_fl$fleet_one)), .id = "step") %>%
  mutate(step = as.numeric(step))

effort %>%
  ungroup() %>%
  ggplot(aes(step, effort)) +
  geom_line()


profits <-
  map_df(reef_sim, ~ map_df(.x, ~tibble(profit = colSums(.x$prof_p_fl)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step))

profits %>%
  ungroup() %>%
  ggplot(aes(step, profit, color = critter)) +
  geom_line()

  old_profits <- profits %>%
  group_by(step) %>%
  summarise(profit = sum(profit))

  old_profits %>%
  ungroup() %>%
  ggplot(aes(step, profit)) +
  geom_line()

grid <- expand_grid(x = 1:resolution, y= 1:resolution) %>%
  mutate(patch = 1:nrow(.))

patch_biomass <-
  map_df(reef_sim, ~ map_df(.x, ~tibble(biomass = rowSums(.x$ssb_p_a), patch = 1:nrow(.x$ssb_p_a)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step)) %>%
  left_join(grid, by = "patch")

patch_biomass %>%
  group_by(critter, step) %>%
  mutate(sbiomass = biomass / sum(biomass)) %>%
  ungroup() %>%
  filter(critter == "grouper", step < 10) %>%
  ggplot(aes(x,y, fill = sbiomass))+
  geom_tile() +
  facet_wrap(~step) +
  scale_fill_viridis_c()

patch_biomass <-
  map_df(reef_sim, ~ map_df(.x, ~tibble(biomass = rowSums(.x$ssb_p_a), patch = 1:nrow(.x$ssb_p_a)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step)) %>%
  left_join(grid, by = "patch")

patch_biomass %>%
  group_by(critter, step) %>%
  mutate(sbiomass = biomass / sum(biomass)) %>%
  ungroup() %>%
  filter(critter == "grouper", step < 10) %>%
  ggplot(aes(x,y, fill = biomass))+
  geom_tile() +
  facet_wrap(~step) +
  scale_fill_viridis_c(limits = c(0,NA))


patch_recruits <-
  map_df(reef_sim, ~ map_df(.x, ~tibble(recruits = .x$n_p_a[,1], patch = 1:nrow(.x$ssb_p_a)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step)) %>%
  left_join(grid, by = "patch")

patch_recruits %>%
  group_by(critter, step) %>%
  ungroup() %>%
  filter(critter == "grouper", step < 10) %>%
  ggplot(aes(x,y, fill = recruits))+
  geom_tile() +
  facet_wrap(~step) +
  scale_fill_viridis_c(limits = c(0,NA))

biomass <-
  map_df(reef_sim, ~ map_df(.x, ~tibble(biomass = sum(.x$ssb_p_a)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step))


biomass %>%
  ungroup() %>%
  ggplot(aes(step, biomass, color = critter)) +
  geom_line() +
  facet_wrap(~critter, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA))


processed_reef_sim <- process_marlin(reef_sim)


plot_marlin(processed_reef_sim, max_scale = FALSE, plot_var = "ssb")

plot_marlin(processed_reef_sim, max_scale = FALSE, plot_var = "c")

# spawning_agg <- processed_reef_sim$fauna %>%
#   filter(critter == "grouper") %>%
#   group_by(critter) %>%
#   filter(age == max(age)) %>%
#   group_by(critter, step) %>%
#   mutate(n = n / max(n)) %>%
#   ungroup() %>%
#   ggplot(aes(x,y,fill = n)) +
#   geom_tile() +
#   transition_time(step) +
#   ease_aes('linear') +
#   scale_fill_viridis_c(name = "Tunas") +
#   scale_x_continuous(name = "longitude") +
#   scale_y_continuous(name = "latitude") +
#   labs(title = 'Step: {frame_time}') +
#   facet_wrap(~critter)
#
#
# animate(spawning_agg, nframes = 100, fps=.5)



plot_marlin(processed_reef_sim, max_scale = TRUE, plot_var = "ssb", plot_type = "space")


starting_conditions <-
  reef_sim[(length(reef_sim) - seasons + 1) : length(reef_sim)]

proc_starting_conditions <-
  process_marlin(starting_conditions, keep_age = FALSE)

starting_step = as.numeric(last(names(starting_conditions)))

mpa_sim <- simmar(
  fauna = fauna,
  fleets = fleets,
  years = 20,
  starting_step = starting_step,
  keep_starting_step = FALSE,
  initial_conditions = starting_conditions[[length(starting_conditions)]]
)


    profits <-
      map_df(mpa_sim, ~ map_df(.x, ~tibble(profit = colSums(.x$prof_p_fl)), .id = "critter"), .id = "step") %>%
      mutate(step = as.numeric(step))

    profits %>%
      ungroup() %>%
      ggplot(aes(step, profit, color = critter)) +
      geom_line()

    new_profits <- profits %>%
      group_by(step) %>%
      summarise(profit = sum(profit)) %>%
      ungroup()

    old_profits %>%
      bind_rows(new_profits) %>%
      ggplot(aes(step, profit)) +
      geom_line()

# running mpa experiments -------------------------------------------------

if (run_coral_example == TRUE){

  plan(multisession, workers = experiment_workers)

  case_study_experiments <-
    expand_grid(
      placement_strategy = c("rate", "avoid_fishing", "target_fishing", "area"),
      prop_mpa = seq(0, 1, by = 0.05),
      critters_considered = length(fauna),
      placement_error = c(0),
      mpa_response = c("stay"),
      iter = 1
    )

  a <- Sys.time()
  coral_mpa_experiements <- case_study_experiments %>%
    ungroup() %>%
    mutate(
      results = future_pmap(
        list(
          placement_strategy = placement_strategy,
          prop_mpa = prop_mpa,
          critters_considered = critters_considered,
          placement_error = placement_error,
          mpa_response = mpa_response
        ),
        run_mpa_experiment,
        starting_conditions = starting_conditions,
        proc_starting_conditions = proc_starting_conditions,
        resolution = resolution,
        fleet_model = NA,
        fauna = fauna,
        fleets = fleets,
        years = 20,
        .options = furrr_options(seed = 42),
        .progress = TRUE
      )
    ) %>%
    mutate(prop_ssb0_mpa = map_dbl(results, ~sum(.x$mpa$ssb0[.x$mpa$mpa == TRUE], na.rm = TRUE) / sum(.x$mpa$ssb0)))

  Sys.time() - a

  future::plan(future::sequential)

  write_rds(coral_mpa_experiements, file = file.path(results_path, "coral_mpa_experiements.rds"))

} else {

  coral_mpa_experiements <- read_rds(file = file.path(results_path, "coral_mpa_experiements.rds"))


}

coral_mpa_experiements$mpas <- map(coral_mpa_experiements$results, "mpa")

coral_mpa_experiements$obj <- map(coral_mpa_experiements$results, "obj")


examine_mpas <- coral_mpa_experiements %>%
  unnest(cols = mpas)

mpas <- examine_mpas %>%
  filter(mpa_response == "stay") %>%
  ggplot(aes(x,y,fill = mpa)) +
  geom_tile() +
  transition_time(prop_mpa) +
  scale_x_continuous(name = "longitude") +
  scale_y_continuous(name = "latitude") +
  labs(title = 'Step: {frame_time}') +
  facet_wrap(~placement_strategy)


examine_results <- coral_mpa_experiements %>%
  unnest(cols = obj)

examine_results %>%
  mutate(mpa_bin = cut(prop_mpa,4)) %>%
  ggplot(aes(biodiv, yield, color = placement_strategy)) +
  geom_point() +
  facet_grid(critter~mpa_bin, scales = "free")

examine_results %>%
  filter(between(prop_mpa, 0.29, 0.31)) %>%
  ggplot(aes(biodiv, yield, color = placement_strategy)) +
  geom_point() +
  facet_wrap(~critter, scales = "free")

examine_results %>%
  ggplot(aes(prop_mpa, biodiv, color = placement_strategy)) +
  geom_line() +
  facet_wrap(~critter) +
  scale_y_continuous(limits = c(0, 1.5))


# REALLY interesting. the rate strategy looks for places with the highest rates of the most depleted species, sharks and closes those first. But, those are in the far east, since that's the only place sharks are left when the simulation starts. So, you protect the current home of the sharks, while fishing harder in the historic home, driving down the other species that live closer to shore


examine_results %>%
  ggplot(aes(prop_ssb0_mpa, biodiv, color = placement_strategy)) +
  geom_line() +
  facet_wrap(~critter) +
  scale_y_continuous(limits = c(0, 1.5))


examine_results %>%
  group_by(prop_mpa, placement_strategy) %>%
  summarise(yield = sum(yield)) %>%
  ggplot(aes(prop_mpa, yield, color = placement_strategy)) +
  geom_line()

examine_results %>%
  ggplot(aes(prop_ssb0_mpa,  yield, color = placement_strategy)) +
  geom_line() +
  facet_wrap(~critter, scales = "free_y")


examine_results %>%
  pivot_longer(starts_with("fleet_"), names_to = "fleet", values_to = "fleet_yield") %>%
  ggplot(aes(prop_ssb0_mpa, fleet_yield, color = placement_strategy)) +
  geom_line() +
  facet_grid(critter ~ fleet, scales = "free")


examine_results %>%
  pivot_longer(starts_with("fleet_"), names_to = "fleet", values_to = "fleet_yield") %>%
  group_by(prop_mpa, fleet, placement_strategy) %>%
  summarise(yield = sum(fleet_yield)) %>%
  ggplot(aes(prop_mpa, yield, color = placement_strategy)) +
  geom_line() +
  facet_wrap(~ fleet, scales = "free")



# looking at fleet one, you could get up to a biodiv of near 2 while increasing yield, while fleet one will take a hit or lose severely to get to that level
examine_results %>%
  pivot_longer(starts_with("fleet_"), names_to = "fleet", values_to = "fleet_yield") %>%
  group_by(prop_mpa, fleet, placement_strategy) %>%
  summarise(yield = sum(fleet_yield),biodiv = sum(unique(biodiv))) %>%
  ggplot(aes(biodiv, yield, color = placement_strategy)) +
  geom_point() +
  facet_wrap(~ fleet, scales = "free")




examine_results %>%
  group_by(prop_mpa, placement_strategy) %>%
  summarise(profits = sum(econ)) %>%
  ggplot(aes(prop_mpa, profits, color = placement_strategy)) +
  geom_line()


