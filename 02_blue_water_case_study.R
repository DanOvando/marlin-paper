source(file.path("00_setup.R"))

resolution <- 12 # number of patches per side of simulation grid

patches <- resolution ^ 2

# target simulation area: 14,000,000 km2 = 3741 side length

patch_area <- 14e6 / patches # km2

patch_side <- sqrt(patch_area) # km

simulation_area <- patch_area * patches #km2

message(glue::glue("Blue water simulation area is {scales::comma(patch_area)} km2, spread over {patches} patches each with an area of {scales::comma(patch_area)} km2 or {scales::comma(patch_side)} km per side"))

seasons <- 4

time_step <- 1 / seasons # in years

tune_type <- "explt"

experiment_workers <- 8

experiment_years <- 20

adult_diffusion <- 0.8 * simulation_area  # km2, which means that each year a fish can roam an area 4km

max_hab_mult = 20

# load bycatch risk layers ------------------------------------------------


mats <- list.files(path = here("data", "species-matrices"))

mats <- mats[str_detect(mats, "cpue_mean")]

file_comps <- str_split(mats, "_", simplify = TRUE)

species <- str_replace(file_comps[, 1], "-", " ")

# go through and figure out the best matrix for each species in a really hacky manner

best_mat <- rep(NA, n_distinct(species))
for (s in seq_along(unique(species))){

  fls <- mats[species == unique(species)[s]]

  if (all(str_detect(fls,"ps_cpue"))){
    # if the only thing available is purse seine CPUE

    fls <- fls[str_detect(fls,"ps_cpue")]


  } else {

    fls <- fls[!str_detect(fls,"ps_cpue")]

  }

  if (any(str_detect(fls,"mtper"))){

    fls <- fls[str_detect(fls,"mtper")]


  } else {

    fls <- fls[str_detect(fls,"countper")]
  }


  # preference: longline weight cpue

  # then longline number cpue

  # then purse seine


  best_mat[s] <- fls

}

get_layer <- function(file) {

  file_comps <- str_split(file, "_", simplify = TRUE)

  species <- str_replace(file_comps[, 1], "-", " ")


  hab <-
    read_csv(here("data", "species-matrices", file),
             col_names = FALSE,
             skip = 1) %>%
    rename(lat = X1) %>%
    mutate(lat = 1:nrow(.)) %>%
    pivot_longer(
      -lat,
      names_to = "lon",
      values_to = "hab",
      names_prefix = "X"
    ) %>%
    mutate(hab = tidyr::replace_na(hab, 0),
           lon = as.numeric(lon) - 1)



  x_binsize <- (max(hab$lon) - min(hab$lon) + 1) / resolution

  y_binsize <- (max(hab$lat) - min(hab$lat) + 1) / resolution

  if (x_binsize < 1 | y_binsize < 1) {
    stop("resolution is too high for input data")
  }


  hab <- hab %>%
    mutate(X = lon - min(lon),
           Y = lat - min(lat)) %>%
    mutate(rough_x = floor(X / x_binsize),
           rough_y = floor(Y / y_binsize)) %>%
    select(contains("rough_"), hab)


  hab <- hab %>%
    group_by(rough_x, rough_y) %>%
    summarise(hab = sum(hab)) %>%
    mutate(xy = rough_x * rough_y)

  mod <-
    gamm4::gamm4(hab ~ s(rough_x) + s(rough_y) + s(xy), data = hab)

  hab$interp_cpue <- as.numeric(predict(mod$gam))

  hab$habitat <- scales::rescale(hab$interp_cpue, c(0,log(20)))

  # hab$habitat <- pmax(0, hab$interp_cpue / max(hab$interp_cpue) * adult_diffusion)


  if (sqrt(nrow(hab)) != resolution) {
    stop("habitat layer does not match simulation resolution")
  }
  hab <- hab %>%
    select(contains("rough"), habitat) %>%
    rename(x = rough_x, y = rough_y) %>%
    ungroup()

  out <- tibble(scientific_name = species, habitat = list(hab))

}


mats <- map(best_mat, get_layer)

check_habitat <- mats %>%
  bind_rows() %>%
  unnest(cols = habitat) %>%
  ggplot(aes(x, y, fill = habitat)) +
  geom_tile() +
  facet_wrap( ~ scientific_name) +
  scale_fill_viridis_c()


mats <- mats %>%
  bind_rows()



# create experiments ------------------------------------------------------

casestudy <- tibble(scientific_name = unique(marlin_inputs$scientific_name)) %>%
  left_join(mats, by = "scientific_name") %>%
  filter(!map_lgl(.$habitat, is.null))

case_study_species = unique(casestudy$scientific_name)

casestudy <- casestudy %>%
  mutate(critter = pmap(
    list(
      sciname = scientific_name,
      habitat = habitat,
      ontogenetic_shift = FALSE,
      seasonal_movement = FALSE,
      random_rec = FALSE
    ),
    create_critters,
    marlin_inputs = marlin_inputs,
    seasons = seasons,
    adult_diffusion = adult_diffusion,
    resolution = resolution,
    max_hab_mult = max_hab_mult,
    patch_area = patch_area)) %>%
  mutate(xid = 1)

casestudy <- casestudy %>%
  group_by(xid) %>%
  nest() %>%
  mutate(fauna = map(data, ~ .x$critter %>% set_names(.x$scientific_name)))

check_pop_sizes <- map_dbl(casestudy$fauna[[1]], "ssb0")

# tibble(sciname = names(check_pop_sizes), ssb0 = check_pop_sizes) %>%
#   ggplot(aes(reorder(sciname, ssb0), ssb0)) +
#   geom_col() +
#   coord_flip()



# tune fleet --------------------------------------------------------------



casestudy <- casestudy %>%
  ungroup() %>%
  mutate(fleet = map(fauna, compile_fleet))



# starting conditions -----------------------------------------------------


fauna <- casestudy$fauna[[1]]

fleets <- casestudy$fleet[[1]]

starting_trajectory <- simmar(fauna = fauna, fleets = fleets, years = 100)



ssb <-
  map_df(starting_trajectory, ~ map_df(.x, ~tibble(ssb = rowSums(.x$ssb_p_a), patch = 1:nrow(.x$ssb_p_a)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step))


ssb %>%
  group_by(critter, step) %>%
  summarise(ssb = sum(ssb)) %>%
  group_by(critter) %>%
  mutate(depletion = ssb / ssb[step == 0]) %>%
  ggplot(aes(step, ssb)) +
  geom_line()+
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~critter, scales = "free_y")



# starting <- starting_trajectory[[length(starting_trajectory)]]
#
# starting_trajectory <- simmar(fauna = fauna,
#                               fleets = fleets,
#                               year = years,
#                               initial_conditions = starting)




starting_conditions <-
  starting_trajectory[(length(starting_trajectory) - seasons + 1) : length(starting_trajectory)]


proc_starting_conditions <- process_marlin(starting_conditions)

baseline_effort <- as.data.frame(t(colSums(starting_conditions[[1]][[1]]$e_p_fl)))

write_rds(
  list(fauna = fauna, fleets = fleets),
  file = file.path(results_path, "blue_water_fauna_and_fleets.rds")
)

if (run_blue_water_example == TRUE){

  future::plan(future::multisession, workers = experiment_workers)


  # create range shifts


  habitat_storage <- vector(mode = "list", length = experiment_years)


  future_habitat <- purrr::map(set_names(names(fauna)), ~ habitat_storage)



  for (s in seq_along(future_habitat)) {
    for (year in 1:experiment_years) {
      tmp_hab <-
        mats$habitat[[which(mats$scientific_name == names(future_habitat[s]))]] %>%
        mutate(xy = x * y) # baseline habitat for the critter in question

      # range shift the preferred habitat, i.e. keep the shape of the habitat the same, but move it northward
      future_hab <- tmp_hab %>%
        mutate(y = y + .2 * year) %>%
        mutate(xy = x * y)

      og_habitat <- tmp_hab$habitat %>% sum()

      mod <-
        gamm4::gamm4(habitat ~ s(x) + s(y) + s(xy), data = future_hab) # create the new habitat model, that reflects a preference for the more northward habitat

      tmp_hab$habitat <- as.numeric(predict(mod$gam, newdata = tmp_hab)) # apply the new habitat preference to the current domain

      tmp_hab$habitat <- pmax(0, tmp_hab$habitat)

      tmp_hab$habitat <- (tmp_hab$habitat / sum(tmp_hab$habitat)) * og_habitat
      new_matrix <- tmp_hab %>%
        select(-xy) %>%
        pivot_wider(names_from = y, values_from = habitat) %>%
        select(-x) %>%
        as.matrix()

      image(new_matrix)

      future_habitat[[s]][[year]] <- new_matrix

    }

  } # loop over future habitat

  write_rds(future_habitat, file.path(results_path,"future_habitat.rds"))

  image(future_habitat$`thunnus obesus`[[experiment_years]])

  image(future_habitat$`carcharhinus longimanus`[[experiment_years]])

  sim_climate <- simmar(fauna = fauna,
                        fleets = fleets,
                        habitat = future_habitat,
                        years = experiment_years,
                        initial_conditions = starting_conditions[[1]],
                        manager = list(effort_cap = baseline_effort))

  grid <- expand_grid(x = 1:resolution, y= 1:resolution) %>%
    mutate(patch = 1:nrow(.))

  patch_biomass <-
    map_df(sim_climate, ~ map_df(.x, ~tibble(biomass = rowSums(.x$ssb_p_a), patch = 1:nrow(.x$ssb_p_a)), .id = "critter"), .id = "step") %>%
    mutate(step = as.numeric(step)) %>%
    left_join(grid, by = "patch")

  patch_biomass %>%
    group_by(step, critter) %>%
    summarise(b = sum(biomass)) %>%
    ggplot(aes(step, b)) +
    geom_line() +
    facet_wrap(~critter, scales = "free_y")


  patch_effort <-   map_df(sim_climate, ~ map_df(.x, ~.x$e_p_fl %>% mutate(patch = 1:nrow(.)), .id = "critter"), .id = "step") %>%
    mutate(step = as.numeric(step)) %>%
    filter(critter ==  names(fauna)[1]) %>%
    pivot_longer(names(fleets), names_to = "fleet", values_to = "effort")

  patch_effort %>%
    group_by(step, fleet) %>%
    summarise(effort = sum(effort)) %>%
    ggplot(aes(step,effort))+
    geom_line() +
    facet_wrap(~fleet)


  patch_biomass %>%
    group_by(step, critter) %>%
    summarise(b = sum(biomass)) %>%
    ggplot(aes(step, b)) +
    geom_line() +
    facet_wrap(~critter, scales = "free_y")

  patch_biomass %>%
    # filter(critter %in% c("xiphias gladius","thunnus obesus","carcharhinus longimanus","thunnus albacares")) %>%
    group_by(critter, step) %>%
    mutate(sbiomass = biomass / sum(biomass)) %>%
    ungroup() %>%
    filter(step == min(step) | step == max(step)) %>%
    ggplot(aes(x,y, fill = sbiomass))+
    geom_tile() +
    facet_grid(critter~step) +
    scale_fill_viridis_c()


  case_study_experiments <-
    expand_grid(
      # placement_strategy = c("target_fishing"),
      placement_strategy = c("rate", "target_fishing"),
      fleet_model = c("open access"),
      prop_mpa = seq(0, 1, by = 0.05),
      critters_considered = length(fauna),
      placement_error = c(0),
      mpa_response = c("stay"),
      iter = 1
    )

  a <- Sys.time()
  blue_water_climate_experiments <- case_study_experiments %>%
    ungroup() %>%
    mutate(
      results = future_pmap(
        list(
          placement_strategy = placement_strategy,
          prop_mpa = prop_mpa,
          critters_considered = critters_considered,
          placement_error = placement_error,
          mpa_response = mpa_response,
          fleet_model = fleet_model
        ),
        run_mpa_experiment,
        starting_conditions = starting_conditions,
        proc_starting_conditions = proc_starting_conditions,
        resolution = resolution,
        fauna = fauna,
        fleets = fleets,
        future_habitat = future_habitat,
        years = experiment_years,
        effort_cap = baseline_effort,
        .options = furrr_options(seed = 42),
        .progress = TRUE
      )
    ) %>%
    mutate(prop_ssb0_mpa = map_dbl(results, ~sum(.x$mpa$ssb0[.x$mpa$mpa == TRUE], na.rm = TRUE) / sum(.x$mpa$ssb0)))

  Sys.time() - a


  write_rds(blue_water_climate_experiments, file = file.path(results_path, "blue_water_climate_experiments.rds"))


  a <- Sys.time()
  blue_water_experiments <- case_study_experiments %>%
    ungroup() %>%
    mutate(
      results = future_pmap(
        list(
          placement_strategy = placement_strategy,
          prop_mpa = prop_mpa,
          critters_considered = critters_considered,
          placement_error = placement_error,
          mpa_response = mpa_response,
          fleet_model = fleet_model
        ),
        run_mpa_experiment,
        starting_conditions = starting_conditions,
        proc_starting_conditions = proc_starting_conditions,
        resolution = resolution,
        fauna = fauna,
        fleets = fleets,
        years = experiment_years,
        effort_cap = baseline_effort,
        .options = furrr_options(seed = 42),
        .progress = TRUE
      )
    ) %>%
    mutate(prop_ssb0_mpa = map_dbl(results, ~sum(.x$mpa$ssb0[.x$mpa$mpa == TRUE], na.rm = TRUE) / sum(.x$mpa$ssb0)))

  Sys.time() - a

  future::plan(future::sequential)

  write_rds(blue_water_experiments, file = file.path(results_path, "blue_water_experiments.rds"))

} else {

  blue_water_experiments <- read_rds(file = file.path(results_path, "blue_water_experiments.rds"))

  blue_water_climate_experiments <- read_rds(file = file.path(results_path, "blue_water_climate_experiments.rds"))

} # close MPA experiments

blue_water_experiments$mpas <- map(blue_water_experiments$results, "mpa")

blue_water_experiments$obj <- map(blue_water_experiments$results, "obj")

blue_water_climate_experiments$mpas <- map(blue_water_climate_experiments$results, "mpa")

blue_water_climate_experiments$obj <- map(blue_water_climate_experiments$results, "obj")

blue_fleets <- fleets


examine_mpas <- blue_water_experiments %>%
  unnest(cols = mpas)

# mpas <- examine_mpas %>%
#   filter(mpa_response == "stay") %>%
#   ggplot(aes(x,y,fill = mpa)) +
#   geom_tile() +
#   transition_time(prop_mpa) +
#   scale_x_continuous(name = "longitude") +
#   scale_y_continuous(name = "latitude") +
#   labs(title = 'Step: {frame_time}') +
#   facet_wrap(~placement_strategy)


static_results <- blue_water_experiments %>%
  unnest(cols = obj) %>%
  mutate(climate = FALSE)

climate_results <- blue_water_climate_experiments %>%
  unnest(cols = obj) %>%
  mutate(climate = TRUE)

blue_water_results <-static_results %>%
  bind_rows(climate_results) %>%
  left_join(sci_to_com, by = "critter") %>%
  select(-critter) %>%
  rename(critter = common_name)


fleet_climate_results <- blue_water_results %>%
  select(placement_strategy,
         fleet_model,
         prop_mpa,
         climate,
         longline,
         critter,
         purseseine) %>%
  pivot_longer(c(longline,purseseine), names_to = "fleet", values_to = "yield") %>%
  pivot_wider(names_from = "climate", values_from =  "yield") %>%
  mutate(delta = `TRUE` - `FALSE`) %>%
  group_by(placement_strategy, prop_mpa, fleet) %>%
  summarise(delta = sum(delta))

fleet_climate_impacts <- fleet_climate_results %>%
  ggplot(aes(prop_mpa, delta, color = fleet)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line() +
  facet_wrap(~placement_strategy) +
  scale_y_continuous(name = "Difference in Yield") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())


critter_climate_results <- blue_water_results %>%
  select(placement_strategy,
         fleet_model,
         prop_mpa,
         climate,
         critter,
         biodiv) %>%
  pivot_wider(names_from = "climate", values_from =  "biodiv") %>%
  mutate(delta = `TRUE` - `FALSE`)


critter_climate_impacts <- critter_climate_results %>%
  ggplot(aes(prop_mpa, delta, color = critter)) +
  geom_line() +
  facet_wrap(~placement_strategy) +
  scale_x_continuous(name = "MPA",labels = scales::label_percent()) +
  scale_y_continuous(name = "Difference in SSB/SSB0")

fleet_climate_impacts / critter_climate_impacts

linesize <- 0.5

# blue_water_results <- blue_water_results %>%
#   rename(`purse seine` = purseseine)

blue_fleet_frontier <- blue_water_results %>%
  pivot_longer(names(blue_fleets), names_to = "fleet", values_to = "fleet_yield") %>%
  group_by(prop_mpa, fleet, placement_strategy, climate) %>%
  summarise(yield = sum(fleet_yield),biodiv = sum(unique(biodiv))) %>%
  ggplot(aes(biodiv, yield, color = placement_strategy, linetype = climate)) +
  geom_line() +
  scale_x_continuous(name = "Change in Total SSB/SSB0",limits = c(0, NA)) +
  scale_y_continuous(name = "Total Yield",limits = c(0, NA)) +
  facet_wrap(~ fleet, scales = "free_y")

blue_frontier <- blue_water_results %>%
  filter(climate == FALSE) %>%
  group_by(prop_mpa, placement_strategy, climate) %>%
  summarise(biodiv = sum(biodiv), yield = sum(yield)) %>%
  group_by(placement_strategy) %>%
  mutate(delta_yield = yield / yield[prop_mpa == 0] - 1,
         delta_biodiv = biodiv / biodiv[prop_mpa == 0] - 1) %>%
  ggplot(aes(delta_biodiv, delta_yield, color = placement_strategy, linetype = climate)) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_line(size = linesize) +
  scale_x_continuous(name = "Change in Conservation", labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(name = "Change in Yield", labels = scales::label_percent(accuracy = 1)) +
  theme(legend.position = "top")


blue_fleet_yield <- blue_water_results %>%
  filter(placement_strategy %in% c("target_fishing", "rate")) %>%
  pivot_longer(names(blue_fleets),
               names_to = "fleet",
               values_to = "fleet_yield") %>%
  group_by(prop_mpa, fleet, placement_strategy, climate) %>%
  summarise(yield = sum(fleet_yield), biodiv = sum(unique(biodiv))) %>%
  group_by(fleet, placement_strategy) %>%
  mutate(delta_yield = yield / yield[prop_mpa == 0] - 1) %>%
  ggplot(aes(prop_mpa, delta_yield, color = climate)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_line(size = linesize, show.legend = FALSE) +
  facet_grid(fleet ~ placement_strategy, scales = "free_y", labeller = labeller(fleet = titler, placement_strategy = titler)) +
  scale_x_continuous(name = "MPA Size",
                     labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(name = "Change in Yield",
                     labels = scales::label_percent(accuracy = 1)) +
  scale_color_manual(
    values = c("#63B8FF", "#FF4500"),
    labels = c("Status Quo", "Range Shift"),
    name = ''
  ) +
  theme(strip.text = element_text(size = 8)) +
  labs(tag = "A")



blue_critter_bio <- blue_water_results %>%
  filter(placement_strategy %in% c("target_fishing", "rate")) %>%
  group_by(prop_mpa, critter, placement_strategy, climate) %>%
  summarise(biodiv = sum(unique(biodiv))) %>%
  group_by(critter, placement_strategy) %>%
  mutate(delta_bio = biodiv) %>%
  ungroup() %>%
  mutate(placement_strategy = fct_relabel(as.factor(placement_strategy), titler)) %>%
  ggplot(aes(prop_mpa, pmin(1,delta_bio), color = climate, linetype = placement_strategy)) +
  geom_line(size = linesize) +
  facet_wrap(~critter) +
  scale_color_manual(values = c("#63B8FF","#FF4500"), labels = c("Status Quo","Range Shift"), name = '') +
  scale_x_continuous(name = "MPA Size",
                     labels = scales::label_percent(accuracy = 1), guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "SSB/SSB0",limits = c(0, NA)) +
  scale_linetype(name = "MPA Strategy") +
  labs(tag = "B")+
  theme(strip.text = element_text(size = 7),
        axis.text.y = element_text(size = 6))


# blue_frontier / blue_critter_bio + plot_layout(heights = c(1,2))
(blue_fleet_yield /
    blue_critter_bio) + plot_layout(heights = c(1.5,2))
