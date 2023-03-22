source(file.path("00_setup.R"))

set.seed(42)

resolution <- 20

patches <- resolution^2

seasons <- 4

tune_type <- "explt"

experiment_workers <- 1

years <- 50


# setup spatial things ----------------------------------------------------

reef_width <- 2.5

reefs <- data.frame(x = c(2,2,2,2,10,10,10,19), y = c(2,4,6,12,14,15,7,9))


snapper_diffusion <- 0.25 * patches

deep_snapper_diffusion <- .2 * patches

grouper_diffusion <- patches

shark_diffusion <- patches

max_hab_mult = 1.001

long_reef_habitat <- expand.grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(habitat = 0)

long_spawning_ground <- expand.grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(habitat = dnorm(x, reefs$x[1], reef_width / 2) * dnorm(y, reefs$y[1], reef_width / 2))


long_spawning_ground$habitat <- scales::rescale(long_spawning_ground$habitat, c(0, log(3)))

for (i in 1:nrow(reefs)){

  long_reef_habitat$habitat <- long_reef_habitat$habitat + dnorm(long_reef_habitat$x, reefs$x[i], reef_width) * dnorm(long_reef_habitat$y, reefs$y[i], reef_width)

}

long_reef_habitat$habitat <- scales::rescale(long_reef_habitat$habitat, c(0, log(3)))


long_spawning_ground %>%
  ggplot(aes(x,y,fill = habitat)) +
  geom_tile()

long_reef_habitat %>%
  ggplot(aes(x,y,fill = habitat)) +
  geom_tile() + 
  scale_fill_viridis()

reef_habitat <- long_reef_habitat %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()


deep_reef_habitat <- long_reef_habitat %>%
  mutate(habitat = habitat * 1 / (1 + exp(-(x - resolution / 1.5))),
         habitat = scales::rescale(habitat, c(0, log(3)))) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()


image(deep_reef_habitat)


image(deep_reef_habitat)

shallow_reef_habitat <- long_reef_habitat %>%
  mutate(habitat = habitat * (1 - 1 / (1 + exp(-(x - resolution / 2)))),
         habitat = scales::rescale(habitat, c(0, log(3)))) %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()


spawning_ground <- long_spawning_ground %>%
  pivot_wider(names_from = y, values_from = habitat) %>%
  select(-x) %>%
  as.matrix()

check <- outer((long_reef_habitat$habitat), (long_reef_habitat$habitat), '-')


ports <-  data.frame(x =  c(1,1), y = c(2,15), fleet =c( 1, 2))
write_rds(ports, file.path(results_path,"coral_ports.rds"))

# setup basline fauna -----------------------------------------------------


write_rds(list(shallow_reef_habitat = shallow_reef_habitat, deep_reef_habitat = deep_reef_habitat, spawning_ground = spawning_ground), file.path(results_path,"coral_habitat.rds"))

snapper <- create_critter(
  scientific_name = "lutjanus malabaricus",
  habitat = lapply(1:seasons,function(x) shallow_reef_habitat),
  recruit_habitat = shallow_reef_habitat,
  adult_diffusion = snapper_diffusion, # standard deviation of the number of patches moved by adults
  recruit_diffusion = patches,
  density_dependence = "post_dispersal", # recruitment form, where 1 implies local recruitment
  seasons = seasons,
  resolution = resolution,
  init_explt = 0.1,
  ssb0 = 100000,
  max_hab_mult = max_hab_mult
)

# deep water snapper

deep_snapper <- create_critter(
  scientific_name = "Pristipomoides filamentosus",
  habitat = lapply(1:seasons,function(x) deep_reef_habitat),
  recruit_habitat = deep_reef_habitat,
  adult_diffusion = deep_snapper_diffusion, # standard deviation of the number of patches moved by adults
  recruit_diffusion = patches,
  density_dependence = "post_dispersal", # recruitment form, where 1 implies local recruitment
  seasons = seasons,
  resolution = resolution,
  init_explt = 0.15,
  ssb0 = 10000,
  steepness = 0.6,
  max_hab_mult = max_hab_mult
)


# grouper


grouper <- create_critter(
  scientific_name = "Epinephelus fuscoguttatus",
  habitat = list(reef_habitat, reef_habitat, reef_habitat, spawning_ground),
  recruit_habitat = spawning_ground,
  fec_expo = 1.5,
  adult_diffusion = grouper_diffusion,
  recruit_diffusion = patches,
  fished_depletion = .25,
  density_dependence = "pre_dispersal",
  seasons = seasons,
  resolution = resolution,
  init_explt = 0.4,
  steepness = 0.6,
  spawning_seasons = c(4),
  max_hab_mult = max_hab_mult,
  ssb0 = 50000)


# shark

reef_shark <- create_critter(
  scientific_name = "Carcharhinus amblyrhynchos",
  habitat = list(reef_habitat, reef_habitat, reef_habitat, spawning_ground),
  recruit_habitat = reef_habitat,
  adult_diffusion = shark_diffusion,
  recruit_diffusion = 1,
  fished_depletion = 0.1,
  density_dependence = "local_habitat", # recruitment form, where 1 implies local recruitment
  seasons = seasons,
  fec_form = "pups",
  resolution = resolution,
  init_explt = 0.4,
  pups = 6,
  max_hab_mult = max_hab_mult,
  ssb0 = 1000)

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
      sel_start = .5,
      sel_delta = 1,
      p_explt = 1
    ),
    deep_snapper = Metier$new(
      critter = fauna$deep_snapper,
      price = 0,
      sel_form = "logistic",
      sel_start = .5,
      sel_delta = .2,
      p_explt = 1
    ),
    grouper = Metier$new(
      critter = fauna$grouper,
      price = 2,
      sel_form = "dome",
      sel_start = 0.5,
      sel_delta = 1,
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
  fleet_model = "constant effort",
  spatial_allocation = "ppue"
)

fleet_two <- create_fleet(
  list(
    snapper = Metier$new(
      critter = fauna$snapper,
      price = 1,
      sel_form = "dome",
      sel_start = 0.5,
      sel_delta = 1,
      p_explt = 2
    ),
    deep_snapper = Metier$new(
      critter = fauna$deep_snapper,
      price = 3,
      sel_form = "logistic",
      sel_start = 0.5,
      sel_delta = .1,
      p_explt = 1
    ),
    grouper = Metier$new(
      critter = fauna$grouper,
      price = 1,
      sel_form = "dome",
      sel_start = 0.5,
      sel_delta = 1,
      p_explt = .25
    ),
    reef_shark = Metier$new(
      critter = fauna$reef_shark,
      price = 0,
      sel_form = "logistic",
      sel_start = 0.25,
      sel_delta = .2,
      p_explt = .25
    )
  ),
  ports = ports[2,],
  cost_per_unit_effort = 1,
  cost_per_distance = 4,
  responsiveness = 0.2,
  cr_ratio = 1,
  resolution = resolution,
  mpa_response = "stay",
  fleet_model = "open access",
  spatial_allocation = "ppue"
)


fleets <- list(fleet_one = fleet_one,
               fleet_two = fleet_two)

fleets$fleet_one$metiers$snapper$sel_at_age %>% plot()


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
  map_df(reef_sim, ~ data.frame(effort = sum(.x$grouper$e_p_fl$fleet_two)), .id = "step") %>%
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

    write_rds(list(fauna = fauna, fleets = fleets), file = file.path(results_path, "coral_fauna_and_fleets.rds"))

    coral_fauna <- fauna
    
    coral_fleets <- fleets
    
    resolution <- sqrt((coral_fauna[[1]]$patches))
    
    grid <- expand_grid(x = 1:resolution, y= 1:resolution) %>%
      mutate(patch = 1:nrow(.))
    
    
    mpa_locations <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
      mutate(mpa = TRUE)
    
    coral_sim <- simmar(
      fauna = coral_fauna,
      fleets = coral_fleets,
      manager = list(mpas = list(
        locations = mpa_locations,
        mpa_year = 1
      )),
      years = 2
    )
    
    
    
    
    patch_biomass <-
      map_df(coral_sim, ~ map_df(.x, ~tibble(biomass = rowSums(.x$ssb_p_a), patch = 1:nrow(.x$ssb_p_a)), .id = "critter"), .id = "step") %>%
      mutate(step = as.numeric(step)) %>%
      left_join(grid, by = "patch")
    
    titler <- function(x){
      
      stringr::str_to_title(stringr::str_replace_all(x,"_"," "))
      
    }
    
    
    patch_biomass %>%
      group_by(critter, step) %>%
      mutate(sbiomass = biomass / max(biomass)) %>%
      ungroup() %>%
      filter(between(step,1,2)) %>%
      mutate(step = fct_recode(as.factor(step), "2" = "1.25", "3" = "1.5", "4" = "1.75")) %>% 
      mutate(Season = step) %>% 
      ggplot()+
      geom_tile(aes(x,y, fill = sbiomass),show.legend = FALSE) +
      geom_text_repel(data = ports, aes(x, y, label = fleet), color = "red") +
      facet_grid(critter~Season, labeller = labeller(critter = titler, Season = label_both)) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_viridis_c() + 
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank())

if (run_coral_example == TRUE){

  future::plan(future::multisession, workers = experiment_workers)

  case_study_experiments <-
    expand_grid(
      placement_strategy = c("rate"),
      prop_mpa = seq(0, 1, by = 0.05),
      critters_considered = length(fauna),
      placement_error = c(0),
      mpa_response = c("stay"),
      iter = 1
    )

  effort_cap <- map(names(fleets), ~Inf, .id = "")

  names(effort_cap) <- names(fleets)

  a <- Sys.time()
  coral_mpa_experiments <- case_study_experiments %>%
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
        effort_cap = effort_cap,
        years = 20,
        .options = furrr_options(seed = 42),
        .progress = TRUE
      )
    ) %>%
    mutate(prop_ssb0_mpa = map_dbl(results, ~sum(.x$mpa$ssb0[.x$mpa$mpa == TRUE], na.rm = TRUE) / sum(.x$mpa$ssb0)))

  Sys.time() - a

  future::plan(future::sequential)

  write_rds(coral_mpa_experiments, file = file.path(results_path, "coral_mpa_experiments.rds"))


} else {

  coral_mpa_experiments <- read_rds(file = file.path(results_path, "coral_mpa_experiments.rds"))


}

coral_mpa_experiments$mpas <- map(coral_mpa_experiments$results, "mpa")

coral_mpa_experiments$obj <- map(coral_mpa_experiments$results, "obj")


examine_mpas <- coral_mpa_experiments %>%
  unnest(cols = mpas)



coral_results <- coral_mpa_experiments %>%
  unnest(cols = obj)

coral_fleet_frontier <- coral_results %>%
  pivot_longer(starts_with("fleet_"), names_to = "fleet", values_to = "fleet_yield") %>%
  group_by(prop_mpa, fleet, placement_strategy) %>%
  summarise(yield = sum(fleet_yield),biodiv = sum(unique(biodiv))) %>%
  ggplot(aes(biodiv, yield, color = placement_strategy)) +
  geom_line() +
  scale_x_continuous(name = "Change in SSB/SSB0",limits = c(0, NA)) +
  scale_y_continuous(name = "Total Yield",limits = c(0, NA)) +
  facet_wrap(~ fleet, scales = "free_y")

tmp <- coral_results %>% 
  group_by(prop_mpa, placement_strategy) %>% 
  summarise(biodiv = sum(biodiv), yield = sum(yield)) %>% 
  group_by(placement_strategy) %>% 
  mutate(delta_yield = yield / yield[prop_mpa == 0] - 1,
         delta_biodiv = biodiv / biodiv[prop_mpa == 0] - 1) %>% 
  ungroup() %>% 
  mutate(placement_strategy= fct_relabel(placement_strategy, titler))

frontier_labels <- tmp %>% 
  mutate(delta_50 = (delta_yield - -0.5)^2,
         delta_0 = delta_yield^2) %>% 
  group_by(placement_strategy) %>% 
  filter(prop_mpa > 0) %>% 
  filter(yield == max(yield) | delta_0 == min(delta_0) | delta_50 == min(delta_50)) %>% 
  ungroup() %>% 
  mutate(pmpa = scales::percent(prop_mpa))

coral_frontier <- tmp %>% 
  ggplot(aes(delta_biodiv, delta_yield)) + 
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_line(alpha = 0.8, aes(color = placement_strategy),size = 1.1) + 
  geom_text_repel(data = frontier_labels, aes(label = pmpa), min.segment.length = 0, box.padding = 0.5) +
  scale_x_continuous(name = "Change in Total SSB/SSB0", labels = scales::label_percent(accuracy = 1), guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "Change in Yield", labels = scales::label_percent(accuracy = 1)) + 
  scale_color_manual(values = c("tomato", "steelblue"),name = '') +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 4))) + 
  labs(tag = "C")  + 
  theme(
    legend.position = c(0.725, .95),
    legend.background = element_rect(fill = "transparent"),
    legend.text = element_text(size = 8)
  )

coral_fleet_yield <- coral_results %>%
  pivot_longer(starts_with("fleet_"),
               names_to = "fleet",
               values_to = "fleet_yield") %>%
  group_by(prop_mpa, fleet, placement_strategy) %>%
  summarise(yield = sum(fleet_yield), biodiv = sum(unique(biodiv))) %>%
  group_by(fleet, placement_strategy) %>%
  mutate(delta_yield = yield / yield[prop_mpa == 0] - 1) %>%
  ungroup() %>% 
  mutate(fleet= fct_relabel(fleet, titler)) %>% 
  ggplot(aes(prop_mpa, delta_yield, color = fleet)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_line() +
  facet_wrap( ~ placement_strategy, labeller = labeller(placement_strategy = titler)) +
  scale_x_continuous(name = "MPA Size",
                     labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(name = "Yield",
                     labels = scales::label_percent(accuracy = 1)) + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8))  + 
  scale_color_discrete(name = '') + 
  labs(tag = "A") 




coral_critter_bio <- coral_results %>%
  group_by(prop_mpa, critter, placement_strategy) %>%
  summarise(biodiv = sum(unique(biodiv))) %>%
  group_by(critter, placement_strategy) %>%
  mutate(delta_bio = biodiv) %>%
  mutate(critter = fct_relabel(critter, titler)) %>% 
  # filter(prop_mpa < 0.4) %>% 
  ggplot(aes(prop_mpa, pmin(Inf,delta_bio), color = critter)) +
  geom_line() +
  facet_wrap( ~ placement_strategy, labeller = labeller(placement_strategy = titler)) +
  scale_x_continuous(name = "MPA Size",
                     labels = scales::label_percent(accuracy = 1),
                     guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "SSB/SSB0") + 
  scale_color_brewer(name = '', type = "qual", palette = "Dark2") + 
  labs(tag = "B") + 
  theme(axis.text = element_text(size = 8), axis.text.x = element_text(size = 8))






((coral_fleet_yield / coral_critter_bio) | (coral_frontier + plot_layout(guides = "keep")) ) & theme(strip.text = element_text(size  = 8), plot.margin = margin(.1, .1, .1, .1, "cm"), panel.spacing = unit(1, "lines"), legend.text = element_text(size = 7))