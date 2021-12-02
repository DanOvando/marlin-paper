source(file.path("scripts", "00_setup.R"))

marlin_inputs <- readxl::read_xlsx(here("data","marlin-inputs-highf.xlsx"), sheet = "inputs",na = c("NA",""))


marlin_inputs$bycatch <- marlin_inputs$scientific_name %in% unique(tolower(bycatch$species_sciname))



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
  # file <- mats[3]

  file_comps <- str_split(file, "_", simplify = TRUE)

  species <- str_replace(file_comps[, 1], "-", " ")

  # fleet <- file_comps[, 2]


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

  # hab %>%
  #   ggplot(aes(lon, lat, fill = hab)) +
  #   geom_tile()


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
  #
    # hab %>%
    #   ggplot(aes(rough_x, rough_y, fill = hab)) +
    #   geom_tile()

  hab <- hab %>%
    group_by(rough_x, rough_y) %>%
    summarise(hab = sum(hab)) %>%
    mutate(xy = rough_x * rough_y)

  mod <-
    gamm4::gamm4(hab ~ s(rough_x) + s(rough_y) + s(xy), data = hab)

  hab$interp_cpue <- as.numeric(predict(mod$gam))

  hab$habitat <- pmax(0, hab$interp_cpue / max(hab$interp_cpue))


  if (sqrt(nrow(hab)) != resolution) {
    stop("habitat layer does not match simulation resolution")
  }
  hab <- hab %>%
    select(contains("rough"), habitat) %>%
    rename(x = rough_x, y = rough_y) %>%
    ungroup()

  # hab %>%
  #   ggplot(aes(x, y, fill = habitat)) +
  #   geom_tile()

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
    seasons = seasons
  )) %>%
  mutate(xid = 1)

casestudy <- casestudy %>%
  group_by(xid) %>%
  nest() %>%
  mutate(fauna = map(data, ~ .x$critter %>% set_names(.x$scientific_name)))

check_pop_sizes <- map_dbl(casestudy$fauna[[1]], "ssb0")

tibble(sciname = names(check_pop_sizes), ssb0 = check_pop_sizes) %>%
  ggplot(aes(reorder(sciname, ssb0), ssb0)) +
  geom_col() +
  coord_flip()

casestudy <- casestudy %>%
  ungroup() %>%
  mutate(fleet = map(fauna, compile_fleet_highf))


# initialize starting conditions

fauna <- casestudy$fauna[[1]]

fleets <- casestudy$fleet[[1]]

starting_trajectory <- simmar(fauna = fauna, fleets = fleets, years = 200)

# proc_starting_conditions <- process_marlin(starting_trajectory)
#
# plot_marlin(proc_starting_conditions, max_scale = FALSE)

starting <- starting_trajectory[[length(starting_trajectory)]]

revenues <-  map_df(starting, ~.x$r_p_a_fl %>%
                      reshape2::melt() %>%
                      group_by(Var3) %>%
                      summarise(revenue = sum(value, na.rm = TRUE))) %>%
  group_by(Var3) %>%
  rename(fleet = Var3) %>%
  summarise(revenue = sum(revenue))

# effort <- map_df(starting, ~.x$e_p_fl) %>%
#   ungroup() %>%
#   mutate(patch = 1:nrow(.)) %>%
#   pivot_longer(-patch, names_to = "fleet", values_to = "effort") %>%
#   group_by(fleet) %>%
#   summarise(e2 = sum((effort / length(fauna))^1),
#             ee = sum(effort / length(fauna)))

effort <- map_df(starting[1], ~.x$e_p_fl) %>%
  ungroup() %>%
  mutate(patch = 1:nrow(.)) %>%
  pivot_longer(-patch, names_to = "fleet", values_to = "effort") %>%
  group_by(fleet) %>%
  summarise(e2 = sum(((effort + 1)^fleets[[1]]$effort_cost_exponent)),
            check = sum(effort))


# revenues$revenue <- 339613.9

profits <- revenues %>%
  left_join(effort, by = "fleet") %>%
  mutate(cost = revenue / e2) %>%
  mutate(profit = revenue - cost * e2)

max_rev <- map_dbl(fauna, "ssb0")

prices = pluck(fleets, 1,1) %>%
  map_dbl("price")

max_p <- sum(max_rev * prices[names(max_rev)])

profits <- profits %>%
  mutate(theta = log((1 + max_delta)) / (max_p))

fleets$longline$cost_per_unit_effort <- profits$cost[profits$fleet == "longline"]

fleets$longline$profit_sensitivity <- profits$theta[profits$fleet == "longline"]

fleets$purseseine$spatial_allocation <- "profit"

fleets$purseseine$cost_per_unit_effort <- profits$cost[profits$fleet == "purseseine"]

fleets$purseseine$profit_sensitivity <- profits$theta[profits$fleet == "purseseine"]

fleets$purseseine$spatial_allocation <- "profit"


# browser()
#

starting_trajectory <- simmar(fauna = fauna,
                                      fleets = fleets,
                                      year = years,
                                      initial_conditions = starting)


starting_conditions <- starting_trajectory[[length(starting_trajectory)]]

proc_starting_conditions <- process_marlin(starting_trajectory)

plot_marlin(proc_starting_conditions, max_scale = FALSE)
if (run_casestudy == TRUE){

  future::plan(future::multisession, workers = experiment_workers)

  case_study_experiments <-
    expand_grid(
      placement_strategy = c("depletion", "rate", "avoid_fishing", "target_fishing", "area"),
      prop_mpa = seq(0,1, by = 0.01),
      critters_considered = length(fauna),
      placement_error = c(0),
      mpa_response = c("stay")
    )

  a <- Sys.time()
  case_study_experiments <- case_study_experiments %>%
    ungroup() %>%
    mutate(results = future_pmap(list(placement_strategy = placement_strategy,
                               prop_mpa = prop_mpa,
                               critters_considered = critters_considered,
                               placement_error = placement_error,
                               mpa_response = mpa_response),
                          run_mpa_experiment,
                          starting_conditions = starting_conditions,
                          proc_starting_conditions = proc_starting_conditions,
                          resolution = resolution,
                          fauna = fauna,
                          fleets = fleets,
                          .options = furrr_options(seed = 42),
                          .progress = TRUE))

  Sys.time() - a

  future::plan(future::sequential)

write_rds(case_study_experiments, file = file.path(results_path, "case_study_experiments_highf.rds"))

} else {

  case_study_experiments <- read_rds(file = file.path(results_path, "case_study_experiments_highf.rds"))


}


# optimize network --------------------------------------------------------
if (optimize_casestudy == TRUE){

  optimized_networks <- tibble(alpha = seq(0,1, length.out = 20))

  # optimized_networks <- tibble(alpha = c(0,1))

  a <- Sys.time()
  optimized_networks <- optimized_networks %>%
    mutate(
      network = pmap(
        list(alpha = alpha),
        optimize_mpa,
        fauna = fauna,
        fleets = fleets,
        max_prop_mpa = 1,
        prop_sampled = 0.25,
        starting_conditions = starting_conditions,
        resolution = resolution,
        workers = experiment_workers
      ))

  Sys.time() - a

  write_rds(optimized_networks, file = file.path(results_path,"optimized_networks_highf.rds"))

  # a <- Sys.time()
  # min_loss_optimized_networks <- optimized_networks %>%
  #   mutate(
  #     network = pmap(
  #       list(alpha = alpha),
  #       optimize_mpa,
  #       fauna = casestudy$fauna[[1]],
  #       fleets = casestudy$fleet[[1]],
  #       max_prop_mpa = 1,
  #       prop_sampled = 0.2,
  #       starting_conditions = starting_conditions,
  #       resolution = resolution,
  #       objective = "min_loss",
  #       workers = 8
  #     ))
  #
  # Sys.time() - a
  #
  # write_rds(min_loss_optimized_networks, file = file.path(results_path,"min_loss_optimized_networks.rds"))


  #
  # check <- optimized_networks %>%
  #   mutate(outcomes = map(network,"outcomes")) %>%
  #   unnest(cols = outcomes)
  #
  # check %>%
  #   ggplot(aes(p_protected, ssb_v_ssb0, color = factor(alpha))) +
  #   geom_line() +
  #   facet_wrap(~critter, scales = "free_y")
  #
  #
  #
  # check %>%
  #   ggplot(aes(ssb_v_ssb0, econ, color = factor(alpha))) +
  #   geom_point(show.legend = FALSE) +
  #   facet_wrap(~critter, scales = "free")
  #
  # #
  # mpa_check <- optimized_networks %>%
  #   mutate(network = map(network,"mpa_network")) %>%
  #   unnest(cols = network)
  #
  # mpa_check %>%
  #   # filter(alpha == 0.3) %>%
  #   ggplot(aes(x,y,fill = mpa)) +
  #   geom_tile() +
  #   facet_wrap( ~ p_protected) +
  #   theme(axis.text = element_blank(),
  #         strip.text = element_blank())
  #
  #
  # # hmmm
  #
  # hm <- mpa_check %>%
  #   filter(alpha == 0, p_protected %in% c(1,90)) %>%
  #   group_by(alpha, p_protected) %>%
  #   nest()
  #
  # sim1 <- simmar(
  #   fauna = fauna,
  #   fleets = fleets,
  #   years = 200,
  #   mpas = list(locations = hm$data[[1]],
  #               mpa_year = 1)
  # )
  #
  # sim2 <- simmar(
  #   fauna = fauna,
  #   fleets = fleets,
  #   years = 200,
  #   mpas = list(locations = hm$data[[2]],
  #               mpa_year = 1)
  # )
  #
  #
  # # check revenue per unit effort
  #
  # e_p_f <- map(sim1[[length(sim1)]],"e_p_fl") %>%
  #   bind_rows(.id = "critter") %>%
  #   pivot_longer(-critter, names_to = "fleet", values_to = "effort") %>%
  #   group_by(critter, fleet) %>%
  #   mutate(patch = seq_along(effort)) %>%
  #   group_by(fleet, patch) %>%
  #   summarise(effort = sum(effort))
  #
  #
  # r_p_f <- map(sim1[[length(sim2)]],~ reshape::melt(.x$r_p_a_fl) %>%
  #                purrr::set_names("patch", "age", "fleet", "revenue")) %>%
  #   bind_rows(.id = "critter") %>%
  #   group_by(patch, fleet) %>%
  #   summarise(revenue = sum(revenue, na.rm = TRUE))
  #
  # rpue = r_p_f %>%
  #   left_join(e_p_f, by = c("patch","fleet")) %>%
  #   mutate(rpue = revenue / (effort))
  #
  # rpue %>%
  #   filter(!is.na(rpue) & rpue > 0) %>%
  #   ggplot(aes((rpue))) +
  #   geom_histogram() +
  #   facet_wrap(~fleet)
  #
  #
  # procsim1 <- process_marlin(sim1, keep_age = FALSE)
  #
  # procsim2 <- process_marlin(sim2, keep_age = FALSE)
  #
  # procsim1_age <- process_marlin(sim1, keep_age = TRUE)
  #
  # procsim2_age <- process_marlin(sim2, keep_age = TRUE)
  #
  # plot_marlin(`no mpa` = procsim1 , `yes mpa` = procsim2)
  #
  # procsim1$fauna <-  procsim1$fauna %>%
  #   mutate(u = c / b)
  #
  # procsim2$fauna <-  procsim2$fauna %>%
  #   mutate(u = c / b)
  #
  # plot_marlin(`no mpa` = procsim1, `yes mpa` = procsim2,plot_var = "u", plot_type = "space")
  #
  # plot_marlin(`no mpa` = procsim1_age, `yes mpa` = procsim2_age,plot_var = "ssb", plot_type = "length", fauna = fauna)



} else {

  optimized_networks <-
    read_rds(file = file.path(results_path, "optimized_networks_highf.rds"))

  # min_loss_optimized_networks <- read_rds(file = file.path(results_path,"min_loss_optimized_networks.rds"))


}

# process results ---------------------------------------------------------

# ex <- optimized_networks %>%
#   filter(alpha == 0) %>%
#   slice(1)
#
# ex <- ex$network[[1]]$mpa_network %>%
#   group_by(p_protected) %>%
#   nest() %>%
#   ungroup()
#
#
# foo <- function(mpa){
#
# tmp_result <- simmar(
#   fauna = casestudy$fauna[[1]],
#   fleets = casestudy$fleet[[1]],
#   years = years,
#   mpas = list(locations = mpa,
#               mpa_year = 1),
#   initial_conditions = starting_conditions
# )
#
# res <-
#   tmp_result[[length(tmp_result)]] # for now, just calculate in the final timestep
#
# foo2 <- function(z){
#
#   out <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
#     mutate(dep = (z$ssb_p_a %>% as.data.frame() %>% rowSums())) %>%
#     mutate(dep = dep / (z$ssb0_p))
#
# }
#
# out <- map_df(res,foo2, .id = "critter")
# #
# # out %>%
# #   ggplot(aes(x,y,fill = dep)) +
# #   geom_tile() +
# #   facet_wrap(~critter)
#
# return(out)
# }
#
# ex <- ex %>%
#   # slice(1:10) %>%
#   mutate(cs_result = map(data, foo))
#
# mpas <- ex %>%
#   select(p_protected, data) %>%
#   unnest(cols = data)

# a = ex %>%
#   unnest(cols = cs_result) %>%
#   left_join(mpas, by = c("p_protected", "x", "y")) %>%
#   mutate(p_protected = round(p_protected / max(p_protected),2)) %>%
#   ggplot(aes(x, y, fill = dep)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   facet_wrap(~ critter) +
#   transition_time(p_protected) +
#   labs(title = 'MPA %: {frame_time}')
#
# a
# anim_save("cs_anim.gif")
# ex %>%
#   # filter(p_protected == 100) %>%
#   ggplot(aes(x,y,fill = mpa)) +
#   geom_tile() +
#   transition_time(p_protected)

# calculate objective function for base case (no mpa)

bau_biodiv <-
  (map_df(starting_conditions, ~ sum(.x$ssb_p_a) / .x$ssb0)) %>%  # calculate biodiversity component of objective function
  pivot_longer(everything(), names_to = "critter",values_to = "bau_biodiv")

# econ <- sum(map_dbl(res, ~sum(.x$c_p_a))) #  calculate econ component of objective function

bau_econ <-
  (map_df(starting_conditions, ~ sum(.x$prof_p_fl, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "critter",values_to = "bau_econ")
#  calculate econ component of objective function, currently revenues across all fleets and species

bau_yield <-
  (map_df(starting_conditions, ~ sum(.x$c_p_fl, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "critter",values_to = "bau_yield")
#  calculate yield component of objective function, currently revenues across all fleets and species


bau <- bau_econ %>%
  left_join(bau_biodiv, by = "critter") %>%
  left_join(bau_yield, by = "critter") %>%
  mutate(bau_econ = bau_econ / 1e6,
         bau_yield = bau_yield / 1e6)


# calculate objective function for experiments

experiment_obj <- case_study_experiments %>%
  mutate(obj = map(results, "obj")) %>%
  select(-results) %>%
  unnest(cols = obj) %>%
  mutate(econ = econ / 1e6,
         yield = yield / 1e6) %>%
  filter(prop_mpa < 1) %>%
  mutate(bycatch = !(str_detect(critter, "thunnus") | str_detect(critter,"katsuwonus")))


experiment_obj <- experiment_obj %>%
  left_join(bau, by = "critter") %>%
  mutate(delta_biodiv = biodiv - bau_biodiv,
         delta_econ = econ - bau_econ,
         delta_yield = yield - bau_yield)


experiment_obj %>%
  ggplot(aes(biodiv, econ, color = placement_strategy)) +
  geom_point() +
  facet_grid(placement_strategy~critter, scales = "free_y")

experiment_obj %>%
  ggplot(aes(prop_mpa, biodiv, color = placement_strategy)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~critter, scales = "free_y")

experiment_obj %>%
  filter(placement_strategy == "area") %>%
  ggplot(aes(prop_mpa, biodiv, color = log(econ))) +
  geom_point() +
  facet_wrap(~critter, scales = "free_y")



experiment_obj %>%
  ggplot(aes(prop_mpa, econ, color = placement_strategy)) +
  geom_point() +
  facet_wrap( ~ critter, scales = "free_y")


# calculate objective function for optimization

opt_obj <- optimized_networks %>%
  mutate(obj = map(network,"outcomes")) %>%
  select(-network) %>%
  unnest(cols = obj) %>%
  mutate(econ = econ / 1e6,
         yield = yield / 1e6) %>%
  mutate(bycatch = !(str_detect(critter, "thunnus") | str_detect(critter,"katsuwonus")))


obj_objective_value <- optimized_networks %>%
  mutate(obj = map(network,"objective")) %>%
  select(-network) %>%
  unnest(cols = obj) %>%
  select(alpha, p_protected, obj)

opt_obj <- opt_obj %>%
  left_join(obj_objective_value, by = c("alpha","p_protected"))

opt_obj %>%
  ggplot(aes(p_protected, ssb_v_ssb0)) +
  geom_point() +
  facet_wrap(~critter)



opt_obj %>%
  ggplot(aes(p_protected, econ)) +
  geom_point() +
  facet_wrap(~critter, scales = "free_y")


# combine experiment and optimization, and then calculate change in objective function relative to status quo
#


experiment_obj %>%
  ggplot(aes(prop_mpa, delta_econ)) +
  geom_point() +
  facet_wrap(~critter, scales = "free_y")



hf_cs_fig_2 <- experiment_obj %>%
  ggplot(aes(delta_biodiv, delta_econ, color = placement_error)) +
  geom_point() +
  facet_wrap( ~ critter, scales = "free_y")


opt_obj <- opt_obj %>%
  left_join(bau, by = "critter") %>%
  mutate(delta_biodiv = ssb_v_ssb0 - bau_biodiv,
         delta_econ = econ - bau_econ,
         delta_yield = yield - bau_yield)

opt_frontier <- opt_obj %>%
  group_by(alpha) %>%
  filter(obj == max(obj))

total_opt_obj <- opt_obj %>%
  group_by(alpha, p_protected) %>%
  summarise(
    obj_fun = unique(obj),
    real_loss =  mean(ssb_v_ssb0 <= (0.9 *bau_biodiv)),
    loss = mean(ssb_v_ssb0 <= bau_biodiv),
    total_ssb_change = sum(ssb_v_ssb0 - bau_biodiv),
    total_ssb_loss = sum((ssb_v_ssb0 - bau_biodiv)[ssb_v_ssb0 < bau_biodiv], na.rm = TRUE),
    total_bycatch_ssb_change = sum((ssb_v_ssb0 - bau_biodiv)[bycatch == TRUE]),
    total_ssb_loss = sum((ssb_v_ssb0 - bau_biodiv)[ssb_v_ssb0 < bau_biodiv & bycatch], na.rm = TRUE),
    econ = sum(econ),
    yield = sum(yield),
            biodiv = sum(ssb_v_ssb0),
            bau_econ = sum(bau_econ),
    bau_yield = sum(bau_yield),
            bau_biodiv = sum(bau_biodiv)) %>%
  ungroup() %>%
  mutate(delta_biodiv = biodiv - bau_biodiv,
         delta_econ = econ - bau_econ,
         delta_yield = yield - bau_yield)

total_opt_frontier <- total_opt_obj %>%
  group_by(alpha) %>%
  filter(obj_fun == max(obj_fun))


hf_cs_fig_5 <- experiment_obj %>%
  group_by(prop_mpa) %>%
  summarise(delta = mean(biodiv < (0.9 *bau_biodiv))) %>%
  ggplot(aes(prop_mpa, delta)) +
  geom_point()

experiment_obj <- experiment_obj %>%
  mutate(critters_considered = prop_critters_considered)

total_experiment_obj <- experiment_obj %>%
  group_by(placement_strategy,
           prop_mpa,
           critters_considered,
           placement_error) %>%
  summarise(
    loss = mean(biodiv <= bau_biodiv),
    real_loss = mean(biodiv <= (0.9 * bau_biodiv)),
    total_ssb_loss = sum((biodiv - bau_biodiv)[biodiv < bau_biodiv], na.rm = TRUE),
    total_ssb_change = sum(biodiv - bau_biodiv),
    total_bycatch_ssb_change = sum((biodiv - bau_biodiv)[bycatch == TRUE]),
    total_ssb_loss = sum((biodiv - bau_biodiv)[biodiv < bau_biodiv & bycatch], na.rm = TRUE),
    econ = sum(econ),
    biodiv = sum(biodiv),
    yield = sum(yield),
    bau_econ = sum(bau_econ),
    bau_biodiv = sum(bau_biodiv),
    bau_yield = sum(bau_yield)
  ) %>%
  ungroup() %>%
  mutate(delta_biodiv = biodiv - bau_biodiv,
         delta_econ = econ - bau_econ,
         delta_yield = yield - bau_yield)


hf_cs_fig_6 <- total_experiment_obj %>%
  filter(prop_mpa > 0) %>%
  ggplot(aes(prop_mpa, real_loss)) +
  geom_smooth(data = total_opt_obj,
              aes(
                p_protected / 100,
                real_loss,
                color = alpha,
                group = alpha
              )) +
  geom_point() +
  scale_x_continuous(labels = scales::percent, name = "Area in MPA") +
  scale_y_continuous(labels = scales::percent, name = "% Experiments With Conservation Loss") +
  facet_wrap( ~ placement_strategy) +
  scale_color_viridis_c(name = "Conservation Weighting in Optimization")


hf_cs_fig_8 <- total_experiment_obj %>%
  filter(prop_mpa > 0) %>%
  ggplot(aes(prop_mpa, delta_econ)) +
  geom_smooth(data = total_opt_obj, aes(p_protected / 100, delta_econ, color = alpha, group = alpha)) +
  geom_jitter(alpha = 0.1) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = 2) +
  scale_x_continuous(labels = scales::percent, name = "Area in MPA") +
  scale_y_continuous(labels = scales::percent, "Change in Total Revenue") +
  facet_wrap(~placement_strategy) +
  scale_color_viridis_c(name = "Conservation Weighting in Optimization")

# ggsave("cs_fig_8.pdf", io_rpue_outcomes_plot, width = 8, height = 6)


hf_cs_fig_9 <- total_opt_obj %>%
  ggplot() +
  geom_smooth(aes(biodiv, econ, group = alpha, color = alpha), se = FALSE) +
  geom_point(
    data = total_experiment_obj,
    aes(biodiv, econ),
    size = 2,
    alpha = 0.5,
    fill = "red",
    shape = 21
  ) +
  facet_wrap( ~ placement_strategy) +
  scale_color_viridis_c(name = "Conservation Weighting in Optimization")


a = opt_obj %>%
  filter(alpha == 0) %>%
  ggplot(aes(p_protected, econ)) +
  geom_line() +
  facet_wrap(~critter, scales = "free_y") +
  scale_x_continuous(name = "% MPA") +
  scale_y_continuous(name = "Revenue")

b = opt_obj %>%
  filter(alpha == 0) %>%
  ggplot(aes(p_protected, ssb_v_ssb0)) +
  geom_line() +
  facet_wrap(~critter, scales = "free_y") +
  scale_x_continuous(name = "% MPA") +
  scale_y_continuous(name = "SSB/SSB0")

opt_obj %>%
  ggplot(aes(p_protected, ssb_v_ssb0)) +
  geom_point() +
  facet_wrap(~critter, scales = "free_y")

a / b
total_opt_obj %>%
  group_by(alpha) %>%
  filter(obj_fun == max(obj_fun)) %>%
  ggplot(aes(p_protected, econ, color = factor(alpha))) +
  geom_point() +
  scale_color_viridis_d()

total_opt_obj %>%
  ggplot(aes(p_protected, biodiv, color = factor(alpha))) +
  geom_line() +
  scale_color_viridis_d()



hf_cs_fig_10 <- total_opt_obj %>%
  group_by(alpha) %>%
  filter(obj_fun == max(obj_fun)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(delta_biodiv, delta_econ, color = alpha), size = 2) +
  geom_point(
    data = total_experiment_obj,
    aes(delta_biodiv, delta_econ),
    size = 2,
    alpha = 0.1,
    fill = "red",
    shape = 21
  ) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_abline(intercept = 0, slope = -1) +
  facet_wrap( ~ placement_strategy) +
  scale_color_viridis_c(name = "Conservation Weighting in Optimization") +
  scale_x_continuous(name = "Change in Total Biodiversity Relative to BAU ") +
  scale_y_continuous(name = "Change in Total Profits Relative to BAU ") +
  theme(axis.text.x = element_text(size = 8),
        legend.position = "top")


hf_cs_fig_10_yield <- total_opt_obj %>%
  group_by(alpha) %>%
  filter(obj_fun == max(obj_fun)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(delta_biodiv, delta_yield, color = alpha), size = 2) +
  geom_point(
    data = total_experiment_obj,
    aes(delta_biodiv, delta_yield),
    size = 2,
    alpha = 0.1,
    fill = "red",
    shape = 21
  ) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_abline(intercept = 0, slope = -1) +
  facet_wrap( ~ placement_strategy) +
  scale_color_viridis_c(name = "Conservation Weighting in Optimization") +
  scale_x_continuous(name = "Change in Total Biodiversity Relative to BAU ") +
  scale_y_continuous(name = "Change in Total Yield Relative to BAU ") +
  theme(axis.text.x = element_text(size = 8),
        legend.position = "top")

hf_cs_fig_10a <- total_opt_obj %>%
  ggplot() +
  geom_smooth(aes(delta_biodiv, delta_econ, group = alpha, color = alpha), se = FALSE) +
  geom_point(
    data = total_experiment_obj %>%  filter(placement_strategy == "target_fishing"),
    aes(delta_biodiv, delta_econ),
    size = 2,
    alpha = 0.1,
    fill = "red",
    shape = 21
  ) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_abline(intercept = 0, slope = -1) +
  scale_color_viridis_c(
    name = "Conservation Weighting in Optimization",
    guide = guide_colorbar(
      frame.colour = "black",
      ticks.colour = "black",
      barwidth = unit(10, "lines")
    ),
    labels = scales::percent
  ) +
  scale_x_continuous(labels = scales::percent, name = "Change in Total Biodiversity Relative to BAU ") +
  scale_y_continuous(labels = scales::percent, name = "Change in Revenue Relative to BAU ") +
  theme(axis.text.x = element_text(size = 8),
        legend.position = "top",
        plot.margin = unit(c(1,1,1,1),"cm"))

# ggsave("cs_fig_10a.pdf", cs_fig_10a, width = 8, height = 6)


hf_cs_fig_11 <- total_opt_obj %>%
  filter(p_protected > 1, p_protected < 50) %>%
  ggplot() +
  geom_bin2d(data = total_experiment_obj %>%  filter(prop_mpa > .1),aes(real_loss, delta_econ), show.legend = FALSE) +
  geom_point(aes(real_loss, delta_econ, group = alpha, color= alpha), alpha = 1) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_abline(intercept = 0, slope = -1) +
  facet_wrap(~placement_strategy) +
  scale_fill_gradient(low = "grey", high = "red") +
  scale_color_viridis_c(name = "Conservation Weighting") +
  scale_x_continuous(labels = scales::percent, name = "% Sims With Loss in SSB/SSB0 Relative to BAU") +
  scale_y_continuous(name = "Change in Revenue Relative to BAU ") +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "top")


# save things -------------------------------------------------------------


write_rds(opt_obj,
          file = file.path(results_path, "opt_obj_highf.rds"))

write_rds(total_opt_obj,
          file = file.path(results_path, "total_opt_obj_highf.rds"))

write_rds(experiment_obj,
          file = file.path(results_path, "experiment_obj_highf.rds"))

write_rds(total_experiment_obj,
          file = file.path(results_path, "total_experiment_obj_highf.rds"))


write_rds(case_study_experiments,
          file = file.path(results_path, "case_study_experiments_highf.rds"))

figs <- ls()[str_detect(ls(), "cs_fig_")]

save(file = file.path(results_path, "casestudy_figures_highf.Rdata"),
     list = figs)





