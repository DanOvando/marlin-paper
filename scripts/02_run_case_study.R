source(file.path("scripts", "00_setup.R"))



# load bycatch risk layers ------------------------------------------------


mats <- list.files(path = here("data", "matrices"))

mats <- mats[str_detect(mats, "longline_wcpfc")]

get_layer <- function(file) {
  # file <- mats[3]

  file_comps <- str_split(file, "_", simplify = TRUE)

  species <- str_replace(file_comps[, 1], "-", " ")

  fleet <- file_comps[, 2]


  hab <-
    read_csv(here("data", "matrices", file),
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
  #

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
  #   hab %>%
  #     ggplot(aes(rough_x, rough_y, fill = hab)) +
  #     geom_tile()

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
  #   ggplot(aes(rough_x, rough_y, fill = habitat)) +
  #   geom_tile()

  out <- tibble(scientific_name = species, habitat = list(hab))


}


mats <- map(mats, get_layer)

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

casestudy <- casestudy %>%
  mutate(critter = pmap(
    list(
      sciname = scientific_name,
      habitat = habitat,
      ontogenetic_shift = FALSE,
      seasonal_movement = FALSE
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
  mutate(fleet = map(fauna, compile_fleet))


# initialize starting conditions

fauna <- casestudy$fauna[[1]]

fleets <- casestudy$fleet[[1]]

starting_trajectory <- simmar(fauna = fauna, fleets = fleets, years = 100)

check <- process_marlin(starting_trajectory, keep_age = FALSE)

plot_marlin(check)

starting_conditions <- starting_trajectory[length(starting_trajectory)]


proc_starting_conditions <- process_marlin(starting_conditions)



if (run_casestudy == TRUE){

  future::plan(future::multisession, workers = 8)

  case_study_experiments <-
    expand_grid(
      placement_strategy = c("depletion", "rate", "avoid_fishing", "target_fishing", "area"),
      prop_mpa = seq(0, 1, by = 0.01),
      prop_critters_considered = seq(.1,1, length.out = 4),
      placement_error = seq(0,2, length.out = 5)
    )

  a <- Sys.time()
  case_study_experiments <- case_study_experiments %>%
    mutate(results = future_pmap(list(placement_strategy = placement_strategy,
                               prop_mpa = prop_mpa,
                               prop_critters_considered = prop_critters_considered,
                               placement_error = placement_error),
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

write_rds(case_study_experiments, file = file.path(results_path, "case_study_experiments.rds"))

} else {

  case_study_experiments <- read_rds(file = file.path(results_path, "case_study_experiments.rds"))


}


# optimize network --------------------------------------------------------

if (optimize_casestudy == TRUE){

  optimized_networks <- tibble(alpha = seq(0,1, length.out = 20))

  fauna <- casestudy$fauna[[1]]

  fleets <- casestudy$fleet[[1]]

  optimized_networks <- optimized_networks %>%
    mutate(
      network = purrr::pmap(
        list(alpha = alpha),
        optimize_mpa,
        fauna = casestudy$fauna[[1]],
        fleets = casestudy$fleet[[1]],
        max_prop_mpa = 1,
        prop_sampled = 0.2,
        starting_conditions = starting_conditions,
        resolution = resolution,
      workers = 8
      ))

  write_rds(optimized_networks, file = file.path(results_path,"optimized_networks.rds"))

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
    read_rds(file = file.path(results_path, "optimized_networks.rds"))


}


# process results ---------------------------------------------------------


# calculate objective function for base case (no mpa)

bau_biodiv <-
  (map_df(starting_conditions[[1]], ~ sum(.x$ssb_p_a) / .x$ssb0)) %>%  # calculate biodiversity component of objective function
  pivot_longer(everything(), names_to = "critter",values_to = "bau_biodiv")

# econ <- sum(map_dbl(res, ~sum(.x$c_p_a))) #  calculate econ component of objective function

bau_econ <-
  (map_df(starting_conditions[[1]], ~ sum(.x$r_p_a_fl, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "critter",values_to = "bau_econ")
#  calculate econ component of objective function, currently revenues across all fleets and species

bau <- bau_econ %>%
  left_join(bau_biodiv, by = "critter")


# calculate objective function for experiments

experiment_obj <- case_study_experiments %>%
  mutate(obj = map(results, "obj")) %>%
  select(-results) %>%
  unnest(cols = obj)

experiment_obj %>%
  ggplot(aes(biodiv, econ, color = placement_strategy)) +
  geom_point() +
  facet_grid(placement_strategy~critter, scales = "free_y")

experiment_obj %>%
  ggplot(aes(prop_mpa, biodiv, color = log(econ))) +
  geom_point() +
  facet_wrap(~critter, scales = "free_y")

experiment_obj %>%
  filter(placement_strategy == "avoid_fishing") %>%
  ggplot(aes(prop_mpa, biodiv, color = log(econ))) +
  geom_point() +
  facet_wrap(~critter, scales = "free_y")




experiment_obj %>%
  ggplot(aes(prop_mpa, econ, fill = factor(placement_error))) +
  geom_smooth() +
  facet_grid(placement_strategy~critter, scales = "free_y")

# calculate objective function for optimization

opt_obj <- optimized_networks %>%
  mutate(obj = map(network,"outcomes")) %>%
  select(-network) %>%
  unnest(cols = obj)


# combine experiment and optimization, and then calculate change in objective function relative to status quo
#

experiment_obj <- experiment_obj %>%
  left_join(bau, by = "critter") %>%
  mutate(delta_biodiv = biodiv / bau_biodiv - 1,
         delta_econ = econ / bau_econ - 1)

cs_fig_1 <- experiment_obj %>%
  ggplot(aes(delta_biodiv))


cs_fig_2 <- experiment_obj %>%
  ggplot(aes(delta_biodiv, delta_econ, color = placement_error)) +
  geom_point() +
  facet_wrap(~critter)


opt_obj <- opt_obj %>%
  left_join(bau, by = "critter") %>%
  mutate(delta_biodiv = ssb_v_ssb0 / bau_biodiv - 1,
         delta_econ = econ / bau_econ - 1)

total_opt_obj <- opt_obj %>%
  group_by(alpha, p_protected) %>%
  summarise(
    loss = mean(ssb_v_ssb0 <= bau_biodiv),
    total_loss = sum(ssb_v_ssb0 - bau_biodiv),
    econ = sum(econ),
            biodiv = sum(ssb_v_ssb0),
            bau_econ = sum(bau_econ),
            bau_biodiv = sum(bau_biodiv)) %>%
  ungroup() %>%
  mutate(delta_biodiv = biodiv / bau_biodiv - 1,
         delta_econ = econ / bau_econ - 1)

cs_fig_3 <- total_opt_obj %>%
  ggplot(aes(biodiv, econ, color = factor(alpha))) +
  geom_smooth(se = FALSE)


cs_fig_4 <- experiment_obj %>%
  ggplot(aes(prop_mpa, biodiv / bau_biodiv - 1)) +
  geom_point()

cs_fig_5 <- experiment_obj %>%
  group_by(prop_mpa) %>%
  summarise(delta = mean((biodiv / bau_biodiv - 1) < 0)) %>%
  ggplot(aes(prop_mpa, delta)) +
  geom_point()

total_experiment_obj <- experiment_obj %>%
  group_by(placement_strategy, prop_mpa, prop_critters_considered,placement_error) %>%
  summarise(
    loss = mean(biodiv <= bau_biodiv),
    total_loss = sum(biodiv - bau_biodiv),
    econ = sum(econ),
            biodiv = sum(biodiv),
            bau_econ = sum(bau_econ),
            bau_biodiv = sum(bau_biodiv)
           ) %>%
  ungroup() %>%
  mutate(delta_biodiv = biodiv / bau_biodiv - 1,
         delta_econ = econ / bau_econ - 1)


cs_fig_6 <- total_experiment_obj %>%
  filter(prop_mpa > 0) %>%
  ggplot(aes(prop_mpa, loss)) +
  geom_smooth(data = total_opt_obj, aes(p_protected / 100, loss, color = alpha, group = alpha)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent, name = "Area in MPA") +
  scale_y_continuous(labels = scales::percent, name = "% Experiments With Conservation Loss") +
  facet_wrap(~placement_strategy) +
  scale_color_viridis_c(name = "Conservation Weighting in Optimization")


cs_fig_7 <- total_experiment_obj %>%
  filter(prop_mpa > 0) %>%
  ggplot(aes(prop_mpa, total_loss)) +
  geom_jitter(alpha = 0.25) +
  geom_smooth(data = total_opt_obj, aes(p_protected / 100, total_loss, color = alpha, group = alpha)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = 2) +
  scale_x_continuous(labels = scales::percent, name = "Area in MPA") +
  scale_y_continuous("Change in Total SSB/SSB0") +
  facet_wrap(~placement_strategy) +
  scale_color_viridis_c(name = "Conservation Weighting in Optimization") +
  theme(legend.position = "top")


cs_fig_8 <- total_experiment_obj %>%
  filter(prop_mpa > 0) %>%
  ggplot(aes(prop_mpa, delta_econ)) +
  geom_smooth(data = total_opt_obj, aes(p_protected / 100, delta_econ, color = alpha, group = alpha)) +
  geom_jitter(alpha = 0.1) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = 2) +
  scale_x_continuous(labels = scales::percent, name = "Area in MPA") +
  scale_y_continuous(labels = scales::percent, "Change in Total Revenue") +
  facet_wrap(~placement_strategy) +
  scale_color_viridis_c(name = "Conservation Weighting in Optimization")



cs_fig_9 <- total_opt_obj %>%
  ggplot() +
  geom_smooth(aes(biodiv, econ, group = alpha, color= alpha), se = FALSE) +
  geom_point(data = total_experiment_obj,aes(biodiv, econ),size = 2, alpha = 0.5, fill = "red", shape = 21) +
  facet_wrap(~placement_strategy) +
  scale_color_viridis_c(name = "Conservation Weighting in Optimization")


cs_fig_10 <- total_opt_obj %>%
  ggplot() +
  geom_smooth(aes(delta_biodiv, delta_econ, group = alpha, color= alpha), se = FALSE) +
  geom_point(data = total_experiment_obj,aes(delta_biodiv, delta_econ),size = 2, alpha = 0.1, fill = "red", shape = 21) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_abline(intercept = 0, slope = -1) +
  facet_wrap(~placement_strategy) +
  scale_color_viridis_c(name = "Conservation Weighting in Optimization") +
  scale_x_continuous(labels = scales::percent, name = "Change in Total Biodiversity Relative to BAU ") +
  scale_y_continuous(labels = scales::percent, name = "Change in Revenue Relative to BAU ") +
  theme(axis.text.x = element_text(size = 8),
        legend.position = "top")


cs_fig_11 <- total_opt_obj %>%
  ggplot() +
  geom_bin2d(data = total_experiment_obj,aes(loss, delta_econ), binwidth = .1, show.legend = FALSE) +
  geom_point(aes(loss, delta_econ, group = alpha, color= alpha), alpha = 0.25) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_abline(intercept = 0, slope = -1) +
  facet_wrap(~placement_strategy) +
  scale_fill_gradient(low = "grey", high = "red") +
  scale_color_viridis_c(name = "Conservation Weighting") +
  scale_x_continuous(labels = scales::percent, name = "% Sims With Loss in SSB/SSB0 Relative to BAU") +
  scale_y_continuous(labels = scales::percent, name = "Change in Revenue Relative to BAU ") +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "top")


# save things -------------------------------------------------------------


write_rds(total_opt_obj,
          file = file.path(results_path, "total_opt_obj.rds"))

write_rds(experiment_obj,
          file = file.path(results_path, "experiment_obj.rds"))

write_rds(total_experiment_obj,
          file = file.path(results_path, "total_experiment_obj.rds"))


write_rds(case_study_experiments,
          file = file.path(results_path, "case_study_experiments.rds"))

figs <- ls()[str_detect(ls(), "cs_fig_")]

save(file = file.path(results_path, "casestudy_figures.Rdata"),
     list = figs)





