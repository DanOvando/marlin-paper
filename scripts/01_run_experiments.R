source(file.path("scripts", "00_setup.R"))


# generate state experiments. This is a somewhat tricky process where the actual generated values are created for many variables in create_experiment_critters

n_states <- 600

if (run_experiments) {
  state_experiments <-
    tibble(
      sigma_centroid = runif(n_states, .1 * resolution, resolution ^ 2 / 2),
      sigma_hab = runif(n_states, .1 * resolution, resolution ^ 2 / 8),
      spatial_q = sample(c(FALSE,TRUE), n_states, replace = TRUE, prob = c(3,1)),
      spatial_allocation = sample(c("ppue","rpue", "revenue",'profit'), n_states, replace = TRUE)
    ) %>%
    mutate(state_id = 1:nrow(.))


  # function to assign habitat for each species based on

  critters <-
    tibble(
      scientific_name = c(
        "katsuwonus pelamis",
        "kajikia audax",
        "carcharhinus longimanus"
      )
    ) %>%
    mutate(centroid = NA)


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


      critters$centroid <-
        pmin(nrow(base_layer), pmax(1, round(
          rnorm(nrow(critters), centroid_index, sigma_centroid)
        )))

      critters$habitat <-
        vector(mode = "list", length = nrow(critters))

      for (i in 1:nrow(critters)) {
        tmp <- base_layer

        tmp <- base_layer %>%
          mutate(c_x = base_layer$x[critters$centroid[i]],
                 c_y = base_layer$y[critters$centroid[i]]) %>%
          mutate(distance = sqrt((c_x - x) ^ 2 + (c_y - y) ^ 2)) %>%
          mutate(habitat = dnorm(distance, 0, sigma_hab)) %>%
          select(x, y, habitat)




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


  state_experiments <- state_experiments %>%
    mutate(
      habitats = map2(
        sigma_centroid,
        sigma_hab,
        create_critter_habitats,
        critters = critters,
        resolution = resolution
      )
    ) %>%
    unnest(cols = habitats) %>%
    mutate(
      seasonal_movement = sample(c(TRUE,FALSE), nrow(.), replace = TRUE),
      f_v_m = sample(c(0.4,0.8,1.6), nrow(.), replace = TRUE),
      adult_movement_sigma = runif(
        nrow(.),
        min = 0.01 * resolution,
        max = .5 * resolution
      ),
      recruit_movement_sigma = runif(
        nrow(.),
        min = 0.01 * resolution,
        max = .5 * resolution
      ),
      hyper = sample(c(1,1.5),nrow(.), replace = TRUE),
      rec_form = sample(c(0, 1, 2, 3), nrow(.), replace = TRUE)
    )

  state_experiments$habitat[[sample(1:n_states, 1)]] %>%
    ggplot(aes(x, y, fill = habitat)) +
    geom_tile() +
    scale_fill_viridis_c()


  state_experiments <- state_experiments %>%
    mutate(
      critter = pmap(
        list(
          sciname = scientific_name,
          habitat = habitat,
          seasonal_movement = seasonal_movement,
          f_v_m = f_v_m,
          adult_movement_sigma = adult_movement_sigma,
          recruit_movement_sigma = recruit_movement_sigma,
          rec_form = rec_form,
          hyper = hyper
        ),
        create_experiment_critters,
        marlin_inputs = marlin_inputs,
        seasons = seasons
      )
    )


  # aggregate into lists of fauna

  state_experiments <- state_experiments %>%
    group_by(state_id) %>%
    nest() %>%
    mutate(fauna = map(data, ~ .x$critter %>% set_names(.x$scientific_name)))

  check_pop_sizes <- map_dbl(state_experiments$fauna[[1]], "ssb0")

  tibble(sciname = names(check_pop_sizes), ssb0 = check_pop_sizes) %>%
    ggplot(aes(reorder(sciname, ssb0), ssb0)) +
    geom_col() +
    coord_flip()



  # create fleets -----------------------------------------------------------
  # create fleet objects

  state_experiments <- state_experiments %>%
    ungroup() %>%
    mutate(fleet = map2(fauna,data, compile_experiment_fleet))

  # add in starting conditions
  init_condit <- function(fauna, fleets, years = 100) {
    starting_trajectory <-
      simmar(fauna = fauna,
             fleets = fleets,
             years = years)

    # plot_marlin(check)

    starting_conditions <-
      starting_trajectory[length(starting_trajectory)]


    proc_starting_conditions <-
      process_marlin(starting_conditions, keep_age = FALSE)

    out <- list(starting_conditions = starting_conditions[[1]],
                proc_starting_conditions = proc_starting_conditions)

  }

  state_experiments <- state_experiments %>%
    mutate(tmp = map2(fauna, fleet, init_condit))

  state_experiments$starting_conditions <-
    map(state_experiments$tmp, "starting_conditions")

  assign_costs <- function(init_condit, fleets, fauna){

    starting <- init_condit

    revenues <-  map_df(starting, ~.x$r_p_a_fl %>%
                          reshape2::melt() %>%
                          group_by(Var3) %>%
                          summarise(revenue = sum(value, na.rm = TRUE))) %>%
      group_by(Var3) %>%
      rename(fleet = Var3) %>%
      summarise(revenue = sum(revenue))

    effort <- map_df(starting[1], ~.x$e_p_fl) %>%
      ungroup() %>%
      mutate(patch = 1:nrow(.)) %>%
      pivot_longer(-patch, names_to = "fleet", values_to = "effort") %>%
      group_by(fleet) %>%
      summarise(e2 = sum(((effort + 1)^fleets[[1]]$effort_cost_exponent)),
                check = sum(effort))


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

    return(fleets)

  }

  state_experiments$fleet <-
    pmap(
      list(
        init_condit = state_experiments$starting_conditions,
        fauna = state_experiments$fauna,
        fleets = state_experiments$fleet
      ),
      assign_costs
    )

  # reset initial conditions based on costs
  state_experiments <- state_experiments %>%
    mutate(tmp = map2(fauna, fleet, init_condit))


  state_experiments$starting_conditions <-
    map(state_experiments$tmp, "starting_conditions")

  state_experiments$proc_starting_conditions <-
    map(state_experiments$tmp, "proc_starting_conditions")

  state_experiments <- state_experiments %>%
    select(-tmp)


  placement_experiments <- expand_grid(
    placement_strategy = c(
      "depletion",
      "rate",
      "avoid_fishing",
      "target_fishing",
      "area"
    ),
    prop_mpa = seq(0, 1, by = 0.05),
    prop_critters_considered = 1,
    placement_error = c(0,.2)
  ) %>%
    group_by_at(colnames(.)[!colnames(.) %in% c("temp", "prop_mpa")]) %>%
    nest() %>%
    ungroup() %>%
    mutate(placement_id = 1:nrow(.)) %>%
    unnest(cols = data)
  # safety stop -------------------------------------------------------------

  if (safety_stop) {
    i <- sample(1:n_states, 1)
    # i = 1
    safety_sim <- simmar(
      fauna = state_experiments$fauna[[i]],
      fleets = state_experiments$fleet[[i]],
      years = years,
      mpas = list()
    )

    state_experiments$fleet[[i]]$longline$spatial_allocation
    proc_safety <- process_marlin(safety_sim, keep_age = FALSE)

    plot_marlin(proc_safety)

    space <-
      (
        plot_marlin(
          proc_safety,
          plot_type = "space",
          steps_to_plot = 1
        ) + labs(title = "Summer")
      ) +
      (
        plot_marlin(
          proc_safety,
          plot_type = "space",
          steps_to_plot = 1.5
        ) + labs(title = "Winter")
      ) & theme(strip.text = element_text(size = 6))
    space

    sample_mpas <- place_mpa(
      target_fauna = "carcharhinus longimanus",
      size = .9,
      fauna = state_experiments$fauna[[1]],
      placement_error = 0,
      place_randomly = FALSE
    )

    # sample_mpas <- place_mpa(target_fauna = "prionace glauca",
    #                          size = 0.2, fauna = fauna_frame$fauna[[1]])

    sample_mpas %>%
      ggplot(aes(x, y, fill = mpa)) +
      geom_tile()

    safety_mpa_sim <- simmar(
      fauna = state_experiments$fauna[[i]],
      fleets = state_experiments$fleet[[i]],
      mpas = list(locations = sample_mpas,
                  mpa_year = floor(years * .5)),
      years = years
    )

    proc_safety_mpa <-
      process_marlin(safety_mpa_sim, keep_age = FALSE)

    plot_marlin(no_mpa = proc_safety,
                with_mpa = proc_safety_mpa,
                plot_var = "ssb")

    plot_marlin(no_mpa = proc_safety,
                with_mpa = proc_safety_mpa,
                plot_var = "c",
                max_scale = FALSE)


  }

  # check distribution of starting conditions

  tmp <- map(state_experiments$starting_conditions, ~ map_df(.x,  ~ sum(.x$ssb_p_a) / .x$ssb0))

  check <-
    tibble(state_id = state_experiments$state_id, tmp = (tmp)) %>%
    unnest(cols = tmp) %>%
    pivot_longer(cols = -state_id,
                 names_to = "critter",
                 values_to = "depletion")

  check %>%
    ggplot(aes(depletion)) +
    geom_histogram() +
    facet_wrap( ~ critter)
  # generate MPA outcomes ---------------------------------------------------

  state_experiments %>%
    select(state_id, data) %>%
    write_rds(file = file.path(results_path, "state_experiments.rds"))

  write_rds(placement_experiments,
            file = file.path(results_path, "placement_experiments.rds"))


  future::plan(future::multisession, workers = experiment_workers)

  experiment_results <-
    vector(mode = "list", length = nrow(placement_experiments))


  pb <- progress_bar$new(
    format = "  Running Experiments [:bar] :percent eta: :eta",
    total = nrow(placement_experiments),
    clear = FALSE,
    width = 60
  )

  pb$tick(0)

  for (p in 1:nrow(placement_experiments)) {
    # memory problem trying to do it all at once so breaking it up a bit


    # a <- Sys.time()
    tmp <- state_experiments %>%
      ungroup() %>%
      mutate(
        results = future_pmap(
          list(
            starting_conditions = starting_conditions,
            proc_starting_conditions = proc_starting_conditions,
            fauna = fauna,
            fleets = fleet
          ),
          run_mpa_experiment,
          placement_strategy = placement_experiments$placement_strategy[p],
          prop_mpa = placement_experiments$prop_mpa[p],
          prop_critters_considered = placement_experiments$prop_critters_considered[p],
          placement_error = placement_experiments$placement_error[p],
          resolution = resolution,
          .options = furrr_options(seed = 42),
          .progress = FALSE
        )
      )

    tmp$results <-
      purrr::set_names(tmp$results, state_experiments$state_id)
    # Sys.time() - a

    experiment_results[[p]] <- tmp$results

    pb$tick()
    # message(
    #   glue::glue(
    #     "{scales::percent(p/nrow(placement_experiments), accuracy = 0.01)} done"
    #   )
    # )

  } # close p loop

  future::plan(future::sequential)


  raw_experiment_results <- experiment_results
  write_rds(experiment_results,
            file = file.path(results_path, "raw_experiment_results.rds"))
} else {
  raw_experiment_results <-
    read_rds(file = file.path(results_path, "raw_experiment_results.rds"))


  placement_experiments <-
    read_rds(file = file.path(results_path, "placement_experiments.rds"))

  state_experiments <-
    read_rds(file = file.path(results_path, "state_experiments.rds"))


}

group_var <- "placement_strategy"

results <- placement_experiments %>%
  mutate(temp = raw_experiment_results) %>%
  mutate(state_id = map(raw_experiment_results, names)) %>%
  unnest(cols = c(temp, state_id)) %>%
  mutate(obj = map(temp, "obj"),
         state_id = as.numeric(state_id)) %>%
  unnest(cols = obj) %>%
  mutate(
    critter = fct_recode(
      critter,
      "Shark" = "carcharhinus longimanus",
      "Tuna" = "katsuwonus pelamis",
      "Marlin" = "kajikia audax"
    )
  )

if (any(results$econ[results$prop_mpa == 1] > 0)) {
  stop("something has gone wrong: yields present with 100% mpa")
}


states <- state_experiments %>%
  select(state_id, data) %>%
  unnest(cols= data) %>%
  mutate(
    critter = fct_recode(
      scientific_name,
      "Shark" = "carcharhinus longimanus",
      "Tuna" = "katsuwonus pelamis",
      "Marlin" = "kajikia audax"
    )
  )



calc_centroid_distance <- function(tmp_state){

  distance_matrix <-
    expand_grid(x = 1:resolution, y = 1:resolution) %>%
    dist() %>%
    as.matrix()

  centroid_distances <- expand_grid(x = tmp_state$centroid, y = tmp_state$centroid) %>%
    rowwise() %>%
    mutate(distance = distance_matrix[x,y])

  cd <- sum(centroid_distances$distance)

}

calc_sigma_habitat <- function(tmp_state){


  habitat <- tmp_state %>%
    select(scientific_name, contains("sigma_"), habitat) %>%
    unnest(cols = habitat) %>%
    mutate(
      scientific_name = fct_recode(
        scientific_name,
        "Shark" = "carcharhinus longimanus",
        "Tuna" = "katsuwonus pelamis",
        "Marlin" = "kajikia audax"
      )
    ) %>%
    select(x,y,scientific_name,habitat) %>%
    pivot_wider(names_from = scientific_name, values_from = habitat) %>%
    select(-x,-y) %>%
    as.matrix()


  sigma_habitat <- sd(sqrt(habitat))

}



states <- states %>%
  group_by(state_id) %>%
  nest() %>%
  mutate(centroid_distance = map_dbl(data,calc_centroid_distance),
         sigma_habitat = map_dbl(data,calc_sigma_habitat)) %>%
  unnest(cols = data)


results <- results %>%
  left_join(states, by = c("state_id", "critter"))

# compare to BAU

results <- results %>%
  group_by(placement_id, state_id, critter) %>%
  mutate(bau_biodiv = biodiv[prop_mpa == 0],
         bau_econ = econ[prop_mpa == 0])

results %>%
  ggplot(aes(biodiv - bau_biodiv)) +
  geom_histogram()


excols <-
  state_experiments %>%
  select(state_id, data) %>%
  unnest(cols = data) %>%
  select(state_id:sigma_hab) %>%
  ungroup() %>%
  unique()


total_results <- results %>%
  group_by(
    placement_id,
    placement_strategy,
    prop_critters_considered,
    placement_error,
    state_id,
    prop_mpa
  ) %>%
  summarise(
    loss = mean(biodiv < bau_biodiv),
    n_loss = sum(biodiv < bau_biodiv),
    signif_loss = mean(biodiv < (0.9 * bau_biodiv)),
    total_loss = sum(biodiv - bau_biodiv),
    econ = sum(econ),
    biodiv = sum(biodiv),
    bau_econ = sum(bau_econ),
    bau_biodiv = sum(bau_biodiv)
  ) %>%
  ungroup() %>%
  mutate(delta_biodiv = biodiv / bau_biodiv - 1,
         abs_delta_biodiv = biodiv - bau_biodiv,
         delta_econ = econ / bau_econ - 1) %>%
  left_join(excols, by = "state_id")

total_results %>%
  ggplot(aes(biodiv - bau_biodiv)) +
  geom_histogram()

# total_results %>%
#   ggplot(
#     aes(
#       prop_critters_considered,
#       loss,
#       color = sigma_centroid,
#       alpha = sigma_hab,
#       group = interaction(state_id, placement_id)
#     )
#   ) +
#   geom_jitter(show.legend = FALSE) +
#   facet_wrap(~ placement_strategy)

# make an infographic summary

# find scenario with the least loss in ssb/ssb0

experiment_summary <- total_results %>%
  filter(prop_mpa < 0.5,
         !placement_strategy %in% c("area", "avoid_fishing"),
         placement_strategy == "rate",
         placement_error == min(placement_error)) %>%
  group_by(placement_id, state_id) %>%
  summarise(
    max_loss = max(signif_loss),
    min_loss = max(n_loss),
    total_gain = sum(abs_delta_biodiv)
  ) %>%
  ungroup()

worst_case <- experiment_summary %>%
  # filter(max_loss == max(max_loss)) %>%
  arrange(total_gain) %>%
  # filter(total_gain == min(total_gain)) %>%
  slice(2)


worst_state <- state_experiments %>%
  filter(state_id == worst_case$state_id)

worst_result <- results %>%
  filter(placement_id == worst_case$placement_id,
         state_id == worst_case$state_id,
         prop_mpa <0.5)

worst_result %>%
  ggplot(aes(biodiv - bau_biodiv)) +
  geom_histogram()

worst_result_plot <- worst_result %>%
  ggplot() +
  geom_text(data = tibble(
    x = .1,
    y = max(worst_result$biodiv - worst_result$bau_biodiv)
  ),
  aes(x = x, y = y, label = "Worst Case"),
  size = 4) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(aes(prop_mpa, biodiv - bau_biodiv, color = critter),
            show.legend = FALSE, size = 1.25) +
  scale_x_continuous(labels = scales::percent, name = "MPA Size") +
  scale_y_continuous(
    name = ''
  )


worst_habitat <- worst_state %>%
  select(state_id, data) %>%
  unnest(cols = data) %>%
  select(scientific_name, contains("sigma_"), habitat) %>%
  unnest(cols = habitat) %>%
  mutate(
    scientific_name = fct_recode(
      scientific_name,
      "Shark" = "carcharhinus longimanus",
      "Tuna" = "katsuwonus pelamis",
      "Marlin" = "kajikia audax"
    )
  )

worst_habitat_plot <- worst_habitat %>%
  ggplot(aes(x, y, z = (habitat), color = scientific_name)) +
  geom_contour(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank()
  ) #+
  # facet_wrap(~scientific_name)

best_case <- experiment_summary %>%
  filter(min_loss == min(min_loss, na.rm = TRUE)) %>%
  filter(total_gain == max(total_gain, na.rm = TRUE)) %>%
  slice(1)

best_state <- state_experiments %>%
  filter(state_id == best_case$state_id)

best_result <- results %>%
  filter(placement_id == best_case$placement_id,
         state_id == best_case$state_id,
         prop_mpa < 0.5)

best_result_plot <- best_result %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(aes(prop_mpa, biodiv - bau_biodiv, color = critter), size = 1.25) +
  geom_text(data = tibble(
    x = .1,
    y = max(best_result$biodiv - best_result$bau_biodiv)
  ),
  aes(x = x, y = y, label = "Best Case"),
  size = 4) +
  scale_x_continuous(labels = scales::percent, name = "") +
  scale_y_continuous("Change in Spawning Biomass") +
  theme(legend.position = "top") +
  scale_color_discrete(name = element_blank()) +
  labs(title = "Change in SSB/SSB0")


best_habitat <- best_state %>%
  select(state_id, data) %>%
  unnest(cols = data) %>%
  select(scientific_name, contains("sigma_"), habitat) %>%
  unnest(cols = habitat) %>%
  mutate(
    scientific_name = fct_recode(
      scientific_name,
      "Shark" = "carcharhinus longimanus",
      "Tuna" = "katsuwonus pelamis",
      "Marlin" = "kajikia audax"
    )
  )

best_habitat_plot <- best_habitat %>%
  ggplot(aes(x, y, z = (habitat), color = scientific_name)) +
  geom_contour(show.legend = FALSE) +
  labs(title = "Habitat Distribution") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank()
  ) # +
  # facet_wrap(~scientific_name)



fig_info <- (best_result_plot / worst_result_plot) |
  (best_habitat_plot / worst_habitat_plot)
fig_info
# find the scenario with the most loss in ssb/ssb0


# make other plots



# across all of these: potential is context dependent, so not going to be any super clear answers, but we did some digging for some of the clearest patterns. Include a shiny app so that people can explore.


# interesting message: lots of ways for at least some species to have loss across a range of MPA sizes
fig_1 <- total_results %>%
  ggplot(aes(prop_mpa, loss)) +
  geom_jitter(alpha = 0.25) +
  geom_bin2d() +
  facet_wrap( ~ placement_strategy) +
  scale_x_continuous(name = "% MPA", labels = scales::percent) +
  scale_y_continuous(name = "% Species With Loss", labels = scales::percent) +
  scale_fill_viridis_c()

# interesting message: lots of ways for at least some species to have loss across a range of MPA sizes. But, significant losses are much rarer. So, lots of ways to provide no effect
fig_2 <- total_results %>%
  ggplot(aes(prop_mpa, signif_loss)) +
  geom_jitter(alpha = 0.25) +
  geom_bin2d() +
  facet_wrap( ~ placement_strategy) +
  scale_x_continuous(name = "% MPA", labels = scales::percent) +
  scale_y_continuous(name = "% Species With Loss", labels = scales::percent) +
  scale_fill_viridis_c()


# effect of degree of habitat overlap.. collapse into something?
fig_3 <- total_results %>%
  filter(
    prop_critters_considered == max(prop_critters_considered),
    placement_error == min(placement_error)
  ) %>%
  ggplot(aes(prop_mpa, loss, color = (sigma_centroid))) +
  geom_jitter(show.legend = TRUE, alpha = 0.5) +
  facet_wrap(~ placement_strategy) +
  scale_color_viridis_c()

fig_4 <- total_results %>%
  filter(
    prop_critters_considered == max(prop_critters_considered),
    round(prop_mpa, 1) == 0.3
  ) %>%
  ggplot(aes(placement_error, loss, color = (sigma_centroid))) +
  geom_jitter() +
  facet_wrap(~ placement_strategy)

fig_5 <- total_results %>%
  filter(
    placement_error == min(placement_error),
    round(prop_mpa, 1) == 0.3
  ) %>%
  ggplot(aes(prop_critters_considered, loss, color = (sigma_centroid))) +
  geom_jitter() +
  facet_wrap(~ placement_strategy)

reg_1 <-
  glm(
    loss ~ sigma_hab + sigma_centroid + placement_error,
    data = total_results %>% filter(round(prop_mpa, 1) == 0.3)
  )


# fig_4 <- results %>%
#   left_join(excols, by = "xid") %>%
#   ggplot(aes(prop_mpa, biodiv - bau_biodiv, color = factor(sigma_centroid))) +
#   geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
#   geom_jitter(alpha = 0.1) +
#   facet_wrap( ~ critter, scales = "free_y") +
#   scale_x_continuous(labels = scales::percent, name = "% MPA") +
#   scale_y_continuous(name = "Change in SSB/SSB0")

total_results %>%
  ggplot(aes(total_loss, delta_econ)) +
  geom_point() +
  facet_wrap(~ placement_strategy)

total_results %>%
  filter(prop_mpa > 0,
         placement_error == 0,
         prop_critters_considered == 1) %>%
  ggplot(aes(prop_mpa, total_loss, color = factor(sigma_centroid))) +
  geom_jitter(show.legend = FALSE) +
  facet_wrap(~ placement_strategy)




fig_6 <- results %>%
  group_by(state_id, placement_id) %>%
  mutate(real_sigma_centroid = sd(centroid)) %>%
  ggplot(aes(
    prop_mpa,
    biodiv - bau_biodiv,
    color = seasonal_movement,
    group = interaction(placement_id, state_id)
  )) +
  geom_line(alpha = 0.2) +
  facet_grid(critter ~ placement_strategy) +
  scale_color_viridis_d()

fig_7 <- results %>%
  group_by(state_id, placement_id) %>%
  mutate(real_sigma_centroid = sd(centroid)) %>%
  ggplot(aes(
    prop_mpa,
    pmin(2,biodiv / bau_biodiv - 1),
    color = factor(f_v_m),
    group = interaction(placement_id, state_id)
  )) +
  geom_line(alpha = 0.2) +
  facet_grid(critter ~ placement_strategy) +
  scale_color_viridis_d()



fig_8 <- results %>%
  filter(prop_mpa < 1) %>%
  # filter(f_v_m > 1.5) %>%
  group_by(state_id, placement_id) %>%
  ggplot(aes(
    prop_mpa,
    pmin(2,biodiv / bau_biodiv -1),
    color =  sigma_habitat,
    group = interaction(placement_id, state_id)
  )) +
  geom_line(alpha = 0.5) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
  facet_grid(critter ~ placement_strategy) +
  scale_color_viridis_c(name = "Habitat Difference") +
  scale_y_continuous(labels = scales::percent,"Change in SSB/SSB0 relative to BAU") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), name = "MPA Size") +
  theme(legend.position = "top")


fig_9 <- results %>%
  group_by(state_id, placement_id) %>%
  mutate(real_sigma_centroid = sd(centroid)) %>%
  ggplot(aes(
    prop_mpa,
    pmin(2,biodiv / bau_biodiv -1),
    color = factor(spatial_allocation),
    group = interaction(placement_id, state_id)
  )) +
  geom_line(alpha = 0.3) +
  facet_grid(critter ~ placement_strategy)+
  scale_color_discrete(name = "Spatial Allocation Strategy") +
  theme(legend.position = "top") +
  scale_y_continuous(name = "MPA Effect on SSB/SSB0")
  # scale_color_viridis_d()

fig_9


# fig_10 <- results %>%
#   left_join(states, by = c("state_id", "critter")) %>%
#   group_by(state_id, placement_id) %>%
#   mutate(real_sigma_centroid = sd(centroid)) %>%
#   ggplot(aes(
#     prop_mpa,
#     biodiv - bau_biodiv,
#     color = spatial_q,
#     group = interaction(placement_id, state_id)
#   )) +
#   geom_line(alpha = 0.2) +
#   facet_grid(critter ~ placement_strategy) +
#   scale_color_viridis_d()
#
# fig_10


critter_results <- results %>%
  group_by(state_id, placement_id) %>%
  mutate(delta_biodiv = pmin(2,biodiv / bau_biodiv),
         loss = biodiv < bau_biodiv,
         real_loss = biodiv <= (0.9 * bau_biodiv))

critter_results %>%
  ggplot(aes(delta_biodiv)) +
  geom_histogram() +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0.9)


mean(critter_results$loss, na.rm = TRUE)


# save results ------------------------------------------------------------


# write_rds(experiments, file = file.path(results_path, "experiments.rds"))

# write_rds(results, file = file.path(results_path, "experiment_results.rds"))

write_rds(results %>% select(-temp),
          file = file.path(results_path, "compact_experiment_results.rds"))

write_rds(total_results,
          file = file.path(results_path, "total_experiment_results.rds"))

figs <- ls()[str_detect(ls(), "fig_")]

walk(figs, ~ ggsave(filename = file.path(results_path,paste0(.x,".pdf")), get(.x)))

#
# save(file = file.path(results_path, "experiment_figures.Rdata"),
#      list = figs)
