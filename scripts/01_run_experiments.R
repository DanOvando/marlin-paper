source(file.path("scripts", "00_setup.R"))


# generate state experiments. This is a somewhat tricky process where the actual generated values are created for many variables in create_experiment_critters

n_states <- 250

state_experiments <-
  tibble(
    sigma_centroid = runif(n_states,.1 * resolution, resolution ^ 2 / 2),
    sigma_hab = runif(n_states,.1 * resolution, resolution ^ 2 / 8)
  ) %>%
  mutate(state_id = 1:nrow(.))


# function to assign habitat for each species based on

critters <-
  tibble(scientific_name = c("katsuwonus pelamis","kajikia audax","carcharhinus longimanus")) %>%
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
  mutate(seasonal_movement = sample(c(TRUE, FALSE), nrow(.), replace = TRUE),
         f_v_m = runif(nrow(.), .1,3),
         adult_movement_sigma = runif(nrow(.), min = 0.05 * resolution, max = 3 * resolution),
         recruit_movement_sigma = runif(nrow(.), min = 0.05 * resolution, max = 3 * resolution),
         rec_form = sample(c(0,1,2,3), nrow(.), replace = TRUE)
         )

state_experiments$habitat[[sample(1:n_states,1)]] %>%
  ggplot(aes(x, y, fill = habitat)) +
  geom_tile() +
  scale_fill_viridis_c()


state_experiments <- state_experiments %>%
  mutate(critter = pmap(
    list(
      sciname = scientific_name,
      habitat = habitat,
      seasonal_movement = seasonal_movement,
      f_v_m = f_v_m,
      adult_movement_sigma = adult_movement_sigma,
      recruit_movement_sigma = recruit_movement_sigma,
      rec_form = rec_form
    ),
    create_experiment_critters,
    marlin_inputs = marlin_inputs,
    seasons = seasons
  ))


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
  mutate(fleet = map(fauna, compile_experiment_fleet))

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

  out <- list(starting_conditions = starting_conditions,
              proc_starting_conditions = proc_starting_conditions)

}

state_experiments <- state_experiments %>%
  mutate(tmp = map2(fauna, fleet, init_condit))

state_experiments$starting_conditions <-
  map(state_experiments$tmp, "starting_conditions")

state_experiments$proc_starting_conditions <-
  map(state_experiments$tmp, "proc_starting_conditions")

state_experiments <- state_experiments %>%
  select(-tmp)


placement_experiments <- expand_grid(
  placement_strategy = c("depletion", "rate", "avoid_fishing", "target_fishing", "area"),
  prop_mpa = seq(0, 1, by = 0.05),
  prop_critters_considered = 1,
  placement_error = c(0,.1,.5)
)

# safety stop -------------------------------------------------------------

if (safety_stop) {
  i <- sample(1:n_states,1)
  safety_sim <- marlin::simmar(fauna = state_experiments$fauna[[i]],
                               fleets = state_experiments$fleet[[i]],
                               years = years)
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
    size = 0.9,
    fauna = state_experiments$fauna[[1]],
    placement_error = 0,
    place_randomly = FALSE
  )

  # sample_mpas <- place_mpa(target_fauna = "prionace glauca",
  #                          size = 0.2, fauna = fauna_frame$fauna[[1]])

  sample_mpas %>%
    ggplot(aes(x, y, fill = mpa)) +
    geom_tile()

  safety_mpa_sim <- marlin::simmar(
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
              plot_var = "c")


}

# check distribution of starting conditions

tmp <- map(state_experiments$starting_conditions,1)

tmp <- map(tmp, ~map_df(.x,~sum(.x$ssb_p_a) / .x$ssb0))

check <- tibble(state_id = state_experiments$state_id, tmp = (tmp)) %>%
  unnest(cols = tmp) %>%
  pivot_longer(cols = -state_id, names_to = "critter", values_to = "depletion")

check %>%
  ggplot(aes(depletion)) +
  geom_histogram() +
  facet_wrap(~critter)

# generate MPA outcomes ---------------------------------------------------

if (run_experiments) {
  future::plan(future::multisession, workers = 8)

  experiment_results <-
    vector(mode = "list", length = nrow(placement_experiments))


  pb <- progress_bar$new(
    format = "  Running Experiments [:bar] :percent eta: :eta",
    total = nrow(placement_experiments), clear = FALSE, width= 60)

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

    tmp$results <- purrr::set_names(tmp$results, state_experiments$state_id)
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
}
# (diff / nrow(experiments)) * 60
#


group_var <- "placement_strategy"


results <- placement_experiments %>%
  mutate(temp = raw_experiment_results) %>%
  group_by_at(colnames(.)[!colnames(.) %in% c("temp", "prop_mpa")]) %>%
  nest() %>%
  ungroup() %>%
  mutate(placement_id = 1:nrow(.)) %>%
  unnest(cols = data) %>%
  mutate(state_id = map(raw_experiment_results, names)) %>%
  unnest(cols = c(temp, state_id)) %>%
  mutate(obj = map(temp, "obj"),
         state_id = as.numeric(state_id)) %>%
  unnest(cols = obj)

if (any(results$econ[results$prop_mpa == 1] > 0)) {
  stop("something has gone wrong: yields present with 100% mpa")
}

warning("move placement experiement id back above run_experiments")

# total_experiment <- placement_experiment %>%
#   left_join(experiments, by = 'xid')


# results <- results %>%
#   left_join(placement_experiment)

# compare to BAU

results <- results %>%
  group_by(placement_id, state_id, critter) %>%
  mutate(bau_biodiv = biodiv[prop_mpa == 0],
         bau_econ = econ[prop_mpa == 0])


excols <-
  state_experiments %>%
  select(state_id, data) %>%
  unnest(cols = data) %>%
  select(-habitat,-critter, -scientific_name, -centroid) %>%
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
    signif_loss = mean(biodiv < (0.9 * bau_biodiv)),
    total_loss = sum(biodiv - bau_biodiv),
    econ = sum(econ),
    biodiv = sum(biodiv),
    bau_econ = sum(bau_econ),
    bau_biodiv = sum(bau_biodiv)
  ) %>%
  ungroup() %>%
  mutate(delta_biodiv = biodiv / bau_biodiv - 1,
         delta_econ = econ / bau_econ - 1) %>%
  left_join(excols, by = "state_id")

total_results %>%
  ggplot(aes(
    prop_critters_considered,
    loss,
    color = sigma_centroid,
    alpha = sigma_hab,
    group = interaction(state_id, placement_id)
  )) +
  geom_jitter(show.legend = FALSE) +
  facet_wrap( ~ placement_strategy)

# across all of these: potential is context dependent, so not going to be any super clear answers, but we did some digging for some of the clearest patterns. Include a shiny app so that people can explore.


# interesting message: lots of ways for at least some species to have loss across a range of MPA sizes
fig_1 <- total_results %>%
  ggplot(aes(prop_mpa, loss)) +
  geom_jitter(alpha = 0.25) +
  geom_bin2d() +
  facet_wrap(~ placement_strategy) +
  scale_x_continuous(name = "% MPA", labels = scales::percent) +
  scale_y_continuous(name = "% Species With Loss", labels = scales::percent) +
  scale_fill_viridis_c()

# interesting message: lots of ways for at least some species to have loss across a range of MPA sizes. But, significant losses are much rarer. So, lots of ways to provide no effect
fig_2 <- total_results %>%
  ggplot(aes(prop_mpa, signif_loss)) +
  geom_jitter(alpha = 0.25) +
  geom_bin2d() +
  facet_wrap(~ placement_strategy) +
  scale_x_continuous(name = "% MPA", labels = scales::percent) +
  scale_y_continuous(name = "% Species With Loss", labels = scales::percent) +
  scale_fill_viridis_c()


# effect of degree of habitat overlap.. collapse into something?
fig_3 <- total_results %>%
  filter(
    prop_critters_considered == max(prop_critters_considered),
    placement_error == min(placement_error),
    seasonal_movement == FALSE) %>%
  ggplot(aes(prop_mpa, loss, color = (sigma_centroid))) +
  geom_jitter(show.legend = TRUE, alpha = 0.5) +
  facet_wrap( ~ placement_strategy) +
  scale_color_viridis_c()

fig_4 <- total_results %>%
  filter(
    prop_critters_considered == max(prop_critters_considered),
    seasonal_movement == FALSE,
    round(prop_mpa, 1) == 0.3
  ) %>%
  ggplot(aes(placement_error, loss, color = (sigma_centroid))) +
  geom_jitter() +
  facet_wrap( ~ placement_strategy)

fig_5 <- total_results %>%
  filter(
    placement_error == min(placement_error),
    seasonal_movement == FALSE,
    round(prop_mpa, 1) == 0.3
  ) %>%
  ggplot(aes(prop_critters_considered, loss, color = (sigma_centroid))) +
  geom_jitter() +
  facet_wrap( ~ placement_strategy)

reg_1 <-
  glm(
    loss ~ sigma_hab + sigma_centroid + placement_error + prop_critters_considered  + seasonal_movement,
    data = total_results %>% filter(round(prop_mpa, 1) == 0.3)
  )


summary(reg_1)

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
  facet_wrap( ~ placement_strategy)

total_results %>%
  filter(prop_mpa > 0,
         placement_error == 0,
         prop_critters_considered == 1) %>%
  ggplot(aes(prop_mpa, total_loss, color = factor(sigma_centroid))) +
  geom_jitter() +
  facet_wrap( ~ placement_strategy)


tmp <- results %>%
  left_join(excols, by = "state_id") %>%
  ggplot(aes(prop_mpa, biodiv - bau_biodiv)) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
  geom_bin2d(binwidth = .1) +
  facet_grid( critter ~ placement_strategy) +
  scale_x_continuous(labels = scales::percent, name = "% MPA") +
  scale_y_continuous(name = "Change in SSB/SSB0") +
  scale_fill_viridis_c(trans = "log10")

tmp
# save results ------------------------------------------------------------

write_rds(placement_experiments, file = file.path(results_path, "placement_experiments.rds"))


# write_rds(experiments, file = file.path(results_path, "experiments.rds"))

# write_rds(results, file = file.path(results_path, "experiment_results.rds"))

write_rds(total_results,
          file = file.path(results_path, "total_experiment_results.rds"))

figs <- ls()[str_detect(ls(), "fig_")]
#
save(file = file.path(results_path, "experiment_figures.Rdata"),
     list = figs)
