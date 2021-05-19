source(file.path("scripts", "00_setup.R"))


# for now let's leave the life history stuff out of it... and then once you have a sense of timing for this decide on how many iterations to do of each
experiments <-
  expand_grid(
    sigma_centroid = seq(.25 * resolution, resolution ^ 2 / 2, length.out = 5),
    sigma_hab = c(20, 5),
    ontogenetic_shift = c(TRUE, FALSE),
    seasonal_movement = c(TRUE, FALSE)
  ) %>%
  mutate(xid = 1:nrow(.))


# function to assign habitat for each species based on

critters <- tibble(scientific_name = unique(marlin_inputs$scientific_name)) %>%
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


experiments <- experiments %>%
  mutate(
    habitats = map2(
      sigma_centroid,
      sigma_hab,
      create_critter_habitats,
      critters = critters,
      resolution = resolution
    )
  ) %>%
  unnest(cols = habitats)

experiments$habitat[[3]] %>%
  ggplot(aes(x, y, fill = habitat)) +
  geom_tile() +
  scale_fill_viridis_c()


experiments <- experiments %>%
  mutate(critter = pmap(
    list(
      sciname = scientific_name,
      habitat = habitat,
      ontogenetic_shift = ontogenetic_shift,
      seasonal_movement = seasonal_movement
    ),
    create_critters,
    marlin_inputs = marlin_inputs,
    seasons = seasons
  ))


# aggregate into lists of fauna

experiments <- experiments %>%
  group_by(xid) %>%
  nest() %>%
  mutate(fauna = map(data, ~ .x$critter %>% set_names(.x$scientific_name)))

check_pop_sizes <- map_dbl(experiments$fauna[[1]], "ssb0")

tibble(sciname = names(check_pop_sizes), ssb0 = check_pop_sizes) %>%
  ggplot(aes(reorder(sciname, ssb0), ssb0)) +
  geom_col() +
  coord_flip()



# create fleets -----------------------------------------------------------
# create fleet objects

experiments <- experiments %>%
  ungroup() %>%
  mutate(fleet = map(fauna, compile_fleet))

# add in starting conditions

init_condit <- function(fauna, fleets, years = 100) {

starting_trajectory <- simmar(fauna = fauna, fleets = fleets, years = years)

# plot_marlin(check)

starting_conditions <- starting_trajectory[length(starting_trajectory)]


proc_starting_conditions <- process_marlin(starting_conditions, keep_age = FALSE)

out <- list(starting_conditions = starting_conditions,
            proc_starting_conditions = proc_starting_conditions)

}

experiments <- experiments %>%
  mutate(tmp = map2(fauna, fleet, init_condit))

experiments$starting_conditions <- map(experiments$tmp,"starting_conditions")

experiments$proc_starting_conditions <- map(experiments$tmp,"proc_starting_conditions")

experiments <- experiments %>%
  select(-tmp)


placement_experiment <- expand_grid(
placement_strategy = c("depletion", "rate", "avoid_fishing", "target_fishing", "area"),
prop_mpa = seq(0, 1, by = 0.05),
prop_critters_considered = seq(.1,1, length.out = 4),
placement_error = seq(0,2, length.out = 5)
)


# placement_experiment <- placement_experiment %>%
#   group_by_at(colnames(.)[colnames(.) != "prop_mpa"]) %>%
#   nest() %>%
#   ungroup() %>%
#   mutate(placement_id = 1:nrow(.)) %>%
#   unnest(cols = data) %>%
#   arrange(prop_mpa)

# a <- experiments
#
#
# write_rds(critter_lookup, file = file.path(results_path, 'critter-lookup.rds'))

# safety stop -------------------------------------------------------------

if (safety_stop) {
    safety_sim <- marlin::simmar(fauna = experiments$fauna[[19]],
                                 fleets = experiments$fleet[[19]],
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
      size = 0.2,
      fauna = experiments$fauna[[1]],
      placement_error = 0,
      place_randomly = FALSE
    )

    # sample_mpas <- place_mpa(target_fauna = "prionace glauca",
    #                          size = 0.2, fauna = fauna_frame$fauna[[1]])

    sample_mpas %>%
      ggplot(aes(x, y, fill = mpa)) +
      geom_tile()

    safety_mpa_sim <- marlin::simmar(
      fauna = experiments$fauna[[1]],
      fleets = experiments$fleet[[1]],
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

    ggsave(
      "test-space.pdf",
      plot = space,
      height = 20,
      width = 10
    )

}
# tune system -------------------------------------------------------------


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

nrow(experiments)
if (run_experiments) {

  future::plan(future::multisession, workers = 8)

  experiment_results <- vector(mode = "list", length = nrow(placement_experiment))

  for (p in 1:nrow(placement_experiment)){ # memory problem trying to do it all at once so breaking it up a bit


    # a <- Sys.time()
    tmp <- experiments %>%
      ungroup() %>%
      mutate(results = future_pmap(list(
                                        starting_conditions = starting_conditions,
                                        proc_starting_conditions = proc_starting_conditions,
                                        fauna = fauna,
                                        fleets = fleet),
                                   run_mpa_experiment,
                                   placement_strategy = placement_experiment$placement_strategy[p],
                                   prop_mpa = prop_mpa[p],
                                   prop_critters_considered = placement_experiment$prop_critters_considered[p],
                                   placement_error = placement_experiment$placement_error[p],
                                   resolution = resolution,
                                   .options = furrr_options(seed = 42),
                                   .progress = FALSE))

    tmp$results <- purrr::set_names(tmp$results, experiments$xid)
    # Sys.time() - a

    experiment_results[[p]] <- tmp$results

    message(glue::glue("{scales::percent(p/nrow(placement_experiment), accuracy = 0.01)} done"))

  } # close p loop



  future::plan(future::sequential)

  write_rds(experiment_results, file = file.path(results_path, "experiment_results.rds"))
} else {
  experiment_results <-
    read_rds(file = file.path(results_path, "experiment_results.rds"))
}
# (diff / nrow(experiments)) * 60
#
tmp <- experiment_results  %>%
  map(~.x$obj)


group_var <- "placement_strategy"



  results <- placement_experiment %>%
  mutate(temp = experiment_results) %>%
  group_by_at(colnames(.)[!colnames(.) %in%c("temp","prop_mpa")]) %>%
    nest() %>%
    ungroup() %>%
    mutate(placement_id = 1:nrow(.)) %>%
    unnest(cols = data) %>%
  mutate(xid = map(experiment_results,names)) %>%
  unnest(cols = c(temp, xid)) %>%
  mutate(obj = map(temp, "obj"),
         xid = as.numeric(xid)) %>%
  unnest(cols = obj)

  if (any(results$econ[results$prop_mpa == 1] > 0)){
    stop("something has gone wrong: yields present with 100% mpa")
  }

  warning("move placement experiement id back above run_experiments")

  # total_experiment <- placement_experiment %>%
  #   left_join(experiments, by = 'xid')


  # results <- results %>%
  #   left_join(placement_experiment)

# compare to BAU

results <- results %>%
  group_by(placement_id, xid, critter) %>%
  mutate(bau_biodiv = biodiv[prop_mpa == 0],
         bau_econ = econ[prop_mpa == 0])


excols <-
  experiments %>%
  select(xid, data) %>%
  unnest(cols = data) %>%
  select(-habitat, -critter,-scientific_name,-centroid) %>%
  ungroup() %>%
  unique()


total_results <- results %>%
  group_by(placement_id,placement_strategy,prop_critters_considered,placement_error, xid, prop_mpa) %>%
  summarise(
    loss = mean(biodiv < bau_biodiv),
    total_loss = sum(biodiv - bau_biodiv),
    econ = sum(econ),
    biodiv = sum(biodiv),
    bau_econ = sum(bau_econ),
    bau_biodiv = sum(bau_biodiv)) %>%
  ungroup() %>%
  mutate(delta_biodiv = biodiv / bau_biodiv - 1,
         delta_econ = econ / bau_econ - 1) %>%
  left_join(excols, by = "xid")


total_results %>%
  ggplot(aes(total_loss, delta_econ)) +
  geom_point() +
  facet_wrap(~placement_strategy)

total_results %>%
  filter(prop_mpa > 0,
         placement_error == 0,
         prop_critters_considered == 1) %>%
  ggplot(aes(prop_mpa, total_loss, color = factor(sigma_centroid))) +
  geom_jitter() +
  facet_wrap(~placement_strategy)


results %>%
  left_join(excols, by = "xid") %>%
  filter(prop_mpa > 0,
         placement_error == 0,
         placement_strategy == "depletion") %>%
  ggplot(aes(prop_mpa, biodiv - bau_biodiv, color = factor(sigma_centroid))) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
  geom_jitter(alpha = 0.25) +
  facet_wrap(~critter, scales = "free_y") +
  scale_x_continuous(labels = scales::percent, name = "% MPA") +
  scale_y_continuous(name = "Change in SSB/SSB0")

a = lm(loss ~ sigma_centroid*prop_mpa,data = total_results)
