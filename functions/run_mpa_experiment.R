run_mpa_experiment <-
  function(placement_strategy = "depletion",
           fleet_model = "constant effort",
           starting_conditions,
           proc_starting_conditions,
           prop_mpa = 0.3,
           fauna,
           fleets,
           placement_error = 0,
           mpa_response = "stay",
           critters_considered = NA,
           random_mpas = FALSE,
           max_delta = 1,
           resolution,
           years = 50,
           future_habitat = list(),
           spawning_ground = spawning_ground,
           effort_cap) {

    options(dplyr.summarise.inform = FALSE)


    fleets <-  purrr::modify_in(fleets, list(1, "mpa_response"), ~ mpa_response)

    if (!is.na(fleet_model)) {
      fleets <-
        purrr::modify_in(fleets, list(1, "fleet_model"), ~ fleet_model)


    }

    n_mpa <- round(prop_mpa * resolution^2)

    # set up open access

    #
    if (all(is.na(critters_considered))){
      critters_considered <-  length(fauna)
    }

    critters_considered <- sample(names(fauna), critters_considered, replace = FALSE)

    if (placement_strategy == "depletion"){

      # place MPAs in proportion to depletion-weighted spawning stock biomass
      depletion <-
        map_df(starting_conditions[1], ~ (1 - (map_df(.x, ~ sum(.x$ssb_p_a) / .x$ssb0))) %>%  # depletion of each species
        pivot_longer(everything(), names_to = "critter", values_to = "weight"), .id = "step") %>%
        select(-step)

      priorities <- proc_starting_conditions$fauna %>%
        left_join(depletion, by = "critter") %>%
        filter(critter %in% critters_considered) %>%
        group_by(critter, step) %>%
        mutate(patch_weight = (ssb / sum(ssb)) * weight) %>%
        group_by(patch, step) %>%
        summarise(patch_weight = sum(patch_weight)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(desc(patch_weight))

      # decision: pick MPA basis step based on the step with the highest variation in patch weights, e.g. spawning aggregation season should have higher varition thatn a uniform situation
      step_priority <- priorities %>%
        group_by(step) %>%
        summarise(pwsd = sd(patch_weight)) %>%
        filter(pwsd == max(pwsd))

      priorities <-  priorities %>%
        filter(step == step_priority$step[1]) %>%
        arrange(desc(patch_weight))

    } else if (placement_strategy == "rate"){

      # place in proportion to depletion weighted catch relative to total catch. So, cells in which most of the catch comes from really depleted species, higher priority
      depletion <-
        map_df(
          starting_conditions[1],
          ~ (1 - (map_df(
            .x, ~ sum(.x$ssb_p_a) / .x$ssb0
          ))) %>%  # depletion of each species
            pivot_longer(everything(), names_to = "critter", values_to = "weight"),
          .id = "step"
        ) %>%
        select(-step)

      priorities <- proc_starting_conditions$fauna %>%
        left_join(depletion, by = "critter") %>%
        filter(critter %in% critters_considered) %>%
        group_by(patch, step) %>%
        summarise(patch_weight = sum(c * weight) / sum(c)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(desc(patch_weight))

      step_priority <- priorities %>%
        group_by(step) %>%
        summarise(pwsd = sd(patch_weight)) %>%
        filter(pwsd == max(pwsd))

      priorities <-  priorities %>%
        filter(step == step_priority$step[1]) %>%
        arrange(desc(patch_weight))



    } else if (placement_strategy == "avoid_fishing"){

      priorities <- proc_starting_conditions$fauna %>%
        filter(critter %in% critters_considered) %>%
        group_by(patch, step) %>%
        summarise(patch_weight = sum(c)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(patch_weight)

      step_priority <- priorities %>%
        group_by(step) %>%
        summarise(pwsd = sd(patch_weight)) %>%
        filter(pwsd == max(pwsd))

      priorities <-  priorities %>%
        filter(step == step_priority$step[1]) %>%
        arrange((patch_weight))




    } else if (placement_strategy == "target_fishing"){

      priorities <- proc_starting_conditions$fauna %>%
        filter(critter %in% critters_considered) %>%
        group_by(patch, step) %>%
        summarise(patch_weight = sum(c)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(desc(patch_weight))


      step_priority <- priorities %>%
        group_by(step) %>%
        summarise(pwsd = sd(patch_weight)) %>%
        filter(pwsd == max(pwsd))

      priorities <-  priorities %>%
        filter(step == step_priority$step[1]) %>%
        arrange(desc(patch_weight))



    } else if (placement_strategy == "area"){

      priorities <- proc_starting_conditions$fauna %>%
        group_by(patch) %>%
        summarise(patch_weight = unique(patch)) %>%
        arrange((patch_weight))


    } else if (placement_strategy == "spawning_ground"){

      priorities <- spawning_ground |>
        mutate(patch = 1:length(x)) %>%
        arrange(desc(habitat))


    } else if (placement_strategy == "coast"){

      priorities <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
        mutate(patch = 1:nrow(.)) |>
        mutate(distance = x - 1) |>
        arrange(distance)


    }else {
      stop("invalid placement strategy")
    }


    # place MPA
    if (n_mpa > 0){
    mpa_locs <- priorities$patch[1:n_mpa]
    } else {
      mpa_locs <- -999
    }

    ssb0s <- map_df(fauna, "ssb0_p", .id = "critter")

    ssb0s$ssb0 <- rowSums(ssb0s)

    mpas <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
      mutate(patch = 1:nrow(.)) %>%
      mutate(mpa = patch %in% mpa_locs) %>%
      bind_cols(ssb0s)

    # mpas |>
    #   ggplot(aes(x,y,fill = mpa)) +
    #   geom_tile()
    #
    # run MPA simulation

    # starting_step = as.numeric(last(names(starting_conditions)))

    starting_step = as.numeric(gsub("step_","",last(names(starting_conditions))))


    mpa_sim <- simmar(
      fauna = fauna,
      fleets = fleets,
      years = years,
      manager = list(mpas = list(locations = mpas,
                  mpa_year = ceiling(starting_step)),
                  effort_cap = effort_cap),
      habitat = future_habitat,
      starting_step = starting_step,
      keep_starting_step = FALSE,
      initial_conditions = starting_conditions[[length(starting_conditions)]]
    )


    # grid <- expand_grid(x = 1:resolution, y= 1:resolution) %>%
    #   mutate(patch = 1:nrow(.))
    #
    # patch_biomass <-
    #   map_df(mpa_sim, ~ map_df(.x, ~tibble(biomass = rowSums(.x$ssb_p_a), patch = 1:nrow(.x$ssb_p_a)), .id = "critter"), .id = "step") %>%
    #   mutate(step = as.numeric(step)) %>%
    #   left_join(grid, by = "patch")

    # patch_biomass %>%
    #   group_by(critter, step) %>%
    #   mutate(sbiomass = biomass / sum(biomass)) %>%
    #   ungroup() %>%
    #   filter(step == max(step)) %>%
    #   ggplot(aes(x,y, fill = sbiomass))+
    #   geom_tile() +
    #   facet_grid(critter~step) +
    #   scale_fill_viridis_c()
    #
    #
    # browser()
    #
    #
    # effort <-
    #   map_df(mpa_sim, ~ data.frame(effort = sum(.x$snapper$e_p_fl$fleet_two)), .id = "step") %>%
    #   mutate(step = as.numeric(step))
    #
    # effort %>%
    #   ungroup() %>%
    #   ggplot(aes(step, effort)) +
    #   geom_line()


    # process results

    distance_to_mpa <- marlin::get_distance_to_mpas(mpas, resolution = resolution)

    final_step <- mpa_sim[[length(mpa_sim)]]

    b_p = map_df(final_step, ~ data.frame(b_p = rowSums(.x$b_p_a), patch = 1:nrow(.x$b_p_a)), .id = "critter")

    response_ratio <- b_p |>
      group_by(patch) |>
      summarise(biomass = sum(b_p)) |>
      left_join(mpas |> select(x,y,patch, mpa, ssb0), by = "patch") |>
      group_by(mpa) |>
      summarise(biomass = mean(biomass))

    # browser()

    # rr <- broom::tidy(lm(log(biomass) ~ mpa + ssb0, data = response_ratio))

    biodiv <-
      (map_df(mpa_sim[[length(mpa_sim)]], ~ sum(.x$ssb_p_a) / .x$ssb0)) %>%
      pivot_longer(everything(), names_to = "critter",values_to = "biodiv")

    if (any(biodiv$biodiv > 2) | any(is.na(biodiv$biodiv))){
      stop()
    }


    fleet_econ <- map_df(mpa_sim[[length(mpa_sim)]], ~ colSums(.x$prof_p_fl, na.rm = TRUE), .id = "critter")

    econ <-
      (map_df(mpa_sim[[length(mpa_sim)]], ~ sum(.x$prof_p_fl, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "critter",values_to = "econ")
    #  calculate econ component of objective function, currently revenues across all fleets and species

    fleet_yield <- (map_df(mpa_sim[[length(mpa_sim)]], ~ colSums(.x$c_p_fl, na.rm = TRUE), .id = "critter"))

    yield <-
      (map_df(mpa_sim[[length(mpa_sim)]], ~ sum(.x$c_p_fl, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "critter",values_to = "yield")
    #  calculate econ component of objective function, currently revenues across all fleets and species

    objective_outcomes <- biodiv %>%
      left_join(econ, by = "critter") %>%
      left_join(yield, by = "critter") %>%
      left_join(fleet_yield, by = "critter")

    outcomes <- list()

    outcomes$obj <- objective_outcomes

    outcomes$mpa <- mpas

    outcomes$response_ratio <- response_ratio

    return(outcomes)

  } # close run_mpa_experiment
