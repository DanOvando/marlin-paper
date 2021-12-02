run_mpa_experiment <-
  function(placement_strategy = "depletion",
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
           resolution) {


    fleets <-  purrr::modify_in(fleets, list(2, "mpa_response"), ~ mpa_response)

    n_mpa <- round(prop_mpa * resolution^2)

    # set up open access

    # starting <- starting_conditions[[length(starting_conditions)]]

    if (is.na(critters_considered)){
      n_critters_considered <-  length(fauna)
    }

    critters_considered <- sample(names(fauna), n_critters_considered, replace = FALSE)

    if (placement_strategy == "depletion"){

      # place MPAs in proportion to depletion-weighted spawning stock biomass
      depletion <-
        (1 - (map_df(starting_conditions, ~ sum(.x$ssb_p_a) / .x$ssb0))) %>%  # depletion of each species
        pivot_longer(everything(), names_to = "critter", values_to = "weight")

      priorities <- proc_starting_conditions$fauna %>%
        left_join(depletion, by = "critter") %>%
        filter(critter %in% critters_considered) %>%
        group_by(critter) %>%
        mutate(patch_weight = ssb / sum(ssb) * weight) %>%
        group_by(patch) %>%
        summarise(patch_weight = sum(patch_weight)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(desc(patch_weight))

      # browser()
      # priorities %>%
      #   ggplot(aes(patch_weight, patch_weight2)) +
      #   geom_point()

    } else if (placement_strategy == "rate"){

      # place in proportino to depletion weighted catch relative to total catch. So, cells in which most of the catch comes from really depleted species, higher priority

      depletion <-
        (1 - (map_df(starting_conditions, ~ sum(.x$ssb_p_a) / .x$ssb0))) %>%  # depletion of each species
        pivot_longer(everything(), names_to = "critter", values_to = "weight")

      priorities <- proc_starting_conditions$fauna %>%
        left_join(depletion, by = "critter") %>%
        filter(critter %in% critters_considered) %>%
        group_by(patch) %>%
        summarise(patch_weight = sum(c * weight) / sum(c)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(desc(patch_weight))


      # priorities %>%
      #   ggplot(aes(patch_weight, patch_weight2)) +
      #   geom_point()


    } else if (placement_strategy == "avoid_fishing"){

      priorities <- proc_starting_conditions$fauna %>%
        filter(critter %in% critters_considered) %>%
        group_by(patch) %>%
        summarise(patch_weight = sum(c)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(patch_weight)



    } else if (placement_strategy == "target_fishing"){

      priorities <- proc_starting_conditions$fauna %>%
        filter(critter %in% critters_considered) %>%
        group_by(patch) %>%
        summarise(patch_weight = sum(c)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(desc(patch_weight))


    } else if (placement_strategy == "area"){

      priorities <- proc_starting_conditions$fauna %>%
        group_by(patch) %>%
        summarise(patch_weight = unique(patch)) %>%
        arrange((patch_weight))


    } else {
      stop("invalid placement strategy")
    }


    # place MPA

    if (n_mpa > 0){
    mpa_locs <- priorities$patch[1:n_mpa]
    } else {
      mpa_locs <- -999
    }

    mpas <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
      mutate(patch = 1:nrow(.)) %>%
      mutate(mpa = patch %in% mpa_locs)

    # mpas %>%
    #   ggplot(aes(x,y,fill = mpa)) +
    #   geom_tile()


    # run MPA simulation

    mpa_sim <- simmar(
      fauna = fauna,
      fleets = fleets,
      years = years,
      mpas = list(locations = mpas,
                  mpa_year = 1),
      initial_conditions = starting_conditions
    )

    # process results

    # outcomes <- process_marlin(mpa_sim,time_step = fauna[[1]]$time_step, keep_age = FALSE)

    #
    # plot_marlin(outcomes, max_scale = FALSE, plot_var = "c")

    biodiv <-
      (map_df(mpa_sim[[length(mpa_sim)]], ~ sum(.x$ssb_p_a) / .x$ssb0)) %>%
      pivot_longer(everything(), names_to = "critter",values_to = "biodiv")

    # print(max(biodiv$biodiv))
    if (any(biodiv$biodiv > 2) | any(is.na(biodiv$biodiv))){
      stop()
    }
    # calculate biodiversity component of objective function

    # econ <- sum(map_dbl(res, ~sum(.x$c_p_a))) #  calculate econ component of objective function


    # econ <-
    #   (map_df(mpa_sim[[length(mpa_sim)]], ~ sum(.x$r_p_a_fl, na.rm = TRUE))) %>%
    #   pivot_longer(everything(), names_to = "critter",values_to = "econ")
    # #  calculate econ component of objective function, currently revenues across all fleets and species


    econ <-
      (map_df(mpa_sim[[length(mpa_sim)]], ~ sum(.x$prof_p_fl, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "critter",values_to = "econ")
    #  calculate econ component of objective function, currently revenues across all fleets and species

    yield <-
      (map_df(mpa_sim[[length(mpa_sim)]], ~ sum(.x$c_p_fl, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "critter",values_to = "yield")
    #  calculate econ component of objective function, currently revenues across all fleets and species

    # out <- tibble(biodiv = biodiv, econ = econ)

    objective_outcomes <- biodiv %>%
      left_join(econ, by = "critter") %>%
      left_join(yield, by = "critter")

    outcomes <- list()

    outcomes$obj <- objective_outcomes

    outcomes$mpa <- mpas

    return(outcomes)

  } # close run_mpa_experiment
