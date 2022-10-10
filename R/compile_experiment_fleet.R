compile_experiment_fleet <-
  function(fauna,
           state,
           use_ports,
           port_locations,
           tune_type = "explt",
           effort_cost_exponent = 1) {
    if (all(state$spatial_q == TRUE)) {
      tuna_spatial_q <-
        state$habitat[state$scientific_name == "katsuwonus pelamis"][[1]] %>%
        pivot_wider(names_from = y, values_from = habitat) %>%
        select(-x) %>%
        as.matrix()

      marlin_spatial_q <-
        state$habitat[state$scientific_name == "kajikia audax"][[1]] %>%
        pivot_wider(names_from = y, values_from = habitat) %>%
        select(-x) %>%
        as.matrix()

      shark_spatial_q <-
        state$habitat[state$scientific_name == "carcharhinus longimanus"][[1]] %>%
        pivot_wider(names_from = y, values_from = habitat) %>%
        select(-x) %>%
        as.matrix()

    } else {
      tuna_spatial_q <- NA

      marlin_spatial_q <- NA

      shark_spatial_q <- NA

    }

    if (use_ports == TRUE){
      ports <- port_locations
    } else {
      ports <- NULL
    }

    fleets <- list(
      "longline" = create_fleet(
        list(
          "katsuwonus pelamis" = Metier$new(
            critter = fauna$`katsuwonus pelamis`,
            price = 1.36,
            sel_form = "logistic",
            sel_start = fauna$`katsuwonus pelamis`$length_50_mature *  sample(c(0.6), 1, replace = TRUE),
            sel_delta = 0.01,
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = tuna_spatial_q

          ),
          "kajikia audax" = Metier$new(
            critter = fauna$`kajikia audax`,
            price = 5.16,
            sel_form = "logistic",
            sel_start = fauna$`kajikia audax`$length_50_mature *  sample(c(0.7), 1, replace = TRUE),
            sel_delta = 0.01,
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = marlin_spatial_q

          ),
          "carcharhinus longimanus" = Metier$new(
            critter = fauna$`carcharhinus longimanus`,
            price = 0,
            sel_form = "logistic",
            sel_start = fauna$`carcharhinus longimanus`$length_50_mature *
              sample(c(0.9), 1, replace = TRUE),
            sel_delta = 0.01,
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = shark_spatial_q
          )
        ),
        base_effort = resolution ^ 2,
        cost_per_unit_effort = 10,
        cost_per_distance = 20,
        effort_cost_exponent = effort_cost_exponent,
        spatial_allocation = state$spatial_allocation[1],
        resolution = resolution,
        fleet_model = state$fleet_model[1],
        mpa_response = state$mpa_response[1],
        ports = ports
      )
    )

    fleets <- tune_fleets(fauna, fleets, tune_type = tune_type)
  }
