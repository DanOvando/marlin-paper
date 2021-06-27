create_experiment_critters <-
  function(sciname,
           habitat,
           seasons = 1,
           marlin_inputs,
           adult_movement_sigma = 10,
           recruit_movement_sigma = 20,
           seasonal_movement = FALSE,
           ontogenetic_shift = FALSE,
           f_v_m,
           rec_form) {
    # sciname <- marlin_inputs$scientific_name[[1]]
    #
    #

    # habitat <- experiments$habitat[[1]]
    tmp_inputs <- marlin_inputs %>%
      filter(scientific_name == sciname)

    hab <- habitat %>%
      pivot_wider(names_from = y, values_from = habitat) %>%
      select(-x) %>%
      as.matrix()

    # seasonal_movement <- sample(c(1, 0), 1, replace = TRUE)

    # uniform_rec_hab <- sample(c(1, 0), 1, replace = TRUE)


    # seasonal_movement <- 0
    if (seasonal_movement) {
      hab <- list(hab,-hab - (min(-hab)))

    } else {
      hab <- list(hab, hab)
    }

    # if (random_rec){
    #   rec_form <-  sample(c(0, 1), 1, replace = TRUE)
    #
    # } else {
    #   rec_form <- 2
    # }
    # rec_form <- 1 # sample(c(0, 1, 2, 3), 1, replace = TRUE)


    if (ontogenetic_shift) {
      recruit_habitat <-
        -hab[[1]] - min(-hab[[1]]) # place recruits in different places than adults

      # set recruitment form to allow for recruit habitat
      #
      rec_form <- 0
    } else {
      recruit_habitat <- hab[[1]]
    }

    critter <- marlin::create_critter(
      scientific_name = sciname,
      seasonal_habitat = hab,
      recruit_habitat = recruit_habitat,
      adult_movement = 0,
      adult_movement_sigma = adult_movement_sigma,
      recruit_movement_sigma = recruit_movement_sigma,
      rec_form = rec_form,
      fec_form = ifelse(str_detect(sciname,"carcharhinus"),"pups","weight"),
      pups = 6,
      # weight_a = ifelse(str_detect(sciname,"carcharhinus"),2,NA),
      seasons = seasons,
      steepness =  ifelse(is.nan(
        mean(tmp_inputs$steepness, na.rm = TRUE)
      ), 0.8, mean(tmp_inputs$steepness, na.rm = TRUE)),
      ssb0 = ifelse(is.nan(
        mean(tmp_inputs$ssb0 * 1000, na.rm = TRUE)
      ), 1e4, mean(tmp_inputs$ssb0 * 1000, na.rm = TRUE))
    )

    critter$init_explt = max(critter$m_at_age) * f_v_m * critter$steepness

      # ifelse(str_detect(sciname,"carcharhinus"),f_v_m / 2,f_v_m)


    return(critter)

  }
