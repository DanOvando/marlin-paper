create_critters <-
  function(sciname,
           habitat,
           seasons = 1,
           marlin_inputs,
           seasonal_movement = FALSE,
           ontogenetic_shift = FALSE,
           random_rec = FALSE) {
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

    if (random_rec){
    rec_form <-  sample(c(0, 1,2,3), 1, replace = TRUE)

    } else {
      rec_form <- 2 # local density dependence then disperse recruits per recruit movement
    }
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
      adult_movement_sigma = 4,
      recruit_movement_sigma = 2,
      rec_form = rec_form,
      fec_form = ifelse(str_detect(sciname,"carcharhinus"),"pups","weight"),
      pups = 6,
      seasons = seasons,
      init_explt = ifelse(is.nan(
        mean(tmp_inputs$current_f, na.rm = TRUE)
      ), .4, mean(tmp_inputs$current_f, na.rm = TRUE)),
      steepness =  ifelse(is.nan(
        mean(tmp_inputs$steepness, na.rm = TRUE)
      ), 0.8, mean(tmp_inputs$steepness, na.rm = TRUE)),
      ssb0 = ifelse(is.nan(
        mean(tmp_inputs$ssb0 * 1000, na.rm = TRUE)
      ), 1e4, mean(tmp_inputs$ssb0 * 1000, na.rm = TRUE))
    )


    # # later sub in a lookup table for this
    # trait_frame <- tibble(
    #   seasonal_habitat = list(habitat$data[[which(habitat$species_sciname == sciname)]]),
    #   adult_movement = 1,
    #   adult_movement_sigma = runif(1,min = 1, max = 30),
    #   seasons = seasons,
    #   init_explt = sample(c(.05,.1,.2), 1, replace = TRUE),
    #   rec_form = sample(c(0,1,2,3),1, replace = TRUE),
    #   fec_form = c("power"),
    #   weight_a = NA
    # )

  }
