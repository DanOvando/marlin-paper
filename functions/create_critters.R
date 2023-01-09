create_critters <-
  function(sciname,
           habitat,
           seasons = 1,
           marlin_inputs,
           seasonal_movement = FALSE,
           ontogenetic_shift = FALSE,
           random_rec = FALSE,
           adult_diffusion = 10,
           taxis_to_diff_ratio = 2) {
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
      density_dependence <-  sample(c("global_habitat", "local_habitat","pre_dispersal","post_dispersal","global_ssb"), 1, replace = TRUE)

    } else {
      density_dependence <- "global_ssb" # local density dependence then disperse recruits per recruit movement
    }

    if (ontogenetic_shift) {
      recruit_habitat <-
        -hab[[1]] - min(-hab[[1]]) # place recruits in different places than adults

      # set recruitment form to allow for recruit habitat
      #
      density_dependence <- "global_ssb"
    } else {
      recruit_habitat <- hab[[1]]
    }

    critter <- marlin::create_critter(
      scientific_name = sciname,
      base_habitat = hab,
      recruit_habitat = recruit_habitat,
      adult_diffusion = adult_diffusion,
      taxis_to_diff_ratio = taxis_to_diff_ratio,
      recruit_diffusion = 0,
      density_dependence = density_dependence,
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


  }
