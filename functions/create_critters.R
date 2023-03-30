create_critters <-
  function(sciname,
           habitat,
           seasons = 1,
           marlin_inputs,
           seasonal_movement = FALSE,
           ontogenetic_shift = FALSE,
           random_rec = FALSE,
           adult_diffusion = 10,
           resolution = resolution,
           max_hab_mult = 5,
           patch_area = 1) {

    tmp_inputs <- marlin_inputs %>%
      filter(scientific_name == sciname & !is.na(scientific_name))

    hab <- habitat %>%
      pivot_wider(names_from = y, values_from = habitat) %>%
      select(-x) %>%
      as.matrix()


    # if (seasonal_movement) {
    #   hab <- list(hab,-hab - (min(-hab)))
    #
    # } else {
    #   hab <- list(hab, hab)
    # }

    if (random_rec){
      density_dependence <-  sample(c("global_habitat", "local_habitat","pre_dispersal","post_dispersal","global_ssb"), 1, replace = TRUE)

    } else {
      density_dependence <- "global_ssb" # local density dependence then disperse recruits per recruit movement
    }

    critter <- marlin::create_critter(
      scientific_name = sciname,
      habitat = hab,
      recruit_habitat = hab,
      adult_diffusion = adult_diffusion,
      recruit_diffusion = adult_diffusion / 2,
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
      ), 1e4, mean(tmp_inputs$ssb0 * 1000, na.rm = TRUE)),
      max_hab_mult = max_hab_mult,
      resolution = resolution
    )


  }
