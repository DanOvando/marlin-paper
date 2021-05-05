run_mpa_experiment <- function(fauna, fleet, placement_error = 0, random_mpas = FALSE, xid){


  mpa_sims <-
    expand_grid(
      target_fauna = list(
        "carcharhinus longimanus",
        unique(marlin_inputs$scientific_name[marlin_inputs$bycatch])
      ),
      mpa_size = seq(0, 1, by = .05),
      random_mpas = c(random_mpas)
    )


  # hmm will need to adjust this so placement is scenario specific
  mpa_sims <- mpa_sims %>%
    mutate(mpa = pmap(
      list(
        target_fauna = target_fauna,
        size = mpa_size,
        place_randomly = random_mpas
      ),
      place_mpa,
      fauna = fauna,
      placement_error = placement_error
    )) %>%
    mutate(id = 1:nrow(.))

  #
  #
  # i = 3
  # mpa_sims$mpa[[i]] %>%
  #   ggplot(aes(x,y,fill = mpa)) +
  #   geom_tile()

  steps_to_keep <- c(max(time_steps))

  run_and_process <-
    function(fauna,
             fleet,
             mpa,
             steps_to_keep,
             years,
             seasons,
             id,
             keep_age = FALSE,
             write_sim = FALSE,
             results_path) {


      # fauna <- sims$fauna[[1]]
      #
      # fleet <- sims$fleet[[1]]
      #
      # mpa <- sims$mpa[[1]]
      #

      sim_base <- simmar(fauna = fauna,
                         fleets = fleet,
                         years = years)

      proc_sim_base <-
        process_marlin(
          sim = sim_base,
          time_step =  fauna[[1]]$time_step,
          steps_to_keep = steps_to_keep,
          keep_age = keep_age
        )


      sim_mpa <- simmar(
        fauna = fauna,
        fleets = fleet,
        years = years,
        mpas = list(
          locations = mpa,
          mpa_year = floor(years * .5)
        )
      )
      proc_sim_mpa <-
        process_marlin(
          sim = sim_mpa,
          time_step =  fauna[[1]]$time_step,
          steps_to_keep = steps_to_keep,
          keep_age = keep_age
        )
      # browser()
      #     plot_marlin(proc_sim_mpa, proc_sim_base) +
      #       geom_vline(aes(xintercept = floor(years * seasons * .5)))

      out <- list(with_mpa = proc_sim_mpa,
                  without_mpa = proc_sim_base)

      if (write_sim){

        readr::write_rds(out, file.path(results_path,"sims", paste0("sim_",id,".rds")))

        out <- NA
      }

      return(out)

    }

  a <- Sys.time()

  mpa_sims <- mpa_sims %>%
    # slice(1:3) %>%
    mutate(sim = future_pmap(
      list(
        mpa = mpa,
        id = id
      ),
      run_and_process,
      year = years,
      steps_to_keep = steps_to_keep,
      fauna = fauna,
      fleet = fleet,
      seasons = seasons,
      write_sim = FALSE,
      results_path = results_path,
      .progress = TRUE
    ))


  assess_sim <- function(sim, fauna,thing = "fauna"){

    # thing = "fauna"
    # sim <- sims$sim[[1]]
    #

    ssb0s <- fauna %>%
      map_dfc("ssb0",.id = "critter") %>%
      pivot_longer(everything(), names_to = "critter", values_to = "ssb0")

    tmp <- sim %>% map(thing) %>%
      bind_rows(.id = "scenario") %>%
      pivot_longer(n:c, names_to = "variable", values_to = "value") %>%
      pivot_wider(names_from = "scenario", values_from = "value") %>%
      group_by(step, variable, critter) %>%
      summarise(
        with_mpa = sum(with_mpa),
        without_mpa = sum(without_mpa),
        percent_improved = mean(with_mpa > without_mpa)
      ) %>%
      ungroup() %>%
      left_join(ssb0s, by = "critter") %>%
      mutate(abs_change = with_mpa - without_mpa,
             percent_change = with_mpa / without_mpa - 1,
             percent_change_ssb0 = (with_mpa - without_mpa) / ssb0)
  }

  print(xid)


  mpa_sims <- mpa_sims %>%
    mutate(results = map(sim, assess_sim, fauna = fauna))

  return(mpa_sims)

} # close run_mpa_experiment
