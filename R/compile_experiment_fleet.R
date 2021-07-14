compile_experiment_fleet <- function(fauna, tune_type = "explt") {
  fleets <- list("longline" = create_fleet(
    list(
      "katsuwonus pelamis" = Metier$new(
        critter = fauna$`katsuwonus pelamis`,
        price = 1.36,
        sel_form = "logistic",
        sel_start = fauna$`katsuwonus pelamis`$length_50_mature *  sample(c(0.6), 1, replace = TRUE),
        sel_delta = 0.01,
        catchability = .1,
        p_explt = 1,
        sel_unit = "length"

      ),
      "kajikia audax" = Metier$new(
        critter = fauna$`kajikia audax`,
        price = 5.16,
        sel_form = "logistic",
        sel_start = fauna$`kajikia audax`$length_50_mature *  sample(c(0.7), 1, replace = TRUE),
        sel_delta = 0.01,
        catchability = .1,
        p_explt = 1,
        sel_unit = "length"

      ),
      "carcharhinus longimanus" = Metier$new(
        critter = fauna$`carcharhinus longimanus`,
        price = 1.89,
        sel_form = "logistic",
        sel_start = fauna$`carcharhinus longimanus`$length_50_mature *
          sample(c(0.9), 1, replace = TRUE),
        sel_delta = 0.01,
        catchability = .1,
        p_explt = 1,
        sel_unit = "length"

      )
    ),
    base_effort = resolution
  ))


  # a <- Sys.time()

  fleets <- tune_fleets(fauna, fleets, tune_type = tune_type)
}
