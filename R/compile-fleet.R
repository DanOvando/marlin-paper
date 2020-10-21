compile_fleet <- function(fauna, tune_type = "explt") {

  # browser()
  # fauna <- wtf

  fleets <- list(
    "longline" = create_fleet(
      list(
        "Thunnus obesus" = Metier$new(
          critter = fauna$`Thunnus obesus`,
          price = 10,
          sel_form = "logistic",
          sel_start = 1,
          sel_delta = .01,
          catchability = .1,
          p_explt = .5
        ),
        `Katsuwonus pelamis` = Metier$new(
          critter = fauna$`Katsuwonus pelamis`,
          price = 1,
          sel_form = "logistic",
          sel_start = 1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        "Thunnus albacares" = Metier$new(
          critter = fauna$`Thunnus albacares`,
          price = 15,
          sel_form = "logistic",
          sel_start = 1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        `Kajikia audax` = Metier$new(
          critter = fauna$`Kajikia audax`,
          price = 5,
          sel_form = "logistic",
          sel_start = .2,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9
        ),
        "Carcharhinus longimanus" = Metier$new(
          critter = fauna$`Carcharhinus longimanus`,
          price = 0,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        )
      ),
      base_effort = resolution ^ 2
    ),
    "purseseine" = create_fleet(
      list(
        `Thunnus obesus` = Metier$new(
          critter = fauna$`Thunnus obesus`,
          price = 1,
          sel_form = "logistic",
          sel_start = .25,
          sel_delta = .01,
          catchability = .1,
          p_explt = .5
        ),
        `Katsuwonus pelamis` = Metier$new(
          critter = fauna$`Katsuwonus pelamis`,
          price = 5,
          sel_form = "logistic",
          sel_start = 1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        `Thunnus albacares` = Metier$new(
          critter = fauna$`Thunnus albacares`,
          price = 1,
          sel_form = "logistic",
          sel_start = .25,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        `Kajikia audax` = Metier$new(
          critter = fauna$`Kajikia audax`,
          price = 10,
          sel_form = "logistic",
          sel_start = .25,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9
        ),
        `Carcharhinus longimanus` = Metier$new(
          critter = fauna$`Carcharhinus longimanus`,
          price = 0,
          sel_form = "logistic",
          sel_start = .25,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        )
      ),
      base_effort = resolution ^ 2
    )
  )


  # a <- Sys.time()

  fleets <- tune_fleets(fauna, fleets, tune_type = tune_type)
}
