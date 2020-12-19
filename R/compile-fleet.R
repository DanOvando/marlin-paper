compile_fleet <- function(fauna, tune_type = "explt") {

  # browser()
  # fauna <- wtf

  fleets <- list(
    "longline" = create_fleet(
      list(
        "thunnus obesus" = Metier$new(
          critter = fauna$`thunnus obesus`,
          price = 10,
          sel_form = "logistic",
          sel_start = 1,
          sel_delta = .01,
          catchability = .1,
          p_explt = .5
        ),
        "thunnus alalunga" = Metier$new(
          critter = fauna$`thunnus alalunga`,
          price = 5,
          sel_form = "logistic",
          sel_start = 1,
          sel_delta = .01,
          catchability = .1,
          p_explt = .5
        ),
        "katsuwonus pelamis" = Metier$new(
          critter = fauna$`katsuwonus pelamis`,
          price = 1,
          sel_form = "logistic",
          sel_start = 1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        "thunnus albacares" = Metier$new(
          critter = fauna$`thunnus albacares`,
          price = 15,
          sel_form = "logistic",
          sel_start = 1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        "kajikia audax" = Metier$new(
          critter = fauna$`kajikia audax`,
          price = 5,
          sel_form = "logistic",
          sel_start = .2,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9
        ),
        "carcharhinus longimanus" = Metier$new(
          critter = fauna$`carcharhinus longimanus`,
          price = 0,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9
        ),
        "istiompax indica" = Metier$new(
          critter = fauna$`istiompax indica`,
          price = 5,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9
        ),
        "makaira mazara" = Metier$new(
          critter = fauna$`makaira mazara`,
          price = 5,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9
        ),
        "carcharhinus falciformis" = Metier$new(
          critter = fauna$`carcharhinus falciformis`,
          price = 0,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9
        ),
        "prionace glauca" = Metier$new(
          critter = fauna$`prionace glauca`,
          price = 0,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9
        ),
        "isurus oxyrinchus" = Metier$new(
          critter = fauna$`isurus oxyrinchus`,
          price = 0,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9
        )
      ),
      base_effort = resolution ^ 2
    ),
    "purseseine" = create_fleet(
      list(
        "thunnus obesus" = Metier$new(
          critter = fauna$`thunnus obesus`,
          price = 1,
          sel_form = "logistic",
          sel_start = .2,
          sel_delta = .01,
          catchability = .1,
          p_explt = .5
        ),
        "thunnus alalunga" = Metier$new(
          critter = fauna$`thunnus alalunga`,
          price = 1,
          sel_form = "logistic",
          sel_start = .2,
          sel_delta = .01,
          catchability = .1,
          p_explt = .2
        ),
        "katsuwonus pelamis" = Metier$new(
          critter = fauna$`katsuwonus pelamis`,
          price = 5,
          sel_form = "logistic",
          sel_start = 1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9
        ),
        "thunnus albacares" = Metier$new(
          critter = fauna$`thunnus albacares`,
          price = 3,
          sel_form = "logistic",
          sel_start = .2,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        "kajikia audax" = Metier$new(
          critter = fauna$`kajikia audax`,
          price = 0,
          sel_form = "logistic",
          sel_start = 2,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        "carcharhinus longimanus" = Metier$new(
          critter = fauna$`carcharhinus longimanus`,
          price = 0,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        "istiompax indica" = Metier$new(
          critter = fauna$`istiompax indica`,
          price = 5,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        "makaira mazara" = Metier$new(
          critter = fauna$`makaira mazara`,
          price = 5,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        "carcharhinus falciformis" = Metier$new(
          critter = fauna$`carcharhinus falciformis`,
          price = 5,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        "prionace glauca" = Metier$new(
          critter = fauna$`prionace glauca`,
          price = 5,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1
        ),
        "isurus oxyrinchus" = Metier$new(
          critter = fauna$`isurus oxyrinchus`,
          price = 0,
          sel_form = "logistic",
          sel_start = .1,
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
