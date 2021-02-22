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
          sel_start = 100,
          sel_delta = .01,
          catchability = .1,
          p_explt = .5,
          sel_unit = "length"
        ),
        "thunnus alalunga" = Metier$new(
          critter = fauna$`thunnus alalunga`,
          price = 4.57,
          sel_form = "logistic",
          sel_start = 79.9,
          sel_delta = .01,
          catchability = .1,
          p_explt = 1,
          sel_unit = "length"
        ),
        "katsuwonus pelamis" = Metier$new(
          critter = fauna$`katsuwonus pelamis`,
          price = 1.36,
          sel_form = "logistic",
          sel_start = 65,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1,
          sel_unit = "length"

        ),
        "thunnus albacares" = Metier$new(
          critter = fauna$`thunnus albacares`,
          price = 7.52,
          sel_form = "logistic",
          sel_start = 90,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .75,
          sel_unit = "length"

        ),
        "kajikia audax" = Metier$new(
          critter = fauna$`kajikia audax`,
          price = 5.16,
          sel_form = "logistic",
          sel_start = 175,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9,
          sel_unit = "length"

        ),
        "carcharhinus longimanus" = Metier$new(
          critter = fauna$`carcharhinus longimanus`,
          price = 1.89,
          sel_form = "logistic",
          sel_start = 50,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9,
          sel_unit = "length"

        ),
        "istiompax indica" = Metier$new(
          critter = fauna$`istiompax indica`,
          price = 2.74,
          sel_form = "logistic",
          sel_start = 50,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9,
          sel_unit = "length"

        ),
        "makaira mazara" = Metier$new(
          critter = fauna$`makaira mazara`,
          price = 3.74,
          sel_form = "logistic",
          sel_start = 150,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9,
          sel_unit = "length"

        ),
        "carcharhinus falciformis" = Metier$new(
          critter = fauna$`carcharhinus falciformis`,
          price = 2.16,
          sel_form = "logistic",
          sel_start = 150,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9,
          sel_unit = "length"

        ),
        "prionace glauca" = Metier$new(
          critter = fauna$`prionace glauca`,
          price = 10,
          sel_form = "logistic",
          sel_start = 200,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9,
          sel_unit = "length"

        ),
        "isurus oxyrinchus" = Metier$new(
          critter = fauna$`isurus oxyrinchus`,
          price = 4.11,
          sel_form = "logistic",
          sel_start = 125,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9,
          sel_unit = "length"

        )
      ),
      base_effort =0
    ),
    "purseseine" = create_fleet(
      list(
        "thunnus obesus" = Metier$new(
          critter = fauna$`thunnus obesus`,
          price = 1,
          sel_form = "logistic",
          sel_start = 50,
          sel_delta = .01,
          catchability = .1,
          p_explt = .5,
          sel_unit = "length"

        ),
        "thunnus alalunga" = Metier$new(
          critter = fauna$`thunnus alalunga`,
          price = 4.57,
          sel_form = "logistic",
          sel_start = 100,
          sel_delta = .01,
          catchability = .1,
          p_explt = 1e-3,
          sel_unit = "length"

        ),
        "katsuwonus pelamis" = Metier$new(
          critter = fauna$`katsuwonus pelamis`,
          price = 1.36,
          sel_form = "logistic",
          sel_start = 50,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .9,
          sel_unit = "length"

        ),
        "thunnus albacares" = Metier$new(
          critter = fauna$`thunnus albacares`,
          price = 7.52,
          sel_form = "logistic",
          sel_start = 60,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .2,
          sel_unit = "length"

        ),
        "kajikia audax" = Metier$new(
          critter = fauna$`kajikia audax`,
          price = 5.16,
          sel_form = "logistic",
          sel_start = 175,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1,
          sel_unit = "length"

        ),
        "carcharhinus longimanus" = Metier$new(
          critter = fauna$`carcharhinus longimanus`,
          price = 1.89,
          sel_form = "dome",
          sel_start = 160,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1,
          sel_unit = "length"

        ),
        "istiompax indica" = Metier$new(
          critter = fauna$`istiompax indica`,
          price = 2.74,
          sel_form = "logistic",
          sel_start = 50,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1,
          sel_unit = "length"

        ),
        "makaira mazara" = Metier$new(
          critter = fauna$`makaira mazara`,
          price = 3.74,
          sel_form = "logistic",
          sel_start = 1000,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = 1e-3,
          sel_unit = "length"

        ),
        "carcharhinus falciformis" = Metier$new(
          critter = fauna$`carcharhinus falciformis`,
          price = 2.16,
          sel_form = "logistic",
          sel_start = .1,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .1,
          sel_unit = "length"

        ),
        "prionace glauca" = Metier$new(
          critter = fauna$`prionace glauca`,
          price = 5,
          sel_form = "logistic",
          sel_start = 110,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = .25,
          sel_unit = "length"

        ),
        "isurus oxyrinchus" = Metier$new(
          critter = fauna$`isurus oxyrinchus`,
          price = 4.11,
          sel_form = "logistic",
          sel_start = 190,
          sel_delta = 0.01,
          catchability = .1,
          p_explt = 1e-3,
          sel_unit = "length"

        )
      ),
      base_effort = 0
    )
  )


  # a <- Sys.time()

  fleets <- tune_fleets(fauna, fleets, tune_type = tune_type)
}
