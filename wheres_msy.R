library(marlin)
library(tidyverse)

seasons = 4

resolution <- 10

fauna <-
  list(
    "bigeye" = create_critter(
      scientific_name =  "thunnus obesus",
      adult_diffusion = 10,
      density_dependence = "post_dispersal",
      seasons = seasons,
      resolution = resolution,
      age_mature = 1,
      steepness = 0.9,
      ssb0 = 1000
    )
  )



find_ssbmsy <- function(mult = 1,sel_start = .01,fauna, years = 50, use = "graphs"){


  tmp_metier <- list(Metier$new(
    critter = fauna[[1]],
    sel_form = "logistic",
    sel_start = sel_start,
    sel_delta = .01,
    p_explt = 1,
    catchability = 1
  ))

  names(tmp_metier) <- names(fauna[1])

  fleets <- list(
    "longline" = create_fleet(
      tmp_metier,
      base_effort = mult * resolution ^ 2,
      resolution = resolution
    )
  )

  sim <- simmar(fauna = fauna, fleets = fleets, years = years)

  eqish <- sim[[length(sim)]]
  if (use == "graphs"){
    out <- data.frame(ssb = sum(eqish[[1]]$ssb_p_a),
                      b = sum(eqish[[1]]$b_p_a),
                      yield = sum(eqish[[1]]$c_p_a))

  } else {
    out <- -sum(eqish[[1]]$c_p_a)
  }

}

bmsy_grid <- expand_grid(mult = seq(0,.25, length.out = 50), sel_start = 0.01) %>%
  mutate(tmp = map2(mult,sel_start, find_ssbmsy, fauna = fauna))

bmsy_grid <- bmsy_grid %>%
  unnest(cols = tmp)

bmsy_grid %>%
  group_by(sel_start) %>%
  mutate(syield = yield / max(yield)) %>%
  ggplot(aes(ssb / fauna[[1]]$ssb0, yield, color = sel_start, group = sel_start)) +
  geom_line() +
  scale_x_continuous(limits = c(0, NA))

emsy <- nlminb(0.1, find_ssbmsy, fauna = fauna, lower = 0, upper = 1, use = "optim")

baseline_ssbmsy <- find_ssbmsy(emsy$par, fauna = fauna)$ssb

baseline_ssbmsy

baseline_ssbmsy / fauna[[1]]$ssb0

emsy$objective

max(bmsy_grid$yield)
