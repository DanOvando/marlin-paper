create_fauna <- function(scientific_name,
                         seasonal_habitat,
                         season_blocks,
                         adult_movement,
                         adult_movmement_sigma,
                         rec_form,
                         seasons,
                         init_explt,
                         explt_type = "f"){

  tmp <-  create_critter(
    scientific_name = scientific_name,
    seasonal_habitat = seasonal_habitat,
    season_blocks = season_blocks,
    adult_movement = adult_movement,
    adult_movement_sigma = adult_movement_sigma,
    rec_form = rec_form,
    seasons = seasons,
    init_explt = init_explt,
    explt_type = "explt_type",
  )



}
