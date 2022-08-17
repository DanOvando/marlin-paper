create_fauna <- function(scientific_name,
                         seasonal_habitat,
                         season_blocks,
                         adult_diffusion,
                         rec_form,
                         seasons,
                         init_explt,
                         explt_type = "f",
                         recruit_habitat = NA){

  tmp <-  create_critter(
    scientific_name = scientific_name,
    seasonal_habitat = seasonal_habitat,
    season_blocks = season_blocks,
    adult_diffusion = adult_diffusion,
    rec_form = rec_form,
    seasons = seasons,
    init_explt = init_explt,
    explt_type = "explt_type",
    recruit_habitat = recruit_habitat
  )



}
