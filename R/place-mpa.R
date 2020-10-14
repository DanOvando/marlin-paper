#' Place MPA
#'
#' @param target_fauna a vector of critter names around which to base MPA
#' @param size the proportion of available patches to place in an MPA
#' @param fauna a fauna object
#'
#' @return a data frame with coordinates of MPA locations
place_mpa <- function(target_fauna, size = 0.2, fauna) {
  # target_fauna <- c("bigeye", "yellowfin")

  # size <- 0.1

  targets <- fauna[target_fauna]

  nps <- targets %>%
    map_df( ~ rowSums(.x$n_p_a_0)) %>%
    mutate(patch = 1:nrow(.)) %>%
    pivot_longer(-patch, names_to = "critter", values_to = "n") %>%
    group_by(patch) %>%
    summarise(n = sum(n))

  resolution <-  sqrt(n_distinct(nps$patch))

  n_mpa <- round(resolution ^ 2 * size)

  mpas <- nps %>%
    arrange(desc(n)) %>%
    slice(1:n_mpa)

  mpas <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
    mutate(patch = 1:nrow(.)) %>%
    mutate(mpa = patch %in% mpas$patch) %>%
    select(x, y, mpa)



  # mpas %>%
  #   ggplot(aes(x,y,fill = mpa)) +
  #   geom_tile()
  #

  return(mpas)

}
