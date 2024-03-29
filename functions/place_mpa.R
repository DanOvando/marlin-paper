#' Place MPA
#'
#' @param target_fauna a vector of critter names around which to base MPA
#' @param size the proportion of available patches to place in an MPA
#' @param fauna a fauna object
#'
#' @return a data frame with coordinates of MPA locations
place_mpa <-
  function(target_fauna,
           size = 0.2,
           fauna,
           placement_error = 0,
           seed = 42,
           place_randomly = FALSE) {
    # target_fauna <- c("carcharhinus longimanus")

    # size <- 0.1

    targets <- fauna[target_fauna]

    nps <- targets %>%
      map_df( ~ rowSums(.x$n_p_a_0)) %>%
      mutate(patch = 1:nrow(.)) %>%
      pivot_longer(-patch, names_to = "critter", values_to = "n") %>%
      group_by(patch) %>%
      summarise(n = sum(n))


    if (placement_error > 0) {
      set.seed(seed)

      error <- rlnorm(nrow(nps), 0, placement_error)

      nps$n <- nps$n * error

    }

    resolution <-  sqrt(n_distinct(nps$patch))

    n_mpa <- round(resolution ^ 2 * size)

    if (place_randomly) {
      mpa_locs <- sample(nps$patch, n_mpa, prob = nps$n)


    } else {
      mpas <- nps %>%
        arrange(desc(n)) %>%
        slice(1:n_mpa)

      mpa_locs <- mpas$patch

      if (n_mpa == 0){
        mpa_locs <- -999
      }
    }

    mpas <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
      mutate(patch = 1:nrow(.)) %>%
      mutate(mpa = patch %in% mpa_locs) %>%
      select(x, y, mpa)

    # mpas <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
    #   mutate(patch = 1:nrow(.)) %>%
    #   mutate(mpa = patch %in% mpa_locs,
    #          n = nps$n)
    #
    #
    #
    # mpas %>%
    #   ggplot(aes(x,y,fill = mpa)) +
    #   geom_tile()


    return(mpas)

  }
