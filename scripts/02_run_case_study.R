source(file.path("scripts", "00_setup.R"))



# load bycatch risk layers ------------------------------------------------


mats <- list.files(path = here("data", "matrices"))

mats <- mats[str_detect(mats, "longline_wcpfc")]

get_layer <- function(file) {
  # file <- mats[3]

  file_comps <- str_split(file, "_", simplify = TRUE)

  species <- str_replace(file_comps[, 1], "-", " ")

  fleet <- file_comps[, 2]


  hab <-
    read_csv(here("data", "matrices", file),
             col_names = FALSE,
             skip = 1) %>%
    rename(lat = X1) %>%
    mutate(lat = 1:nrow(.)) %>%
    pivot_longer(
      -lat,
      names_to = "lon",
      values_to = "hab",
      names_prefix = "X"
    ) %>%
    mutate(hab = tidyr::replace_na(hab, 0),
           lon = as.numeric(lon) - 1)

  # hab %>%
  #   ggplot(aes(lon, lat, fill = hab)) +
  #   geom_tile()
  #

  x_binsize <- (max(hab$lon) - min(hab$lon) + 1) / resolution

  y_binsize <- (max(hab$lat) - min(hab$lat) + 1) / resolution

  if (x_binsize < 1 | y_binsize < 1) {
    stop("resolution is too high for input data")
  }


  hab <- hab %>%
    mutate(X = lon - min(lon),
           Y = lat - min(lat)) %>%
    mutate(rough_x = floor(X / x_binsize),
           rough_y = floor(Y / y_binsize)) %>%
    select(contains("rough_"), hab)
  #
  #   hab %>%
  #     ggplot(aes(rough_x, rough_y, fill = hab)) +
  #     geom_tile()

  hab <- hab %>%
    group_by(rough_x, rough_y) %>%
    summarise(hab = sum(hab)) %>%
    mutate(xy = rough_x * rough_y)

  mod <-
    gamm4::gamm4(hab ~ s(rough_x) + s(rough_y) + s(xy), data = hab)

  hab$interp_cpue <- as.numeric(predict(mod$gam))

  hab$habitat <- pmax(0, hab$interp_cpue / max(hab$interp_cpue))


  if (sqrt(nrow(hab)) != resolution) {
    stop("habitat layer does not match simulation resolution")
  }
  hab <- hab %>%
    select(contains("rough"), habitat) %>%
    rename(x = rough_x, y = rough_y) %>%
    ungroup()

  # hab %>%
  #   ggplot(aes(rough_x, rough_y, fill = habitat)) +
  #   geom_tile()

  out <- tibble(scientific_name = species, habitat = list(hab))


}


mats <- map(mats, get_layer)

check_habitat <- mats %>%
  bind_rows() %>%
  unnest(cols = habitat) %>%
  ggplot(aes(x, y, fill = habitat)) +
  geom_tile() +
  facet_wrap( ~ scientific_name) +
  scale_fill_viridis_c()


mats <- mats %>%
  bind_rows()

# create experiments ------------------------------------------------------

casestudy <- tibble(scientific_name = unique(marlin_inputs$scientific_name)) %>%
  left_join(mats, by = "scientific_name") %>%
  filter(!map_lgl(.$habitat, is.null))

casestudy <- casestudy %>%
  mutate(critter = pmap(
    list(
      sciname = scientific_name,
      habitat = habitat,
      ontogenetic_shift = FALSE,
      seasonal_movement = FALSE
    ),
    create_critters,
    marlin_inputs = marlin_inputs,
    seasons = seasons
  )) %>%
  mutate(xid = 1)

casestudy <- casestudy %>%
  group_by(xid) %>%
  nest() %>%
  mutate(fauna = map(data, ~ .x$critter %>% set_names(.x$scientific_name)))

check_pop_sizes <- map_dbl(casestudy$fauna[[1]], "ssb0")

tibble(sciname = names(check_pop_sizes), ssb0 = check_pop_sizes) %>%
  ggplot(aes(reorder(sciname, ssb0), ssb0)) +
  geom_col() +
  coord_flip()

casestudy <- casestudy %>%
  ungroup() %>%
  mutate(fleet = map(fauna, compile_fleet))


if (run_casestudy == TRUE){



a <- Sys.time()
nrow(casestudy)

casestudy <- casestudy %>%
  mutate(experiment = pmap(
    list(fauna = fauna, fleet = fleet,
         xid = xid),
    run_mpa_experiment,
    placement_error = 0
  ))

diff <- Sys.time() - a

write_rds(casestudy, file = file.path(results_path, "casestudy.rds"))

} else {

  casestudy <- read_rds(file = file.path(results_path, "casestudy.rds"))


}
scenes <- casestudy %>%
  select(xid, data) %>%
  unnest(cols = data) %>%
  select(xid) %>%
  unique()


mpa_results <-  casestudy %>%
  select(xid, experiment) %>%
  unnest(cols = experiment) %>%
  select(xid,target_fauna, mpa_size, results) %>%
  unnest(cols = results) %>%
  mutate(target_fauna = map_chr(target_fauna, ~paste(.x, collapse = ","))) %>%
  mutate(bycatch = critter %in% marlin_inputs$scientific_name[marlin_inputs$bycatch]) %>%
  # left_join(a, by = c("critter" = "species_sciname")) %>%
  left_join(scenes, by = "xid") %>%
  mutate(bycatch = ifelse(bycatch == TRUE, "Bycatch Species","Target Species"))

mpa_strategy <- c(
  `carcharhinus longimanus` = "MPAs for OWT Shark",
  `prionace glauca,carcharhinus longimanus,isurus oxyrinchus,carcharhinus falciformis` = "MPAs for All Bycatch"
)



mpa_results %>%
  filter(variable == "ssb") %>%
  ggplot(aes(
    mpa_size,
    percent_change_ssb0,
    color = critter,
    group = interaction(xid,critter,bycatch)
  )) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(size = 1.5, alpha = 0.25) +
  facet_grid( bycatch~ target_fauna, labeller = labeller(target_fauna = mpa_strategy), scales = "free_y") +
  scale_x_continuous(name = "MPA Size", labels = scales::percent) +
  scale_y_continuous(name = "% of SSB0 Change", labels = scales::percent) +
  scale_color_discrete(name = "Species") +
  scale_linetype(name = '') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))


mpa_strategy <- c(
  `carcharhinus longimanus` = "MPAs for OWT Shark",
  `prionace glauca,carcharhinus longimanus,isurus oxyrinchus,carcharhinus falciformis` = "MPAs for All Bycatch"
)


mpa_results %>%
  filter(variable == "c") %>%
  ggplot(aes(
    mpa_size,
    percent_change,
    color = critter,
    group = interaction(xid,critter,bycatch)
  )) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(size = 1, alpha = 0.25) +
  facet_wrap(bycatch ~ target_fauna, labeller = labeller(target_fauna = mpa_strategy)) +
  scale_x_continuous(name = "MPA Size", labels = scales::percent) +
  scale_y_continuous(name = "% Change in Catch", labels = scales::percent) +
  scale_color_discrete(name = "Species") +
  scale_linetype(name = '') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))



mpa_strategy <- c("MPAs for OWT Shark" = "carcharhinus longimanus" ,
                  "MPAs for All Bycatch" = "prionace glauca,carcharhinus longimanus,isurus oxyrinchus,carcharhinus falciformis")

mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  select(mpa_size,
         percent_change,
         critter,
         target_fauna,
         variable,
         xid) %>%
  filter(variable %in% c("c", "ssb")) %>%
  pivot_wider(names_from = "variable", values_from = percent_change) %>%
  ggplot(aes(ssb, c, shape = target_fauna, linetype = target_fauna)) +
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  # geom_line() +
  geom_point(aes(color = mpa_size), size = 2) +
  facet_wrap(~ critter, scales = "free") +
  scale_y_continuous(name = "% Change in Catch", labels = scales::percent) +
  scale_x_continuous(name = "% of SSB0 Change", labels = scales::percent) +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top",
    legend.direction = "horizontal"
  ) +
  scale_colour_viridis_b(
    name = "MPA Size",
    labels = scales::percent,
    guide = guide_colorbar(barwidth = unit(9, "lines"))
  ) +
  scale_shape(name = "") +
  scale_linetype(name = "")
