mpa_locations <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
  mutate(mpa = x < 0.3 * resolution)

# fleets$longline$fleet_model
#
# fleets$purseseine$fleet_model

with_mpa <- simmar(
  fauna = fauna,
  fleets = fleets,
  manager = list(mpas = list(
    locations = mpa_locations,
    mpa_year = round(years / 2)
  )),
  years = years
)

grid <- expand_grid(x = 1:resolution, y= 1:resolution) %>%
  mutate(patch = 1:nrow(.))


patch_effort <-   map_df(with_mpa, ~ map_df(.x, ~.x$e_p_fl %>% mutate(patch = 1:nrow(.)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step)) %>%
  left_join(grid, by = "patch") %>%
  filter(critter ==  names(fauna)[1]) %>%
  pivot_longer(names(fleets), names_to = "fleet", values_to = "effort")

patch_effort %>%
  filter(step == max(step)) %>%
  ggplot(aes(x,y, fill = effort))+
  geom_tile() +
  facet_wrap(fleet~step) +
  scale_fill_viridis_c()


patch_catch <-
  map_df(with_mpa,
         ~ map_df(.x, ~ as_tibble(.x$c_p_fl) %>% mutate(patch = 1:nrow(.)), .id = "critter"),
         .id = "step") %>%
  mutate(step = as.numeric(step)) %>%
  left_join(grid, by = "patch") %>%
  pivot_longer(names(fleets), names_to = "fleet", values_to = "catch")



patch_cpue <- patch_catch %>%
  left_join(patch_effort %>% select(step, patch, starts_with("fleet"), effort)) %>%
  mutate(cpue = catch / effort) %>%
  mutate(mpa = TRUE)

patch_cpue %>%
  group_by(critter, step, fleet) %>%
  mutate(scpue = cpue / max(cpue, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(step == max(step)) %>%
  ggplot(aes(x,y, fill = scpue))+
  geom_tile() +
  facet_grid(critter~fleet) +
  scale_fill_viridis_c()

with_mpa_biomass <-
  map_df(with_mpa, ~ map_df(.x, ~tibble(biomass = sum(.x$ssb_p_a)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step)) %>%
  mutate(mpa = TRUE)


with_mpa_biomass %>%
  ungroup() %>%
  ggplot(aes(step, biomass, color = critter)) +
  geom_line() +
  facet_wrap(~critter, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA))


without_mpa <- simmar(
  fauna = fauna,
  fleets = fleets,
  years = years
)

no_mpa_patch_effort <-   map_df(without_mpa, ~ map_df(.x, ~.x$e_p_fl %>% mutate(patch = 1:nrow(.)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step)) %>%
  left_join(grid, by = "patch") %>%
  filter(critter ==  names(fauna)[1]) %>%
  pivot_longer(names(fleets), names_to = "fleet", values_to = "effort")

no_mpa_patch_effort %>%
  filter(step == max(step)) %>%
  ggplot(aes(x,y, fill = effort))+
  geom_tile() +
  facet_wrap(~step) +
  scale_fill_viridis_c()


no_mpa_patch_catch <-   map_df(without_mpa, ~ map_df(.x, ~as_tibble(.x$c_p_fl) %>% mutate(patch = 1:nrow(.)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step)) %>%
  left_join(grid, by = "patch") %>%
  pivot_longer(names(fleets), names_to = "fleet", values_to = "catch")



no_mpa_patch_cpue <- no_mpa_patch_catch %>%
  left_join(no_mpa_patch_effort %>% select(step, patch,starts_with("fleet"), effort)) %>%
  mutate(cpue = catch / effort) %>%
  mutate(mpa = FALSE)

no_mpa_patch_cpue %>%
  group_by(critter, step, fleet) %>%
  mutate(scpue = cpue / max(cpue, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(step == max(step)) %>%
  ggplot(aes(x,y, fill = scpue))+
  geom_tile() +
  facet_grid(critter~fleet) +
  scale_fill_viridis_c()

no_mpa_biomass <-
  map_df(without_mpa, ~ map_df(.x, ~tibble(biomass = sum(.x$ssb_p_a)), .id = "critter"), .id = "step") %>%
  mutate(step = as.numeric(step)) %>%
  mutate(mpa = FALSE)

no_mpa_biomass %>%
  ungroup() %>%
  ggplot(aes(step, biomass, color = critter)) +
  geom_line() +
  facet_wrap(~critter, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA))


# put together

biomass <- with_mpa_biomass %>%
  bind_rows(no_mpa_biomass) %>%
  pivot_wider(names_from = mpa, values_from = biomass) %>%
  mutate(delta = `TRUE` / `FALSE` - 1)

biomass %>%
  ggplot(aes(step, delta, color = critter)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), name = "% Change in Total biomass")

cpue_and_friends <- patch_cpue %>%
  bind_rows(no_mpa_patch_cpue)


cpue_effect <- cpue_and_friends %>%
  select(step, critter, patch, x,y,fleet, cpue, mpa) %>%
  pivot_wider(names_from = mpa, values_from = cpue) %>%
  mutate(delta = `TRUE` - `FALSE`)

cpue_effect %>%
  group_by(critter, step, fleet) %>%
  mutate(sdelta = delta / max(delta, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(step == max(step), critter == "snapper") %>%
  ggplot(aes(x,y, fill = delta))+
  geom_tile() +
  facet_grid(critter~fleet) +
  scale_fill_gradient2(name = "Change in CPUE with MPA",low = "red", high = "blue", mid = "white", midpoint = 0) +
  theme_marlin(base_size = 18)

yield_effect <- cpue_and_friends %>%
  select(step, critter, patch, x,y,fleet, catch, mpa) %>%
  pivot_wider(names_from = mpa, values_from = catch) %>%
  mutate(delta = `TRUE` - `FALSE`,
         pdelta = `TRUE` / `FALSE` - 1)


yield_effect %>%
  group_by(step, fleet) %>%
  summarise(
    yield_change = sum(`TRUE`, na.rm = TRUE) - sum(`FALSE`, na.rm = TRUE),
    delta_yield = sum(`TRUE`, na.rm = TRUE) / sum(`FALSE`, na.rm = TRUE) - 1) %>%
  ggplot(aes(step, delta_yield, color = fleet)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_line(size = 2) +
  scale_x_continuous("Time Step") +
  scale_y_continuous("% Change in Total Catches", labels = scales::percent)



yield_effect %>%
  group_by(step, fleet) %>%
  summarise(yield_change = sum(delta, na.rm = TRUE)) %>%
  ggplot(aes(step, yield_change, color = fleet)) +
  geom_line()




yield_effect %>%
  group_by(critter, step, fleet) %>%
  mutate(sdelta = delta / max(delta, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(critter, fleet) %>%
  mutate(delta = scale(delta)) %>%
  ungroup() %>%
  filter(step == max(step)) %>%
  ggplot(aes(x,y, fill = delta))+
  geom_tile() +
  facet_grid(critter~fleet) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0)
