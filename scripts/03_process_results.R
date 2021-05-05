source(file.path("scripts", "00_setup.R"))


experiments <- read_rds(file = file.path(results_path, "experiements.rds"))

critter_lookup <- read_rds(file = file.path(results_path, 'critter-lookup.rds'))

rough_habitat <- read_rds(file = file.path(results_path, "rough-habitat.rds"))

a = critter_lookup %>%
  left_join(rough_habitat %>% select(species_commonname, bycatch) %>% unique)


scenes <- experiments %>%
  select(xid, data) %>%
  unnest(cols = data) %>%
  select(xid:seasonal_movement) %>%
  unique()


mpa_results <-  experiments %>%
  select(xid, experiment) %>%
  unnest(cols = experiment) %>%
  select(xid,target_fauna, mpa_size, results) %>%
  unnest(cols = results) %>%
  mutate(target_fauna = map_chr(target_fauna, ~paste(.x, collapse = ","))) %>%
  left_join(a, by = c("critter" = "species_sciname")) %>%
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
    color = species_commonname,
    group = interaction(xid,species_commonname,bycatch)
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
    color = species_commonname,
    group = interaction(xid,species_commonname,bycatch)
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
         species_commonname,
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
  facet_wrap(~ species_commonname, scales = "free") +
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





a %>% knitr::kable() %>% kableExtra::kable_paper()


# what is interesting
#
#
# proportion stocks benefiting / losing as a functino of MPA size and spatial homogeneity
#

all_species_p_ssb_effect_plot <- mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= 0),
            percent_lose = 1 - percent_improve) %>%
  ggplot(aes(mpa_size, percent_lose, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA reduces SSB") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")


mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= .1),
            percent_lose = mean(percent_change <= -.1)) %>%
  ggplot(aes(mpa_size, percent_lose, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where SSB Decreaes by more than 10%") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")

mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= .1),
            percent_lose = mean(percent_change <= -.1)) %>%
  ggplot(aes(mpa_size, percent_improve, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPAs increase by more htan 10%") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")



all_species_mean_p_ssb_plot <- mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(net_percent_change = weighted.mean(percent_change,ssb0)) %>%
  ggplot(aes(mpa_size, net_percent_change, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA reduces SSB") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")



all_species_net_ssb_effect_plot <- mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(net_change = sum(abs_change)) %>%
  ggplot(aes(mpa_size, net_change, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(name = "Net Change in SSB") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")


bycatch_ssb_effect_plot <- mpa_results %>%
  filter(bycatch == "Bycatch Species") %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "ssb") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= 0),
            percent_lose = 1 - percent_improve) %>%
  ggplot(aes(mpa_size, percent_lose, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA reduces SSB") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")


all_species_catch_effect_plot <- mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "c") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= 0),
            percent_lose = 1 - percent_improve) %>%
  ggplot(aes(mpa_size, percent_lose, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA reduces Catch") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")

mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "c") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change >= .1),
            percent_lose = mean(percent_change <= -.1)) %>%
  ggplot(aes(mpa_size, percent_lose, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA reduces Catch by more than 10%") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")


mpa_results %>%
  mutate(target_fauna = as.factor(target_fauna)) %>%
  mutate(target_fauna = fct_recode(target_fauna,!!!mpa_strategy)) %>%
  filter(variable == "c") %>%
  group_by(mpa_size, sigma_centroid, seasonal_movement, target_fauna) %>%
  summarise(percent_improve = mean(percent_change > .1),
            percent_lose = mean(percent_change < -.1)) %>%
  ggplot(aes(mpa_size, percent_improve, color = factor(sigma_centroid))) +
  geom_line(size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_viridis_d(name = "Heterogeneity in Adult Habitat") +
  scale_x_continuous(labels = scales::percent, name = 'Area in MPA') +
  scale_y_continuous(labels = scales::percent, name = "Sims where MPA incease Catch by >10%") +
  facet_grid(target_fauna~seasonal_movement, labeller = label_both) +
  theme(legend.position = "top")
