library(tidyverse)

moorea_coral <- read_csv(
  "data/moorea_coral.csv",
  na = c("", "NA", "ND") # This vector tells read_csv() which values to interpret as missing data
)

moorea_fish <- read_csv(
  "data/moorea_fish.csv",
  na = c("", "NA", "ND")
)


# Exercise 1 -------------------------------------------------------------

non_coral <- c(
  "Sand",
  "CTB",
  "Macroalgae",
  "Non-coralline Crustose Algae",
  "Unknown or Other"
)

moorea_filtered <- moorea_coral |>
  filter(
    Taxonomy_Substrate_or_Functional_Group %in% non_coral,
    Depth < 17
  )

coral_summary <- moorea_filtered |>
  mutate(Year = as.numeric(str_sub(Date, start = 1L, end = 4L))) |>
  summarize(
    quadrat_cover = sum(Percent_Cover, na.rm = TRUE),
    .by = c(Year, Site, Habitat, Depth, Quad40)
  ) |>
  summarize(
    mean_coral_cover = mean(quadrat_cover, na.rm = TRUE),
    .by = c(Year, Site, Habitat, Depth)
  ) |>
  arrange(Year, Site, Depth)


# Exercise #2 ------------------------------------------------------------

fish_summary <- moorea_fish |>
  filter(Coarse_Trophic == "Primary Consumer") |>
  summarize(
    total_biomass = sum(Biomass, na.rm = TRUE),
    .by = c(Site, Habitat, Year)
  ) |>
  arrange(Year, Site, Habitat)

# exercise 3 -------------------------------------------------------------

reef_joined <- coral_summary |>
  inner_join(
    fish_summary,
    by = join_by(Site, Habitat, Year)
  )


# Exercise 4 -------------------------------------------------------------
reef_joined |>
  select(Site, Habitat, Year, mean_coral_cover) |>
  pivot_wider(
    names_from = Habitat,
    values_from = mean_coral_cover
  ) |>
  mutate(
    coral_difference = Fringing - Forereef
  ) |>
  ggplot(
    mapping = aes(x = coral_difference)
  ) +
  geom_histogram()

# Exercise 5 -------------------------------------------------------------
reef_joined |>
  ggplot(
    mapping = aes(x = mean_coral_cover, y = total_biomass)
  ) +
  geom_point() +
  labs(x = "Mean Coral Cover", y = "Primary Consumer Biomass") +
  theme_classic()
