mc_results = read_csv("03_mc_results.csv") %>% 
  mutate(EU_diff = tall_brown - brown_tall)

regression_model = glm(EU_diff ~ n_obj + sd_brown + sd_tall + theta + dist_brown + dist_tall,
                       data = mc_results)

summary(regression_model)
