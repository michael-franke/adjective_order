mc_results = read_csv("03_mc_results.csv") %>% 
  mutate(EU_diff = tall_brown - brown_tall,
         sd_diff = sd_tall - sd_brown)

model_saturated = glm(EU_diff ~ n_obj *  sd_diff * theta,
                       data = mc_results)
summary(model_saturated)

model_simpler = step(model_saturated)
summary(model_simpler)
