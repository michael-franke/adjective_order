library(ggthemes)

mc_results = read_csv("03_mc_results.csv") %>% 
  mutate(dist_brown = factor(ifelse(dist_brown == "open", 
                                    "relative (less subjective)", 
                                    "absolute (less subjective)"), ordered = T) %>% fct_rev(),
         dist_tall = factor(ifelse(dist_tall == "open", 
                                   "relative (more subjective)", 
                                   "absolute (more subjective)"), ordered = T) %>% fct_rev())

EU_plot = mc_results %>% 
  # filter(dist_brown == "open") %>% 
  ggplot(aes(x = theta, y = tall_brown - brown_tall)) + 
  geom_smooth() + 
  geom_hline(yintercept = 0, color = "firebrick") +
  facet_grid(dist_tall ~ dist_brown) 

show(EU_plot)
