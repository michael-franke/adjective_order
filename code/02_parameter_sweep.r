source("01_referential_success_k-percent_semantics.r")

## collect results in a tibble ::: 
#### ideally, for each parameter tuple, "tall_brown" should have a higher mean_success than "brown_tall"

iterations = 1000
results = rbind(
  get_outcomes(iterations, n_obj = 4, sd_brown = 0.1, sd_tall = 0.25, theta = 0.5),
  get_outcomes(iterations, n_obj = 6, sd_brown = 0.1, sd_tall = 0.25, theta = 0.5),
  get_outcomes(iterations, n_obj = 8, sd_brown = 0.1, sd_tall = 0.25, theta = 0.5),
  get_outcomes(iterations, n_obj = 4, sd_brown = 0.2, sd_tall = 0.3, theta = 0.5),
  get_outcomes(iterations, n_obj = 6, sd_brown = 0.2, sd_tall = 0.3, theta = 0.5),
  get_outcomes(iterations, n_obj = 8, sd_brown = 0.2, sd_tall = 0.3, theta = 0.5),
  get_outcomes(iterations, n_obj = 4, sd_brown = 0.1, sd_tall = 0.25, theta = 0.7),
  get_outcomes(iterations, n_obj = 6, sd_brown = 0.1, sd_tall = 0.25, theta = 0.7),
  get_outcomes(iterations, n_obj = 8, sd_brown = 0.1, sd_tall = 0.25, theta = 0.7),
  get_outcomes(iterations, n_obj = 4, sd_brown = 0.2, sd_tall = 0.3, theta = 0.7),
  get_outcomes(iterations, n_obj = 6, sd_brown = 0.2, sd_tall = 0.3, theta = 0.7),
  get_outcomes(iterations, n_obj = 8, sd_brown = 0.2, sd_tall = 0.3, theta = 0.7)
)
show(results)

results_plot = select(filter(results, context_types == "all"), -sum_success_prob) %>% 
  spread(key = ordering, value = mean_success) %>%
  ggplot(aes(x = as.factor(n_obj), y = tall_brown - brown_tall, fill = as.factor(theta))) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(sd_brown ~ sd_tall) +
  ylab("difference in mean success probability") +
  xlab("number of objects in context")  +
  scale_fill_brewer(palette="Dark2", name = "semantic threshold")

show(results_plot)

