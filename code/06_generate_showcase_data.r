source("01_referential_success_k-percent_semantics.r", encoding="utf-8")

iterations = 10000

data = list(); i = 1;
for (n in seq(4,20,4)) {
  for (th in seq(0.2,0.8,0.2)) {
    dat  <- get_outcomes(iterations, n_obj = n, sd_brown = 0.1, sd_tall = 0.25, theta = th)
    dat1  <- get_outcomes(iterations, n_obj = n, sd_brown = 0.1, sd_tall = 0.3, theta = th)
    dat2  <- get_outcomes(iterations, n_obj = n, sd_brown = 0.2, sd_tall = 0.25, theta = th)
    dat3  <- get_outcomes(iterations, n_obj = n, sd_brown = 0.2, sd_tall = 0.3, theta = th)
    data[[i]] <- dat; data[[i+1]] <- dat1;  data[[i+2]] <- dat2;  data[[i+3]] <- dat3
    i = i + 4
  }
}
results = do.call(rbind, data)

selection = select(filter(results, context_types == "all"), -sum_success_prob) %>% 
  spread(key = ordering, value = mean_success) 

write_csv(x = selection, path = "../results/06_showcase_results.csv")