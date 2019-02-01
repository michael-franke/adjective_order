source("01_referential_success_k-percent_semantics.r")

## sample from the parameter space and collect predictions

## number of samples to take
# careful! this is `n_samples` samples per 9 combinations of distribution types
n_samples = 100000

out = map_df(1:n_samples, function(i) {
  
  # sample parameter values
  n_obj = sample(4:20,1)
  sd_adjs = runif(2, 0, 0.5)
  sd_brown = min(sd_adjs)
  sd_tall = max(sd_adjs) 
  theta = runif(1, 0, 1)
  
  # rbind(
  #   get_single_outcome(n_obj = n_obj, sd_brown = sd_brown, sd_tall = sd_tall, theta = theta, 
  #                      dist_brown = open, dist_tall = open, short_output = F),
  #   get_single_outcome(n_obj = n_obj, sd_brown = sd_brown, sd_tall = sd_tall, theta = theta, 
  #                      dist_brown = open, dist_tall = closed, short_output = F),
  #   # get_single_outcome(n_obj = n_obj, sd_brown = sd_brown, sd_tall = sd_tall, theta = theta, 
  #   #                    dist_brown = open, dist_tall = half_open, short_output = F),
  #   get_single_outcome(n_obj = n_obj, sd_brown = sd_brown, sd_tall = sd_tall, theta = theta, 
  #                      dist_brown = closed, dist_tall = open, short_output = F),
  #   get_single_outcome(n_obj = n_obj, sd_brown = sd_brown, sd_tall = sd_tall, theta = theta, 
  #                      dist_brown = closed, dist_tall = closed, short_output = F)
  #   # get_single_outcome(n_obj = n_obj, sd_brown = sd_brown, sd_tall = sd_tall, theta = theta, 
  #   #                    dist_brown = closed, dist_tall = half_open, short_output = F),
  #   # get_single_outcome(n_obj = n_obj, sd_brown = sd_brown, sd_tall = sd_tall, theta = theta, 
  #   #                    dist_brown = half_open, dist_tall = open, short_output = F),
  #   # get_single_outcome(n_obj = n_obj, sd_brown = sd_brown, sd_tall = sd_tall, theta = theta, 
  #   #                    dist_brown = half_open, dist_tall = closed, short_output = F),
  #   # get_single_outcome(n_obj = n_obj, sd_brown = sd_brown, sd_tall = sd_tall, theta = theta, 
  #   #                    dist_brown = half_open, dist_tall = half_open, short_output = F)
  # )
  get_single_outcome(n_obj = n_obj, sd_brown = sd_brown, sd_tall = sd_tall, theta = theta, 
                     dist_brown = open, dist_tall = open, short_output = F)
})  %>% filter(! is.na(referent))

write_csv(x = out, path = "03_mc_results.csv")

#print success
print(paste0("average EU of 'tall brown': ", mean(out$tall_brown) %>% round(4)))
print(paste0("average EU of 'brown tall': ", mean(out$brown_tall) %>% round(4)))


