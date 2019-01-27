## we compare the probability of successfully communicating a given referent
## with one of two adjective orderings
## we fix the number of objects in context, sample their properties from iid standard Gaussians;
## the speaker's and listener's representations of these objects are noise-perturbed copies of 
## the true feature vectors;
## the more subjective an adjective, the more noise 

library(tidyverse)
library(HDInterval)
library(truncdist)

## convenience functions

get_agent_representation = function(true_values, n_obj, sd_brown, sd_tall, bounded_dist_behaviour = "norm") {
  out = matrix(c(rnorm(n_obj, mean = true_values[1,], sd_brown),
                 rnorm(n_obj, mean = true_values[2,], sd_tall)), 
               nrow = 2, byrow = TRUE)
  colnames(out) = 1:n_obj
  rownames(out) = c("brown", "tall")
  out
}

get_truth_values = function(representation, theta) {
  out = representation > (max(representation) - min(representation) * theta) + min(representation)
  # at least the maximal element has the property in question
  if (sum(out) == 0) {out[which.max(as.vector(representation))] = TRUE}
  names(out) = names(representation)
  out
}

get_ground_value_for_adj = function(n_obj, adj_dist_type, norm_dist_behaviour) {
  out = switch(adj_dist_type, #adjective is normally distributed 
                              norm={switch(norm_dist_behaviour,
                                                 #normal dist without boundaries
                                                 norm = rnorm(n_obj),
                                                 #bounded "normal" dist
                                                 trunc = rtrunc(n_obj, spec="norm", a=0, b=1),
                                                 minmax = pmin(pmax(rnorm(n_obj),0),1))},
                              #beta(1,1)
                              beta_1_1={rbeta(n_obj, shape1 = 1, shape2 = 1)},
                              #beta(1,5)
                              beta_1_5={rbeta(n_obj, shape1 = 1, shape2 = 5)}
  )
  out
}
  

## main function to get an outcome for given parameter settings

get_outcome = function(n_obj, sd_brown, sd_tall, theta, ground_truth_dist_brown,  ground_truth_dist_tall, norm_dist_behaviour = "norm") {

    true_values = matrix(
      c(get_ground_value_for_adj(n_obj, ground_truth_dist_brown, norm_dist_behaviour), get_ground_value_for_adj(n_obj, ground_truth_dist_brown, norm_dist_behaviour)), 
      nrow = 2,
      byrow = TRUE)
    
    speaker_representation = get_agent_representation(true_values, n_obj, sd_brown, sd_tall) 
    listener_representation = get_agent_representation(true_values, n_obj, sd_brown, sd_tall) 
    
    brown_world = get_truth_values(true_values[1,],theta)
    tall_world = get_truth_values(true_values[2,],theta)
    
    brown_sp = get_truth_values(speaker_representation[1,], theta)
    tall_sp  = get_truth_values(speaker_representation[2,], theta)
    
    brown_li = get_truth_values(listener_representation[1,], theta)
    tall_li  = get_truth_values(listener_representation[2,], theta)
    
    referent = which(tall_sp & brown_sp)[1]
    
    tall_brown = as.integer(names(which(get_truth_values(listener_representation[2,][which(brown_li)], theta))))
    brown_tall = as.integer(names(which(get_truth_values(listener_representation[1,][which(tall_li)], theta))))
    
    tibble(#output 
           tall_brown = ifelse(referent %in% tall_brown, 1/length(tall_brown), 0),
           brown_tall = ifelse(referent %in% brown_tall, 1/length(brown_tall), 0),
           referent = referent,
           success = tall_brown > brown_tall,
           exception = !success,
           sd_adjs_equal = sd_tall == sd_brown,
           num_referents_speaker = sum(tall_sp & brown_sp),
           num_referents_listener = sum(tall_li & brown_li),
           num_referents_world = sum(brown_world & tall_world),
           unique_tallbrown = num_referents_speaker == 1,
           #input
           n_obj = n_obj,
           sd_brown = sd_brown,
           sd_tall = sd_tall, 
           theta = theta, 
           ground_truth_dist_brown = ground_truth_dist_brown, 
           ground_truth_dist_tall = ground_truth_dist_tall
           )
}


distributions = c("norm", "beta_1_1", "beta_1_5")

#three possible match_normal_dist_to_closed_dist_behaviors
dist_behaviours = c("norm", "trunc", "minmax")
iterations = 5*10^6

data = list(); data_trunc = list(); data_minmax = list();
for (i in 1:iterations) {
  nobj = sample(4:20,1)
  
  sd_adjs = runif(2, 0.1, 0.4)
  sd_adj1 = min(sd_adjs) # we want to make sure that the first adj is less subjective
  sd_adj2 = max(sd_adjs) 
  
  threshold = runif(1, 0.2, 0.8)
  
  ground_dist_adj1 = distributions[sample(1:3,1)]
  ground_dist_adj2 = distributions[sample(1:3,1)]
  dist_behaviour = dist_behaviours[sample(1:3,1)] 
  
  dataline = get_outcome(n_obj = nobj, sd_brown = sd_adj1, sd_tall = sd_adj2, theta = threshold,
                         ground_truth_dist_brown = ground_dist_adj1,
                         ground_truth_dist_tall = ground_dist_adj2, 
                         norm_dist_behaviour = dist_behaviour)
    data[[i]] <- dataline
}

results=bind_rows(data)

#filter out meaningless cases
results = results %>% filter(! is.na(referent))

# write csv
write.csv(results, file = "big.csv")

#print success
print(sum(results$success)/nrow(results))
