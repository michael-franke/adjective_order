## we compare the probability of successfully communicating a given referent
## with one of two adjective orderings
## we fix the number of objects in context, sample their properties from iid standard Gaussians;
## the speaker's and listener's representations of these objects are noise-perturbed copies of 
## the true feature vectors;
## the more subjective an adjective, the more noise 

library(tidyverse)
library(HDInterval)

## convenience functions

get_agent_representation = function(true_values, n_obj, sd_brown, sd_tall) {
  out = matrix(c(rnorm(n_obj, mean = true_values[1,], sd_brown),
                 rnorm(n_obj, mean = true_values[2,], sd_tall)), 
               nrow = 2, byrow = TRUE)
  colnames(out) = 1:n_obj
  rownames(out) = c("brown", "tall")
  out
}

# get_truth_values = function(representation, theta) {
#   out = rank(as.vector(representation)) > ceiling(length(representation) * theta)
#   # at least the maximal element has the property in question
#   if (sum(out) == 0) {out[which.max(as.vector(representation))] = TRUE} 
#   names(out) = names(representation)
#   out
# }

get_truth_values = function(representation, theta) {
  out = representation > (max(representation) - min(representation) * theta) + min(representation)
  # at least the maximal element has the property in question
  if (sum(out) == 0) {out[which.max(as.vector(representation))] = TRUE}
  names(out) = names(representation)
  out
}

## main function to get an outcome for given parameter settings

get_outcome = function(n_obj, sd_brown, sd_tall, theta, ground_truth_dist_brown,  ground_truth_dist_tall, bounded_dist_behaviour = "not implemented") {


    true_values = matrix(c( switch(ground_truth_dist_brown, norm={rnorm(n_obj)}, beta_1_1={rbeta(n_obj, shape1 = 1, shape2 = 1)}, beta_1_5={rbeta(n_obj, shape1 = 1, shape2 = 5)}), 
                            switch(ground_truth_dist_brown, norm={rnorm(n_obj)}, beta_1_1={rbeta(n_obj, shape1 = 1, shape2 = 1)}, beta_1_5={rbeta(n_obj, shape1 = 1, shape2 = 5)}) 
                           ), 
                         nrow = 2,
                         byrow = TRUE)
    
    # TODO: bounded_dist_behaviour
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
           exception = tall_brown < brown_tall,
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
           ground_truth_dist_tall = ground_truth_dist_tall,
           bounded_dist_behaviour = bounded_dist_behaviour
           )
}


distributions = c("norm", "beta_1_1", "beta_1_5")
closed_dist_behavior= c("map_to_boundary", "truncated_dist")


iterations = 50
data = list()

for (i in 1:iterations) {
  nobj = sample(4:20,1)
  
  sd_adjs = runif(2, 0.1, 0.5)
  sd_adj1 = min(sd_adjs) # we want to make sure that the first adj is less subjective
  sd_adj2 = max(sd_adjs) 
  
  threshold = runif(1, 0, 1)
  
  ground_dist_adj1 = distributions[sample(1:3,1)]
  ground_dist_adj2 = distributions[sample(1:3,1)]
  
  dataline = get_outcome(n_obj = nobj, sd_brown = sd_adj1, sd_tall = sd_adj2, theta = threshold, ground_truth_dist_brown = ground_dist_adj1,  ground_truth_dist_tall = ground_dist_adj2)
  data[[i]] <- dataline
}

results=bind_rows(data)


# write csv
write.csv(results, file = "big.csv")

