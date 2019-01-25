## we compare the probability of successfully communicating a given referent
## with one of two adjective orderings
## we fix the number of objects in context, sample their properties from iid standard Gaussians;
## the speaker's and listener's representations of these objects are noise-perturbed copies of 
## the true feature vectors;
## the more subjective an adjective, the more noise 

library(tidyverse)
library(HDInterval)
library(reshape)

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

## main function to gather outcomes for different parameter settings

get_outcomes = function(iterations, n_obj = 6, sd_brown = 0.1, sd_tall = 0.1, theta = 0.5) {
  outcomes = map_df(1:iterations, function(i, nobj = n_obj) {
    
    true_values = matrix(c(rnorm(n_obj),
                           rnorm(n_obj)), 
                         nrow = 2, byrow = TRUE)
    
    brown_world = get_truth_values(true_values[1,],theta)
    tall_world = get_truth_values(true_values[2,],theta)
    
    speaker_representation = get_agent_representation(true_values, n_obj, sd_brown, sd_tall) 
    listener_representation = get_agent_representation(true_values, n_obj, sd_brown, sd_tall) 
    
    brown_sp = get_truth_values(speaker_representation[1,], theta)
    tall_sp  = get_truth_values(speaker_representation[2,], theta)
    
    brown_li = get_truth_values(listener_representation[1,], theta)
    tall_li  = get_truth_values(listener_representation[2,], theta)
    
    referent = which(tall_sp & brown_sp)[1]
    
    tall_brown = as.integer(names(which(get_truth_values(listener_representation[2,][which(brown_li)], theta))))
    brown_tall = as.integer(names(which(get_truth_values(listener_representation[1,][which(tall_li)], theta))))
    
    tibble(tall_brown = ifelse(referent %in% tall_brown, 1/length(tall_brown), 0),
           brown_tall = ifelse(referent %in% brown_tall, 1/length(brown_tall), 0),
           referent = referent,
           exception = tall_brown < brown_tall,
           #num_referents_speaker = sum(tall_sp & brown_sp),
           #num_referents_listener = sum(tall_li & brown_li),
           #num_referents_world = sum(brown_world & tall_world),
           unique_tallbrown = sum(tall_sp & brown_sp) == 1)
    
  }) %>% filter(! is.na(referent))
  
  outcomes_all = outcomes %>% 
    gather(ordering, success_prob, brown_tall, tall_brown) %>% 
    group_by(ordering) %>% 
    summarize(context_types = "all", 
              sum_success_prob = sum(success_prob),
              eff_samples = n(),     # not all iterations give a data point:
                                     # we discard an iteration if it does not have any tall & brown object
              sum_expections = sum(exception)) %>% 
    ungroup() %>% 
    mutate(mean_success = sum_success_prob / eff_samples,
           proportion_exceptions = sum_expections / eff_samples,
           iterations = iterations,
           n_obj = n_obj, 
           sd_brown = paste0("sd_brown: ", sd_brown),
           sd_tall = paste0("sd_tall: ", sd_tall),
           theta = theta)
  
  outcomes_uniqueContexts = outcomes %>% filter(unique_tallbrown == TRUE) %>% 
    gather(ordering, success_prob, brown_tall, tall_brown) %>% 
    group_by(ordering) %>% 
    summarize(context_types = "unique", 
              sum_success_prob = sum(success_prob),
              eff_samples = n(),     # not all iterations give a data point:
              # we discard an iteration if it does not have any tall & brown object
              sum_expections = sum(exception)) %>% 
    ungroup() %>% 
    mutate(mean_success = sum_success_prob / eff_samples,
           proportion_exceptions = sum_expections / eff_samples,
           iterations = iterations,
           n_obj = n_obj, 
           sd_brown = paste0("sd_brown: ", sd_brown),
           sd_tall = paste0("sd_tall: ", sd_tall),
           theta = theta)
  
  rbind(outcomes_all, outcomes_uniqueContexts)
}
