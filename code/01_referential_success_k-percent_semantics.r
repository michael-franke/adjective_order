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

get_agent_representation = function(true_values, 
                                    n_obj, 
                                    sd_brown, 
                                    sd_tall, 
                                    lower_bound_brown = -Inf, 
                                    upper_bound_brown = Inf,
                                    lower_bound_tall = -Inf, 
                                    upper_bound_tall = Inf) {
  out = matrix(
    c(
      map_dbl(1:n_obj, function(i) {
        rtrunc(n = 1, 
               spec = "norm", 
               a = lower_bound_brown, 
               b = upper_bound_brown, 
               mean = true_values[1,i], 
               sd = sd_brown)
      }),
      map_dbl(1:n_obj, function(i) {
        rtrunc(n = 1, 
               spec = "norm", 
               a = lower_bound_tall, 
               b = upper_bound_tall, 
               mean = true_values[2,i], 
               sd = sd_tall)
      })
    ),
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

## define ground-truth distributions

open = list("name" = "open",
            "fct_call" = "rnorm",
            "parameters" = list(mean = 0, sd = 1),
            "lower_bound" = -Inf,
            "upper_bound" = Inf)

closed = list("name" = "closed",
              "fct_call" = "runif",
              # the size of the interval matters because the standard deviation of the subjective noise
              # is relative to the "spacing between objects" so to speak; we set it, therefore, in 
              # correspondence to the 99.99% credible interval of the standard normal distribution
              "parameters" = list(min = 0, max = 20*qnorm(0.9999)),
              "lower_bound" = -Inf,
              "upper_bound" = Inf)

# half_open = list("name" = "half-open",
#                    "fct_call" = "rbeta",
#                    "parameters" = list(shape1 = 5, shape2 = 1),
#                    "lower_bound" = -Inf,
#                    "upper_bound" = Inf)

## generate a single context / data point
get_single_outcome = function(n_obj = 6, sd_brown = 0.1, sd_tall = 0.1, theta = 0.5, 
                              dist_brown = open, dist_tall = open, short_output = T) {
  
  # add the number of objects to the distributions from which
  # to sample actual feature values
  dist_brown$parameters$n = n_obj
  dist_tall$parameters$n = n_obj
  
  true_values = matrix(c(do.call(dist_brown$fct_call, dist_brown$parameters),
                         do.call(dist_tall$fct_call, dist_tall$parameters)), 
                       nrow = 2, byrow = TRUE)
  
  speaker_representation = get_agent_representation(
    true_values, 
    n_obj, 
    sd_brown, 
    sd_tall,
    lower_bound_brown = dist_brown$lower_bound,
    upper_bound_brown = dist_brown$upper_bound,
    lower_bound_tall = dist_tall$lower_bound,
    upper_bound_tall = dist_tall$upper_bound
  ) 
  
  listener_representation = get_agent_representation(
    true_values, 
    n_obj, 
    sd_brown, 
    sd_tall,
    lower_bound_brown = dist_brown$lower_bound,
    upper_bound_brown = dist_brown$upper_bound,
    lower_bound_tall = dist_tall$lower_bound,
    upper_bound_tall = dist_tall$upper_bound
  ) 
  
  brown_world = get_truth_values(true_values[1,],theta)
  tall_world = get_truth_values(true_values[2,],theta)
  
  brown_sp = get_truth_values(speaker_representation[1,], theta)
  tall_sp  = get_truth_values(speaker_representation[2,], theta)
  
  brown_li = get_truth_values(listener_representation[1,], theta)
  tall_li  = get_truth_values(listener_representation[2,], theta)
  
  referent = which(tall_sp & brown_sp)[1]
  
  tall_brown = as.integer(names(which(get_truth_values(listener_representation[2,][which(brown_li)], theta))))
  brown_tall = as.integer(names(which(get_truth_values(listener_representation[1,][which(tall_li)], theta))))
  
  if (short_output) {
    tibble(
      tall_brown = ifelse(referent %in% tall_brown, 1/length(tall_brown), 0),
      brown_tall = ifelse(referent %in% brown_tall, 1/length(brown_tall), 0),
      referent = referent,
      exception = tall_brown < brown_tall,
      unique_tallbrown = length(which(tall_sp & brown_sp)) == 1
    )  
  } else {
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
      dist_brown = dist_brown$name, 
      dist_tall = dist_tall$name
    )
  }
  
}

## gather outcomes for a fixed parameter setting and summarize over them
get_outcomes = function(iterations, n_obj = 6, sd_brown = 0.1, sd_tall = 0.1, theta = 0.5, 
                        dist_brown = open, dist_tall = open) {
  
  # generate outcomes
  outcomes = map_df(1:iterations, function(i) get_single_outcome(
    n_obj, 
    sd_brown, 
    sd_tall,
    theta, 
    dist_brown, 
    dist_tall,
    short_output = T,
  )) %>% filter(! is.na(referent))
  
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

