process_results <- function(res, p, method = 'fs', alpha = NULL, true_predictors, s = 10) { 
  
  if (method == 'enet' & is.null(alpha)) { 
    stop("alpha should be given when method is enet") 
  }
  
  false_predictors <- setdiff(1:p, true_predictors)
  
  # initialize performance data frame
  performance <- dplyr::tibble(
    method = method, 
    alpha = NA, 
    p = p, 
    s = s,
    k = 1:p,
    finished = NA, 
    TP = NA, 
    TN = NA,
    FP = NA,
    FN = NA
  )
  
  compute_performance_measures <- function(selected_predictors, index) { 
    not_selected_predictors <- setdiff(1:p, selected_predictors) # all variables not selected
    performance$TP[index] <<- length( intersect(selected_predictors, true_predictors) )
    performance$TN[index] <<- length( intersect(not_selected_predictors, false_predictors) )
    performance$FP[index] <<- length( intersect(selected_predictors, false_predictors) )
    performance$FN[index] <<- length( intersect(not_selected_predictors, true_predictors) )
  }
  
  if (method == 'fs') { 
    
    k <- length(res$id)
    
    sapply(1:k, function(j) { 
      selected_predictors <- res$id[1:j] # all predictors up to j
      compute_performance_measures(selected_predictors, j)
      
      ### TODO remove
      
      print(selected_predictors)
      print(true_predictors)
      
      ### TODO remove
    })
  }
  if (method == 'bs') { 
    k <- length(res)
    sapply(1:k, function(j) { 
      selected_predictors <- res[[j]] # all predictors up to j
      compute_performance_measures(selected_predictors, j)
    })
  }
  
  if (method == 'enet') { 
    performance$alpha <- alpha 
    
    sapply(1:p, function(j) { 
      selected_predictors <- res$id[1:j] # all predictors up to j
      
      ### TODO remove
      
      print(selected_predictors)
      print(true_predictors)
      
      ### TODO remove
      compute_performance_measures(selected_predictors, j)
    })
  }
  
  performance %>% dplyr::mutate(
    recall = TP / (TP + FN),
    precision = TP / (TP + FP), 
    F1 = 2*(precision*recall)/(precision + recall)
  )
}
