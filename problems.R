source('simulate.R')

simulator_wrapper <-
  function(data, job, p, s, corr_level, snr, full_dataset, n, ...) { #path_raw_dataset, ...) {

  data <- generate_semisynthetic_dataset(p = p, 
                                         s = s,
                                         corr_level = corr_level,
                                         snr = snr,
                                         full_dataset = full_dataset, 
                                         n = n)#, 
                                         #path_raw_dataset = path_raw_dataset) 
    
  }
