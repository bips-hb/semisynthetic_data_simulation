sim_param <- dplyr::tibble(
  expand.grid(
    p = c(1000),
    s = c(10),
    corr_level = c("low", "high"),
    snr = c(.42, 1.22, 3.52), 
    full_dataset = FALSE, 
    n = 100#, 
    #path_raw_dataset = "./tcgaExpression.RData"
    )
  )
