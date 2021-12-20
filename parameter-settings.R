sim_param <- dplyr::tibble(
  expand.grid(
    p = c(1000),
    s = c(10),
    corr_level = c("low", "high"),
    SNR = c(.5, 1, 2), 
    full_dataset = TRUE, 
    n = NA, 
    path_raw_dataset = "./tcgaExpression.RData"
    )
  )
