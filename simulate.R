#' Simulate a semi-synthetic dataset based on TCGA data
#'
#' Returns a semi-synthetic dataset based on TCGA data. 
#'
#' @param p number of parameters
#' @param s number of non-zero coefficients
#' @param corr_level either 'high' or 'low'
#' @param snr signal-to-noise ratio
#' @param full_dataset if true, full dataset (n = 594) is used, 
#'                    otherwise n observations are select (Default = TRUE)
#' @param n number of observations (Default: not used)
#' @param path_raw_dataset Path to the raw dataset 
#'                              (Default = './tcgaExpression.RData')
#'
#' @return A list with X, y, beta and (indices of the) true_predictors 
#' @export
generate_semisynthetic_dataset <- function(p = 1000, s = 10,
                    corr_level = 'low',
                    snr = 1,
                    full_dataset = TRUE, 
                    n = NA, 
                    path_raw_dataset = "./tcgaExpression.RData") {
  
  # Load in the raw dataset (TODO: read in the dataset only once)
  load(path_raw_dataset) 
  
  # transpose data since rows are variables and columns are observations
  tcgaExpression <- t(tcgaExpression)
  
  # subsample the dataset 
  if (full_dataset) { 
    X <- tcgaExpression[ ,sample( 1:ncol(tcgaExpression), size = p) ]
    n <- nrow(tcgaExpression)
    
  } else { 
    
    # check whether parameter settings are valid
    if (is.na(n)) { 
       stop("n needs to be set if full_dataset = FALSE")
    }
    if (n > nrow(tcgaExpression)) { 
      stop("n is larger than the number of observations in the original dataset") 
    }
    
    X <- tcgaExpression[sample(1:nrow(tcgaExpression), size = n) ,sample(1:ncol(tcgaExpression), size = p) ] 
  }

  # standardize variables
  X <- scale(X)
  
  # get correlation matrix of the subsampled dataset
  corr_X <- cor(X)
  

  # no strong correlation between the true predictors. True predictors 
  # are selected at random
  if (corr_level == 'low') { 
    non_zero_indices <- sample(1:p, size = s)
  }
  
  # strong correlation between the true predictors
  if (corr_level == 'high') { 
    # since it is symmetric we need only the upper triangle to find the highest correlations
    corr_X[lower.tri(corr_X)] <- 0
    
    # set diag to zero since it contains 1s
    diag(corr_X) <- 0
    
    # find highest correlation and corresponding variables
    max_indices <- 
      which(corr_X == max(corr_X), arr.ind = TRUE)
    
    # get the 9 other variables with the highest correlation with variable 1
    non_zero_indices <- 
      sort(
        c(max_indices[1], 
          which(corr_X[max_indices[1],] %in% 
                  sort(corr_X[max_indices[1],], decreasing = TRUE)[2:10]))
      )
    
  }

  # beta vector
  beta <- rep(0, ncol(X))
  beta[non_zero_indices] <- 1
  
  sigma <- sqrt( (t(beta) %*% corr_X %*% beta) / snr )
  
  error <- rnorm(n) %*% sigma  
  
  y <- X %*% beta + error
  
  list(
    X = X,
    y = y, 
    beta = beta,
    true_predictors = non_zero_indices
  )
}
