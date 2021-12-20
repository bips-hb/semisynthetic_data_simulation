# Moritz's script 

# load TCGA Data
load("./tcgaExpression.RData") 

# transpose data since rows are variables and columns are observations
tcgaExpression <- t(tcgaExpression)

# number of variables for the reduced data set
subdata_size <- 1000

# draw variables
TCGA_subdata <- tcgaExpression[,sample(1:ncol(tcgaExpression), size = subdata_size)]

# standardize variables
TCGA_subdata <- scale(TCGA_subdata)

# get correlation matrix
Corr_tcga <- cor(TCGA_subdata)

# since it is symmetric we need only the upper triangle to find the highest correlations
Corr_tcga[lower.tri(Corr_tcga)] <- 0

# set diag to zero since it contains 1s
diag(Corr_tcga) <- 0

# find highest correlation and corresponding variables
max_indices <- 
  which(Corr_tcga == max(Corr_tcga), arr.ind = TRUE)

# get the 9 other variables with the highest correlation with variable 1
non_zero_indices <- 
  sort(
    c(max_indices[1], 
      which(Corr_tcga[max_indices[1],] %in% 
              sort(Corr_tcga[max_indices[1],], decreasing = TRUE)[2:10]))
  )

snr <- 1
n <- 1000

beta <- rep(0, ncol(TCGA_subdata))

beta[non_zero_indices] <- 1

Sigma <- cor(TCGA_subdata)

sigma <- sqrt( (t(beta) %*% Sigma %*% beta) / snr )

error <- rnorm(n) %*% sigma  
