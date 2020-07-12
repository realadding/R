install.packages('isotree')

### Random data from a standard normal distribution
library(isotree)
set.seed(1)
n <- 100
m <- 2
X <- matrix(rnorm(n * m), nrow = n)

### Will now add obvious outlier point (3, 3) to the data
X <- rbind(X, c(3, 3))

### Fit a small isolation forest model
iso <- isolation.forest(X, ntrees = 100, nthreads = 1)

### Check which row has the highest outlier score

(pred <- predict(iso, X))

cat("Point with highest outlier score: ",X[which.max(pred), ], "\n")