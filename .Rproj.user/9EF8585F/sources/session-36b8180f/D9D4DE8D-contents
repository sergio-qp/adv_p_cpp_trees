source("R/utils.R") #call our utils script
library(Rcpp)
library(microbenchmark)
# You must program this function, first in R, then in C++
gini_impurity_R <- function(left,right){
  # first, we calculate left and right gini scores using the formula provided, getting the ratio of 1s and 0s in each
  ginileft = 1 - ( (sum(left)/length(left))^2 + (( 1-(sum(left)/length(left)) )^2) )
  giniright = 1 - ( (sum(right)/length(right))^2 + (( 1-(sum(right)/length(right)) )^2) )
  tot_length = length(left) + length(right) #length of the collective data
  # calculate weighted average gini using formula and return it
  avg_gini = (length(left)/tot_length)*ginileft + (length(right)/tot_length)*giniright
  avg_gini
}

best_split_R <- function(X, y) {
  best_gini <- 1.0
  best_feature <- -1
  n_features <- ncol(X)
  
  for (feature in 1:n_features) {
    values <- unique(X[, feature])
    
    for (value in sort(values)) { #important to include sort or it doesn't match with the C version
      left_indices <- X[, feature] == value
      right_indices <- X[, feature] != value
      
      left <- y[left_indices]
      right <- y[right_indices]
      
      gini <- gini_impurity_R(left, right)
      
      if (sum(left[left == 0]) > sum(left[left == 1])) { left_majority = "No"; right_majority = "Yes"; }
      else { left_majority = "Yes"; right_majority = "No"; }

      if (gini < best_gini) {
        best_gini <- gini
        best_feature <- feature # Store the best feature index
        best_value <- value
        left_majority = left_majority
        right_majority = right_majority
      }
    }
  }
  
  output <- list(
    best_feature = best_feature,
    best_value = best_value,
    best_gini = best_gini,
    left_majority = left_majority,
    right_majority = right_majority
  )
  
  output
}

# X is a numeric matrix with the input predictors
# y is a numeric vector with the response values (the two classes: 0 or 1)
# In order to obtain X and y from the play_tennis dataset, you'll need the 
# num_matrix_from_df function.

fit_decision_stump_R <- function(X, y) {
  best_split_R(X, y) # Call best_split and return the best feature index
}


#X is factor matrix, y is response matrix, we apply the num_matrix function right away cuz why not
X = num_matrix_from_df(play_tennis[c(-5)])
y = num_matrix_from_df(play_tennis[c(5)])
# 
# fit_decision_stump_R(X,y) #nice, it seems to work
# 
# sourceCpp('src\\ds_cpp.cpp')
# 
# best_split_R(X, y)
# best_split_C(X, y)

#testing with randomized y vector

# rand_y = sample(y)
# best_split_R(X, rand_y)
# best_split_C(X, rand_y)

#microbenchmarking, looks good
# microbenchmark(
#   best_split_R(X, y),
#   best_split_C(X, y),
#   times = 100
# )


