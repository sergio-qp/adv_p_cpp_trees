#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn

// [[Rcpp::export]]
double gini_impurity_C(NumericVector left, NumericVector right) {
  double ginileft = 1 - ( pow((sum(left)/left.length()), 2) + pow((1 - (sum(left)/left.length())), 2) );
  double giniright = 1 - ( pow((sum(right)/right.length()), 2) + pow((1 - (sum(right)/right.length())), 2) );
  double tot_length = left.length() + right.length();
  double avg_gini = (left.length()/tot_length)*ginileft + (right.length()/tot_length)*giniright;
  return avg_gini;
}

// so, the simple way this works is we loop thru each of our features and thru each
// of the values of those features, splitting our y response vector by it each time.
// We then calculate the average gini for these and if it's the smallest one so far,
// we save the gini, feature and value combination. The smallest one gets output.

// [[Rcpp::export]]
NumericVector best_split_C(NumericMatrix X, NumericVector y) {
  //declare our variables
  double best_gini = 1.0;
  int best_feature;
  int best_value;
  int maj_left;
  int maj_right;
  int n_features = X.ncol();
  int feature; //iterator/tracker for feature
  
  NumericVector values;
  for (feature = 0; feature < n_features; feature++) { //cycling by feature
    values = unique(X( _, feature)); //our list of possible values for this feature
    int value; //iterator/tracker for value

    for (value = 0; value <= max(values); value++) { //cycle through all our values
      NumericVector left_indices(X.nrow());
      NumericVector right_indices(X.nrow());
      double gini;
      int i; //general iterator

      //roundabout way of making the index vectors, using -1 instead of 0 since c++ indexes from 0
      //we save the actual index rather than a 1 so we can directly access the correct y values later
      for (i = 0; i < X.nrow(); i++) { 
        if (X( _, feature)[i] == value){
          left_indices[i] = i;
          right_indices[i] = -1;
        } else {
          left_indices[i] = -1;
          right_indices[i] = i;
        }
      }
    
      //index vectors shrunk to not have the -1s, not cloning because we're not gonna modify them
      NumericVector l_idx = left_indices[left_indices != -1];
      NumericVector r_idx = right_indices[right_indices != -1];
      
      NumericVector left(l_idx.length());
      NumericVector right(r_idx.length());
      
      //get the corresponding y's from the left and right indices
      for (i = 0; i < left.length(); i++) {
        left[i] = y[l_idx[i]];
      }
      for (i = 0; i < right.length(); i++) {
        right[i] = y[r_idx[i]];
      }
      
      //do the left and right class majorities, with 1 being Yes and 0 being No
      NumericVector zerosum = left[left == 0];
      NumericVector onesum = left[left == 1];
      
      double left_majority;
      double right_majority;
      if (sum(zerosum) > sum(onesum)) { 
        left_majority = 0; 
        right_majority = 1; 
      } else {
        left_majority = 1;
        right_majority = 0;
      }
      
      //compare gini to previous best
      gini = gini_impurity_C(left,right);
      
      if (gini < best_gini) { 
        best_gini = gini;
        best_feature = feature;
        best_value = value;
        maj_left = left_majority;
        maj_right = right_majority;
      }
    }
  }
  NumericVector output(5);
  output[0] = best_feature; //different from R because C++ is 0 indexed, referring to same feature
  output[1] = best_value;
  output[2] = best_gini;
  output[3] = maj_left;
  output[4] = maj_right;
  

  return output;
}