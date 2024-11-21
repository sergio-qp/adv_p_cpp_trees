library(rpart)
library(rpart.plot)

#REPORT WHAT HAPPENS WHEN YOU CHANGE MAXDEPTH
load("R/data/play_tennis.rda")
model <- rpart(PlayTennis ~ ., play_tennis,
               control = rpart.control(cp = 0, maxdepth = 5, minsplit = 1,
                                       minbucket = 1))
rpart.plot(model)

num_matrix_from_df <- function(tennis_data){
  as.matrix(as.data.frame(lapply(tennis_data,
                                 function(x) as.integer(x)-1)))
  }

