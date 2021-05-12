library(tidyverse)
library(lubridate)
library(dslabs)
library(dplyr)
library(tidyverse)
library(matrixStats)
library(caret)
library(recosystem)
library(tinytex)

######Movielens quiz
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")

# if using R 3.5 or earlier, use `set.seed(1)` instead

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#Exploration

# checking the stracture and for "NAs"
str(edx)
sum(is.na(edx))

# Number of distinct users and movies
edx %>% summarize(n_users = n_distinct(userId),n_movies = n_distinct(movieId))

# Ratings by movie
edx %>% dplyr::count(movieId)%>%ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black")+scale_x_log10() + labs(title="Movies", x="", y="Count")

# Ratings by user
edx %>% dplyr::count(userId)%>%ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + scale_x_log10() + labs(title="Users", x="", y="Count")

# Table that shows year, average, frequency of rating, overall average and spread (the difference)
edx %>% mutate(timestamp=as_datetime(timestamp)) %>% mutate(Year=year(timestamp)) %>% group_by(Year) %>% 
  summarize(Average=round(mean(rating), digits=2), N=n()) %>% arrange(Year) %>% data.frame() %>% 
  cbind(.,Overall_Average=round(mean(edx$rating), digits=2)) %>% mutate(Spread=Average-Overall_Average)

# Ratings by year
edx %>% mutate(timestamp=as_datetime(timestamp)) %>% mutate(year=year(timestamp)) %>% group_by(year, rating) %>%
  summarize(n = n()) %>% arrange(year) %>% ggplot()+geom_point(aes(year,rating, color=rating, size=n/10))

# Yearly averages
edx %>% mutate(timestamp=as_datetime(timestamp)) %>% mutate(year=year(timestamp)) %>% group_by(year) %>% 
  summarize(ave=mean(rating), n=n()) %>% arrange(year) %>% ggplot(aes(year,ave,size=sqrt(n), color=ave )) + 
  geom_point()+geom_hline(yintercept = mean(edx$rating))

# Year movie released vs number of ratings
years<-seq(1980, 2009)
nrating_year_released<-sapply(as.character(years), function(t){
  sum(str_detect(edx$title, t))
})
data.frame(Year_Released=c(1980:2009),N_Ratings=nrating_year_released, row.names = NULL) %>% 
  ggplot(aes(Year_Released, sqrt(N_Ratings))) + geom_point() + labs(title="Year of Release", x=" Year", y="Times Rated")

# Index for 1995 movies
ind95<-which(str_detect(edx$title, "1995")==TRUE)

# Summary of 1995 movies
data.frame(NReviews=nrow(edx[ind95,]),Ave=mean(edx[ind95,]$rating))
edx$title[ind95] %>% n_distinct()

# Top rated in 1995
edx95<-edx[ind95,] %>% group_by(movieId)%>%summarize(n=n(), ave_rating=mean(rating)) %>% arrange(desc(n)) %>%data.frame()

# Top 5 rated movies 
unique(edx$title[edx$movieId==edx95[1:5,1]])

# Plot of movie ratings in 1995
edx95 %>% ggplot(aes(n, ave_rating))+geom_point()+geom_smooth()

# Plot of movie ratings all
edx %>% group_by(movieId) %>% summarize(n=n(), ave_rating=mean(rating)) %>% 
  arrange(desc(n)) %>% ggplot(aes(n, ave_rating))+geom_point()+geom_smooth()

# Plot of user ratings all
edx %>% group_by(userId) %>% summarize(n=n(), ave_rating=mean(rating)) %>% 
  arrange(desc(n)) %>% ggplot(aes(n, ave_rating))+geom_point()+geom_smooth()

# Plot of average ratings by hour 
edx %>% mutate(time=hour(as_datetime(timestamp))) %>% group_by(time) %>% 
  summarize( ave_rating=mean(rating)) %>% ggplot(aes(time, ave_rating))+geom_point()+geom_smooth()

# Plot of number of ratings by hour
edx %>% mutate(time=hour(as_datetime(timestamp))) %>% ggplot(aes(time)) + 
  geom_histogram(bins=24, color = "black")+labs(title="", x="Time of the Day", y="Frequency of Ratings")

# Plot of average ratings by week
edx %>% mutate(week=week(as_datetime(timestamp))) %>% group_by(week) %>% 
  summarize( ave_rating=mean(rating), n=n()) %>% ggplot(aes(week, ave_rating, size=n))+geom_point()+geom_smooth()

# Clustering data - top 25 matrix
top <- edx %>%
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>% top_n(25, n) %>%
  pull(movieId)

x <- edx %>%
  filter(movieId %in% top) %>% group_by(userId) %>%
  filter(n() >= 25) %>%
  ungroup() %>%
  select(title, userId, rating) %>% spread(userId, rating)
head(x)
row_names <- str_remove(x$title, ": Episode") %>% str_trunc(20) 
x <- x[,-1] %>% as.matrix()
head(x)
x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- row_names

# Calculating the distances
d <- dist(x)

# Hierarchical clustering
h <- hclust(d)
plot(h, cex = 0.65, main = "Hierarchical clustering", xlab = "Top 25 Rated movies")
groups <- cutree(h, k = 5)
split(names(groups),groups)

# Using k-means clustering
set.seed(10, sample.kind="Rounding")
x_0 <- x
x_0[is.na(x_0)] <- 0
k <- kmeans(x_0, centers = 5)
groupsk <- k$cluster
split(names(groupsk),groupsk)

# Heatmap sorted by users from the highest standard deviation to low 
sds <- colSds(x, na.rm = TRUE)
o <- order(sds, decreasing = TRUE)[1:25]
heatmap(x[,o], col = RColorBrewer::brewer.pal(11, "Spectral"))

# Per course instructions which state:
#  "IMPORTANT: Make sure you do NOT use the validation set to train your algorithm. 
#   The validation set should ONLY be used to test your final algorithm. 
#   You should split the edx data into a training and test set."

# We split the data into training and test sets

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2,
                                  list = FALSE) 
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# We make sure we don’t include users and movies in the test set that do not appear in the training set.
test_set <- test_set %>% semi_join(train_set, by = "movieId") %>% semi_join(train_set, by = "userId")

# We also define loss function using the following code:

RMSE <- function(true_ratings, predicted_ratings){ sqrt(mean((true_ratings - predicted_ratings)^2))
}

# MODEL 0
mu <- mean(train_set$rating)
RMSE_0 <- RMSE(test_set$rating, mu)
RMSE_0
rmse_results0 <- data_frame(Model = "Naive (just the average)", RMSE = RMSE_0, Improvement = NA, OvImprovement = NA)
rmse_results0

# MODEL 1
mu
movie_avgs <- train_set %>%group_by(movieId) %>% summarize(b_i = mean(rating - mu))
predicted_ratings <- test_set %>%left_join(movie_avgs, by='movieId') %>% mutate(pred = mu + b_i) %>% pull(pred)
RMSE_1<-RMSE(test_set$rating, predicted_ratings)
RMSE_1
rmse_results1 <- bind_rows(rmse_results0, data_frame(Model = "Movie effect", RMSE = RMSE_1, Improvement = RMSE_0-RMSE_1, OvImprovement = RMSE_0-RMSE_1))
rmse_results1

# MODEL 2
user_avgs <- train_set %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>%left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId')%>%mutate(pred = mu + b_i + b_u) %>% pull(pred)
RMSE_2<-RMSE(test_set$rating, predicted_ratings)
RMSE_2
RMSE_1-RMSE_2
rmse_results2 <- bind_rows(rmse_results1, data_frame(Model = "Movie and User effect", RMSE = RMSE_2, Improvement = RMSE_1-RMSE_2, OvImprovement = RMSE_0-RMSE_2))
rmse_results2

# MODEL 3
genres_avgs<-train_set %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId')%>%group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))
predicted_ratings <- test_set %>%left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId')%>%
  left_join(genres_avgs, by='genres')%>%mutate(pred = mu + b_i + b_u + b_g) %>% pull(pred)
RMSE_3<-RMSE(test_set$rating, predicted_ratings)
RMSE_3
RMSE_2-RMSE_3
rmse_results3 <- bind_rows(rmse_results2, data_frame(Model = "Movie, User and Genre effect", RMSE = RMSE_3, Improvement = RMSE_2-RMSE_3, OvImprovement = RMSE_0-RMSE_3))
rmse_results3

# MODEL 4
time_avgs<-train_set %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId')%>%left_join(genres_avgs, by='genres')%>%
  mutate(time=hour(as_datetime(timestamp)))%>%group_by(time) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u - b_g))
predicted_ratings <- test_set %>%left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId')%>%
  left_join(genres_avgs, by='genres') %>% mutate(time=hour(as_datetime(timestamp)))%>%left_join(time_avgs, by='time')%>% mutate(pred = mu + b_i + b_u + b_g + b_t) %>% pull(pred)
RMSE_4<-RMSE(test_set$rating, predicted_ratings)
RMSE_4
RMSE_3-RMSE_4
rmse_results4 <- bind_rows(rmse_results3, data_frame(Model = "Movie, User, Genre and Time effect", RMSE = RMSE_4, Improvement = RMSE_3-RMSE_4, OvImprovement = RMSE_0-RMSE_4))
rmse_results4

# MODEL 5
week_avgs<-train_set %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId')%>%left_join(genres_avgs, by='genres')%>%
  mutate(week=week(as_datetime(timestamp)))%>%group_by(week) %>%
  summarize(b_w = mean(rating - mu - b_i - b_u - b_g))
predicted_ratings <- test_set %>%left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId')%>%
  left_join(genres_avgs, by='genres') %>% mutate(week=week(as_datetime(timestamp)))%>%left_join(week_avgs, by='week')%>% mutate(pred = mu + b_i + b_u + b_g + b_w) %>% pull(pred)
RMSE_5<-RMSE(test_set$rating, predicted_ratings)
RMSE_5
RMSE_3-RMSE_5
rmse_results5 <- bind_rows(rmse_results4, data_frame(Model = "Movie, User, Genre and Week effect", RMSE = RMSE_5, Improvement = RMSE_3-RMSE_5, OvImprovement = RMSE_0-RMSE_5))
rmse_results5

# MODEL 6
year_avgs<-train_set %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId')%>%left_join(genres_avgs, by='genres')%>%
  mutate(year=year(as_datetime(timestamp)))%>%group_by(year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u - b_g))
predicted_ratings <- test_set %>%left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId')%>%
  left_join(genres_avgs, by='genres') %>% mutate(year=year(as_datetime(timestamp)))%>%left_join(year_avgs, by='year')%>% mutate(pred = mu + b_i + b_u + b_g + b_y) %>% pull(pred)
RMSE_6<-RMSE(test_set$rating, predicted_ratings)
RMSE_6
RMSE_3-RMSE_6
rmse_results6 <- bind_rows(rmse_results5, data_frame(Model = "Movie, User, Genre and Year effect", RMSE = RMSE_6, Improvement = RMSE_3-RMSE_6, OvImprovement = RMSE_0-RMSE_6))
rmse_results6



# To regularize the model, we will apply 5-fold cross validation to pick the most optimum lambda 

set.seed(1, sample.kind = "Rounding")
# make 5 splits
cv_5 <- createFolds(train_set$rating, k=5, returnTrain =TRUE)

# define a matrix to store the results of cross validation with 5 rows for each fold (k) and 49 columns for each lambda (length(seq(2,14,0.25)))
RMSEs <- matrix(nrow=5,ncol=49)
lambdas <- seq(2, 14, 0.25)

# NOTE: the following code will take some time to run, instead you can skip to directly assigning lambda to 4.75 (see below)

# perform 5-fold cross validation to determine the optimal lambda
for(k in 1:5) {
  train_set_cv <- train_set[cv_5[[k]],]
  test_set_cv <- train_set[-cv_5[[k]],]
  
  # Making sure userId and movieId in test set are also in the train set
  test_joined <- test_set_cv %>% 
    semi_join(train_set_cv, by = "movieId") %>%
    semi_join(train_set_cv, by = "userId")
  
  # Add rows removed back into the set
  removed <- anti_join(test_set_cv, test_joined)
  train_joined <- rbind(train_set_cv, removed)
  
  mu <- mean(train_joined$rating)
  
  RMSEs[k,] <- sapply(lambdas, function(l){
    b_i <- train_joined %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    b_u <- train_joined %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    b_g <- train_joined %>%
      left_join(b_i, by='movieId') %>% left_join(b_u, by='userId') %>% 
      group_by(genres) %>% summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
    predicted_ratings <- 
      test_joined %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>% left_join(b_g, by="genres") %>% 
      mutate(pred = mu + b_i + b_u + b_g) %>%
      pull(pred)
    return(RMSE(test_joined$rating, predicted_ratings))
  })
}
RMSEs
rmses_cv <- colMeans(RMSEs)
qplot(lambdas,rmses_cv)
lambda <- lambdas[which.min(rmses_cv)] 
lambda

# let's now apply this lambda to the model 

# Model 7
# lambda<-4.75

mu <- mean(train_set$rating)
b_i_reg <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u_reg <- train_set %>% 
  left_join(b_i_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
b_g_reg <- train_set %>%
  left_join(b_i_reg, by="movieId") %>% left_join(b_u_reg, by="userId") %>%
  group_by(genres) %>% summarize(b_g=sum(rating - b_i - b_u - mu)/(n()+lambda))
predicted_ratings7 <- test_set %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  left_join(b_g_reg, by="genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
RMSE_7 <- RMSE(test_set$rating, predicted_ratings7)  
RMSE_7

# compare RMSEs of the model 3 selected above with the optimized one
RMSE_3-RMSE_7

# table with results
rmse_results7 <- bind_rows(rmse_results6, data_frame(Model = "Movie, User, and Genre effect regularized", RMSE = RMSE_7, Improvement = RMSE_3-RMSE_7, OvImprovement = RMSE_0-RMSE_7))
rmse_results7 

# Now, lets apply it to the VALIDATION SET to see how the model would behave:
predicted_ratings_fin <- validation %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  left_join(b_g_reg, by="genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
RMSE_8<-RMSE(validation$rating, predicted_ratings_fin)

# table with results
final_assessment8 <- bind_rows(rmse_results7, data_frame(Model = "Test Validation Set: Movie, User, and Genre effect reg", RMSE = RMSE_8, Improvement = NA, OvImprovement = NA))
final_assessment8

# NOTE: the following code will take some time to run, instead you can use final_assessment above that was applied to the VALIDATION SET and yeilds 0.864461 RMSE


### This part provides further look into creating a better model employing matrix factorization  

# Applying recosystem package (R wrapper of the LIBMF library which is an open source tool for approximating an incomplete matrix using the product of two matrices.
# More information can be found reading help documentation. 
# The algorithm used by this package is described in the following paper: http://www.csie.ntu.edu.tw/~cjlin/papers/libmf/libmf_journal.pdf.

# Calculatings residuals by substracting regularized model predictors
train_set_res <- train_set %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  left_join(b_g_reg, by="genres")%>%
  mutate(residual = rating - mu - b_i - b_u - b_g) %>%
  select(userId, movieId, residual)
head(train_set_res)

# Now, we follow the step-by-step instructions listed in the recosystem package:
  
#1. Create a model object (a Reference Class object in R) by calling Reco().
#2. Call the $tune() method to select best tuning parameters along a set of candidate values.
#3. Train the model by calling the $train() method. A number of parameters can be set inside the function, possibly coming from the result of $tune().
#4. Use the $predict() method to compute predicted values.

# Please note, the code below takes about 20 minutes to execute.

# Creating two matrices that include train set and test set:
train_set_mf <- as.matrix(train_set_res)
test_set_mf <- test_set %>% 
  select(userId, movieId, rating)
test_set_mf <- as.matrix(test_set_mf)

# An object of class DataSource specifies the source of a data set, which can be created by calling it from a hard disk or an R object. 
# In this example, we will write it to a hard disc by using the write.table command and call it using data_file() command per recosystem's package instructions.
write.table(train_set_mf , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(test_set_mf, file = "testset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

# use data_file() to specify a data set from a file on the hard disk.
set.seed(1) 
train_set_mf <- data_file("trainset.txt")
test_set_mf <- data_file("testset.txt")

# create a model object (step 1)
r <-Reco()

# per recosystem's help documentation (step 2), tuning training set with parameters picked 
opts <- r$tune(train_set_mf, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 1, niter = 10))
opts

# training the recommender model (step 3)
r$train(train_set_mf, opts = c(opts$min, nthread = 1, niter = 20))

# writing prediction to file (step 4) 
pred_file <- tempfile()
r$predict(test_set_mf, out_file(pred_file))  
predicted_res_mf <- scan(pred_file)

# adding residual predictor ratings to previously predicted ratings
predicted_ratings_mf <- predicted_ratings7 + predicted_res_mf

# calculating RMSE
RMSE_MF <- RMSE(test_set$rating, predicted_ratings_mf) 
RMSE_MF
rmse_results9 <- bind_rows(final_assessment8,
                          data_frame(Model="Movie, User, and Genre effect regularized and MF",  
                                     RMSE = RMSE_MF, Improvement = RMSE_7-RMSE_MF, OvImprovement = RMSE_0-RMSE_MF))
rmse_results9

# Final test using VALIDATION DATA. Repeating steps 1 through 4 above.
pred_file <- tempfile()
final_validation<-validation %>% select(userId, movieId, rating) %>% as.matrix()
write.table(final_validation, file = "finvalset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
final_validation <- data_file("finvalset.txt")

r$predict(final_validation, out_file(pred_file))  
predicted_res_mf <- scan(pred_file)
final_predicted_ratings_mf <- predicted_ratings_fin + predicted_res_mf

# calculating RMSE using VALIDATION DATA
RMSE_MF_FIN<- RMSE(test_set$rating, predicted_ratings_mf) 
RMSE_MF_FIN
final_assessment_fm <- bind_rows(rmse_results9,
                          data_frame(Model="Final Validation Set: Movie, User, Genre effect regularized and MF",  
                                     RMSE = RMSE_MF_FIN, Improvement = RMSE_8-RMSE_MF_FIN, OvImprovement = RMSE_8-RMSE_MF_FIN))
final_assessment_fm 

#The End. Thank You!

