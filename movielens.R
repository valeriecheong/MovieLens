#DATA PREPARATION
if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(car)
library(dslabs)
library(broom)
library(lubridate)
library(scales)
library(stringr)
library(kableExtra)

##DOWNLOADING THE DATA AND DATA WRANGLING
#Movielens 10M dataset
#https://grouplens.org/datasets/movielens/10m/
#https://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl)) {
  download.file("https//files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
}

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file)) {
  unzip(dl)
}

movies_file <- "ml-10M100K/movies.dat"

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE), stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId), movieId = as.integer(movieId), rating = as.numeric(rating), timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE), stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

#Exploratory Data Analysis of Movielens Dataset
str(movielens)

knitr::kable(summary(movielens), caption = "Summary of 'movielens' dataset")

knitr::kable(head(movielens), caption = "First rows of 'movielens' dataset")

knitr::kable(tail(movielens), caption = "Last rows of 'movielens' dataset")

##SETTING UP OF TRAINING AND VALIDATION SET

#Final Hold-out test set will be 10% of Movielens data
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

#Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)


#Exploratory Data Analysis of edx dataset
summary(edx)

#How many threes were given as rating in the edx dataset?
ratings_3 <- edx %>%
  filter(ratings == 3)
nrow(ratings_3)

#Different movies and users
edx %>% summarise(n_users = n_distinct(userId), n_movies = n_distinct(movieId))


#Number of Movie Ratings Per Genres
genres <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Fantasy", "IMAX", "Sci-Fi", "Drama", "Horror", "Mystery", "Romance", "Thriller", "Crime", "War", "Western", "Musical", "Documentary", "Film-Noir")

genres_edx <- data.frame(Genres = genres, count = sapply(genres, function(x){
  sum(str_detect(edx$genres, x))
})
)

view(genres_edx)

#Order data frame genres_edx in ascending order
order(genres_edx[,"count"])


#Movie with the greatest number of ratings
edx_movies <- edx %>%
  group_by(title) %>%
  summarise(count = n()) %>%
  top_n(20, count) %>%
  arrange(desc(count))
view(edx_movies)

#Five Most Given Ratings
edx_ratings <- edx %>%
  group_by(rating) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
edx_ratings

##Deep Dive Analysis
#Number of ratings for every movie
ratings_per_movie <-edx %>%
  group_by(movieId) %>%
  summarise(n_ratings = n())

#Histogram of number of ratings for every movie
ggplot(ratings_per_movie, aes(x = n_ratings)) +
  geom_histogram(bins = 30, color = "black") +
  theme_gray()

#Number of Ratings For Every User
ratings_per_user <- edx %>%
  group_by(userId) %>%
  summarise(count = n())

#Histogram number of ratings for every user
ggplot(ratings_per_user, aes(x = count)) +
  geom_histogram(bins = 30, color = "blue") +
  scale_x_log10() +
  theme_linedraw()

##DATA WRANGLING edx and final_holdout_test DATASETS
#separating year of release from title
#edx dataset
year.released.date <- as.numeric(str_sub(edx$title, start = -5, end = -2))

edx <- edx %>%
  mutate(edx, year.release = year.released.date)


#final_holdout_test dataset
year.released.date.validation <- as.numeric(str_sub(final_holdout_test$title, start = -5, end = -2))

final_holdout_test <- final_holdout_test %>%
  mutate(final_holdout_test, year.release = year.released.date.validation)


#Converting timestamp to date
#edx dataset
edx <- edx %>%
  mutate(edx, date = as_datetime(timestamp))

#changing date format
edx <- edx %>%
  mutate(date = round_date(date, unit = "week"))


#final_holdout_test dataset
#converting timestamp into date
final_holdout_test <- final_holdout_test %>%
  mutate(final_holdout_test, date = as_datetime(timestamp))

#changing date format
final_holdout_test <- final_holdout_test %>%
  mutate(date = round_date(date, unit = "week"))


#Ratings Per Movie
#edx dataset
edx <- edx %>%
  mutate(duration = 2025 - year.release)


#final_holdout_test dataset
final_holdout_test <- final_holdout_test %>%
  mutate(duration = 2025 - year.release)


#Average Rating against Year of Release
edx %>%
  group_by(year.release) %>%
  summarise(avg.rating = mean(rating)) %>%
  ggplot(aes(year.release, avg.rating)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Year of Release", y = "Average Rating", title = "Avg Rating Against Year of Release")

##MOVIE EFFECT
#Average Rating Against Duration
edx %>%
  group_by(duration) %>%
  summarise(average.rating = mean(rating)) %>%
  ggplot(aes(duration, average.rating)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Duration", y = "Average Rating", title = "Average Rating Against Duration")

##TIME EFFECT on Average Rating
edx %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

##USER EFFECT
edx %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

##GENRES EFFECT
edx %>%
  group_by(genres) %>%
  summarise(n = n(), avg = mean(rating), se = sd(rating/sqrt(n()))) %>%
  filter(n >= 1000) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2 * se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.x.text = element_text(angle = 90, hjust = 1))

#Splitting edx Dataset into Training set and Test set
set.seed(755, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
edx_train <- edx[-test_index,]
edx_test <- edx[test_index,]

#Make sure userId and movieId in test set are also in train set
edx_test.final <- edx_test %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

#Add rows removed from test set back into train set
removed <- anti_join(edx_test, edx_test.final)

edx_train <- rbind(edx_train, removed)

rm(test_index, removed, edx_test)

##MODELLING
#Definition of RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean(true_ratings - predicted_ratings)^2)
}

#Calculation of naive RMSE
mu_hat <- mean(edx_train$rating)

naive_RMSE <- RMSE(edx_test.final$rating, mu_hat)

#Creating a results table with this naive approach
rmse_results <- tibble(method = "Just the average", RMSE = naive_RMSE)

#SECOND MODEL - MOVIE EFFECT
mu <- mean(edx_train$rating)
movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

predicted_ratings <- mu + edx_test.final %>%
  left_join(movie_avgs, by = "movieId") %>%
  pull(b_i)

rmse_movie.effect <-RMSE(predicted_ratings, edx_test.final$rating)
rmse_movie.effect

#THIRD MODEL - MOVIE EFFECT AND USER EFFECT
user_avgs <- edx_train %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating -mu -b_i))

predicted_ratings <- edx_test.final %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

RMSE_user.movie.effect <- RMSE(predicted_ratings, edx_test.final$rating)
RMSE_user.movie.effect

#FOURTH MODEL - ADDING GENRES EFFECT
genre_avgs <- edx_train %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating -mu -b_i - b_u))

predicted_ratings <- edx_test.final %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

RMSE_genre.user.movie.effect <-RMSE(predicted_ratings, edx_test.final$rating)
RMSE_genre.user.movie.effect

#FIFTH MODEL - INTEGRATING TIME EFFECT
time_avgs <- edx_train %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  group_by(duration) %>%
  summarise(b_t = mean(rating - mu - b_i - b_u - b_g))

predicted_ratings <- edx_test.final %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(time_avgs, by = "duration") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
  pull(pred)

RMSE_time.genre.user.movie.effect <- RMSE(predicted_ratings, edx_test.final$rating)
RMSE_time.genre.user.movie.effect



##VALIDATING THE FINAL MODEL
mu <- mean(edx$rating)

b_i <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n() + lambda))

b_u <- edx %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - mu - b_i)/(n() + lambda))

b_g <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - mu - b_i - b_u)/(n() + lambda))

b_t <- edx %>%
  left_join(b_i , by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  group_by(duration) %>%
  summarise(b_t = sum(rating - b_i - b_u - b_g)/(n() + lambda))

predicted_ratings <- final_holdout_test %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_t, by = "duration") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
  pull(pred)

final_rmse <- RMSE(final_holdout_test$rating, predicted_ratings)



