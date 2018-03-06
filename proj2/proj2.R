library(dplyr)
library(ggplot2)

setwd("~/Documents/R-projects/proj2")

movies <- read.csv("filmdeathcounts.csv", encoding = "UTF-8")
summary(movies)
head(movies)
str(movies)
movies$body_per_min <- movies$Body_Count / movies$Length_Minutes

ggplot(movies, aes(x=Body_Count)) +
  geom_histogram(bins=20, color="grey", fill="lightblue")

movies %>%
  top_n(n = 10, Body_Count) %>%
  arrange(desc(Body_Count))
movies %>%
  top_n(n = 10, body_per_min) %>%
  arrange(desc(body_per_min))

ggplot(movies, aes(x=IMDB_Rating)) +
  geom_histogram(bins=10, color="gray", fill="lightblue")

imdb_mean <- mean(movies$IMDB_Rating)
print(imdb_mean)
imdb_sd <- sd(movies$IMDB_Rating)
print(imdb_sd)

set.seed(900)
imdb_simulation <- rnorm(n=nrow(movies), mean = imdb_mean, sd = imdb_sd)
print(imdb_simulation)
movies$imdb_simulation <- imdb_simulation

pnorm(4, mean = imdb_mean, sd = imdb_sd) - 
  pnorm(8, mean = imdb_mean, sd=imdb_sd)
cor.test(movies$Body_Count, movies$IMDB_Rating)

ggplot(movies, aes(x=imdb_simulation)) +
  geom_histogram(bins=10, color="grey", fill="lightblue")

ggplot(movies, aes(sample=imdb_simulation)) +
  stat_qq()

ggplot(movies, aes(sample=IMDB_Rating)) +
  stat_qq()

ggplot(movies, aes(x=IMDB_Rating, y=Body_Count)) +
  geom_point(col="blue")


