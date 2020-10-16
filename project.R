library(ggplot2)
library(dplyr)
library(statsr)
library(BAS)
library(tidyverse)
library(broom)

load('movies.Rdata')
View(movies)
mov = movies

movies = movies %>%
  mutate(feature_film = ifelse(title_type == 'Feature Film', 'yes', 'no'))

movies = movies %>%
  mutate(drama = ifelse(genre == 'Drama', 'yes', 'no'))

movies = movies %>%
  mutate(mpaa_rating_R = ifelse(mpaa_rating == 'R', 'yes', 'no'))

movies = movies %>%
  mutate(oscar_season = ifelse(thtr_rel_month == 12 | thtr_rel_month == 11 | thtr_rel_month == 10, 'yes', 'no'))

movies = movies %>%
  mutate(summer_season = ifelse(thtr_rel_month == 5 | thtr_rel_month == 6 | thtr_rel_month == 7 | thtr_rel_month == 8, 'yes', 'no'))

g1 = lm(audience_score ~ feature_film + drama + mpaa_rating_R + oscar_season + summer_season, data = movies)
summary(g1)

plot(g1)
summary(movies)

count(movies, feature_film)
ggplot(data = movies) +
  geom_bar(mapping = aes(x = feature_film), fill = 'firebrick') +
  ggtitle('Feature Film Counts') +
  xlab('') +
  ylab('Count')

count(movies, drama)
ggplot(data = movies) +
  geom_bar(mapping = aes(x = drama), fill = 'goldenrod1') +
  ggtitle('Drama Film Counts') +
  xlab('') +
  ylab('Count')

count(movies, mpaa_rating_R)
ggplot(data = movies) +
  geom_bar(mapping = aes(x = mpaa_rating_R), fill = 'forestgreen') +
  ggtitle('R Rating Counts') +
  xlab('') +
  ylab('Count')

count(movies, oscar_season)
ggplot(data = movies) +
  geom_bar(mapping = aes(x = oscar_season), fill = 'darkturquoise') +
  ggtitle('Oscar Season Film Counts') +
  xlab('') +
  ylab('Count')

count(movies, summer_season)
ggplot(data = movies) +
  geom_bar(mapping = aes(x = summer_season), fill = 'mediumpurple3') +
  ggtitle('Summer Season Film Counts') +
  xlab('') +
  ylab('Count')

bma_lwage_red <- bas.lm(lwage ~ ., data = wage_red,  
                        prior = "ZS-null",
                        modelprior = uniform())

g = bas.lm(audience_score ~ feature_film + drama + runtime + mpaa_rating_R + thtr_rel_year + oscar_season +
             summer_season + imdb_rating + imdb_num_votes + critics_score + best_pic_nom + best_pic_win +
             best_actor_win + best_actress_win + best_dir_win + top200_box, data = movies,
           prior = 'ZS-null', modelprior = uniform())
summary(g)

gx = lm(audience_score ~ feature_film + drama + runtime + mpaa_rating_R + thtr_rel_year + oscar_season +
          summer_season + imdb_rating + imdb_num_votes + critics_score + best_pic_nom + best_pic_win +
          best_actor_win + best_actress_win + best_dir_win + top200_box, data = movies)
plot(gx)
ggplot(data = gx) +
  geom_histogram(mapping = aes(x = gx$residuals), fill = 'royalblue4') +
  ggtitle('Histogram of Residuals') +
  xlab('Residuals') +
  ylab('Count')

BPM_pred_lwage <- predict(bma_lwage, estimator = "BPM", se.fit = TRUE)

prediction = predict(g, estimator = 'BPM', se.fit = TRUE)
variable.names(prediction)
o = which.max(prediction$fit)
ci = confint(prediction, parm = "pred")
ci

ttb = data.frame(runtime = 118, imdb_rating = 7.8, critics_score = 94)

predict(gb, ttb, interval = "prediction", level = 0.95)


gx$coefficients
gb = lm(audience_score ~ runtime + imdb_rating  + critics_score, data = movies)
gb$coefficients
levels(movies$title_type)











