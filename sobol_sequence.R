# load packages
library(tidyverse)
library(magrittr)
library(ggplot2)
library(humanleague)

# specify number of participants
n      <- 2e3
trials <- 10

# create data
data_r <- data.frame(x = round(runif(n * trials, -0.49, 100.49)),
                     y = round(runif(n * trials, -0.49, 100.49))) %>%
  mutate(id    = rep(seq_len(nrow(.) / trials), each = trials), .before = 1) %>%
  mutate(trial = rep(1:trials, nrow(.) / trials), .after = id)

data_s <- sobolSequence(dim = 2, n   = n * trials) %>%
  data.frame() %>%
  select(x = X1,
         y = X2) %>%
  mutate_all(~round((.x * 100.98) - 0.49)) %>%
  mutate(id = rep(seq_len(nrow(.) / trials), each = trials), .before = 1) %>%
  mutate(trial = rep(1:trials, nrow(.) / trials), .after = id)
  

# visualize
data_r %>%
  filter(between(id, 1, 16)) %>%
  mutate(id = factor(id)) %>%
  ggplot() +
    geom_point(aes(x     = x, 
                   y     = y, 
                   color = id)) +
    facet_wrap(~id, nrow = 4, ncol = 4) +
    theme_bw() +
    theme(strip.background = element_blank())
  
data_s %>%
  filter(between(id, 1, 16)) %>%
  mutate(id = factor(id)) %>%
  ggplot() +
    geom_point(aes(x     = x, 
                   y     = y, 
                   color = id)) +
    facet_wrap(~id, nrow = 4, ncol = 4) +
    theme_bw() +
    theme(strip.background = element_blank())

data_r %>%
  mutate(id = factor(id)) %>%
  ggplot() +
    geom_point(aes(x     = x, 
                   y     = y)) +
    theme_bw() +
    theme(strip.background = element_blank())

data_s %>%
  mutate(id = factor(id)) %>%
  ggplot() +
    geom_point(aes(x     = x, 
                   y     = y)) +
    theme_bw() +
    theme(strip.background = element_blank())

# check coverage
barplot(table(data_r$x))
barplot(table(data_r$y))

barplot(table(data_s$x))
barplot(table(data_s$y))

head(panoply::perble(data_r$x))
tail(panoply::perble(data_r$x))
head(panoply::perble(data_r$y))
tail(panoply::perble(data_r$y))

head(panoply::perble(data_s$x))
tail(panoply::perble(data_s$x))
head(panoply::perble(data_s$y))
tail(panoply::perble(data_s$y))

# convert to wide format
data_s_wide <- data_s %>%
  pivot_wider(names_from = trial,
              values_from = c(x, y))

# export data
rio::export(data_s_wide, "/Users/cameronkay/Documents/Projects/Research/1 Active/norms_and_evidence/sobol_sequence_data/sobol_values.csv")
