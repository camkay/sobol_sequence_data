# load packages
library(tidyverse)
library(magrittr)
library(ggplot2)
library(humanleague)

# specify number of participants
n      <- 25e3
trials <- 10

# create data
data_r_1d <- data.frame(x = round(runif(n * trials, -0.49, 100.49))) %>%
  mutate(id    = rep(seq_len(nrow(.) / trials), each = trials), .before = 1) %>%
  mutate(trial = rep(1:trials, nrow(.) / trials), .after = id)

data_s_1d <- sobolSequence(dim = 1, 
                           n   = n * trials) %>%
  data.frame() %>%
  rename(x = 1) %>%
  mutate_all(~round((.x * 100.98) - 0.49)) %>%
  mutate(id = rep(seq_len(nrow(.) / trials), each = trials), .before = 1) %>%
  mutate(trial = rep(1:trials, nrow(.) / trials), .after = id)

data_r_2d <- data.frame(x = round(runif(n * trials, -0.49, 100.49)),
                      y = round(runif(n * trials, -0.49, 100.49))) %>%
  mutate(id    = rep(seq_len(nrow(.) / trials), each = trials), .before = 1) %>%
  mutate(trial = rep(1:trials, nrow(.) / trials), .after = id)

data_s_2d <- sobolSequence(dim = 2, 
                         n   = n * trials) %>%
  data.frame() %>%
  select(x = X1,
         y = X2) %>%
  mutate_all(~round((.x * 100.98) - 0.49)) %>%
  mutate(id = rep(seq_len(nrow(.) / trials), each = trials), .before = 1) %>%
  mutate(trial = rep(1:trials, nrow(.) / trials), .after = id)

# visualize
data_r_1d %>%
  filter(between(id, 1, 4)) %>%
  mutate(id = factor(id)) %>%
  ggplot() +
    geom_point(aes(x     = x, 
                   y     = 1, 
                   color = id)) +
    facet_wrap(~id, nrow = 4, ncol = 1) +
    theme_bw() +
    theme(strip.background = element_blank())

data_s_1d %>%
  filter(between(id, 1, 4)) %>%
  mutate(id = factor(id)) %>%
  ggplot() +
    geom_point(aes(x     = x, 
                   y     = 1, 
                   color = id)) +
    facet_wrap(~id, nrow = 4, ncol = 1) +
    theme_bw() +
    theme(strip.background = element_blank())

data_r_2d %>%
  filter(between(id, 1, 16)) %>%
  mutate(id = factor(id)) %>%
  ggplot() +
    geom_point(aes(x     = x, 
                   y     = y, 
                   color = id)) +
    facet_wrap(~id, nrow = 4, ncol = 4) +
    theme_bw() +
    theme(strip.background = element_blank())
  
data_s_2d %>%
  filter(between(id, 1, 16)) %>%
  mutate(id = factor(id)) %>%
  ggplot() +
    geom_point(aes(x     = x, 
                   y     = y, 
                   color = id)) +
    facet_wrap(~id, nrow = 4, ncol = 4) +
    theme_bw() +
    theme(strip.background = element_blank())

data_r_2d %>%
<<<<<<< HEAD
  slice_head(n = 5000) %>%
=======
  slice_head(n = 4000) %>%
>>>>>>> c17ccae19bac7e04f1445c43b08623554680fdf2
  mutate(id = factor(id)) %>%
  ggplot() +
    geom_point(aes(x     = x, 
                   y     = y)) +
    theme_bw() +
    theme(strip.background = element_blank())


data_s_2d %>%
<<<<<<< HEAD
  slice_head(n = 5000) %>%
=======
  slice_head(n = 4000) %>%
>>>>>>> c17ccae19bac7e04f1445c43b08623554680fdf2
  mutate(id = factor(id)) %>%
  ggplot() +
    geom_point(aes(x     = x, 
                   y     = y)) +
    theme_bw() +
    theme(strip.background = element_blank())

# check coverage
barplot(table(data_r_1d$x))

barplot(table(data_s_1d$x))

barplot(table(data_r_2d$x))
barplot(table(data_r_2d$y))

barplot(table(data_s_2d$x))
barplot(table(data_s_2d$y))

head(panoply::perble(data_r_1d$x))
tail(panoply::perble(data_r_1d$x))

head(panoply::perble(data_s_1d$x))
tail(panoply::perble(data_s_1d$x))

head(panoply::perble(data_r_2d$x))
tail(panoply::perble(data_r_2d$x))
head(panoply::perble(data_r_2d$y))
tail(panoply::perble(data_r_2d$y))

head(panoply::perble(data_s_2d$x))
tail(panoply::perble(data_s_2d$x))
head(panoply::perble(data_s_2d$y))
tail(panoply::perble(data_s_2d$y))

# convert to wide format
data_s_1d_wide <- data_s_1d %>%
  mutate(trial = paste0("x_", trial)) %>%
  pivot_wider(names_from = trial,
              values_from = c(x))

data_s_2d_wide <- data_s_2d %>%
  pivot_wider(names_from = trial,
              values_from = c(x, y))

# chop into different data frames
blocks <- 5

data_s_1d_wide %<>%
  mutate(block = rep(letters[seq_len(blocks)], 
                     each = nrow(.) / blocks),
         .after = id) %>%
  mutate(sobol_block = block, .after = block) %>%
  nest(.by = block)

data_s_2d_wide %<>%
  mutate(block = rep(letters[seq_len(blocks)], 
                     each = nrow(.) / blocks),
         .after = id) %>%
  mutate(sobol_block = block, .after = block) %>%
  nest(.by = block)

<<<<<<< HEAD
=======



>>>>>>> c17ccae19bac7e04f1445c43b08623554680fdf2
# export data
walk2(.x = data_s_1d_wide$data,
      .y = paste0(here::here(),
                  "/sobol_values_1_dim_",
                  data_s_1d_wide$block,
                  ".csv"),
      .f = ~rio::export(.x, .y))

walk2(.x = data_s_2d_wide$data,
      .y = paste0(here::here(),
                  "/sobol_values_2_dim_",
                  data_s_2d_wide$block,
                  ".csv"),
      .f = ~rio::export(.x, .y))
