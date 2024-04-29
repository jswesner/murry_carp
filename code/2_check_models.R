library(tidyverse)
library(isdbayes)
library(janitor)
library(brms)
library(tidybayes)

#1) load data and model

orc_il_prop = readRDS(file = "data/orc_il_prop.rds") 

brm_prop_silver_rand = readRDS(file = "models/brm_prop_silver_rand.rds")

#2) resample data
dat_resampled = orc_il_prop %>% 
  group_by(site_id_f, year, river, site_id) %>%
  sample_n(size = 1000, weight = catch, replace = T) 

#3) plot resampled data
ggplot(data = dat_resampled, aes(x = year, y = wt)) +
  geom_jitter(aes(color = year))

#4) get posteriors
brm_lambdas = orc_il_prop %>% 
  distinct(site_id_f, river, pool, year, prop_silver_wt_s, xmin, xmax) %>% 
  mutate(catch = 1) %>% 
  add_epred_draws(brm_prop_silver_rand, re_formula = NA)

#5) sample new data from the lambdas in each year (first 10 draws)
y_reps = brm_lambdas %>% 
  filter(.draw <= 10) %>% 
  expand_grid(individual = c(1:1000)) %>% 
  mutate(wt = rparetocounts(nrow(.), lambda = .epred, xmin = xmin, xmax = xmax),
         y_data = "yrep")

#6 add original_data
all_data = bind_rows(y_reps,
                     dat_resampled %>% mutate(y_data = "yraw",
                                              .draw = 0))

#7) plot yrep
all_data %>% 
  ggplot(aes(x = .draw, y = wt, color = y_data)) +
  geom_boxplot(aes(group = interaction(y_data, .draw)), 
               outlier.shape = NA) +
  scale_y_log10()

all_data %>% 
  ggplot(aes(x = wt, color = y_data)) +
  geom_histogram() +
  facet_grid(river~.draw, scales = "free_y") +
  NULL


#8) Bayes P-value

y_rep_gms = brm_lambdas %>% 
  filter(.draw <= 200) %>% 
  expand_grid(individual = c(1:1000)) %>% 
  mutate(wt = rparetocounts(nrow(.), lambda = .epred, xmin = xmin, xmax = xmax),
         y_data = "yrep") %>% 
  group_by(.draw, y_data) %>% 
  reframe(gm = exp(mean(log(wt))))

raw_gm = exp(mean(log(dat_resampled$wt)))

y_rep_gms %>% 
  ggplot(aes(x = gm)) +
  geom_histogram() + 
  geom_vline(xintercept = raw_gm)

y_rep_gms %>% 
  mutate(raw_gm = raw_gm,
         diff = raw_gm - gm) %>% 
  reframe(bayes_p = sum(diff>0)/max(.draw))




