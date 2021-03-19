library(knitr)
library(tidyverse)
library(scales)
library(jsonlite)
source('create brackets.R')

#submitted <- create_brackets(espn_picks, 21) %>% mutate(name = paste0('p',sim), sim = NULL, pool = 'UPay')

current_json <- fromJSON('https://projects.fivethirtyeight.com/march-madness-api/2021/madness.json')
current_run <- current_json$forecasts$mens$current_run$teams %>% 
  mutate(
    team_slot = team_slot - team_slot %% 2,
    rd1_win = NULL
  )

max_score <- current_run %>% 
  filter(team_alive == 1) %>% 
  select(team_slot) %>% 
  distinct %>% 
  inner_join(submitted, by = c("team_slot")) %>% 
  mutate(pts = (2^(rd-2)) * 10) %>% 
  group_by(pool, name) %>% 
  summarise(max_pts = sum(pts))

curr_score <- current_run %>% 
  select(team_slot, contains('rd')) %>% 
  pivot_longer(contains('rd'), names_to = c('rd',NA), values_to = 'prob', names_prefix = 'rd', names_sep = '_') %>% 
  mutate(rd = as.numeric(rd)) %>% 
  filter(prob == 1) %>% 
  inner_join(submitted, by = c("team_slot", "rd")) %>% 
  mutate(pts = (2^(rd-2)) * 10) %>% 
  group_by(pool, name) %>% 
  summarise(curr_pts = sum(pts))

sim_results <- current_run %>% 
  create_brackets(200) %>% 
  left_join(submitted, by = c('rd', 'rd_slot'), suffix = c('_actual', '_pick')) %>% 
  mutate(pts = ifelse(team_slot_pick == team_slot_actual, (2^(rd-2)) * 10, 0)) %>% 
  group_by(sim, pool, name) %>% 
  summarise(tot = sum(pts) + (runif(1) / 100), .groups = 'drop') %>% 
  arrange(-tot) %>% 
  group_by(sim, pool) %>% 
  mutate(
    rank = row_number(),
    in_pool = max(rank),
    tot = floor(tot)
  ) %>% 
  ungroup
  

sim_results %>% 
  group_by(pool, name) %>% 
  summarise(
    wins = mean(ifelse(rank == 1, 1, 0)),
    last = mean(ifelse(rank == in_pool, 1, 0)),
    avg_score = mean(tot),
    .groups = 'drop'
  ) %>% 
  left_join(curr_score, by = c("pool", "name")) %>% 
  left_join(max_score, by = c("pool", "name")) %>% 
  mutate(
    curr_pts = ifelse(is.na(curr_pts), 0, curr_pts),
    pts_rem = max_pts - curr_pts
  ) %>% 
  arrange(-last) %>% 
  kable %>% 
  paste(collapse = '\n') %>% 
  write.table('test.txt', row.names = F, col.names = F, quote = F)

  

