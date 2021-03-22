library(tidyverse)
library(scales)
source('create brackets.R')
espn_picks <- readRDS('espn_picks_w.rds')

pred_df <- read_csv('https://projects.fivethirtyeight.com/march-madness-api/2021/fivethirtyeight_ncaa_forecasts.csv') %>% 
  mutate(forecast_date = as.Date(forecast_date)) %>% 
  filter(forecast_date == max(forecast_date) & gender == 'mens') %>%
  mutate(
    team_slot = team_slot - team_slot %% 2,
    team_seed = as.numeric(gsub('b', '', gsub('a', '', team_seed)))
  ) %>% 
  group_by(team_slot, team_region, team_seed) %>% 
  summarise(
    team_id = paste(team_id, collapse = 'x'),
    team_name = paste(team_name, collapse = '/'),
    playin_flag = max(playin_flag),
    rd2_win = sum(rd2_win),
    rd3_win = sum(rd3_win),
    rd4_win = sum(rd4_win),
    rd5_win = sum(rd5_win),
    rd6_win = sum(rd6_win),
    rd7_win = sum(rd7_win)
  ) %>% 
  ungroup

run_sim_bc <- function(group_size, sample_groups, espn_picks, pred_df) {

  espn_sample <- create_brackets(espn_picks, group_size * sample_groups) %>% 
    mutate(group = floor((sim - 0.01) / group_size) + 1)
           
  true_sample <- create_brackets(pred_df, sample_groups) %>% 
    select(group = sim, rd, rd_slot, actual_team_slot = team_slot)
  
  scoring_df <- espn_sample %>% 
    left_join(true_sample, by = c("rd", "rd_slot", "group")) %>% 
    left_join(pred_df %>% select(team_slot, team_seed), by = "team_slot") %>% 
    mutate(pts = ifelse(team_slot == actual_team_slot, (2^(rd-2)) * team_seed, 0))
    #mutate(pts = ifelse(team_slot == actual_team_slot,  * 10, 0))
  
  winners <- scoring_df %>% 
    group_by(group, sim) %>% 
    summarise(tot = sum(pts), .groups = 'drop') %>% 
    arrange(-tot) %>% 
    group_by(group) %>% 
    mutate(winner = ifelse(row_number() == 1, 1, 0)) %>% 
    ungroup %>% 
    select(sim, winner)
    
  winner_picks <- espn_sample %>% 
    inner_join(winners, by = "sim") %>% 
    group_by(rd, rd_slot, team_slot) %>% 
    summarise(picked = n(), wins = sum(winner), .groups = 'drop') %>% 
    arrange(-(wins/picked)) %>% 
    group_by(rd, rd_slot) %>% 
    mutate(pick_rank = row_number()) %>% 
    ungroup %>% 
    return
  
}

multi_sim <- lapply(1:10, function(x) run_sim_bc(30, 3000, espn_picks, pred_df)) %>%
  bind_rows %>% 
  mutate(pick_rank = NULL) %>% 
  group_by(rd, rd_slot, team_slot) %>% 
  summarise_all(sum) %>% 
  arrange(-(wins/picked)) %>% 
  mutate(pick_rank = row_number()) %>% 
  ungroup

pred_df %>% 
  select(team_slot, team_region, team_seed, team_name) %>% 
  right_join(multi_sim) %>% 
  mutate(
    win_pct = wins/picked,
    pick_desc = paste0(team_seed, ')', team_name, ' - ', percent(win_pct, accuracy = 0.1)),
    pick_name = paste0(rd, '_', rd_slot)
  ) %>% 
  filter(pick_rank <= 5) %>% 
  #filter(team_region == 'Midwest' & rd > 5) %>% 
  arrange(-rd, rd_slot) %>% 
  pivot_wider(pick_rank, names_from = pick_name, values_from = pick_desc) %>% 
  arrange(pick_rank) %>% 
  view



run_sim_bc_my_picks <- function(group_size, sample_groups, espn_picks, pred_df, my_picks) {
  
  espn_sample <- create_brackets(espn_picks, group_size * sample_groups) %>% 
    mutate(
      group = floor((sim - 0.01) / group_size) + 1,
      is_me = 0
    )
  
  my_sample <- data.frame(group = 1:sample_groups, sim = (group_size * sample_groups) + 1:sample_groups) %>% 
    full_join(my_picks, by = character()) %>% 
    mutate(is_me = 1)
  
  true_sample <- create_brackets(pred_df, sample_groups) %>% 
    select(group = sim, rd, rd_slot, actual_team_slot = team_slot)
  
  scoring_df <- bind_rows(espn_sample, my_sample) %>% 
    left_join(true_sample, by = c("rd", "rd_slot", "group")) %>% 
    left_join(pred_df %>% select(team_slot, team_seed), by = "team_slot") %>% 
    mutate(pts = ifelse(team_slot == actual_team_slot, (rd-1) * team_seed, 0))
    #mutate(pts = ifelse(team_slot == actual_team_slot, (2^(rd-2)) * 10, 0))
  
  scoring_df %>% 
    group_by(group, sim, is_me) %>% 
    summarise(tot = sum(pts), .groups = 'drop') %>% 
    arrange(-tot) %>% 
    group_by(group) %>% 
    mutate(winner = ifelse(row_number() == 1, 1, 0)) %>% 
    group_by(is_me) %>% 
    summarise(win_pct = sum(winner) / sample_groups) %>% 
    return

}


bc_picks <- read_csv('bc picks.csv') %>% create_brackets(., 1) %>% mutate(sim = NULL)
read_csv('bc picks.csv') %>% select(contains('rd')) %>% summarise_all(sum)
  
run_sim_bc_my_picks(29, 10000, espn_picks, pred_df, bc_picks)



sapply(1:1000, function(x) mean(sample(c(1,0), size = 5000, replace = T, prob = c(0.15,1-0.15)))) %>% sd


run_sim <- function(group_size, sample_groups, espn_picks, pred_df) {
  
  espn_sample <- create_brackets(espn_picks, group_size * sample_groups) %>% 
    mutate(group = floor((sim - 0.01) / group_size) + 1)
  
  true_sample <- create_brackets(pred_df, sample_groups) %>% 
    select(group = sim, rd, rd_slot, actual_team_slot = team_slot)
  
  scoring_df <- espn_sample %>% 
    left_join(true_sample, by = c("rd", "rd_slot", "group")) %>% 
    mutate(pts = ifelse(team_slot == actual_team_slot, (2^(rd-2)) * 10, 0))
  
  winners <- scoring_df %>% 
    group_by(group, sim) %>% 
    summarise(tot = sum(pts), .groups = 'drop') %>% 
    arrange(-tot) %>% 
    group_by(group) %>% 
    mutate(winner = ifelse(row_number() == 1, 1, 0)) %>% 
    ungroup %>% 
    select(sim, winner)
  
  winner_picks <- espn_sample %>% 
    inner_join(winners, by = "sim") %>% 
    group_by(rd, rd_slot, team_slot) %>% 
    summarise(picked = n(), wins = sum(winner), .groups = 'drop') %>% 
    arrange(-(wins/picked)) %>% 
    group_by(rd, rd_slot) %>% 
    mutate(pick_rank = row_number()) %>% 
    ungroup %>% 
    return
  
}


multi_sim <- lapply(1:1, function(x) run_sim(5, 20000, espn_picks, pred_df)) %>%
  bind_rows %>% 
  mutate(pick_rank = NULL) %>% 
  group_by(rd, rd_slot, team_slot) %>% 
  summarise_all(sum) %>% 
  arrange(-(wins/picked)) %>% 
  mutate(pick_rank = row_number()) %>% 
  ungroup

pred_df %>% 
  select(team_slot, team_region, team_seed, team_name) %>% 
  right_join(multi_sim) %>% 
  mutate(
    win_pct = wins/picked,
    pick_desc = paste0(team_seed, ')', team_name, ' - ', percent(win_pct, accuracy = 0.001)),
    pick_name = paste0(rd, '_', rd_slot)
  ) %>% 
  filter(pick_rank <= 5) %>% 
  arrange(-rd, rd_slot) %>% 
  pivot_wider(pick_rank, names_from = pick_name, values_from = pick_desc) %>% 
  arrange(pick_rank) %>% 
  view





##### top 4
run_sim_top4 <- function(group_size, sample_groups, espn_picks, pred_df) {
  
  espn_sample <- create_brackets(espn_picks, group_size * sample_groups) %>% 
    mutate(group = floor((sim - 0.01) / group_size) + 1)
  
  true_sample <- create_brackets(pred_df, sample_groups) %>% 
    select(group = sim, rd, rd_slot, actual_team_slot = team_slot)
  
  scoring_df <- espn_sample %>% 
    left_join(true_sample, by = c("rd", "rd_slot", "group")) %>% 
    mutate(pts = ifelse(team_slot == actual_team_slot, (2^(rd-2)) * 10, 0))
  
  winners <- scoring_df %>% 
    group_by(group, sim) %>% 
    summarise(tot = sum(pts), .groups = 'drop') %>% 
    arrange(-tot) %>% 
    group_by(group) %>% 
    mutate(winner = ifelse(row_number() < 6, 1, 0)) %>% 
    ungroup %>% 
    select(sim, winner)
  
  winner_picks <- espn_sample %>% 
    inner_join(winners, by = "sim") %>% 
    group_by(rd, rd_slot, team_slot) %>% 
    summarise(picked = n(), wins = sum(winner), .groups = 'drop') %>% 
    arrange(-(wins/picked)) %>% 
    group_by(rd, rd_slot) %>% 
    mutate(pick_rank = row_number()) %>% 
    ungroup %>% 
    return
  
}


multi_sim <- lapply(1:5, function(x) run_sim_top4(9, 10000, espn_picks, pred_df)) %>%
  bind_rows %>% 
  mutate(pick_rank = NULL) %>% 
  group_by(rd, rd_slot, team_slot) %>% 
  summarise_all(sum) %>% 
  arrange(-(wins/picked)) %>% 
  mutate(pick_rank = row_number()) %>% 
  ungroup


pred_df %>% 
  select(team_slot, team_region, team_seed, team_name) %>% 
  right_join(multi_sim_top4) %>% 
  mutate(
    win_pct = wins/picked,
    pick_desc = paste0(team_seed, ')', team_name, ' - ', percent(win_pct, accuracy = 0.1)),
    pick_name = paste0(rd, '_', rd_slot)
  ) %>% 
  filter(pick_rank <= 5) %>% 
  arrange(-rd, rd_slot) %>% 
  pivot_wider(pick_rank, names_from = pick_name, values_from = pick_desc) %>% 
  arrange(pick_rank) %>% 
  view


run_sim_top4_my_picks <- function(group_size, sample_groups, espn_picks, pred_df, my_picks) {
  
  espn_sample <- create_brackets(espn_picks, group_size * sample_groups) %>% 
    mutate(
      group = floor((sim - 0.01) / group_size) + 1,
      is_me = 0
    )
  
  my_sample <- data.frame(group = 1:sample_groups, sim = (group_size * sample_groups) + 1:sample_groups) %>% 
    full_join(my_picks, by = character()) %>% 
    mutate(is_me = 1)
  
  true_sample <- create_brackets(pred_df, sample_groups) %>% 
    select(group = sim, rd, rd_slot, actual_team_slot = team_slot)
  
  scoring_df <- bind_rows(espn_sample, my_sample) %>% 
    left_join(true_sample, by = c("rd", "rd_slot", "group")) %>% 
    mutate(pts = ifelse(team_slot == actual_team_slot, (2^(rd-2)) * 10, 0))
  
  scoring_df %>% 
    group_by(group, sim, is_me) %>% 
    summarise(tot = sum(pts), .groups = 'drop') %>% 
    arrange(-tot) %>% 
    group_by(group) %>% 
    mutate(winner = ifelse(row_number() < 6, 1, 0)) %>% 
    filter(is_me == 1) %>% 
    group_by(is_me) %>% 
    summarise(win_pct = mean(winner)) %>% 
    return
  
}

my_picks <- multi_sim %>% filter(pick_rank == 1) %>% select(rd, rd_slot, team_slot)

run_sim_top4_my_picks(8, 10000, espn_picks, pred_df, my_picks)



# 
# espn_sample %>% 
#   filter(rd == 7 & team_slot == 34) %>% 
#   left_join(winners)
# 
# espn_sample %>% 
#   filter(sim == 1783 & rd == 5) %>% 
#   select(team_slot) %>% 
#   left_join(pred_df)
# 
# true_sample %>% 
#   filter(group == 199 & rd == 2) %>% 
#   rename(team_slot = actual_team_slot) %>% 
#   select(team_slot) %>% 
#   left_join(pred_df) %>% 
#   pull(team_name)
# 
# 
# scoring_df %>% 
#   filter(group == 22 & sim == 193) %>% 
#   group_by(rd) %>% 
#   summarise(sum(pts))
# 
