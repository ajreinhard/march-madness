library(rvest)
library(tidyverse)

######
# get who picked whom
espn_picks <- read_html('https://fantasy.espn.com/tournament-challenge-bracket/2021/en/whopickedwhom') %>%
  html_nodes(xpath = '//tr/td/span') %>% 
  html_text() %>% 
  matrix(ncol = 4, byrow = T) %>% 
  data.frame %>% 
  tibble %>% 
  select(1,2,4) %>% 
  rename(seed = 1, espn_team_name = 2, prob = 3) %>% 
  mutate(
    prob = as.numeric(gsub('%','',prob))/100,
    rd = rep(2:7,64)
  )
######

######
# get espn ids from another page via the team logos
espn_html <- read_html('https://fantasy.espn.com/tournament-challenge-bracket/2021/en/nationalBracket')

espn_team_name <- espn_html %>%
  html_nodes(xpath = '//div/div[@class="slots"]/div/span/span[@class="name"]') %>% 
  html_text() %>% 
  .[1:64]

team_abbr <- espn_html %>%
  html_nodes(xpath = '//div/div[@class="slots"]/div/span/span[@class="abbrev"]') %>% 
  html_text() %>% 
  .[1:64]

team_logo_url <- espn_html %>%
  html_nodes(xpath = '//div/div[@class="slots"]/div/span/img/@src') %>% 
  html_text() %>% 
  .[1:64]

team_id_lookup <- tibble(espn_team_name, team_abbr, logo_url = team_logo_url) %>% 
  mutate(
    team_id = gsub('.png&w=36&h=36&scale=crop','',gsub('https://secure.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/','',logo_url)),
    team_id = substr(team_id, 65, nchar(team_id)),
    logo_url = NULL
  )
######

######
# get the 538 teams to get the appropriate team slot
pred_df <- read_csv('https://projects.fivethirtyeight.com/march-madness-api/2021/fivethirtyeight_ncaa_forecasts.csv') %>% 
  mutate(forecast_date = as.Date(forecast_date)) %>% 
  filter(forecast_date == max(forecast_date) & gender == 'mens') %>%
  mutate(team_slot = team_slot - team_slot %% 2) %>% 
  group_by(team_slot, team_region) %>% 
  summarise(
    team_id = paste(team_id, collapse = 'x'),
    team_name = paste(team_name, collapse = '/'),
    playin_flag = max(playin_flag)
  ) %>% 
  ungroup
######

######
# manually input playin team ids if needed
pred_df %>% filter(playin_flag == 1)
team_id_lookup %>% filter(team_id == '')

team_id_lookup <- team_id_lookup %>% 
  mutate(
    team_id = case_when(
      team_abbr == 'NORF/APP' ~ '2026x2450',
      team_abbr == 'WICH/DRKE' ~ '2724x2181',
      team_abbr == 'MSM/TXSO' ~ '116x2640',
      team_abbr == 'MSU/UCLA' ~ '127x26',
      team_id == '2633_ncw' ~ '2633',
      T ~ team_id
    )
  )
######





######
# create the df needed to sim ESPN picks
new_picks <- espn_picks %>% 
  left_join(team_id_lookup) %>% 
  left_join(pred_df) %>% 
  mutate(rd = paste0('rd',rd,'_win')) %>% 
  pivot_wider(names_from = rd, values_from = prob) %>% 
  select(team_slot, team_name, team_id, contains('rd')) 

old_picks <- readRDS('espn_picks.rds')

new_picks %>% pivot_longer(contains('rd'), names_to = 'rd', values_to = 'prob') %>% 
  left_join(old_picks %>% pivot_longer(contains('rd'), names_to = 'rd', values_to = 'prob'), by = c('team_slot', 'team_name',  'team_id', 'rd'), suffix =  c('_new', '_old')) %>% 
  mutate(prob_chng = prob_new - prob_old) %>% 
  arrange(-abs(prob_chng))

new_picks %>% saveRDS('espn_picks.rds')
######
