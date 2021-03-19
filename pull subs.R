library(tidyverse)
library(rvest)

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
  filter(forecast_date == max(forecast_date) & gender == 'mens' & rd1_win > 0) %>%
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
### do the final join
team_slots <- pred_df %>% 
  full_join(team_id_lookup) %>% 
  select(team_slot, espn_team_name)

team_slots %>% view
######

######
# now start scraping individual entries
entry_id_df <- read_csv('entries.csv')

all_submissions <- lapply(1:nrow(entry_id_df), function(i) {
  curr_entry <- entry_id_df[i, ]
  
  bracket <- read_html(paste0('https://fantasy.espn.com/tournament-challenge-bracket/',2021,'/en/entry?entryID=', curr_entry$entry))
  
  teams <- bracket %>% 
    html_nodes(xpath = '//div/div[@class="slots"]/div/span/span[@class="name"]') %>% 
    html_text()
  
  selection_ind <- bracket %>% 
    html_nodes(xpath = '//div/div[@class="slots"]/div/span/@class') %>% 
    html_text()
  
  champ <- bracket %>% 
    html_nodes(xpath = '//div[@class="center"]//@title') %>% 
    html_text %>% 
    rev %>% 
    .[1]
  
  tibble(espn_team_name = c(teams, champ), selection_ind = c(selection_ind,'picked')) %>% 
    filter(grepl('picked', selection_ind)) %>% 
    mutate(
      selection_ind = NULL,
      rd = c(rep(2, 32), rep(3, 16), rep(4, 8), rep(5, 4), rep(6, 2), 7)
    ) %>% 
    left_join(team_slots) %>% 
    mutate(
      name = curr_entry$name,
      pool = curr_entry$pool,
      rd_slot = team_slot - (team_slot %% (2^rd)) + (64 / (2 ^ (7 - rd)))
    ) %>% 
    select(pool, name, rd, rd_slot, team_slot) %>% 
    return
}) %>% bind_rows

saveRDS(all_submissions, 'all-subs.rds')


#html_nodes(xpath = '//div/div[@class="slots"]/div/span[@class="picked"]') %>% 