library(tidyverse)
library(scales)
library(jsonlite)
library(htmlTable)
source('create brackets.R')

#espn_picks <- readRDS('espn_picks.rds')
#submitted <- create_brackets(espn_picks, 22) %>% mutate(name = paste0('p',sim), sim = NULL, pool = c(rep('UPay', 63*9),rep('Otterbein', 63*10),rep('Reinhard', 63*3)))
submitted <- readRDS('all-subs.rds')

sys_time <- Sys.time()
current_json <- fromJSON('https://projects.fivethirtyeight.com/march-madness-api/2021/madness.json')
write_json(current_json, paste0('C:/Users/Owner/Documents/madness-21-saves/538/', gsub(':','_',sys_time), '.json'))
upd_time <- format(sys_time, '%F @ %r')

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
  summarise(max_pts = sum(pts), .groups = 'drop')

curr_score <- current_run %>% 
  select(team_slot, contains('rd')) %>% 
  pivot_longer(contains('rd'), names_to = c('rd',NA), values_to = 'prob', names_prefix = 'rd', names_sep = '_') %>% 
  mutate(rd = as.numeric(rd)) %>% 
  filter(prob == 1) %>% 
  inner_join(submitted, by = c("team_slot", "rd")) %>% 
  mutate(pts = (2^(rd-2)) * 10) %>% 
  group_by(pool, name) %>% 
  summarise(curr_pts = sum(pts), .groups = 'drop')

sim_results <- current_run %>% 
  create_brackets(15000) %>% 
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
  
standings_df <- sim_results %>% 
  group_by(pool, name) %>% 
  summarise(
    win_prob = mean(ifelse(rank == 1, 1, 0)),
    top3_prob = mean(ifelse(rank <= 3, 1, 0)),
    last_prob = mean(ifelse(rank == in_pool, 1, 0)),
    avg = mean(tot),
    .groups = 'drop'
  ) %>% 
  left_join(curr_score, by = c("pool", "name")) %>% 
  left_join(max_score, by = c("pool", "name")) %>% 
  mutate(
    curr_pts = ifelse(is.na(curr_pts), 0, curr_pts),
    time = sys_time
  )

saveRDS(standings_df, paste0('C:/Users/Owner/Documents/madness-21-saves/standings/', gsub(':','_',sys_time), '.rds'))

complete_res <- standings_df %>% 
  mutate(
    `Win %` = case_when(
      win_prob == 0 ~ 'OUT',
      win_prob < 0.001 ~ '<0.1%',
      T ~ percent(win_prob, accuracy = 0.1)
    ),
    `Last %` = case_when(
      last_prob == 0 ~ 'SAFE',
      last_prob < 0.001 ~ '<0.1%',
      T ~ percent(last_prob, accuracy = 0.1)
    ),
    `Avg Pts` = number(avg, accuracy = 0.1)
  ) %>% 
  arrange(-win_prob) %>% 
  select(pool, name, `Win %`, `Last %`, `Avg Pts`, curr_pts, max_pts) %>% 
  rename(`Curr` = curr_pts, `Max` = max_pts, Name = name)
  

html_header <- function(grp) {
  paste0('<head include-html="/header.html">
  	<title>March Madness - ',grp,'</title>
  	<script src="/include.js?v=2"></script>
  	<script src="https://sdk.amazonaws.com/js/aws-sdk-2.7.16.min.js"></script>
  	<link rel="preconnect" href="https://fonts.gstatic.com">
    <link href="https://fonts.googleapis.com/css2?family=Roboto:wght@700&display=swap" rel="stylesheet">
  </head>
  <body>')
}

gen_page <- function(df, grp) {
  df %>%
    filter(pool == grp) %>%
    mutate(pool = NULL) %>% 
    addHtmlTableStyle(
      align = "c",
      align.header = "c",
      css.cell = "padding-left: 1em; padding-right: 1em; font-family: Roboto, sans-serif;",
      col.rgroup = c("none", "#F7F7F7"),
      css.header = "font-weight: bold;"
    ) %>% 
    htmlTable(rnames = F) %>% 
    gsub('<table', '<table onclick="sortColumn(event)"', .) %>% 
    paste0(html_header(grp), ., '\n\nLast Updated: ',upd_time, '</body>') %>% 
    write.table(paste0('madness/',grp,'.html'), row.names = F, col.names = F, quote = F)
}

gen_page(complete_res, 'UPay')
gen_page(complete_res, 'Otterbein')
gen_page(complete_res, 'Reinhard')

### set up auto timer
### set up library to save files
### bracket similarity