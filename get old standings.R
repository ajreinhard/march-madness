standings_files <- dir('C:/Users/Owner/Documents/madness-21-saves/standings', full.names = T) %>% file.info %>% rownames_to_column('file')

standings_30min <- standings_files %>% mutate(time_since = as.numeric(abs(difftime(Sys.time(), ctime, units = 'mins') - 30))) %>% filter(time_since < 10) %>% arrange(time_since) %>% pull(file) %>% .[1]
standings_60min <- standings_files %>% mutate(time_since = as.numeric(abs(difftime(Sys.time(), ctime, units = 'mins') - 60))) %>% filter(time_since < 10) %>% arrange(time_since) %>% pull(file) %>% .[1]
standings_120min <- standings_files %>% mutate(time_since = as.numeric(abs(difftime(Sys.time(), ctime, units = 'mins') - 120))) %>% filter(time_since < 10) %>% arrange(time_since) %>% pull(file) %>% .[1]
standings_24hrs <- standings_files %>% mutate(time_since = as.numeric(abs(difftime(Sys.time(), ctime, units = 'mins') - (60 * 24)))) %>% filter(time_since < 60) %>% arrange(time_since) %>% pull(file) %>% .[1]

prior_standings <- c()
if(!is.na(standings_30min)) {prior_standings <- bind_rows(prior_standings, readRDS(standings_30min) %>% mutate(since = '30mins'))}
if(!is.na(standings_60min)) {prior_standings <- bind_rows(prior_standings, readRDS(standings_60min) %>% mutate(since = '1hr'))}
if(!is.na(standings_120min)) {prior_standings <- bind_rows(prior_standings, readRDS(standings_120min) %>% mutate(since = '2hrs'))}
if(!is.na(standings_24hrs)) {prior_standings <- bind_rows(prior_standings, readRDS(standings_24hrs) %>% mutate(since = '24hrs'))}

old_standings <- prior_standings %>%
  pivot_wider(c(pool, name), names_from = since, values_from = win_prob)
