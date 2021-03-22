library(tidyverse)
library(scales)

standings_file_names <- dir('C:/Users/Owner/Documents/madness-21-saves/standings', full.names = T)

all_standings <- lapply(standings_file_names, readRDS) %>% bind_rows
all_standings %>% saveRDS('standings.rds')

p <- all_standings %>%
  filter(pool == 'UPay') %>% 
  ggplot(aes(x = time, y = last_prob, group = name, color = name)) +
  geom_line() +
  #geom_smooth(se = F) +
  labs(title = 'Last Probability', color = NULL, x = 'Time', y = 'Win %') +
  scale_x_datetime(expand = expansion(add = 0), labels = function(x) format(x, '%I:%M%p')) +
  scale_y_continuous(expand = expansion(add = 0), labels = percent) +
  theme_minimal()

ggsave('standings Ott Day1.png', p, width = 8, height = 4.5, dpi = 'retina')
