library(tidyverse)

create_brackets <- function(df, n) {
  df %>% 
    select(team_slot, contains('rd')) %>% 
    pivot_longer(contains('rd'), names_to = c('rd',NA), values_to = 'prob', names_prefix = 'rd', names_sep = '_') %>% 
    mutate(
      rd = as.numeric(rd),
      rd_slot = team_slot - (team_slot %% (2^rd)) + (64 / (2 ^ (7 - rd)))
    ) %>% 
    group_by(rd, rd_slot) %>% 
    sample_n(size = n, weight = prob, replace = T) %>% 
    mutate(sim = row_number()) %>% 
    ungroup %>% 
    pivot_wider(sim, names_from = c(rd, rd_slot), values_from = team_slot) %>% 
    mutate(
      `6_32` = ifelse(`7_64` <  64, `7_64`, `6_32`),
      `6_96` = ifelse(`7_64` >=  64, `7_64`, `6_96`),
      `5_16` = ifelse(`6_32` <  32, `6_32`, `5_16`),
      `5_48` = ifelse(`6_32` >=  32, `6_32`, `5_48`),
      `5_80` = ifelse(`6_96` <  96, `6_96`, `5_80`),
      `5_112` = ifelse(`6_96` >=  96, `6_96`, `5_112`),
      `4_8` = ifelse(`5_16` <  16, `5_16`, `4_8`),
      `4_24` = ifelse(`5_16` >=  16, `5_16`, `4_24`),
      `4_40` = ifelse(`5_48` <  48, `5_48`, `4_40`),
      `4_56` = ifelse(`5_48` >=  48, `5_48`, `4_56`),
      `4_72` = ifelse(`5_80` <  80, `5_80`, `4_72`),
      `4_88` = ifelse(`5_80` >=  80, `5_80`, `4_88`),
      `4_104` = ifelse(`5_112` <  112, `5_112`, `4_104`),
      `4_120` = ifelse(`5_112` >=  112, `5_112`, `4_120`),
      `3_4` = ifelse(`4_8` <  8, `4_8`, `3_4`),
      `3_12` = ifelse(`4_8` >=  8, `4_8`, `3_12`),
      `3_20` = ifelse(`4_24` <  24, `4_24`, `3_20`),
      `3_28` = ifelse(`4_24` >=  24, `4_24`, `3_28`),
      `3_36` = ifelse(`4_40` <  40, `4_40`, `3_36`),
      `3_44` = ifelse(`4_40` >=  40, `4_40`, `3_44`),
      `3_52` = ifelse(`4_56` <  56, `4_56`, `3_52`),
      `3_60` = ifelse(`4_56` >=  56, `4_56`, `3_60`),
      `3_68` = ifelse(`4_72` <  72, `4_72`, `3_68`),
      `3_76` = ifelse(`4_72` >=  72, `4_72`, `3_76`),
      `3_84` = ifelse(`4_88` <  88, `4_88`, `3_84`),
      `3_92` = ifelse(`4_88` >=  88, `4_88`, `3_92`),
      `3_100` = ifelse(`4_104` <  104, `4_104`, `3_100`),
      `3_108` = ifelse(`4_104` >=  104, `4_104`, `3_108`),
      `3_116` = ifelse(`4_120` <  120, `4_120`, `3_116`),
      `3_124` = ifelse(`4_120` >=  120, `4_120`, `3_124`),
      `2_2` = ifelse(`3_4` <  4, `3_4`, `2_2`),
      `2_6` = ifelse(`3_4` >=  4, `3_4`, `2_6`),
      `2_10` = ifelse(`3_12` <  12, `3_12`, `2_10`),
      `2_14` = ifelse(`3_12` >=  12, `3_12`, `2_14`),
      `2_18` = ifelse(`3_20` <  20, `3_20`, `2_18`),
      `2_22` = ifelse(`3_20` >=  20, `3_20`, `2_22`),
      `2_26` = ifelse(`3_28` <  28, `3_28`, `2_26`),
      `2_30` = ifelse(`3_28` >=  28, `3_28`, `2_30`),
      `2_34` = ifelse(`3_36` <  36, `3_36`, `2_34`),
      `2_38` = ifelse(`3_36` >=  36, `3_36`, `2_38`),
      `2_42` = ifelse(`3_44` <  44, `3_44`, `2_42`),
      `2_46` = ifelse(`3_44` >=  44, `3_44`, `2_46`),
      `2_50` = ifelse(`3_52` <  52, `3_52`, `2_50`),
      `2_54` = ifelse(`3_52` >=  52, `3_52`, `2_54`),
      `2_58` = ifelse(`3_60` <  60, `3_60`, `2_58`),
      `2_62` = ifelse(`3_60` >=  60, `3_60`, `2_62`),
      `2_66` = ifelse(`3_68` <  68, `3_68`, `2_66`),
      `2_70` = ifelse(`3_68` >=  68, `3_68`, `2_70`),
      `2_74` = ifelse(`3_76` <  76, `3_76`, `2_74`),
      `2_78` = ifelse(`3_76` >=  76, `3_76`, `2_78`),
      `2_82` = ifelse(`3_84` <  84, `3_84`, `2_82`),
      `2_86` = ifelse(`3_84` >=  84, `3_84`, `2_86`),
      `2_90` = ifelse(`3_92` <  92, `3_92`, `2_90`),
      `2_94` = ifelse(`3_92` >=  92, `3_92`, `2_94`),
      `2_98` = ifelse(`3_100` <  100, `3_100`, `2_98`),
      `2_102` = ifelse(`3_100` >=  100, `3_100`, `2_102`),
      `2_106` = ifelse(`3_108` <  108, `3_108`, `2_106`),
      `2_110` = ifelse(`3_108` >=  108, `3_108`, `2_110`),
      `2_114` = ifelse(`3_116` <  116, `3_116`, `2_114`),
      `2_118` = ifelse(`3_116` >=  116, `3_116`, `2_118`),
      `2_122` = ifelse(`3_124` <  124, `3_124`, `2_122`),
      `2_126` = ifelse(`3_124` >=  124, `3_124`, `2_126`)
    ) %>% 
    pivot_longer(!sim, names_to = c('rd', 'rd_slot'), names_sep = '_', values_to = 'team_slot') %>% 
    mutate(
      rd = as.numeric(rd),
      rd_slot = as.numeric(rd_slot)
    ) %>% 
    arrange(sim, -rd, rd_slot)
}