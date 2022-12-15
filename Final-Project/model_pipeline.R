library(tidyverse)
library(tidymodels)
library(here)

data1 <- read_csv(here('out/GREEN4.csv')) %>%
    select(wy, month, load_yield_adj, yield, pH_mean, gpp_mean, precip_mean) %>%
    mutate(site = 'Colorado')
data2 <- read_csv(here('out/w3.csv')) %>%
    select(wy, month, load_yield_adj, yield, pH_mean, gpp_mean, precip_mean) %>%
    mutate(site = 'Vermont')
data3 <- read_csv(here('out/GSMACK.csv')) %>%
    select(wy, month, load_yield_adj, yield, pH_mean, gpp_mean, precip_mean) %>%
    mutate(site = 'California')

data <- rbind(data1, data2, data3) %>%
  mutate(month_adj = ifelse(month == 10, 1, 
                            ifelse(month == 11, 2,
                                   ifelse(month == 12, 3, 
                                          ifelse(month == 1, 4, 
                                                 ifelse(month == 2, 5, 
                                                        ifelse(month == 1, 4, 
                                                               ifelse(month == 3, 6, 
                                                                      ifelse(month == 4, 7, 
                                                                             ifelse(month == 5, 8, 
                                                                                    ifelse(month == 6, 9, 
                                                                                           ifelse(month == 7, 10, 
                                                                                                  ifelse(month == 8, 11, 
                                                                                                         ifelse(month == 9, 12, month)))))))))))))) %>%
  select(-month)

pre_caa <- data %>%
    filter(wy < 1990,
           wy > 1987) %>%
    select(-wy)

pre_caa %>%
    select(-site) %>%
    pairs() %>%
ggsave(filename = here('out/pre_pairs.png'))

post_caa <- data %>%
    filter(wy < 2013,
           wy > 2010) %>%
    select(-wy)

post_caa %>%
    select(-site) %>%
    pairs()
ggsave(filename = here('out/post_pairs.png'))



