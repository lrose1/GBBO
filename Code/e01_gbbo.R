library(googlesheets4)
library(tidyverse)
library(estimatr)
library(quantreg)
library(fastDummies)
library(ggpubr)
options(scipen = 999)

# analyze GBBO data, stored in google sheet

#### Import and tidy ####

gbbo <- read_sheet("https://docs.google.com/spreadsheets/d/16DptWY0S9bpmB0HQE91tgKH_9ZpXXPBaSfEVVSzRkpk/edit?ts=5f5d6a4e#gid=0")

cleanit <- gbbo %>% 
  mutate(week = as.character(week)) %>% 
  select(1:8) %>% 
  mutate(order_judged = as.integer(order_judged)) %>% 
  mutate(placement = as.integer(placement)) %>% 
  mutate(week_id = paste0(season, week)) %>% 
  group_by(week_id) %>%
  mutate(order_judged_std = scale(order_judged)) %>% 
  mutate(order_judged_prank = percent_rank(order_judged)) %>% 
  mutate(order_judged_quartile = ntile(order_judged, n = 4)) %>% 
  dummy_cols(., select_columns ="order_judged_quartile") %>% 
  group_by(week_id) %>%
  mutate(placement_std = scale(placement)) %>% 
  mutate(placement_std = ifelse(placement_std >0, 0- placement_std, abs(placement_std))) %>% 
  mutate(placement_prank = 1- percent_rank(placement)) %>% 
  mutate(judged_first = ifelse(order_judged == 1, 1,0)) %>% 
  mutate(judged_last = ifelse(order_judged == max(order_judged), 1,0)) %>% 
  mutate(judged_middle = ifelse(ntile(order_judged, n = 3) == 2, 1, 0)) %>% 
  mutate(baker_id = paste0(baker, "_",season)) %>% 
  mutate(bottom3 = ifelse(ntile(placement, 3) == 3, 1, 0)) %>% 
  mutate(prue_years = ifelse(season == "collection 5" | season == "collection 6" | season == "collection 7", 1,0))


# when they went out 
allbaker_ids1 <- cleanit %>%  ungroup() %>% 
  select(baker_id, season, week) %>% arrange(season, week, baker_id) %>% 
  group_by(season) %>% 
  mutate(week_out = case_when(!baker_id %in% as.list(.$baker_id[.$week == 2]) ~ 1,
                          !baker_id %in% as.list(.$baker_id[.$week == 3]) ~ 2, 
                          !baker_id %in% as.list(.$baker_id[.$week == 4]) ~ 3,
                          !baker_id %in% as.list(.$baker_id[.$week == 5]) ~ 4,
                          !baker_id %in% as.list(.$baker_id[.$week == 6]) ~ 5,
                          !baker_id %in% as.list(.$baker_id[.$week == 7]) ~ 6,
                          !baker_id %in% as.list(.$baker_id[.$week == 8]) ~ 7,
                          !baker_id %in% as.list(.$baker_id[.$week == 9]) ~ 8,
                          !baker_id %in% as.list(.$baker_id[.$week == 10]) ~ 9
                          )) %>% select(-week) %>%  unique()

working <- cleanit %>% 
  left_join(allbaker_ids1) %>% 
  mutate(week_out2 = case_when(week == week_out ~ 1,
                               week != week_out ~ 0, 
                               is.na(week_out) ~ 0 )) %>% ungroup()
write_csv(working, "~/Documents/GBBO/Data/working.csv")

# test
cleanit %>% 
  group_by(week) %>% 
  summarise(place = mean(placement_prank, na.rm=T), placement_std = mean(placement_std, na.rm=T))
working %>% 
  group_by(week) %>% 
  summarise(place = mean(placement_prank, na.rm=T), placement_std = mean(placement_std, na.rm=T))

#### figures ####

# simple x vs. y
working %>% 
  group_by(order_judged) %>% 
  summarise(placement = mean(placement, na.rm=T)) %>% 
  ggplot(., aes(x = order_judged, y = placement)) + geom_point() +
  xlab("Order Judged") +
  ylab("Placement") + 
  stat_smooth(geom = 'line', method = "lm", se = F, colour = "firebrick", alpha = 0.8) + 
  theme_pubclean()

# standardized within week  
working %>% 
  group_by(order_judged) %>% 
  summarise(placement = mean(placement_std, na.rm=T), sd = sd(placement_std, na.rm=T)) %>% 
  ggplot(., aes(x = order_judged, y = placement)) + geom_point() +
  geom_errorbar(ymin = placement - sd, ymax = placement + sd) +
  xlab("Order Judged") +
  ylab("Placement") + 
  theme_pubclean()

library(ggdist)
working %>% 
  filter(!is.na(order_judged)) %>% 
  filter(order_judged <13) %>% 
  # group_by(order_judged) %>% 
  # summarise(placement = mean(placement_std, na.rm=T), sd = sd(placement_std, na.rm=T)) %>% 
  ggplot(., aes(x = order_judged, y = placement, group = as.factor(order_judged), 
                colour = as.factor(order_judged))) +
  geom_jitter() +
  stat_slab(alpha = 0.3) +
  xlab("Order Judged") +
  ylab("Placement") + 
  theme_pubclean()

# tough to see
working %>% 
  mutate(bins = ntile(x = order_judged_std, n=10)) %>% 
  group_by(bins) %>%
  summarise(placement = mean(placement_std, na.rm=T)) %>% 
  ggplot(., aes(x = bins, y = placement)) + geom_point() +
  xlab("Order Judged") +
  ylab("Placement") + 
  stat_smooth(geom = 'line', method = "lm", se = F, colour = "firebrick", alpha = 0.8) + 
  theme_pubclean()

working %>% 
  mutate(bins = ntile(x = order_judged_std, n=10)) %>% 
  group_by(bins) %>%
  summarise(placement = mean(placement_prank, na.rm=T)) %>% 
  ggplot(., aes(x = bins, y = placement)) + geom_point() +
  xlab("Order Judged (10 bins)") +
  ylab("Placement (Percentile)") + 
  stat_smooth(geom = 'line', method = "lm", se = F, colour = "firebrick", alpha = 0.8) + 
  theme_pubclean()

#percent rank
working %>% 
  mutate(bins = ntile(x = order_judged_prank, n = 4)) %>% 
  ggplot(., aes(x = bins, y = placement_prank, group=bins)) + geom_boxplot() +
  xlab("Order Judged") +
  ylab("Placement") + 
  theme_pubclean()

# bottom 3
working %>% 
  mutate(bins = ntile(x = order_judged_std, n=10)) %>% 
  group_by(bins) %>%
  summarise(placement = mean(bottom3, na.rm=T)) %>% 
  ggplot(., aes(x = bins, y = placement)) + geom_point() +
  xlab("Order Judged") +
  ylab("Likelihood of bottom third placement") + 
  theme_pubclean()

# go out
working %>% 
  # filter(week <9) %>% 
  mutate(bins = ntile(x = order_judged_std, n=10)) %>% 
  group_by(bins) %>%
  summarise(placement = mean(week_out2, na.rm=T)) %>% 
  ggplot(., aes(x = bins, y = placement)) + geom_point() +
  stat_smooth(geom = 'line', method = "lm", se = F, colour = "firebrick", alpha = 0.8) + 
  xlab("Order Judged") +
  ylab("Likelihood of leaving") + 
  theme_pubclean()

# does placement matter in general? Yes
tidy(lm_robust(week_out2 ~ placement_prank, data=working))
working %>% 
  mutate(bins = ntile(x = placement_prank, n = 10)) %>% 
  group_by(bins) %>%
  summarise(week_out2 = mean(week_out2, na.rm=T)) %>% 
  ggplot(., aes(x= bins, y = week_out2)) + geom_point()

# by week
working %>% 
  group_by(week, order_judged) %>% 
  summarise(placement = mean(placement_std, na.rm=T)) %>% 
  ggplot(., aes(x = order_judged, y = placement)) + geom_point() +
  xlab("Order Judged") +
  ylab("Placement") + 
  theme_pubclean() +
  facet_wrap(~week,nrow = 3)

# combine weeks
working %>% 
  mutate(week_group = case_when(week== "1" | week == "2" | week =="3" | week == "4" ~ "1-4",
                                week== "5" | week == "6" | week =="7" ~ "5-7", 
                                week== "8" | week == "9" | week =="10" ~ "8-10")) %>% 
  group_by(week_group, order_judged) %>% 
  summarise(placement = mean(placement_prank, na.rm=T)) %>% 
  ggplot(., aes(x = order_judged, y = placement)) + geom_point() +
  xlab("Order Judged") +
  ylab("Placement") + 
  stat_smooth(geom = 'line', method = "lm", se = F, colour = "firebrick", alpha = 0.8) + 
  theme_pubclean() +
  facet_wrap(~week_group,nrow = 3)

# combine weeks pt 2
working %>% 
  mutate(week_group = case_when(week== "1" | week == "2" | week =="3" | week == "4" ~ "1-4",
                                week== "5" | week == "6" | week =="7" |
                                week== "8" | week == "9" | week =="10" ~ "5-10")) %>% 
  filter(!is.na(week_group)) %>% 
  mutate(bins = ntile(x = order_judged_std, n=6)) %>% 
  group_by(bins, week_group) %>% 
  summarise(placement = mean(placement_prank, na.rm=T)) %>% 
  ggplot(., aes(x = bins, y = placement)) + geom_point() +
  xlab("Order Judged") +
  ylab("Placement (Percentile)") + 
  stat_smooth(geom = 'line', method = "lm", se = F, colour = "firebrick", alpha = 0.8) + 
  theme_pubclean() +
  facet_wrap(~week_group,nrow = 1)


working %>% 
  mutate(week_group = case_when(week== "1" | week == "2" | week =="3" | week == "4" ~ "1-4",
                                week== "5" | week == "6" | week =="7" |
                                  week== "8" | week == "9" | week =="10" ~ "5-10")) %>% 
  filter(!is.na(week_group)) %>% 
  # filter(week <9) %>% 
  mutate(bins = ntile(x = order_judged_std, n=10)) %>% 
  group_by(week_group, bins) %>%
  summarise(placement = mean(week_out2, na.rm=T)) %>% 
  ggplot(., aes(x = bins, y = placement)) + geom_point() +
  xlab("Order Judged") +
  ylab("Probability of Leaving") + 
  stat_smooth(geom = 'line', method = "lm", se = F, colour = "firebrick", alpha = 0.8) + 
  theme_pubclean() +
  facet_wrap(~week_group,nrow = 1)

#### regressions ####

# simple
lm_robust(placement ~ order_judged + as.factor(week), data = working)
lm_robust(placement ~ order_judged + as.factor(week) + as.factor(season), data = working)
lm_robust(placement ~ order_judged + as.factor(week)*as.factor(season), data = working)

# percent rank
lm_robust(placement_prank ~ order_judged_prank + as.factor(week), data = working)
lm_robust(placement_prank ~ order_judged_prank + as.factor(week) + as.factor(season) + as.factor(sweet), data = working)
lm_robust(placement_prank ~ order_judged_prank + as.factor(week)*as.factor(season), data = working)
lm_robust(week_out2 ~ order_judged_prank + as.factor(week), data = working)
lm_robust(week_out2 ~ order_judged_prank + as.factor(week) + as.factor(season), data = working %>% group_by(season) %>% filter(week!= max(week)))
lm_robust(week_out2 ~ order_judged_prank + as.factor(week)*as.factor(season), data = working)

# with quartile
lm_robust(placement_prank ~ as.factor(order_judged_quartile) + as.factor(week), data = working)
lm_robust(placement_prank ~ as.factor(order_judged_quartile) + as.factor(week) + as.factor(season), data = working)
lm_robust(placement_prank ~ as.factor(order_judged_quartile)  + as.factor(week)*as.factor(season), data = working)
lm_robust(week_out2 ~ as.factor(order_judged_quartile) + as.factor(week), data = working)
lm_robust(week_out2 ~ as.factor(order_judged_quartile) + as.factor(week) + as.factor(season), data = working)
lm_robust(week_out2 ~ as.factor(order_judged_quartile)  + as.factor(week)*as.factor(season), data = working)


# being last or first 
lm_robust(placement_prank ~ judged_first + as.factor(week), data = working)
lm_robust(placement_prank ~ judged_first + as.factor(week) + as.factor(season) , data = working)
lm_robust(placement_prank ~ judged_first + as.factor(week)*as.factor(season), data = working)
lm_robust(placement_prank ~ judged_last + as.factor(week), data = working)
lm_robust(placement_prank ~ judged_last + as.factor(week) + as.factor(season), data = working)
lm_robust(placement_prank ~ judged_last + as.factor(week)*as.factor(season), data = working)

# voted off the island 
lm_robust(week_out2 ~ judged_first + as.factor(week), data = working)
lm_robust(week_out2 ~ judged_first + as.factor(week) + as.factor(season) + as.factor(sweet), data = working %>% group_by(season) %>% filter(week!= max(week)))
lm_robust(week_out2 ~ judged_first + as.factor(week)*as.factor(season), data = working)
lm_robust(week_out2 ~ judged_last + as.factor(week), data = working)
lm_robust(week_out2 ~ judged_last + as.factor(week) + as.factor(season), data = working %>% group_by(season) %>% filter(week!= max(week)))
lm_robust(week_out2 ~ judged_last + as.factor(week)*as.factor(season), data = working)

# by week
byweek <- lapply(3:10, function(x){
  t1 <- tidy(lm_robust(placement_prank ~ judged_first + as.factor(week) + as.factor(season), data = working %>% filter(week <=x))) %>% 
    filter(term == "judged_first")
  t2 <- tidy(lm_robust(bottom3 ~ judged_first + as.factor(week) + as.factor(season), data = working %>% filter(week <=x & week!= 10))) %>% 
    filter(term == "judged_first")
  t3 <- tidy(lm_robust(week_out2 ~ judged_first + as.factor(week) + as.factor(season), data = working %>% filter(week <=x & week!= 10))) %>% 
    filter(term == "judged_first")
  
  t4 <- tidy(lm_robust(placement_prank ~ judged_last+ as.factor(week)  + as.factor(season), data = working %>% filter(week <=x))) %>% 
    filter(term == "judged_last")
  t5 <- tidy(lm_robust(bottom3 ~ judged_last+ as.factor(week)  + as.factor(season), data = working %>% filter(week <=x))) %>% 
    filter(term == "judged_last")
  t6 <- tidy(lm_robust(week_out2 ~ judged_last + as.factor(week) + as.factor(season), data = working %>% filter(week <=x & week!= 10))) %>% 
    filter(term == "judged_last")
  
  t7 <- tidy(lm_robust(placement_prank ~ judged_middle+ as.factor(week)  + as.factor(season), data = working %>% filter(week <=x))) %>% 
    filter(term == "judged_middle")
  t8 <- tidy(lm_robust(bottom3 ~ judged_middle+ as.factor(week)  + as.factor(season), data = working %>% filter(week <=x))) %>% 
    filter(term == "judged_middle")
  t9 <- tidy(lm_robust(week_out2 ~ judged_middle + as.factor(week) + as.factor(season), data = working %>% filter(week <=x & week!= 10))) %>% 
    filter(term == "judged_middle")
  
  t10 <- tidy(lm_robust(placement_prank ~ order_judged_quartile_4 + as.factor(week)  + as.factor(season), data = working %>% filter(week <=x))) %>% 
    filter(term == "order_judged_quartile_4")
  t11 <- tidy(lm_robust(bottom3 ~ order_judged_quartile_4 + as.factor(week) + as.factor(season), data = working %>% filter(week <=x & week!= 10))) %>% 
    filter(term == "order_judged_quartile_4")
  t12 <- tidy(lm_robust(week_out2 ~ order_judged_quartile_4 + as.factor(week) + as.factor(season), data = working %>% filter(week <=x & week!= 10))) %>% 
    filter(term == "order_judged_quartile_4")
  
  t13 <- tidy(lm_robust(placement_prank ~ order_judged_quartile_1 + as.factor(week)  + as.factor(season), data = working %>% filter(week <=x))) %>% 
    filter(term == "order_judged_quartile_1")
  t14 <- tidy(lm_robust(bottom3 ~ order_judged_quartile_1 + as.factor(week)  + as.factor(season), data = working %>% filter(week <=x))) %>% 
    filter(term == "order_judged_quartile_1")
  t15 <- tidy(lm_robust(week_out2 ~ order_judged_quartile_1 + as.factor(week) + as.factor(season), data = working %>% filter(week <=x & week!= 10))) %>% 
    filter(term == "order_judged_quartile_1")
  
  bind_rows(t1,t2,t3,t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) %>% mutate(week_cutoff = x)
})
byweek <- bind_rows(byweek)

# by season
seasons <- as.list(unique(working$season))
byseason <- lapply(seasons, function(x){
  t1 <- tidy(lm_robust(placement_prank ~ judged_first + as.factor(week) , data = working %>% filter(season ==x))) %>% 
    filter(term == "judged_first")
  t2 <- tidy(lm_robust(week_out2 ~ judged_first + as.factor(week), data = working %>% filter(season ==x & week!= 10))) %>% 
    filter(term == "judged_first")
  t3 <- tidy(lm_robust(placement_prank ~ judged_last+ as.factor(week)  , data = working %>% filter(season ==x))) %>% 
    filter(term == "judged_last")
  t4 <- tidy(lm_robust(week_out2 ~ judged_last + as.factor(week) , data = working %>% filter(season ==x & week!= 10))) %>% 
    filter(term == "judged_last")
  t5 <- tidy(lm_robust(placement_prank ~ judged_middle+ as.factor(week)  , data = working %>% filter(season ==x))) %>% 
    filter(term == "judged_middle")
  t6 <- tidy(lm_robust(week_out2 ~ judged_middle + as.factor(week) , data = working %>% filter(season ==x & week!= 10))) %>% 
    filter(term == "judged_middle")
  t7 <- tidy(lm_robust(placement_prank ~ order_judged_quartile_4 + as.factor(week)  , data = working %>% filter(season ==x))) %>% 
    filter(term == "order_judged_quartile_4")
  t8 <- tidy(lm_robust(week_out2 ~ order_judged_quartile_4 + as.factor(week) , data = working %>% filter(season ==x & week!= 10))) %>% 
    filter(term == "order_judged_quartile_4")
  t9 <- tidy(lm_robust(placement_prank ~ order_judged_quartile_1 + as.factor(week)  , data = working %>% filter(season ==x))) %>% 
    filter(term == "order_judged_quartile_1")
  t10 <- tidy(lm_robust(week_out2 ~ order_judged_quartile_1 + as.factor(week) , data = working %>% filter(season ==x & week!= 10))) %>% 
    filter(term == "order_judged_quartile_1")
  
  bind_rows(t1,t2,t3,t4, t5, t6, t7, t8, t9, t10) %>% mutate(season = x)
})
byseason <- bind_rows(byseason)

# prue years 
prue <- as.list(unique(working$prue_years))
byprue <- lapply(prue, function(x){
  t1 <- tidy(lm_robust(placement_prank ~ judged_first + as.factor(week) + as.factor(season) , data = working %>% filter(prue_years ==x))) %>% 
    filter(term == "judged_first")
  t2 <- tidy(lm_robust(week_out2 ~ judged_first + as.factor(week) + as.factor(season), data = working %>% filter(prue_years ==x & week!= 10))) %>% 
    filter(term == "judged_first")
  t3 <- tidy(lm_robust(placement_prank ~ judged_last+ as.factor(week) + as.factor(season)  , data = working %>% filter(prue_years ==x))) %>% 
    filter(term == "judged_last")
  t4 <- tidy(lm_robust(week_out2 ~ judged_last + as.factor(week)+ as.factor(season) , data = working %>% filter(prue_years ==x & week!= 10))) %>% 
    filter(term == "judged_last")
  t5 <- tidy(lm_robust(placement_prank ~ judged_middle+ as.factor(week)+ as.factor(season)  , data = working %>% filter(prue_years ==x))) %>% 
    filter(term == "judged_middle")
  t6 <- tidy(lm_robust(week_out2 ~ judged_middle + as.factor(week) + as.factor(season), data = working %>% filter(prue_years ==x & week!= 10))) %>% 
    filter(term == "judged_middle")
  t7 <- tidy(lm_robust(placement_prank ~ order_judged_quartile_4 + as.factor(week)+ as.factor(season)  , data = working %>% filter(prue_years ==x))) %>% 
    filter(term == "order_judged_quartile_4")
  t8 <- tidy(lm_robust(week_out2 ~ order_judged_quartile_4 + as.factor(week) + as.factor(season), data = working %>% filter(prue_years ==x & week!= 10))) %>% 
    filter(term == "order_judged_quartile_4")
  t9 <- tidy(lm_robust(placement_prank ~ order_judged_quartile_1 + as.factor(week) + as.factor(season) , data = working %>% filter(prue_years ==x))) %>% 
    filter(term == "order_judged_quartile_1")
  t10 <- tidy(lm_robust(week_out2 ~ order_judged_quartile_1 + as.factor(week)+ as.factor(season) , data = working %>% filter(prue_years ==x & week!= 10))) %>% 
    filter(term == "order_judged_quartile_1")
  
  bind_rows(t1,t2,t3,t4, t5, t6, t7, t8, t9, t10) %>% mutate(prue = x)
})
byprue <- bind_rows(byprue)




