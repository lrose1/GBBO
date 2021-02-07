library(tidyverse)
library(estimatr)
library(quantreg)
library(fastDummies)
library(ggpubr)
library(ggdist)
options(scipen = 999)

setwd("~/")
working <- read_csv("~/Documents/GBBO/Data/working.csv") %>% 
  mutate(num_bins = case_when(
    week == 1 ~ 12,
    week == 2 ~ 11,
    week == 3 ~ 10,
    week == 4 ~ 9,
    week == 5 ~ 8,
    week == 6 ~ 7,
    week == 7 ~ 6,
    week == 8 ~ 5,
    week == 9 ~ 4,
    week == 10 ~ 3,
    is.na(week) ~ 4
  ))

#### Want to make a figure dropping each week ####

allweeks <- as.list(unique(working$week))

test <- lapply(allweeks, function(x){
  temp <- working %>% 
    filter(!is.na(week)) %>% 
    filter(week <= x) 
  
  temp %>% 
    mutate(bins = ntile(x = order_judged_std, n=min(num_bins))) %>% 
    filter(!is.na(bins)) %>% 
    group_by(bins) %>%
    # summarise(placement = mean(placement_std, na.rm=T)) %>% 
    ggplot(., aes(x = bins, y = placement_prank, colour = as.factor(bins))) + geom_jitter(width = .15) +
    # scale_color_brewer(palette="Set1") +
    stat_halfeye(show.legend = F, width = .5, point_colour = "black", interval_alpha = 0, slab_alpha = .3,
                 point_alpha = .8) +
    xlab("Order Judged (Bin)") +
    ylab("Placement (Percentile)") + 
    stat_smooth(geom = 'line', method = "lm", formula = y ~ x + I(x^2), se = F, colour = "firebrick", alpha = 0.8, size = 1.5) + 
    theme_pubclean() +
    theme(legend.position = "none") 
  
})

ggarrange(plotlist = test)
test[9]

working_reg <- working %>% 
  mutate(bins = ntile(x = order_judged_std, n=4))

lm_robust(placement_prank ~ order_judged_prank + as.factor(week)*as.factor(season), data = working)
lm_robust(placement_prank ~ as.factor(order_judged_quartile)  + as.factor(week) + as.factor(season), data = working %>% 
            filter(week!=10))
lm_robust(placement_prank ~ as.factor(bins)  + as.factor(week) + as.factor(season), data = working_reg %>% 
            filter(week!=10))

#### early vs. late weeks ####

temp1 <- working_reg %>% 
  filter(!is.na(week)) %>% 
  filter(week <= 9 & week >=5) 
temp2 <- working_reg %>% 
  filter(!is.na(week)) %>% 
  filter(week <= 4)

late <- temp1 %>% 
  mutate(bins = ntile(x = order_judged_std, n=4)) %>% 
  filter(!is.na(bins)) %>% 
  group_by(bins) %>%
  # summarise(placement = mean(placement_std, na.rm=T)) %>% 
  ggplot(., aes(x = bins, y = placement_prank, colour = as.factor(bins))) + geom_jitter(width = .15) +
  # scale_color_brewer(palette="Set1") +
  stat_halfeye(show.legend = F, width = .5, point_colour = "black", interval_alpha = 0, slab_alpha = .3,
               point_alpha = .8) +
  xlab("Order Judged (Bin) \n Weeks 5-9") +
  ylab("Placement (Percentile)") + 
  stat_smooth(geom = 'line', method = "lm", formula = y ~ x + I(x^2), se = F, colour = "firebrick", alpha = 0.8, size = 1.5) + 
  theme_pubclean() +
  theme(legend.position = "none") 

early <- temp2 %>% 
  mutate(bins = ntile(x = order_judged_std, n=4)) %>% 
  filter(!is.na(bins)) %>% 
  group_by(bins) %>%
  # summarise(placement = mean(placement_std, na.rm=T)) %>% 
  ggplot(., aes(x = bins, y = placement_prank, colour = as.factor(bins))) + geom_jitter(width = .15) +
  # scale_color_brewer(palette="Set1") +
  stat_halfeye(show.legend = F, width = .5, point_colour = "black", interval_alpha = 0, slab_alpha = .3,
               point_alpha = .8) +
  xlab("Order Judged (Bin)  \n Weeks 1-4") +
  ylab("Placement (Percentile)") + 
  stat_smooth(geom = 'line', method = "lm", formula = y ~ x + I(x^2), se = F, colour = "firebrick", alpha = 0.8, size = 1.5) + 
  theme_pubclean() +
  theme(legend.position = "none") 
ggarrange(early, late)

lm_robust(placement_prank ~ as.factor(bins)  + as.factor(week) + as.factor(season), 
          data = temp1)
lm_robust(placement_prank ~ as.factor(bins)  -1, 
          data = temp2)

#### Goes out ####

out <- lapply(allweeks, function(x){
  temp <- working %>% 
    filter(!is.na(week)) %>% 
    filter(week <= x) 
  
  temp %>% 
    mutate(bins = ntile(x = order_judged_std, n=min(num_bins))) %>% 
    filter(!is.na(bins)) %>% 
    group_by(bins) %>%
    summarise(placement = mean(week_out2, na.rm=T), placement_sd = sd(week_out2, na.rm=T), n=n())%>%
    mutate(placement_se = placement_sd/sqrt(n)) %>% 
    ggplot(., aes(x = bins, y = placement, colour = as.factor(bins))) + geom_point() +
    geom_errorbar(aes(ymin = placement - 1.96*placement_se, ymax = placement + 1.96*placement_se)) +
    stat_halfeye(show.legend = F, width = .5, point_colour = "black", interval_alpha = 0, slab_alpha = .3,
                 point_alpha = .8) +
    xlab("Order Judged (Bin)") +
    ylab("Probability of Leaving the Tent") + 
    stat_smooth(geom = 'line', se = F, method = "lm", formula = y ~ x + I(x^2), colour = "firebrick", alpha = 0.8, size = 1.5) + 
    theme_pubclean() +
    theme(legend.position = "none") 
  
})
ggarrange(plotlist = out)
out[[9]]

lm_robust(week_out2 ~ order_judged_prank + as.factor(week) + as.factor(season) , data = working)
lm_robust(week_out2 ~ as.factor(bins) + as.factor(week) + as.factor(season) , data = working_reg %>% 
            filter(week!=10))

#### Go out early vs. late ####

late2 <- temp1 %>% 
  mutate(bins = ntile(x = order_judged_std, n=4)) %>% 
  filter(!is.na(bins)) %>% 
  group_by(bins) %>%
  summarise(placement = mean(week_out2, na.rm=T), placement_sd = sd(week_out2, na.rm=T), n=n())%>%
  mutate(placement_se = placement_sd/sqrt(n)) %>% 
  ggplot(., aes(x = bins, y = placement, colour = as.factor(bins))) + geom_point() +
  geom_errorbar(aes(ymin = placement - 1.96*placement_se, ymax = placement + 1.96*placement_se)) +
  # scale_color_brewer(palette="Set1") +
  stat_halfeye(show.legend = F, width = .5, point_colour = "black", interval_alpha = 0, slab_alpha = .3,
               point_alpha = .8) +
  xlab("Order Judged (Bin) \n Weeks 5-9") +
  ylab("Probability of Leaving the Tent") + 
  stat_smooth(geom = 'line', method = "lm", formula = y ~ x + I(x^2), se = F, colour = "firebrick", alpha = 0.8, size = 1.5) + 
  theme_pubclean() +
  theme(legend.position = "none") 

early2 <- temp2 %>% 
  mutate(bins = ntile(x = order_judged_std, n=4)) %>% 
  filter(!is.na(bins)) %>% 
  group_by(bins) %>%
  summarise(placement = mean(week_out2, na.rm=T), placement_sd = sd(week_out2, na.rm=T), n=n())%>%
  mutate(placement_se = placement_sd/sqrt(n)) %>% 
  ggplot(., aes(x = bins, y = placement, colour = as.factor(bins))) + geom_point() +
  geom_errorbar(aes(ymin = placement - 1.96*placement_se, ymax = placement + 1.96*placement_se)) +
  # scale_color_brewer(palette="Set1") +
  stat_halfeye(show.legend = F, width = .5, point_colour = "black", interval_alpha = 0, slab_alpha = .3,
               point_alpha = .8) +
  xlab("Order Judged (Bin)  \n Weeks 1-4") +
  ylab("Probability of Leaving the Tent") + 
  stat_smooth(geom = 'line', method = "lm", formula = y ~ x + I(x^2), se = F, colour = "firebrick", alpha = 0.8, size = 1.5) + 
  theme_pubclean() +
  theme(legend.position = "none") 
ggarrange(early2, late2)
