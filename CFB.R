###COLLEGE FOOTBALL##

library(devtools)
library(tidyverse)
library(cfbscrapR)
library(gt)
library(ggimage)
##Pull team logos
team_info <- cfbscrapR::cfb_team_info()
team_logos <- team_info %>%
  select(school, color, alt_color, logos, alt_name2) %>%
  mutate(logo = map(logos, magrittr::extract2, 1),
         logo = as.character(logo)) %>% select(-logos)
#load in play by play data for 2019
pbp_2019 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2019 <- bind_rows(pbp_2019, df)
}


##QR Four Data
qr4plays <- pbp_2019 %>% filter(period == 4)
##Eliminates FCS teams and narrows down our table
qr4offense <- qr4plays %>% group_by(offense_play) %>% summarise(ypa = mean(yards_gained[pass==1]), ypr = mean(yards_gained[rush==1]), num.plays = n()) %>% filter(num.plays > 75)

#Actually lets get offense/defense in terms of EPA
qr4offense <- qr4plays %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 75)

qr4defense <- qr4plays %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.pass.def = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 75)

qr4team.epa <- left_join(qr4offense, qr4defense, by = c("offense_play" = "defense_play")) 

head(qr4team.epa)

qr4team.epa$logo <- team_logos$logo

qr4team.epa %>% ggplot(aes(x=epa.rush.off, y=epa.pass.off)) + geom_point() +
  geom_vline(xintercept = mean(team.epa$epa.rush.off), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(team.epa$epa.pass.off), linetype = "dashed", color = "blue") +
  labs(x = "4th Quarter Rush EPA/Play", y= "4th Quarter Pass EPA/Play",
       title = "Who Owns the Fourth Quarter?") +
  geom_image(aes(image=logo), size = 0.05)+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))



##QR 1-3 DATA
qr13plays <- pbp_2019 %>% filter(period != 4)
##Eliminates FCS teams and narrows down our table
qr13offense <- plays %>% group_by(offense_play) %>% summarise(ypa = mean(yards_gained[pass==1]), ypr = mean(yards_gained[rush==1]), num.plays = n()) %>% filter(num.plays > 200)

#Actually lets get offense/defense in terms of EPA
qr13offense <- plays %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 200)

qr13defense <- plays %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.pass.def = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 200)

qr13team.epa <- left_join(qr13offense, qr13defense, by = c("offense_play" = "defense_play")) 

head(qr13team.epa)




qr13team.epa$logo <- team_logos$logo

#WITH A GRAPH
qr13team.epa %>% ggplot(aes(x=epa.rush.off, y=epa.pass.off)) + geom_point() +
  geom_vline(xintercept = mean(team.epa$epa.rush.off), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(team.epa$epa.pass.off), linetype = "dashed", color = "blue") +
  labs(x = "1st-3rd Quarter Rush EPA/Play", y= "1st Through 3rd Quarter Pass EPA/Play",
       title = "Who Owns the Fourth Quarter?") +
  geom_image(aes(image=logo), size = 0.05)+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))



