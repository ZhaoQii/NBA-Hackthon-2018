### Basketball part
library(dplyr)
setwd('/Users/ap/Desktop/2018 Business & Basketball Analytics Hackathon Files/')
source('Part1_functions.r')

# Load data sets
Event <- read.delim('Basketball Analytics/NBA Hackathon - Event Codes.txt', as.is = T)
Lineup <- read.delim('Basketball Analytics/NBA Hackathon - Game Lineup Data Sample (50 Games).txt', as.is = T)
Play <- read.delim('Basketball Analytics/NBA Hackathon - Play by Play Data Sample (50 Games).txt', as.is = T)

# Extract the part necessary to compute +/-
score_sub <- Play %>% filter(Event_Msg_Type == 1 | Event_Msg_Type == 3 | Event_Msg_Type == 8 | Event_Msg_Type == 11)
result <- split(score_sub, f = score_sub$Game_id)


te05071512 <- plyr::llply(result, .fun = each_game)




####### testing
sum(filter(game_df, Team_id == '012059d397c0b7e5a30a5bb89c0b075e' & Event_Msg_Type == 1) %>% select(Option1)) + 
  nrow(filter(game_df, Team_id == '012059d397c0b7e5a30a5bb89c0b075e' & Event_Msg_Type == 3 & Option1 == 1)) 

sum(filter(game_df, Team_id == 'cff694c8186a4bd377de400e4f60fe47' & Event_Msg_Type == 1) %>% select(Option1)) + 
  nrow(filter(game_df, Team_id == 'cff694c8186a4bd377de400e4f60fe47' & Event_Msg_Type == 3 & Option1 == 1)) 

for (i in seq(50)){
  print(sum(te05071512[[i]]$`+/-`))
}


game_test11 <- filter(Play, Game_id == result[[11]][1,1], Period == 1)
