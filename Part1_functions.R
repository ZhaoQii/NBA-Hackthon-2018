# Function for each game
each_game <- function(game_df){
  game_df <- game_df %>% arrange(Period, desc(PC_Time), WC_Time, Event_Num)
  game_id <- game_df[1, 1]
  # All quaters in this game
  quaters <- unique(game_df$Period)
  # Matrix to store all we want
  this_game <- matrix(0, 0, 4)
  colnames(this_game) <- c('Game_id', 'Team_id', 'Person_id', '+/-')
  # Iterate for each quater and update the statistics matrix
  for (i in seq(quaters)){
    this_game <- each_quater(game_id = game_id, quater_no = i, 
                             pre_stat = this_game, game_df = game_df)
  }
  return(this_game)
}
# Function for each equater
each_quater <- function(game_id, quater_no, pre_stat, game_df){
  this_stat <- pre_stat
  # Data for this quater
  this_quater <- filter(game_df, Period == quater_no)
  # Starters for this quater
  this_quater_start <- filter(Lineup, Game_id == game_id, Period == quater_no) %>% select(Game_id, Team_id, Person_id)
  # Chech if they are new players who haven't shown up so far in this game
  new_players_index <- this_quater_start$Person_id %in% this_stat[, 3]
  # Add new players into statistics matrix
  if (sum(new_players_index) != 10){
    this_stat <- rbind(this_stat, cbind(this_quater_start[!new_players_index,], '+/-' = 0))
  }
  # All substitutions during this quater
  this_quater_sub <- filter(game_df, Event_Msg_Type == 8 | Event_Msg_Type == 11, Period == quater_no) %>% select(Game_id, Person1, Person2)
  # Identify the true team new sub belongs to
  find_teamid <- this_stat$Team_id[match(this_quater_sub$Person1, this_stat$Person_id)]
  this_quater_sub <- data.frame('Game_id' = this_quater_sub$Game_id, 'Team_id' = find_teamid, 'Person2' = this_quater_sub$Person2)
  this_quater_sub <- this_quater_sub[complete.cases(this_quater_sub), ]
  # Check if they are new players who haven't shown up so far in this game
  new_sub <- this_quater_sub$Person %in% this_stat$Person_id
  # Add new players into statistics matrix
  if (sum(new_sub) != length(new_sub)){
    new_sub <- cbind(this_quater_sub[!new_sub,], 0)
    colnames(new_sub) <- c('Game_id', 'Team_id', 'Person_id', '+/-')
    this_stat <- rbind(this_stat,unique(new_sub[, 1:4]))
  }
  # For each event, add a true team id according to player1
  this_quater$Real_team <- this_stat$Team_id[match(this_quater$Person1, this_stat$Person_id)]
  # Update each player's +/- in statistics matrix
  this_stat$'+/-' <- this_stat$'+/-' + apply(this_stat, MARGIN = 1,
                                             FUN = cal_player, this_quater = this_quater,
                                             this_quater_start = this_quater_start)
  
  return(this_stat)
}
# Function to compute +/- for each player
cal_player <- function(id, this_quater, this_quater_start, this_stat){
  person_id <- id[3]
  team_id <- id[2]
  is_start <- person_id %in% this_quater_start$Person_id
  # Find all in and out times for this player in this quater
  ins_outs <- find_ins_outs(person_id, this_quater, is_start)
  ins_outs <- matrix(ins_outs[complete.cases(ins_outs)], ncol = 2)
  # Compute the +/- during the time this player is on the court
  s <- apply(ins_outs, FUN = cal_pm, MARGIN = 1, this_quater = this_quater, team_id = team_id)
  return(sum(s))
}
# Function to return in and out times for players each quater
find_ins_outs <- function(person_id, this_quater, is_start){
  in_out_matrix <- matrix(NA, 10, 2)
  colnames(in_out_matrix) <- c('In', 'Out')
  # Whether this player is starter
  in_out_matrix[1, 1] <- ifelse(is_start, 1, NA)
  # Find all in times of this player
  in_index <- which(this_quater$Event_Msg_Type == 8 & this_quater$Person2 == person_id)
  s <- as.numeric(is_start)
  for(eachin in in_index){
    pc_time <- this_quater[eachin, 'PC_Time']
    wc_time <- this_quater[eachin, 'WC_Time']
    # To address the problem of substitution during several free throws as mentioned in the problem
    in_out_matrix[s + 1, 1] <- 1 + eachin + nrow( filter(this_quater, PC_Time == pc_time & WC_Time > wc_time) )
    s <- s + 1
  }
  
  s <- 1
  # Find all out times of this player
  out_indexs <- which(this_quater$Event_Msg_Type == 8 & this_quater$Person1 == person_id)
  # If no out times, no worry for those who don't play this whole quater, we keep only complete cases in line 60
  if (length(out_indexs) == 0){
    in_out_matrix[1, 2] <- nrow(this_quater)
  } else {
    for (eachout in out_indexs){
      pc_time <- this_quater[eachout, 'PC_Time']
      wc_time <- this_quater[eachout, 'WC_Time']
      # To address the problem of substitution during several free throws as mentioned in the problem
      in_out_matrix[s, 2] <- eachout + nrow( filter(this_quater, PC_Time == pc_time & WC_Time > wc_time) )
      s <- s + 1
    }
    # For those playing until this quater ends
    if (sum(!is.na(in_out_matrix[, 1])) > sum(!is.na(in_out_matrix[, 2]))){
      in_out_matrix[sum(!is.na(in_out_matrix[, 1])), 2] <- nrow(this_quater)
    }
    
  }
  return(in_out_matrix)
}
# Compute +/- for each played interval of player
cal_pm <- function(row, this_quater, team_id){
  start <- row[1]
  end <- row[2]
  team_ids <- unique(this_quater$Team_id)
  oppo_team_id <- team_ids[team_id != team_ids]
  interval <- this_quater[start:end, ]
  s <- sum_int(interval, team_id, 1) + sum_int(interval, team_id, 3) - 
    sum_int(interval, oppo_team_id, 1) - sum_int(interval, oppo_team_id, 3)
  return(s)
}
# Function to sum the points, depends on team_id and whether a field goal or free throw
sum_int <- function(interval, team_id, type){
  if (type == 1) {
    on <- filter(interval, Real_team == team_id & Event_Msg_Type == 1)
    s <- ifelse(nrow(on) == 0, 0, sum(select(on, Option1)))
  } else {
    on <- filter(interval, Real_team == team_id & Event_Msg_Type == 3 & Option1 == 1)
    s <- nrow(on)
  }
  return(s)
}
