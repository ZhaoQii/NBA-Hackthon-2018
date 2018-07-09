each_game <- function(game_df){
  game_df <- game_df %>% arrange(Period, desc(PC_Time), WC_Time, Event_Num)
  game_id <- game_df[1, 1]
  quaters <- unique(game_df$Period)
  this_game <- matrix(0, 0, 4)
  colnames(this_game) <- c('Game_id', 'Team_id', 'Person_id', '+/-')
  for (i in seq(quaters)){
    this_game <- each_quater(game_id = game_id, quater_no = i, 
                             pre_stat = this_game, game_df = game_df)
  }
  return(this_game)
}

each_quater <- function(game_id, quater_no, pre_stat, game_df){
  this_stat <- pre_stat
  this_quater_start <- filter(Lineup, Game_id == game_id, Period == quater_no) %>% select(Game_id, Team_id, Person_id)
  new_players_index <- this_quater_start$Person_id %in% this_stat[, 3]
  if (sum(new_players_index) != 10){
    this_stat <- rbind(this_stat, cbind(this_quater_start[!new_players_index,], '+/-' = 0))
  }
  
  this_quater_sub <- filter(game_df, Event_Msg_Type == 8 | Event_Msg_Type == 11, Period == quater_no) %>% select(Game_id, Team_id, Person2)
  new_sub <- this_quater_sub$Person2 %in% this_stat$Person_id
  if (sum(new_sub) != length(new_sub)){
    new_sub <- cbind(this_quater_sub[!new_sub,], 0)
    colnames(new_sub) <- c('Game_id', 'Team_id', 'Person_id', '+/-')
    this_stat <- rbind(this_stat,unique(new_sub[, 1:4]))
  }
  this_stat$'+/-' <- this_stat$'+/-' + apply(this_stat, MARGIN = 1,
                                             FUN = cal_player, game_df = game_df,
                                             quater_no = quater_no,
                                             this_quater_start = this_quater_start)
  
  return(this_stat)
}

cal_player <- function(id, game_df, quater_no, this_quater_start){
  person_id <- id[3]
  team_id <- id[2]
  this_quater <- filter(game_df, Period == quater_no)
  is_start <- person_id %in% this_quater_start$Person_id
  ins_outs <- find_ins_outs(person_id, this_quater, is_start)
  ins_outs <- matrix(ins_outs[complete.cases(ins_outs)], ncol = 2)
  s <- apply(ins_outs, FUN = cal_pm, MARGIN = 1, this_quater = this_quater, team_id = team_id)
  return(sum(s))
}

find_ins_outs <- function(person_id, this_quater, is_start){
  in_out_matrix <- matrix(NA, 10, 2)
  colnames(in_out_matrix) <- c('In', 'Out')
  in_out_matrix[1, 1] <- ifelse(is_start, 1, NA)
  in_index <- which(this_quater$Event_Msg_Type == 8 & this_quater$Person2 == person_id)
  s <- as.numeric(is_start)
  for(eachin in in_index){
    pc_time <- this_quater[eachin, 'PC_Time']
    wc_time <- this_quater[eachin, 'WC_Time']
    in_out_matrix[s + 1, 1] <- 1 + eachin + nrow( filter(this_quater, PC_Time == pc_time, WC_Time > wc_time) )
    s <- s + 1
  }
  
  s <- 1
  out_indexs <- which(this_quater$Event_Msg_Type == 8 & this_quater$Person1 == person_id)
  if (length(out_indexs) == 0){
    in_out_matrix[1, 2] <- nrow(this_quater)
  } else {
    for (eachout in out_indexs){
      pc_time <- this_quater[eachout, 'PC_Time']
      wc_time <- this_quater[eachout, 'WC_Time']
      in_out_matrix[s, 2] <- eachout + nrow( filter(this_quater, PC_Time == pc_time, WC_Time > wc_time) )
      s <- s + 1
    }
    
    if (sum(!is.na(in_out_matrix[, 1])) > sum(!is.na(in_out_matrix[, 2]))){
      in_out_matrix[sum(!is.na(in_out_matrix[, 1])), 2] <- nrow(this_quater)
    }
    
  }
  return(in_out_matrix)
}

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

sum_int <- function(interval, team_id, type){
  if (type == 1) {
    on <- filter(interval, Team_id == team_id & Event_Msg_Type == 1)
    s <- ifelse(nrow(on) == 0, 0, sum(select(on, Option1)))
  } else {
    on <- filter(interval, Team_id == team_id & Event_Msg_Type == 3 & Option1 == 1)
    s <- nrow(on)
  }
  return(s)
}