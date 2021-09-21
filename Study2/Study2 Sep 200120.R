
#### INITIAL ANOVA SEP ####

ANOVA_int_duet <- data.frame(Trial = 1, BallDist = 2, 
                        Segment = 'Play', Familiarity = 'Familiar',
                        Subject = 4, Length = 5)
ANOVA_int_duet$Familiarity <- factor(ANOVA_int_duet$Familiarity, 
                                     levels = c('Familiar', 'Unfamiliar'), 
                                     labels = c('Familiar', 'Unfamiliar'))
ANOVA_int_duet$Segment <- factor(ANOVA_int_duet$Segment, 
                            levels = c('None', 'Control', 'Listen', 'Play',
                                       'Prepare', 'Wait', 'Overlap'), 
                            labels = c('None', 'Control', 'Listen', 'Play',
                                       'Prepare', 'Wait', 'Overlap'))

for(i in which(duetData$segment != 'None')) {
  
  if(i > 2){
    # Create a variable noting when each trial begins
    trialStart <- 1
    
    # If trial is not same as previous row value, set current row to trialStart
    if(duetData$trial[i] != duetData$trial[i - 1]) {
      trialStart <- i
    }
  }
  
  if(duetData$segment[i] != duetData$segment[i + 1]) {

    bt <- i
    
    while(duetData$segment[bt] == duetData$segment[i] & bt > 1) {
      bt <- bt - 1
    }
    
    if(bt > 1) {
      bt <- bt + 1
    }
    
    if(duetData$Subject[i] == duetData$Subject[bt] & bt != i & 
       bt != trialStart) {
      
      x <- data.frame(Trial = duetData$trial[i], 
                      BallDist = mean(duetData$BallDist[bt:i]), 
                      Segment = duetData$segment[i],
                      Familiarity = duetData$Familiarity[i],
                      Subject = duetData$Subject[i],
                      Length = duetData$BallMillis[i] - duetData$duration_ms[bt] - 
                        duetData$BallMillis[bt])
      
      if(x$Length[1] <= 0) {
        x$Length[1] <- 17
      }
      
      ANOVA_int_duet <- rbind(ANOVA_int_duet, x)
      
    } else if(duetData$Subject[i] == duetData$Subject[bt] & bt == i) {
      
      x <- data.frame(Trial = duetData$trial[i], 
                      BallDist = duetData$BallDist[i], 
                      Segment = duetData$segment[i], 
                      Familiarity = duetData$Familiarity[i],
                      Subject = duetData$Subject[i],
                      Length = duetData$duration_ms[i])
      
      if(x$Length[1] <= 0) {
        x$Length[1] <- 17
      }
      
      ANOVA_int_duet <- rbind(ANOVA_int_duet, x)
      
    }
    print(i)
  }
}

ANOVA_int_duet <- ANOVA_int_duet[2:nrow(ANOVA_int_duet), ]

# Remove all rows where segments are shorter than 500ms
ANOVA_int_duet <- ANOVA_int_duet[ANOVA_int_duet$Length >= 500, ]

#### FOR FIRST ANALYSIS ####

ANOVA_1_duet <- data.frame(Trial = 1, BallDist = 2, 
                      Segment = 'Play',
                      Familiarity = 'Familiar',
                      Subject = 4, 
                      Order = 'First')
ANOVA_1_duet$Segment <- factor(ANOVA_1_duet$Segment, levels = 
                            c('Play', 'Listen'),
                          labels = c('Play', 'Listen'))
ANOVA_1_duet$Familiarity <- factor(ANOVA_1_duet$Familiarity, 
                                     levels = c('Familiar', 'Unfamiliar'), 
                                     labels = c('Familiar', 'Unfamiliar'))
ANOVA_1_duet$Order <- factor(ANOVA_1_duet$Order, levels = 
                          c('First', 'Last'),
                        labels = c('First', 'Last'))


# Loop iterating through duetData, picking out relevant values
for(i in 2:(nrow(duetData) - 1)) {
  
  # Create a variable noting when each trial begins
  trialStart <- 1
  
  # If trial is not same as previous row value, set current row to trialStart
  if(duetData$trial[i] != duetData$trial[i - 1]) {
    trialStart <- i
  }
  
  # If trial > 3 & segments are 'Listen' or 'Play' &
  # the current segment is different from the next (meaning we're at the final row of a segment) &
  # the current row is 2000ms after trial start
  if(duetData$trial[i] > 3 & 
     duetData$segment[i] %in% c('Listen', 'Play') &
     duetData$segment[i] != duetData$segment[i + 1] &
     duetData$BallMillis[i] - duetData$BallMillis[trialStart] > 2000) {
    
    # create new variable with current row number
    bt <- i
    
    # decrease bt by 1 until segment at row bt is not the same as at row i
    while(duetData$segment[i] == duetData$segment[bt] & bt > 1) {
      bt <- bt - 1
    }
    
    # If the time between i and bt is more than 1000, segment can be used
    if(duetData$BallMillis[i] - duetData$BallMillis[bt] >= 1000) {
      
      # lt is last time stamp, for last part of segment
      lt <- i
      
      # while time between i (start of segment) and lt is less than 480, go back one row
      # when time is 480, we have the last 480ms of the segment
      while(duetData$BallMillis[i] - duetData$BallMillis[lt] < 480 & lt > 1) {
        lt <- lt - 1
      }
      
      # ft is first time stamp, for first part of segment
      ft <- bt
      
      # while time between bt (beginning of segment) and ft is less than 480, increase row by 1
      # when time is 480, we have first 480 ms of segment
      while(duetData$BallMillis[ft] - duetData$BallMillis[bt] < 480 & ft > 1) {
        ft <- ft + 1
      }
      
      # Create new data frame with the FIRST part of segment (mean BallDist from bt to ft)
      x <- data.frame(Trial = duetData$trial[i], 
                      BallDist = mean(duetData$BallDist[bt:ft]),
                      Segment = duetData$segment[i],
                      Familiarity = duetData$Familiarity[i],
                      Subject = duetData$Subject[i],
                      Order = 'First')
      x$Order <- factor(x$Order, levels = 
                          c('First', 'Last'),
                        labels = c('First', 'Last'))
      
      ANOVA_1_duet <- rbind(ANOVA_1_duet, x)
      
      # Create new data frame with the LAST part of segment (mean BallDist from lt to i)
      x <- data.frame(Trial = duetData$trial[i], 
                      BallDist = mean(duetData$BallDist[lt:i]),
                      Segment = duetData$segment[i],
                      Familiarity = duetData$Familiarity[i],
                      Subject = duetData$Subject[i],
                      Order = 'Last')
      x$Order <- factor(x$Order, levels = 
                          c('First', 'Last'),
                        labels = c('First', 'Last'))
      ANOVA_1_duet <- rbind(ANOVA_1_duet, x)
      
    }
    print(i)
  }
}

# Remove first 
ANOVA_1_duet <- ANOVA_1_duet[2:nrow(ANOVA_1_duet), ]
colnames(ANOVA_1_duet) <- c('Trial', 'BallDist', 'Segment', 'Familiarity',
                       'Subject', 'Order')

# Subtract 3 from all trial values in data frame so that trial 4 is now trial 1
# Because trial 4 is first trial we analyze
ANOVA_1_duet$Trial <- ANOVA_1_duet$Trial - 3



#### FOR SECOND ANALYSIS ####

ANOVA_2_duet <- ANOVA_1_duet[ANOVA_1_duet$Order == 'Last', 1:5]
ANOVA_2_duet$Segment <- factor(ANOVA_2_duet$Segment, levels = 
                            c('Play', 'Listen', 'Prepare', 'Wait'),
                          labels = c('Play', 'Listen', 'Prepare', 'Wait'))

# Add 3 to trial column, to match with duetData
ANOVA_2_duet$Trial <- ANOVA_2_duet$Trial + 3

# Loop iterating through duetData, picking out relevant values
for(i in 2:(nrow(duetData) - 1)) {
  
  # Create a variable noting when each trial begins
  trialStart <- 1
  
  # If trial is not same as previous row value, set current row to trialStart
  if(duetData$trial[i] != duetData$trial[i - 1]) {
    trialStart <- i
  }
  
  # If trial > 3 & segments are 'Prepare' &
  # the current segment is different from the next (meaning we're at the final row of a segment) &
  # the current row is 2000ms after trial start
  if(duetData$trial[i] > 3 & 
     duetData$segment[i] %in% c('Prepare', 'Wait') &
     duetData$segment[i] != duetData$segment[i + 1] &
     duetData$BallMillis[i] - duetData$BallMillis[trialStart] > 2000) {
    
    # Back track to last key press before i
    bt <- i
    while(duetData$segment[i] == duetData$segment[bt] & bt > 1) {
      bt <- bt - 1
    }
    bt <- bt + 1
    
    if(duetData$segment[i] == duetData$segment[bt] &
       duetData$BallMillis[i] - duetData$BallMillis[bt] >= 500) {
      
      
      # Make new data frame with prepare/wait segment
      x <- data.frame(Trial = duetData$trial[i], 
                      BallDist = mean(duetData$BallDist[bt:i]),
                      Segment = duetData$segment[i],
                      Familiarity = duetData$Familiarity[i],
                      Subject = duetData$Subject[i])
      x$Segment <- factor(x$Segment, levels = 
                            c('Play', 'Listen', 'Prepare', 'Wait'),
                          labels = c('Play', 'Listen', 'Prepare', 'Wait'))
      ANOVA_2_duet <- rbind(ANOVA_2_duet, x)
    }
  }
  print(i)
}

colnames(ANOVA_2_duet) <- c('Trial', 'BallDist', 'Segment', 'Familiarity', 'Subject')

# Subtract 3 from trial so that trial 4 is now first trial
ANOVA_2_duet$Trial <- ANOVA_2_duet$Trial - 3


#### ANOVA 4 Create ####

ANOVA_4_duet <- ANOVA_1_duet[(ANOVA_1_duet$Segment == 'Play' & ANOVA_1_duet$Order == 'First') | 
                     (ANOVA_1_duet$Segment == 'Listen' & ANOVA_1_duet$Order == 'Last'),]

ANOVA_4_duet$Segment <- factor(ANOVA_4_duet$Segment, 
                          levels = c('Listen', 'Play',
                                     'Prepare', 'BegPlay', 'EndListen'), 
                          labels = c('Listen', 'Play',
                                     'Prepare', 'BegPlay', 'EndListen'))

ANOVA_4_duet$Segment[ANOVA_4_duet$Segment == 'Play' & 
                  ANOVA_4_duet$Order == 'First'] <- 'BegPlay'

ANOVA_4_duet$Segment[ANOVA_4_duet$Segment == 'Listen' & 
                  ANOVA_4_duet$Order == 'Last'] <- 'EndListen'

ANOVA_4_duet <- ANOVA_4_duet[,1:5]

ANOVA_4_duet <- rbind(ANOVA_4_duet, ANOVA_2_duet[ANOVA_2_duet$Segment == 'Prepare',])

ANOVA_4_duet$Segment <- droplevels(ANOVA_4_duet$Segment)


#### ANOVA 5 Create ####

ANOVA_5_duet <- ANOVA_1_duet[(ANOVA_1_duet$Segment == 'Play' & ANOVA_1_duet$Order == 'Last') | 
                     (ANOVA_1_duet$Segment == 'Listen' & ANOVA_1_duet$Order == 'First'),]

ANOVA_5_duet$Segment <- factor(ANOVA_5_duet$Segment, 
                          levels = c('Listen', 'Play',
                                     'Wait', 'EndPlay', 'BegListen'), 
                          labels = c('Listen', 'Play',
                                     'Wait', 'EndPlay', 'BegListen'))

ANOVA_5_duet$Segment[ANOVA_5_duet$Segment == 'Play' & 
                  ANOVA_5_duet$Order == 'Last'] <- 'EndPlay'

ANOVA_5_duet$Segment[ANOVA_5_duet$Segment == 'Listen' & 
                  ANOVA_5_duet$Order == 'First'] <- 'BegListen'

ANOVA_5_duet <- ANOVA_5_duet[,1:5]

ANOVA_5_duet <- rbind(ANOVA_5_duet, ANOVA_2_duet[ANOVA_2_duet$Segment == 'Wait',])

ANOVA_5_duet$Segment <- droplevels(ANOVA_5_duet$Segment)


#### End of listen ANOVA Sep ####

ANOVA_7_duet <- data.frame(Trial = 1, BallDist = 2, 
                      Segment = 'Listen', Familiarity = 'Familiar',
                      Bin = 1, Subject = 4)
ANOVA_7_duet$Segment <- factor(ANOVA_7_duet$Segment, levels = c('Listen'), 
                          labels = c('Listen'))
ANOVA_7_duet$Familiarity <- factor(ANOVA_7_duet$Familiarity, 
                                   levels = c('Familiar', 'Unfamiliar'), 
                                   labels = c('Familiar', 'Unfamiliar'))
ANOVA_7_duet$Bin <- factor(ANOVA_7_duet$Bin, levels = c(1, 2, 3, 4),
                      labels = c('0-120', '121-240', '241-360', '361-480'))

Has4 <- 0
Not4 <- 0

# Loop iterating through duetData, picking out relevant values
for(i in 2:(nrow(duetData) - 1)) {
  
  # Create a variable for row where trial starts
  trialStart <- 1
  
  # If trial is not same as previous row value, set current row to trialStart
  if(duetData$trial[i] != duetData$trial[i - 1]) {
    trialStart <- i
  }
  
  # If trial > 3 & segments are 'Listen' &
  # the current segment is different from the next (meaning we're at the final row of a segment) &
  # the current row is 2000ms after trial start
  if(duetData$trial[i] > 3 & 
     duetData$segment[i] == 'Listen' &
     duetData$segment[i] != duetData$segment[i + 1] &
     duetData$BallMillis[i] - duetData$BallMillis[trialStart] > 2000) {
    
    # Back track to last key press before i
    bt <- i
    while(duetData$segment[i] == duetData$segment[bt] & bt > 1) {
      bt <- bt - 1
    }
    bt <- bt + 1
    
    # If the time between i and bt is more than 1000, segment can be used
    if(duetData$BallMillis[i] - duetData$BallMillis[bt] > 1000) {
      
      # lt is last time stamp, for last part of segment
      lt <- i
      
      # while time between i (start of segment) and lt is less than 480, go back one row
      # when time is 480, we have the last 480ms of the segment
      while(duetData$BallMillis[i] - duetData$BallMillis[lt] < 480 & lt > 1) {
        lt <- lt - 1
      }
      
      # if final 480ms exist over 4 observations, split into 4 observations and save
      if(i - lt >= 4) {
        
        Has4 <- Has4 + 1
        
        # Get number of rows per bin
        bin_val <- (i - lt)/4
        
        # Create new data frame with the LAST part of segment (mean BallDist from lt to i)
        # Split into 4 bins
        x <- data.frame(Trial = rep(duetData$trial[i], 4), 
                        BallDist = c(mean(duetData$BallDist[lt:(lt + round(bin_val, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val, 0) + 1):(lt + round(bin_val*2, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*2, 0) + 1):(lt + round(bin_val*3, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*3, 0) + 1):i])),
                        Segment = rep(duetData$segment[i], 4),
                        Familiarity = rep(duetData$Familiarity[i], 4),
                        Bin = c(1, 2, 3, 4),
                        Subject = rep(duetData$Subject[i], 4))
        x$Segment <- factor(x$Segment, levels = 
                              c('Listen'),
                            labels = c('Listen'))
        x$Bin <- factor(x$Bin, levels = c(1, 2, 3, 4),
                        labels = c('0-120', '121-240', '241-360', '361-480'))
        ANOVA_7_duet <- rbind(ANOVA_7_duet, x)
      } else if(i - lt < 4) {
        Not4 <- Not4 + 1
      }
    }
    print(i)
  }
}

colnames(ANOVA_7_duet) <- c('Trial', 'BallDist', 'Segment', 'Familiarity', 'Bin', 'Subject')

ANOVA_7_duet <- ANOVA_7_duet[2:nrow(ANOVA_7_duet), ]

# Subtract 3 from trial so that trial 4 is now first trial
ANOVA_7_duet$Trial <- ANOVA_7_duet$Trial - 3

Has4
Not4
Has4/(Not4 + Has4)

#### Pre-play ANOVA Sep ####


ANOVA_8_duet <- data.frame(Trial = 1, BallDist = 2, 
                      Segment = 'Prepare', Familiarity = 'Familiar',
                      Bin = 1, Subject = 4)
ANOVA_8_duet$Segment <- factor(ANOVA_8_duet$Segment, levels = c('Prepare'), 
                          labels = c('Prepare'))
ANOVA_8_duet$Familiarity <- factor(ANOVA_8_duet$Familiarity, 
                                   levels = c('Familiar', 'Unfamiliar'), 
                                   labels = c('Familiar', 'Unfamiliar'))
ANOVA_8_duet$Bin <- factor(ANOVA_8_duet$Bin, levels = c(1, 2, 3, 4),
                      labels = c('0-120', '121-240', '241-360', '361-480'))

PrepareMore480 <- 0
PrepareLess480 <- 0
Has4 <- 0
Not4 <- 0

# Loop iterating through duetData, picking out relevant values
for(i in 2:(nrow(duetData) - 1)) {
  
  # Create a variable for row where trial starts
  trialStart <- 1
  
  # If trial is not same as previous row value, set current row to trialStart
  if(duetData$trial[i] != duetData$trial[i - 1]) {
    trialStart <- i
  }
  
  # If trial > 3 & segments are 'Prepare' &
  # the current segment is different from the next (meaning we're at the final row of a segment) &
  # the current row is 2000ms after trial start
  if(duetData$trial[i] > 3 & 
     duetData$segment[i] == 'Prepare' &
     duetData$segment[i] != duetData$segment[i + 1] &
     duetData$BallMillis[i] - duetData$BallMillis[trialStart] > 2000) {
    
    # Back track to last key press before i
    bt <- i
    while(duetData$segment[i] == duetData$segment[bt] & bt > 1) {
      bt <- bt - 1
    }
    bt <- bt + 1
    
    # If the time between i and bt is more than 1000, segment can be used
    if(duetData$BallMillis[i] - duetData$BallMillis[bt] >= 500) {
      
      PrepareMore480 <- PrepareMore480 + 1
      
      # ft is first time stamp, for first part of segment
      ft <- bt
      
      # while time between bt (beginning of segment) and ft is less than 480, increase row by 1
      # when time is 480, we have first 480 ms of segment
      while(duetData$BallMillis[ft] - duetData$BallMillis[bt] < 480 & ft > 1) {
        ft <- ft + 1
      }
      
      if(ft - bt >= 4) {
        
        Has4 <- Has4 + 1
        
        # Get number of rows per bin
        bin_val <- (ft - bt)/4
        
        # Create new data frame with the LAST part of segment (mean BallDist from lt to i)
        # Split into 4 bins
        x <- data.frame(Trial = rep(duetData$trial[i], 4), 
                        BallDist = c(mean(duetData$BallDist[bt:(bt + round(bin_val, 0))]),
                                     mean(duetData$BallDist[(bt + round(bin_val, 0) + 1):(bt + round(bin_val*2, 0))]),
                                     mean(duetData$BallDist[(bt + round(bin_val*2, 0) + 1):(bt + round(bin_val*3, 0))]),
                                     mean(duetData$BallDist[(bt + round(bin_val*3, 0) + 1):ft])),
                        Segment = rep(duetData$segment[i], 4),
                        Familiarity = rep(duetData$Familiarity[i], 4),
                        Bin = c(1, 2, 3, 4),
                        Subject = rep(duetData$Subject[i], 4))
        x$Segment <- factor(x$Segment, levels = c('Prepare'),
                            labels = c('Prepare'))
        x$Bin <- factor(x$Bin, levels = c(1, 2, 3, 4),
                        labels = c('0-120', '121-240', '241-360', '361-480'))
        ANOVA_8_duet <- rbind(ANOVA_8_duet, x)
        
      } else if(ft - bt < 4) {
        Not4 <- Not4 + 1
      }
    } else if(duetData$BallMillis[i] - duetData$BallMillis[bt] < 480) {
      PrepareLess480 <- PrepareLess480 + 1
    }
    print(i)
  }
}

colnames(ANOVA_8_duet) <- c('Trial', 'BallDist', 'Segment', 'Familiarity', 'Bin', 'Subject')

ANOVA_8_duet <- ANOVA_8_duet[2:nrow(ANOVA_8_duet), ]

# Subtract 3 from trial so that trial 4 is now first trial
ANOVA_8_duet$Trial <- ANOVA_8_duet$Trial - 3

Has4
Not4
Has4/(Not4 + Has4)

PrepareMore480
PrepareLess480
PrepareMore480/(PrepareMore480 + PrepareLess480)

#### Play Bin Separation ####

ANOVA_9_duet <- data.frame(Trial = 1, BallDist = 2, 
                      Segment = 'Listen', Familiarity = 'Familiar',
                      Bin = 1, Subject = 4)
ANOVA_9_duet$Segment <- factor(ANOVA_9_duet$Segment, levels = c('Listen'), 
                          labels = c('Listen'))
ANOVA_9_duet$Familiarity <- factor(ANOVA_9_duet$Familiarity, 
                                   levels = c('Familiar', 'Unfamiliar'), 
                                   labels = c('Familiar', 'Unfamiliar'))
ANOVA_9_duet$Bin <- factor(ANOVA_9_duet$Bin, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                      labels = c('0-120', '121-240', '241-360', '361-480', '481-600',
                                 '601-720', '721-840', '841-960', '961-1080',
                                 '1081-1200', '1201-1320', '1321-1440'))

ListenMore1440 <- 0
ListenLess1440 <- 0
Has12 <- 0
Not12 <- 0

# Loop iterating through duetData, picking out relevant values
for(i in 2:(nrow(duetData) - 1)) {
  
  # Create a variable for row where trial starts
  trialStart <- 1
  
  # If trial is not same as previous row value, set current row to trialStart
  if(duetData$trial[i] != duetData$trial[i - 1]) {
    trialStart <- i
  }
  
  # If trial > 3 & segments are 'Listen' &
  # the current segment is different from the next (meaning we're at the final row of a segment) &
  # the current row is 2000ms after trial start
  if(duetData$trial[i] > 3 & 
     duetData$segment[i] == 'Listen' &
     duetData$segment[i] != duetData$segment[i + 1] &
     duetData$BallMillis[i] - duetData$BallMillis[trialStart] > 2000) {
    
    # Back track to last key press before i
    bt <- i
    while(duetData$segment[i] == duetData$segment[bt] & bt > 1) {
      bt <- bt - 1
    }
    bt <- bt + 1
    
    # If the time between i and bt is more than 1000, segment can be used
    if(duetData$BallMillis[i] - duetData$BallMillis[bt] >= 1440) {
      
      ListenMore1440 <- ListenMore1440 + 1
      
      # lt is last time stamp, for last part of segment
      lt <- i
      
      # while time between i (start of segment) and lt is less than 480, go back one row
      # when time is 480, we have the last 480ms of the segment
      while(duetData$BallMillis[i] - duetData$BallMillis[lt] < 1440 & lt > 1) {
        lt <- lt - 1
      }
      
      if(i - lt >= 12) {
        
        Has12 <- Has12 + 1
        
        # Get number of rows per bin
        bin_val <- (i - lt)/12
        
        # Create new data frame with the LAST part of segment (mean BallDist from lt to i)
        # Split into 4 bins
        x <- data.frame(Trial = rep(duetData$trial[i], 12), 
                        BallDist = c(mean(duetData$BallDist[lt:(lt + round(bin_val, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val, 0) + 1):(lt + round(bin_val*2, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*2, 0) + 1):(lt + round(bin_val*3, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*3, 0) + 1):(lt + round(bin_val*4, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*4, 0) + 1):(lt + round(bin_val*5, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*5, 0) + 1):(lt + round(bin_val*6, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*6, 0) + 1):(lt + round(bin_val*7, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*7, 0) + 1):(lt + round(bin_val*8, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*8, 0) + 1):(lt + round(bin_val*9, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*9, 0) + 1):(lt + round(bin_val*10, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*10, 0) + 1):(lt + round(bin_val*11, 0))]),
                                     mean(duetData$BallDist[(lt + round(bin_val*11, 0) + 1):i])),
                        Segment = rep(duetData$segment[i], 12),
                        Familiarity = rep(duetData$Familiarity[i], 12),
                        Bin = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                        Subject = rep(duetData$Subject[i], 12))
        x$Segment <- factor(x$Segment, levels = 
                              c('Listen'),
                            labels = c('Listen'))
        x$Bin <- factor(x$Bin, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                        labels = c('0-120', '121-240', '241-360', '361-480', '481-600',
                                   '601-720', '721-840', '841-960', '961-1080',
                                   '1081-1200', '1201-1320', '1321-1440'))
        ANOVA_9_duet <- rbind(ANOVA_9_duet, x)
        
      } else if(i - lt < 12) {
        Not12 <- Not12 + 1
      }
    } else if(duetData$BallMillis[i] - duetData$BallMillis[bt] < 1440) {
      ListenLess1440 <- ListenLess1440 + 1
    }
    print(i)
  }
}

colnames(ANOVA_9_duet) <- c('Trial', 'BallDist', 'Segment',
                            'Familiarity', 'Bin', 'Subject')

ANOVA_9_duet <- ANOVA_9_duet[2:nrow(ANOVA_9_duet), ]

# Subtract 3 from trial so that trial 4 is now first trial
ANOVA_9_duet$Trial <- ANOVA_9_duet$Trial - 3

Has12
Not12
Has12/(Has12 + Not12)

ListenMore1440
ListenLess1440
ListenMore1440/(ListenMore1440 + ListenLess1440)
