
#### INITIAL ANOVA SEP ####

ANOVA_int <- data.frame(Trial = 1, BallDist = 2, 
                        Segment = 'Play', Subject = 4, Length = 5)
ANOVA_int$Segment <- factor(ANOVA_int$Segment, 
                            levels = c('None', 'Control', 'Listen', 'Play',
                                       'Prepare', 'Wait', 'Overlap'), 
                            labels = c('None', 'Control', 'Listen', 'Play',
                                       'Prepare', 'Wait', 'Overlap'))

for(i in which(musicData$segment != 'None')) {
  
  if(i > 2){
    # Create a variable noting when each trial begins
    trialStart <- 1
    
    # If trial is not same as previous row value, set current row to trialStart
    if(musicData$trial[i] != musicData$trial[i - 1]) {
      trialStart <- i
    }
  }
  
  if(musicData$segment[i] != musicData$segment[i + 1]) {
    
    bt <- i
    
    while(musicData$segment[bt] == musicData$segment[i] & bt > 1) {
      bt <- bt - 1
    }
    
    if(bt > 1) {
      bt <- bt + 1
    }
    
    if(musicData$Subject[i] == musicData$Subject[bt] & bt != i & 
       bt != trialStart) {
      
      x <- data.frame(Trial = musicData$trial[i], 
                      BallDist = mean(musicData$BallDist[bt:i]), 
                      Segment = musicData$segment[i], 
                      Subject = musicData$Subject[i],
                      Length = musicData$BallMillis[i] - musicData$duration_ms[bt] - 
                        musicData$BallMillis[bt])
      
      if(x$Length[1] <= 0) {
        x$Length[1] <- 17
      }
      
      ANOVA_int <- rbind(ANOVA_int, x)
      
    } else if(musicData$Subject[i] == musicData$Subject[bt] & bt == i) {
      
      x <- data.frame(Trial = musicData$trial[i], 
                      BallDist = musicData$BallDist[i], 
                      Segment = musicData$segment[i], 
                      Subject = musicData$Subject[i],
                      Length = musicData$duration_ms[i])
      
      if(x$Length[1] <= 0) {
        x$Length[1] <- 17
      }
      
      ANOVA_int <- rbind(ANOVA_int, x)
      
    }
    print(i)
  }
}

ANOVA_int <- ANOVA_int[2:nrow(ANOVA_int), ]

# Remove all rows where segments are shorter than 500ms
ANOVA_int <- ANOVA_int[ANOVA_int$Length >= 500, ]

#### FOR FIRST ANALYSIS ####

ANOVA_1 <- data.frame(Trial = 1, BallDist = 2, 
                      Segment = 'Play', Subject = 4, 
                      Order = 'First')
ANOVA_1$Segment <- factor(ANOVA_1$Segment, levels = 
                            c('Play', 'Listen'),
                          labels = c('Play', 'Listen'))
ANOVA_1$Order <- factor(ANOVA_1$Order, levels = 
                          c('First', 'Last'),
                        labels = c('First', 'Last'))


# Loop iterating through musicData, picking out relevant values
for(i in 2:(nrow(musicData) - 1)) {
  
  # Create a variable noting when each trial begins
  trialStart <- 1
  
  # If trial is not same as previous row value, set current row to trialStart
  if(musicData$trial[i] != musicData$trial[i - 1]) {
    trialStart <- i
  }
  
  # If trial > 3 & segments are 'Listen' or 'Play' &
  # the current segment is different from the next (meaning we're at the final row of a segment) &
  # the current row is 2000ms after trial start
  if(musicData$trial[i] > 3 & 
     musicData$segment[i] %in% c('Listen', 'Play') &
     musicData$segment[i] != musicData$segment[i + 1] &
     musicData$BallMillis[i] - musicData$BallMillis[trialStart] > 2000) {
    
    # create new variable with current row number
    bt <- i
    
    # decrease bt by 1 until segment at row bt is not the same as at row i
    while(musicData$segment[i] == musicData$segment[bt] & bt > 1) {
      bt <- bt - 1
    }
    
    # If the time between i and bt is more than 1000, segment can be used
    if(musicData$BallMillis[i] - musicData$BallMillis[bt] >= 1000) {
      
      # lt is last time stamp, for last part of segment
      lt <- i
      
      # while time between i (start of segment) and lt is less than 480, go back one row
      # when time is 480, we have the last 480ms of the segment
      while(musicData$BallMillis[i] - musicData$BallMillis[lt] < 480 & lt > 1) {
        lt <- lt - 1
      }
      
      # ft is first time stamp, for first part of segment
      ft <- bt
      
      # while time between bt (beginning of segment) and ft is less than 480, increase row by 1
      # when time is 480, we have first 480 ms of segment
      while(musicData$BallMillis[ft] - musicData$BallMillis[bt] < 480 & ft > 1) {
        ft <- ft + 1
      }
      
      # Create new data frame with the FIRST part of segment (mean BallDist from bt to ft)
      x <- data.frame(Trial = musicData$trial[i], BallDist = mean(musicData$BallDist[bt:ft]),
                      Segment = musicData$segment[i], Subject = musicData$Subject[i],
                      Order = 'First')
      x$Segment <- factor(x$Segment, levels = 
                            c('Play', 'Listen'),
                          labels = c('Play', 'Listen'))
      x$Order <- factor(x$Order, levels = 
                          c('First', 'Last'),
                        labels = c('First', 'Last'))
      
      ANOVA_1 <- rbind(ANOVA_1, x)
      
      # Create new data frame with the LAST part of segment (mean BallDist from lt to i)
      x <- data.frame(Trial = musicData$trial[i], BallDist = mean(musicData$BallDist[lt:i]),
                      Segment = musicData$segment[i], Subject = musicData$Subject[i],
                      Order = 'Last')
      x$Segment <- factor(x$Segment, levels = 
                            c('Play', 'Listen'),
                          labels = c('Play', 'Listen'))
      x$Order <- factor(x$Order, levels = 
                          c('First', 'Last'),
                        labels = c('First', 'Last'))
      ANOVA_1 <- rbind(ANOVA_1, x)
      
    }
    print(i)
  }
}

# Remove first row
ANOVA_1 <- ANOVA_1[2:nrow(ANOVA_1), ]
colnames(ANOVA_1) <- c('Trial', 'BallDist', 'Segment', 'Subject', 'Order')

# Subtract 3 from all trial values in data frame so that trial 4 is now trial 1
# Because trial 4 is first trial we analyze
ANOVA_1$Trial <- ANOVA_1$Trial - 3



#### FOR SECOND ANALYSIS ####

ANOVA_2 <- ANOVA_1[ANOVA_1$Order == 'Last', 1:4]
ANOVA_2$Segment <- factor(ANOVA_2$Segment, levels = 
                            c('Play', 'Listen', 'Prepare', 'Wait'),
                          labels = c('Play', 'Listen', 'Prepare', 'Wait'))

# Add 3 to trial column, to match with musicData
ANOVA_2$Trial <- ANOVA_2$Trial + 3

# Loop iterating through musicData, picking out relevant values
for(i in 2:(nrow(musicData) - 1)) {
  
  # Create a variable noting when each trial begins
  trialStart <- 1
  
  # If trial is not same as previous row value, set current row to trialStart
  if(musicData$trial[i] != musicData$trial[i - 1]) {
    trialStart <- i
  }
  
  # If trial > 3 & segments are 'Prepare' &
  # the current segment is different from the next (meaning we're at the final row of a segment) &
  # the current row is 2000ms after trial start
  if(musicData$trial[i] > 3 & 
     musicData$segment[i] %in% c('Prepare', 'Wait') &
     musicData$segment[i] != musicData$segment[i + 1] &
     musicData$BallMillis[i] - musicData$BallMillis[trialStart] > 2000) {
    
    # Back track to last key press before i
    bt <- i
    while(musicData$segment[i] == musicData$segment[bt] & bt > 1) {
      bt <- bt - 1
    }
    bt <- bt + 1
    
    if(musicData$segment[i] == musicData$segment[bt] &
       musicData$BallMillis[i] - musicData$BallMillis[bt] >= 500) {
      
      
      # Make new data frame with prepare/wait segment
      x <- data.frame(Trial = musicData$trial[i], BallDist = mean(musicData$BallDist[bt:i]),
                      Segment = musicData$segment[i], Subject = musicData$Subject[i])
      x$Segment <- factor(x$Segment, levels = 
                            c('Play', 'Listen', 'Prepare', 'Wait'),
                          labels = c('Play', 'Listen', 'Prepare', 'Wait'))
      ANOVA_2 <- rbind(ANOVA_2, x)
    }
  }
  print(i)
}

colnames(ANOVA_2) <- c('Trial', 'BallDist', 'Segment', 'Subject')

# Subtract 3 from trial so that trial 4 is now first trial
ANOVA_2$Trial <- ANOVA_2$Trial - 3


#### ANOVA 4 Create ####

ANOVA_4 <- ANOVA_1[(ANOVA_1$Segment == 'Play' & ANOVA_1$Order == 'First') | 
                     (ANOVA_1$Segment == 'Listen' & ANOVA_1$Order == 'Last'),]

ANOVA_4$Segment <- factor(ANOVA_4$Segment, 
                          levels = c('Listen', 'Play',
                                     'Prepare', 'BegPlay', 'EndListen'), 
                          labels = c('Listen', 'Play',
                                     'Prepare', 'BegPlay', 'EndListen'))

ANOVA_4$Segment[ANOVA_4$Segment == 'Play' & 
                  ANOVA_4$Order == 'First'] <- 'BegPlay'

ANOVA_4$Segment[ANOVA_4$Segment == 'Listen' & 
                  ANOVA_4$Order == 'Last'] <- 'EndListen'

ANOVA_4 <- ANOVA_4[,1:4]

ANOVA_4 <- rbind(ANOVA_4, ANOVA_2[ANOVA_2$Segment == 'Prepare',])

ANOVA_4$Segment <- droplevels(ANOVA_4$Segment)


#### ANOVA 5 Create ####

ANOVA_5 <- ANOVA_1[(ANOVA_1$Segment == 'Play' & ANOVA_1$Order == 'Last') | 
                     (ANOVA_1$Segment == 'Listen' & ANOVA_1$Order == 'First'),]

ANOVA_5$Segment <- factor(ANOVA_5$Segment, 
                          levels = c('Listen', 'Play',
                                     'Wait', 'EndPlay', 'BegListen'), 
                          labels = c('Listen', 'Play',
                                     'Wait', 'EndPlay', 'BegListen'))

ANOVA_5$Segment[ANOVA_5$Segment == 'Play' & 
                  ANOVA_5$Order == 'Last'] <- 'EndPlay'

ANOVA_5$Segment[ANOVA_5$Segment == 'Listen' & 
                  ANOVA_5$Order == 'First'] <- 'BegListen'

ANOVA_5 <- ANOVA_5[,1:4]

ANOVA_5 <- rbind(ANOVA_5, ANOVA_2[ANOVA_2$Segment == 'Wait',])

ANOVA_5$Segment <- droplevels(ANOVA_5$Segment)

#### ANOVA 6 ####

ANOVA_6 <- ANOVA_1[(ANOVA_1$Segment == 'Listen' & ANOVA_1$Order == 'Last') | 
                     (ANOVA_1$Segment == 'Listen' & ANOVA_1$Order == 'First'),]

ANOVA_6$Segment <- factor(ANOVA_6$Segment, 
                          levels = c('Listen', 'Play',
                                     'EndListen', 'BegListen'), 
                          labels = c('Listen', 'Play',
                                     'EndListen', 'BegListen'))

ANOVA_6$Segment[ANOVA_6$Segment == 'Listen' & 
                  ANOVA_6$Order == 'Last'] <- 'EndListen'

ANOVA_6$Segment[ANOVA_6$Segment == 'Listen' & 
                  ANOVA_6$Order == 'First'] <- 'BegListen'

ANOVA_6 <- ANOVA_6[,1:4]

ANOVA_6$Segment <- droplevels(ANOVA_6$Segment)

ANOVA_6 = ANOVA_6[ANOVA_6$BallDist > 0, ]

ANOVA_6$log_ball <- log(ANOVA_6$BallDist)

#### End of listen ANOVA Sep ####

ANOVA_7 <- data.frame(Trial = 1, BallDist = 2, 
                      Segment = 'Listen', Bin = 1, Subject = 4)
ANOVA_7$Segment <- factor(ANOVA_7$Segment, levels = c('Listen'), 
                          labels = c('Listen'))
ANOVA_7$Bin <- factor(ANOVA_7$Bin, levels = c(1, 2, 3, 4),
                      labels = c('0-120', '121-240', '241-360', '361-480'))

Has4 <- 0
Not4 <- 0

# Loop iterating through musicData, picking out relevant values
for(i in 2:(nrow(musicData) - 1)) {
  
  # Create a variable for row where trial starts
  trialStart <- 1
  
  # If trial is not same as previous row value, set current row to trialStart
  if(musicData$trial[i] != musicData$trial[i - 1]) {
    trialStart <- i
  }
  
  # If trial > 3 & segments are 'Listen' &
  # the current segment is different from the next (meaning we're at the final row of a segment) &
  # the current row is 2000ms after trial start
  if(musicData$trial[i] > 3 & 
     musicData$segment[i] == 'Listen' &
     musicData$segment[i] != musicData$segment[i + 1] &
     musicData$BallMillis[i] - musicData$BallMillis[trialStart] > 2000) {
    
    # Back track to last key press before i
    bt <- i
    while(musicData$segment[i] == musicData$segment[bt] & bt > 1) {
      bt <- bt - 1
    }
    bt <- bt + 1
    
    # If the time between i and bt is more than 1000, segment can be used
    if(musicData$BallMillis[i] - musicData$BallMillis[bt] > 1000) {
      
      # lt is last time stamp, for last part of segment
      lt <- i
      
      # while time between i (start of segment) and lt is less than 480, go back one row
      # when time is 480, we have the last 480ms of the segment
      while(musicData$BallMillis[i] - musicData$BallMillis[lt] < 480 & lt > 1) {
        lt <- lt - 1
      }
      
      # if final 480ms exist over 4 observations, split into 4 observations and save
      if(i - lt >= 4) {
        
        Has4 <- Has4 + 1
        
        # Get number of rows per bin
        bin_val <- (i - lt)/4
        
        # Create new data frame with the LAST part of segment (mean BallDist from lt to i)
        # Split into 4 bins
        x <- data.frame(Trial = rep(musicData$trial[i], 4), 
                        BallDist = c(mean(musicData$BallDist[lt:(lt + round(bin_val, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val, 0) + 1):(lt + round(bin_val*2, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*2, 0) + 1):(lt + round(bin_val*3, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*3, 0) + 1):i])),
                        Segment = rep(musicData$segment[i], 4), 
                        Bin = c(1, 2, 3, 4),
                        Subject = rep(musicData$Subject[i], 4))
        x$Segment <- factor(x$Segment, levels = 
                              c('Listen'),
                            labels = c('Listen'))
        x$Bin <- factor(x$Bin, levels = c(1, 2, 3, 4),
                        labels = c('0-120', '121-240', '241-360', '361-480'))
        ANOVA_7 <- rbind(ANOVA_7, x)
      } else if(i - lt < 4) {
        Not4 <- Not4 + 1
      }
    }
    print(i)
  }
}

colnames(ANOVA_7) <- c('Trial', 'BallDist', 'Segment', 'Bin', 'Subject')

ANOVA_7 <- ANOVA_7[2:nrow(ANOVA_7), ]

# Subtract 3 from trial so that trial 4 is now first trial
ANOVA_7$Trial <- ANOVA_7$Trial - 3

Has4
Not4
Has4/(Not4 + Has4)

#### Pre-play ANOVA Sep ####


ANOVA_8 <- data.frame(Trial = 1, BallDist = 2, 
                      Segment = 'Prepare', Bin = 1, Subject = 4)
ANOVA_8$Segment <- factor(ANOVA_8$Segment, levels = c('Prepare'), 
                          labels = c('Prepare'))
ANOVA_8$Bin <- factor(ANOVA_8$Bin, levels = c(1, 2, 3, 4),
                      labels = c('0-120', '121-240', '241-360', '361-480'))

PrepareMore480 <- 0
PrepareLess480 <- 0
Has4 <- 0
Not4 <- 0

# Loop iterating through musicData, picking out relevant values
for(i in 2:(nrow(musicData) - 1)) {
  
  # Create a variable for row where trial starts
  trialStart <- 1
  
  # If trial is not same as previous row value, set current row to trialStart
  if(musicData$trial[i] != musicData$trial[i - 1]) {
    trialStart <- i
  }
  
  # If trial > 3 & segments are 'Prepare' &
  # the current segment is different from the next (meaning we're at the final row of a segment) &
  # the current row is 2000ms after trial start
  if(musicData$trial[i] > 3 & 
     musicData$segment[i] == 'Prepare' &
     musicData$segment[i] != musicData$segment[i + 1] &
     musicData$BallMillis[i] - musicData$BallMillis[trialStart] > 2000) {
    
    # Back track to last key press before i
    bt <- i
    while(musicData$segment[i] == musicData$segment[bt] & bt > 1) {
      bt <- bt - 1
    }
    bt <- bt + 1
    
    # If the time between i and bt is more than 1000, segment can be used
    if(musicData$BallMillis[i] - musicData$BallMillis[bt] >= 500) {
      
      PrepareMore480 <- PrepareMore480 + 1
      
      # ft is first time stamp, for first part of segment
      ft <- bt
      
      # while time between bt (beginning of segment) and ft is less than 480, increase row by 1
      # when time is 480, we have first 480 ms of segment
      while(musicData$BallMillis[ft] - musicData$BallMillis[bt] < 480 & ft > 1) {
        ft <- ft + 1
      }
      
      if(ft - bt >= 4) {
        
        Has4 <- Has4 + 1
        
        # Get number of rows per bin
        bin_val <- (ft - bt)/4
        
        # Create new data frame with the LAST part of segment (mean BallDist from lt to i)
        # Split into 4 bins
        x <- data.frame(Trial = rep(musicData$trial[i], 4), 
                        BallDist = c(mean(musicData$BallDist[bt:(bt + round(bin_val, 0))]),
                                     mean(musicData$BallDist[(bt + round(bin_val, 0) + 1):(bt + round(bin_val*2, 0))]),
                                     mean(musicData$BallDist[(bt + round(bin_val*2, 0) + 1):(bt + round(bin_val*3, 0))]),
                                     mean(musicData$BallDist[(bt + round(bin_val*3, 0) + 1):ft])),
                        Segment = rep(musicData$segment[i], 4), 
                        Bin = c(1, 2, 3, 4),
                        Subject = rep(musicData$Subject[i], 4))
        x$Segment <- factor(x$Segment, levels = c('Prepare'),
                            labels = c('Prepare'))
        x$Bin <- factor(x$Bin, levels = c(1, 2, 3, 4),
                        labels = c('0-120', '121-240', '241-360', '361-480'))
        ANOVA_8 <- rbind(ANOVA_8, x)
        
      } else if(ft - bt < 4) {
        Not4 <- Not4 + 1
      }
    } else if(musicData$BallMillis[i] - musicData$BallMillis[bt] < 480) {
      PrepareLess480 <- PrepareLess480 + 1
    }
    print(i)
  }
}

colnames(ANOVA_8) <- c('Trial', 'BallDist', 'Segment', 'Bin', 'Subject')

ANOVA_8 <- ANOVA_8[2:nrow(ANOVA_8), ]

# Subtract 3 from trial so that trial 4 is now first trial
ANOVA_8$Trial <- ANOVA_8$Trial - 3

Has4
Not4
Has4/(Not4 + Has4)

PrepareMore480
PrepareLess480
PrepareMore480/(PrepareMore480 + PrepareLess480)

#### Play Bin Separation ####

ANOVA_9 <- data.frame(Trial = 1, BallDist = 2, 
                      Segment = 'Listen', Bin = 1, Subject = 4)
ANOVA_9$Segment <- factor(ANOVA_9$Segment, levels = c('Listen'), 
                          labels = c('Listen'))
ANOVA_9$Bin <- factor(ANOVA_9$Bin, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                      labels = c('0-120', '121-240', '241-360', '361-480', '481-600',
                                 '601-720', '721-840', '841-960', '961-1080',
                                 '1081-1200', '1201-1320', '1321-1440'))

ListenMore1440 <- 0
ListenLess1440 <- 0
Has12 <- 0
Not12 <- 0

# Loop iterating through musicData, picking out relevant values
for(i in 2:(nrow(musicData) - 1)) {
  
  # Create a variable for row where trial starts
  trialStart <- 1
  
  # If trial is not same as previous row value, set current row to trialStart
  if(musicData$trial[i] != musicData$trial[i - 1]) {
    trialStart <- i
  }
  
  # If trial > 3 & segments are 'Listen' &
  # the current segment is different from the next (meaning we're at the final row of a segment) &
  # the current row is 2000ms after trial start
  if(musicData$trial[i] > 3 & 
     musicData$segment[i] == 'Listen' &
     musicData$segment[i] != musicData$segment[i + 1] &
     musicData$BallMillis[i] - musicData$BallMillis[trialStart] > 2000) {
    
    # Back track to last key press before i
    bt <- i
    while(musicData$segment[i] == musicData$segment[bt] & bt > 1) {
      bt <- bt - 1
    }
    bt <- bt + 1
    
    # If the time between i and bt is more than 1000, segment can be used
    if(musicData$BallMillis[i] - musicData$BallMillis[bt] >= 1440) {
      
      ListenMore1440 <- ListenMore1440 + 1
      
      # lt is last time stamp, for last part of segment
      lt <- i
      
      # while time between i (start of segment) and lt is less than 480, go back one row
      # when time is 480, we have the last 480ms of the segment
      while(musicData$BallMillis[i] - musicData$BallMillis[lt] < 1440 & lt > 1) {
        lt <- lt - 1
      }
      
      if(i - lt >= 12) {
        
        Has12 <- Has12 + 1
        
        # Get number of rows per bin
        bin_val <- (i - lt)/12
        
        # Create new data frame with the LAST part of segment (mean BallDist from lt to i)
        # Split into 4 bins
        x <- data.frame(Trial = rep(musicData$trial[i], 12), 
                        BallDist = c(mean(musicData$BallDist[lt:(lt + round(bin_val, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val, 0) + 1):(lt + round(bin_val*2, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*2, 0) + 1):(lt + round(bin_val*3, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*3, 0) + 1):(lt + round(bin_val*4, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*4, 0) + 1):(lt + round(bin_val*5, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*5, 0) + 1):(lt + round(bin_val*6, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*6, 0) + 1):(lt + round(bin_val*7, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*7, 0) + 1):(lt + round(bin_val*8, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*8, 0) + 1):(lt + round(bin_val*9, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*9, 0) + 1):(lt + round(bin_val*10, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*10, 0) + 1):(lt + round(bin_val*11, 0))]),
                                     mean(musicData$BallDist[(lt + round(bin_val*11, 0) + 1):i])),
                        Segment = rep(musicData$segment[i], 12), 
                        Bin = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                        Subject = rep(musicData$Subject[i], 12))
        x$Segment <- factor(x$Segment, levels = 
                              c('Listen'),
                            labels = c('Listen'))
        x$Bin <- factor(x$Bin, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                        labels = c('0-120', '121-240', '241-360', '361-480', '481-600',
                                   '601-720', '721-840', '841-960', '961-1080',
                                   '1081-1200', '1201-1320', '1321-1440'))
        ANOVA_9 <- rbind(ANOVA_9, x)
        
      } else if(i - lt < 12) {
        Not12 <- Not12 + 1
      }
    } else if(musicData$BallMillis[i] - musicData$BallMillis[bt] < 1440) {
      ListenLess1440 <- ListenLess1440 + 1
    }
    print(i)
  }
}

colnames(ANOVA_9) <- c('Trial', 'BallDist', 'Segment', 'Bin', 'Subject')

ANOVA_9 <- ANOVA_9[2:nrow(ANOVA_9), ]

# Subtract 3 from trial so that trial 4 is now first trial
ANOVA_9$Trial <- ANOVA_9$Trial - 3

Has12
Not12
Has12/(Has12 + Not12)

ListenMore1440
ListenLess1440
ListenMore1440/(ListenMore1440 + ListenLess1440)
