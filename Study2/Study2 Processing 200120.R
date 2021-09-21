
## SCRIPT FOR PREPROCESSING DATA ##

library(stringr)

# Read in all data frames in turn and put A data in A-list and B data in B-list
setwd('C:/Users/samuel/Google Drive/Original Data Duets')
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)


list_a <- list()
list_b <- list()
avail_data = c()
for(i in 1:length(temp)) {
  x <- myfiles[[i]]
  name <- sub('*.csv', '', temp[i])
  assign(name, x)
  if(grepl('A', name) & length(unique(myfiles[[i]]$channel)) == 3) {
    list_a[[length(list_a) + 1]] <- x
    avail_data[length(avail_data) + 1] = name
  } else if(grepl('B', name) & length(unique(myfiles[[i]]$channel)) == 3) {
    x_1 = which(x$channel == 1)
    x_2 = which(x$channel == 2)
    x$channel[x_1] = 2
    x$channel[x_2] = 1
    list_b[[length(list_b) + 1]] <- x
  }
}

# Reset WD
setwd('C:/Users/samuel/Documents/Nina Projects')

# Make df with melody matches
counterbalance = readxl::read_xlsx('C:/Users/samuel/Google Drive/Uni/Year 4/VRA Edinburgh/Counterbalancing.xlsx')
counterbalance = counterbalance[1:24, ]

splitter = function(x) {
  x = str_split(x, 'S2')
  x = x[[1]][2]
  if(substring(x, 1, 1) == '0') {
    x = substring(x, 2)
  }
  return(x)
}

p_list = lapply(avail_data, splitter)

counterbalance$Include = NA

for(i in 1:nrow(counterbalance)) {
  for(item in p_list) {
    if(counterbalance$`Participant 1 from pair`[i] == tolower(item)) {
      counterbalance$Include[i] = 1
    }
  }
}

counterbalance = na.omit(counterbalance)

mel_frame = data.frame(Participant = 1:28,
                       Melody_1 = rep(NA, 28),
                       Melody_2 = rep(NA, 28),
                       Melody_3 = rep(NA, 28),
                       Melody_4 = rep(NA, 28))

j = 1
for(i in 1:nrow(counterbalance)) {
  
  stim_list = list()
  for(k in 3:6) {
    stim = str_split(counterbalance[i, k], 'starts, ')
    stim_list[length(stim_list) + 1] = str_split(stim[[1]][2], ', ', 2)
  }

  for(k in 1:4) {
    if(grepl('u', stim_list[[k]][[1]])) {
      mel_frame[j, k + 1] = 'Unfamiliar'
    } else if(grepl('f', stim_list[[k]][[1]])){
      mel_frame[j, k + 1] = 'Familiar'
    }
  }
  j = j + 1
  for(k in 1:4) {
    if(grepl('u', stim_list[[k]][[2]])) {
      mel_frame[j, k + 1] = 'Unfamiliar'
    } else if(grepl('f', stim_list[[k]][[2]])){
      mel_frame[j, k + 1] = 'Familiar'
    }
  }
  j = j + 1
}

mel_frame = data.frame(Participant = rep(1:28, 4),
                       current_state = rep(c('MELODY_1', 'MELODY_2', 'MELODY_3', 'MELODY_4'), each = 28),
                       Familiar = c(mel_frame$Melody_1, mel_frame$Melody_2,
                                    mel_frame$Melody_3, mel_frame$Melody_4))
mel_frame$current_state = as.character(mel_frame$current_state)
mel_frame$Familiar = as.character(mel_frame$Familiar)

# Segmentation functions that take a participant's data as input
# and outputs a new data frame with events in a 'segment' column
SegmentPA_duet <- function(data) {
  
  start <- Sys.time()
  
  # order data by BallMillis (time),
  # create new columns for rows and segments
  data <- na.omit(data) # remove possible NAs
  vars = names(data) %in% c('trial', 'midi_pitch', 'start_ms', 'duration_ms',
                            'velocity', 'channel', 'BallDist', 'BallMillis', 
                            'current_state')
  data = data[vars]
  data <- data[order(data$BallMillis), ] # order by BallMillis
  data$BallMillis <- data$BallMillis - data$BallMillis[1]
  data <- cbind(1:nrow(data), data) # add column with row numbers
  data$Familiarity <- rep(NA, nrow(data))
  data$Familiarity <- factor(data$Familiarity, levels = c('Familiar', 'Unfamiliar'), 
                         labels = c('Familiar', 'Unfamiliar'))
  data$segment <- rep('None', nrow(data)) # add column for segments
  data$segment <- factor(data$segment, levels = c('None', 'Control', 'Listen', 'Play',
                                                  'Prepare', 'Pause', 'Wait', 'Overlap'), 
                         labels = c('None', 'Control', 'Listen', 'Play',
                                    'Prepare', 'Pause', 'Wait', 'Overlap'))
  colnames(data) <- c('X', 'trial', 'midi_pitch', 'start_ms', 'duration_ms',
                      'velocity', 'channel', 'BallDist', 'BallMillis', 
                      'current_state', 'Familiarity', 'segment')
  
  # Run through data and add trial numbers where 0's where before
  y <- data
  noZero <- data[-which(data$channel == 0), ]
  noZeroAgg <- unique(noZero$trial)
  for(i in noZeroAgg) {
    x <- data[which(data$trial == i), ]
    y$trial[x[1, 1]:nrow(y)] <- i
  }
  data$trial <- y$trial
  
  # Add familiarity to trials 4 and above
  y <- data
  noZeroAgg <- unique(y$trial)
  fourth <- which(noZeroAgg == 4)
  noZeroAgg <- noZeroAgg[fourth:length(noZeroAgg)]
  for(i in noZeroAgg) {
    x <- data[which(data$trial == i), ]
    y$Familiarity[x[1, 1]:nrow(y)] <- mel_frame$Familiar[mel_frame$current_state == x$current_state[1] &
                                                           mel_frame$Participant == s] 
  }
  data$Familiarity <- y$Familiarity
  
  # Put in Listen, Play
  # Listen and Play labels are entered only during key presses
  for(i in which(data$channel != 0 & data$trial != 2)) {
    if(data$channel[i] == 1) {
      data$segment[i] <- 'Play'
    } else if(data$channel[i] == 2) {
      data$segment[i] <- 'Listen'
    }
  }
  
  data$segment[data$trial == 2] <- 'Control'
  
  # Get the row number for key presses
  k <- which(data$channel != 0 & data$trial != 2)
  
  # Create ov_offset = 1, as used in if statement
  ov_offset <- 1
  
  # Loop that iterates through data frame
  for(i in k) {
    
    ## For Pauses, Prepare, and Play ##
    if(i >= 2) {
      
      # For all key presses, go back to previous key press (while loop)
      bt3 <- i - 1
      while(data$channel[bt3] == 0 & data$trial[bt3] == data$trial[i] & 
            bt3 > 1) {
        bt3 <- bt3 - 1
      }
      bt3 <- bt3 + 1
      
      # Create new variable to adjust for overlaps
      if(data$segment[bt3-1] == 'Overlap') {
        ov_offset <- bt3 - 1
        
        while(data$segment[ov_offset] == 'Overlap' & ov_offset >= 1) {
          ov_offset <- ov_offset + 1
        }
      }
      
      # If own channel & previous key press is also own channel &
      # time inbetween is <= 1000ms, rows inbetween get 'Play'
      if(data$channel[i] == 1 & data$channel[bt3-1] == data$channel[i] & 
         data$BallMillis[i] - (data$BallMillis[bt3-1] + data$duration_ms[bt3-1]) <= 1000 & 
         data$trial[bt3-1] == data$trial[i] &
         i - bt3 > 1 & data$duration_ms[bt3-1] >= 0) {
        
        # i is after overlap, insert rows, else move on to next key press
        if(i-(ov_offset+1) > 0) {
          if(data$segment[bt3-1] == 'Overlap') {
            data$segment[(ov_offset+1):(i-1)] <- rep('Play', i-(ov_offset+1))
          } else {
            data$segment[bt3:i] <- rep('Play', i-(bt3-1))
          }
        }
        
        # If i and bt3-1 are other channel, 'Listen'
      } else if(data$channel[i] == 2 & data$channel[bt3-1] == data$channel[i] &
                data$trial[bt3-1] == data$trial[i] &
                i - bt3 > 1 & data$duration_ms[bt3-1] >= 0) {
        
        # i is after overlap, insert rows, else move on to next key press
        if(i-(ov_offset+1) > 0) {
          if(data$segment[bt3-1] == 'Overlap') {
            data$segment[(ov_offset+1):(i-1)] <- rep('Listen', i-(ov_offset+1))
          } else {
            data$segment[bt3:i] <- rep('Listen', i-(bt3-1))
          }
        }
        
        # Same as above but time in between > 1000ms
        # Insert 'Pause' instead
        # Only for play segments
        # Control for note offset
      } else if(data$channel[bt3-1] == data$channel[i] & data$channel[i] == 1 &
                data$BallMillis[i] - (data$BallMillis[bt3-1] + data$duration_ms[bt3-1]) > 1000 & 
                i - bt3 > 1 & data$trial[bt3-1] == data$trial[i] & 
                data$duration_ms[bt3-1] >= 0) {
        
        # Create new variable, note_offset
        note_offset <- bt3 - 1
        
        # If time between BallMillis at note_offset and bt3-1 (last key press) is
        # less than duration_ms, note_offset increases by one until we find the row
        # where the note key is released
        while(data$BallMillis[note_offset] - data$BallMillis[bt3 - 1] <= data$duration_ms[bt3 - 1] & note_offset >= 1) {
          note_offset <- note_offset + 1
        }
        
        # if previous press isn't overlap
        if(i-(ov_offset+1) > 0 & data$segment[bt3-1] != 'Overlap') {
          # If note_offset is after the next key press
          # All rows until next key press are 'Play'
          if(note_offset >= i) {
            data$segment[bt3:(i-1)] <- rep('Play', i-bt3)
            
            # If note_offset is last key press, all between is pause
          } else if(note_offset == bt3 | note_offset == bt3 - 1) {
            data$segment[bt3:(i-1)] <- rep('Pause', i-bt3)
            
            # Else, play until note_offset, then pause
          } else if(note_offset > bt3 & note_offset < i){
            data$segment[bt3:(note_offset-1)] <- rep('Play', note_offset-bt3)
            data$segment[note_offset:(i-1)] <- rep('Pause', i-note_offset)
          }
          
          # Next, if previous press is overlap
        } else if(i-(ov_offset+1) > 0 & data$segment[bt3-1] == 'Overlap') {
          
          # If note_offset is after overlap finishes
          if(note_offset - ov_offset > 0) {
            # If note_offset is after the next key press
            # All rows from ov_offset are 'Play
            if(note_offset >= i) {
              data$segment[ov_offset:(i-1)] <- rep('Play', i-ov_offset)
              
              # If note_offset is last key press, all between is pause
            } else if(note_offset == bt3 | note_offset == bt3 - 1) {
              data$segment[bt3:(i-1)] <- rep('Pause', i-bt3)
              
              # Else, play from ov_offset to note_offset, then pause
            } else if(note_offset > bt3 & note_offset < i){
              data$segment[ov_offset:(note_offset-1)] <- rep('Play', note_offset-ov_offset)
              data$segment[note_offset:(i-1)] <- rep('Pause', i-note_offset)
            }
            
            # if ov_offset is after note_offset, make note-offset to i, 'Pause'
          } else if(note_offset - ov_offset <= 0) {
            data$segment[note_offset:(i-1)] <- rep('Pause', i-note_offset)
          }
        }
        
        # If current press is own channel, and previous other, prepare
      } else if(data$channel[i] == 1 & data$channel[bt3-1] == 2 &
                data$trial[bt3-1] == data$trial[i] &
                i - bt3 > 1 & data$duration_ms[bt3-1] >= 0) {
        
        # Create new variable, note_offset
        note_offset <- bt3 - 1
        
        # If time between BallMillis at note_offset and bt3-1 (last key press) is
        # less than duration_ms, note_offset increases by one until we find the row
        # where the note key is released
        while(data$BallMillis[note_offset] - data$BallMillis[bt3 - 1] <= data$duration_ms[bt3 - 1] & note_offset > 1) {
          note_offset <- note_offset + 1
        }
        
        # if previous press is not overlap, do like normal
        if(i-(ov_offset+1) > 0 & data$segment[bt3-1] != 'Overlap') {
          
          # if note_offset is after i, listen until i, then overlap
          if(note_offset > i) {
            data$segment[bt3:(i-1)] <- rep('Listen', i-bt3)
            data$segment[i:note_offset] <- rep('Overlap', note_offset-(i-1))
            
            # If note_offset IS the next key press
            # Time until next key press is listen and then overlap is next key press
          } else if(note_offset == i) {
            data$segment[bt3:(i-1)] <- rep('Listen', i-bt3)
            data$segment[i] <- 'Overlap'
            
            # Else, listen until note_offset, then prepare
          } else if(note_offset > bt3 & note_offset < i){
            data$segment[bt3:(note_offset-1)] <- rep('Listen', note_offset-bt3)
            data$segment[note_offset:(i-1)] <- rep('Prepare', i-note_offset)
          }
          
          # if previous press is overlap
        } else if(i-(ov_offset+1) > 0 & data$segment[bt3-1] == 'Overlap') {
          
          # If note_offset is after overlap finishes
          if(note_offset - ov_offset > 0) {
            
            # If note_offset is after the next key press
            # Time from ov_offset to i is 'Listen', then overlap
            if(note_offset > i) {
              data$segment[ov_offset:(i-1)] <- rep('Listen', i-ov_offset)
              data$segment[i:note_offset] <- rep('Overlap', note_offset-(i-1))
              
              # If note_offset IS the next key press
              # Time until next key press is listen and then overlap is next key press
            } else if(note_offset == i) {
              data$segment[bt3:(i-1)] <- rep('Listen', i-bt3)
              data$segment[i] <- 'Overlap'
              
              # Else, listen between ov_offset to note_offset, then 'Prepare'
            } else if(note_offset > bt3 & note_offset < i){
              data$segment[ov_offset:(note_offset-1)] <- rep('Listen', note_offset-ov_offset)
              data$segment[note_offset:(i-1)] <- rep('Prepare', i-note_offset)
            }
            
            # if note_offset is before overlap, 'Prepare' from note_offset to i
          } else if(note_offset - ov_offset <= 0) {
            data$segment[note_offset:(i-1)] <- rep('Prepare', i-note_offset)
          }
        }
        
        # If own channel, but Last press was other channel, wait
      } else if(data$channel[i] == 2 & data$channel[bt3-1] == 1 &
                data$trial[bt3-1] == data$trial[i] &
                i - bt3 > 1 & data$duration_ms[bt3-1] >= 0) {
        
        # Create new variable, note_offset
        note_offset <- bt3 - 1
        
        # If time between BallMillis at note_offset and bt3-1 (last key press) is
        # less than duration_ms, note_offset increases by one until we find the row
        # where the note key is released
        while(data$BallMillis[note_offset] - data$BallMillis[bt3 - 1] <= data$duration_ms[bt3 - 1] & note_offset > 1) {
          note_offset <- note_offset + 1
        }
        
        # BIG START
        # if previous press is not overlap, do like normal
        if(i-(ov_offset+1) > 0 & data$segment[bt3-1] != 'Overlap') {
          if(note_offset > i) {
            # if note_offset is after i, 'Play' until i, then overlap
            data$segment[bt3:(i-1)] <- rep('Play', i-bt3)
            data$segment[i:note_offset] <- rep('Overlap', note_offset-(i-1))
            
            # If note_offset IS the next key press
            # Time until next key press is 'Play', then i 'Overlap'
          } else if(note_offset == i) {
            data$segment[ov_offset:(i-1)] <- rep('Play', i-ov_offset)
            data$segment[i] <- 'Overlap'
            
            # Else, 'Play' until note_offset, then prepare
          } else if(note_offset > bt3 & note_offset < i){
            data$segment[bt3:(note_offset-1)] <- rep('Play', note_offset-bt3)
            data$segment[note_offset:(i-1)] <- rep('Wait', i-note_offset)
          }
          
          # if previous press is overlap
        } else if(i-(ov_offset+1) > 0 & data$segment[bt3-1] == 'Overlap') {
          # If note_offset is after overlap finishes
          if(note_offset - ov_offset > 0) {
            # If note_offset is after the next key press
            # Time until next key press is 'Play' and then overlap until note ends
            if(note_offset > i) {
              data$segment[ov_offset:(i-1)] <- rep('Play', i-ov_offset)
              data$segment[i:note_offset] <- rep('Overlap', note_offset-(i-1))
              
              # If note_offset IS the next key press
              # Time until next key press is 'Play' and then overlap is next key press
            } else if(note_offset == i) {
              data$segment[ov_offset:(i-1)] <- rep('Play', i-ov_offset)
              data$segment[i] <- 'Overlap'
              
              # Else, 'Play' until note_offset, then prepare
            } else if(note_offset > bt3 & note_offset < i){
              data$segment[ov_offset:(note_offset-1)] <- rep('Play', note_offset-ov_offset)
              data$segment[note_offset:(i-1)] <- rep('Wait', i-note_offset)
            }
            
            # if overlap ends after note_offset, 'Wait' from note_offset
          } else if(note_offset - ov_offset <= 0) {
            data$segment[note_offset:(i-1)] <- rep('Wait', i-note_offset)
          }
        }
      }
    }
  }
  
  # Look at the final key press and fill in the Last rows with
  # the same label until note offset (only if duration_ms is not negative)
  if(data$duration_ms[k[length(k)]] >= 0) {
    
    # Create new variable, note_offset
    note_offset <- k[length(k)] + 1
    
    while(data$BallMillis[note_offset] - data$BallMillis[k[length(k)]] <= data$duration_ms[k[length(k)]] &
          note_offset < nrow(data)) {
      note_offset <- note_offset + 1
    }
    data$segment[k[length(k)]:note_offset] <- rep(data$segment[k[length(k)]], note_offset - (k[length(k)]-1))
  }
  
  
  # Remove the first bit of any trial
  for(i in which(data$channel == 108 & data$trial != 2)) {
    
    # Go to next key press
    bt <- i + 1
    while(data$channel[bt] == 0 & data$trial[bt] == data$trial[i] & 
          bt > 1) {
      bt <- bt + 1
    }
    bt <- bt - 1
    
    data$segment[i:bt] <- rep('None', bt-i+1)
  }
  
  
  output <<- data
  end <- Sys.time()
  print(start)
  print(end)
}


# Make data frame with all participants
duetData <- cbind(1, S201A[1, ])
colnames(duetData)[1] <- 'X'
duetData$Familiarity <- rep(NA, nrow(duetData))
duetData$Familiarity <- factor(duetData$Familiarity, levels = c('Familiar', 'Unfamiliar'), 
                           labels = c('Familiar', 'Unfamiliar'))
duetData$segment <- 'None'
duetData$segment <- factor(duetData$segment, levels = c('None', 'Control', 'Listen', 'Play',
                                                          'Prepare', 'Pause', 'Wait', 'Overlap'), 
                            labels = c('None', 'Control', 'Listen', 'Play',
                                       'Prepare', 'Pause', 'Wait', 'Overlap'))
duetData$Subject <- 123

# Label all data frames and combine them
# This loop uses SegmantPA and SegmentPB to run through listA and listB
# We define the overlap window using overSec and the number of turns in an overlap segment with Overlap
# s defines the subject number, and is asigned to each subject before being added to duetData
s <- 1
for(i in 1:length(list_a)) {
  
  SegmentPA_duet(list_a[[i]])
  output$Subject <- rep(s, nrow(output))
  duetData <- rbind(duetData, output)
  print(s)
  s <- s + 1
  
  SegmentPA_duet(list_b[[i]])
  output$Subject <- rep(s, nrow(output))
  duetData <- rbind(duetData, output)
  print(s)
  s <- s + 1
}

# This removes the first, redundant, row of duetData
duetData <- duetData[2:nrow(duetData), ]
