
## SCRIPT FOR PREPROCESSING DATA ##

# Segmentation functions that take a participant's data as input
# and outputs a new data frame with events in a 'segment' column
SegmentPA <- function(data) {
  
  start <- Sys.time()
  
  # order data by BallMillis (time),
  # create new columns for rows and segments
  data <- na.omit(data) # remove possible NAs
  data <- data[order(data$BallMillis), ] # order by BallMillis
  data$segment <- rep('None', nrow(data)) # add column for segments
  data$segment <- factor(data$segment, levels = c('None', 'Control', 'Listen', 'Play',
                                                  'Prepare', 'Pause', 'Wait', 'Overlap'), 
                         labels = c('None', 'Control', 'Listen', 'Play',
                                    'Prepare', 'Pause', 'Wait', 'Overlap'))
  if(length(unique(data[, 1])) < 20) {
    data = cbind(1:nrow(data), data)
  } else {
    data$X = 1:nrow(data)
  }
  colnames(data) <- c('X', 'trial', 'midi_pitch', 'start_ms', 'duration_ms',
                      'velocity', 'channel', 'BallDist', 'BallMillis', 'segment')
  
  # Run through data and add trial numbers where 0's where before
  y <- data
  noZero <- data[-which(data$channel == 0), ]
  noZeroAgg <- unique(noZero$trial)
  for(i in noZeroAgg) {
    x <- data[which(data$trial == i), ]
    y$trial[x$X[1]:nrow(y)] <- i
  }
  data$trial <- y$trial
  
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
      
      # If i is own channel & previous key press is also own channel &
      # time between is <= 1000ms, rows between get 'Play'
      if(data$channel[i] == 1 & data$channel[bt3-1] == data$channel[i] & 
         data$BallMillis[i] - (data$BallMillis[bt3-1] + data$duration_ms[bt3-1]) <= 1000 & 
         data$trial[bt3-1] == data$trial[i] &
         i - bt3 > 1 & data$duration_ms[bt3-1] >= 0) {
        
        # if i is after overlap, insert rows, else move on to next key press
        if(i-(ov_offset+1) > 0) {
          if(data$segment[bt3-1] == 'Overlap') {
            data$segment[(ov_offset+1):(i-1)] <- rep('Play', i-(ov_offset+1))
          } else {
            data$segment[bt3:i] <- rep('Play', i-(bt3-1))
          }
        }
        
        # If i and bt3-1 are other channel, 'Listen'
      } else if(data$channel[i] == 2 & data$channel[bt3-1] == data$channel[i] &
                data$BallMillis[i] - (data$BallMillis[bt3-1] + data$duration_ms[bt3-1]) <= 1000 &
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
      } else if(data$channel[bt3-1] == data$channel[i] &
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
            data$segment[bt3:(i-1)] <- rep(data$segment[i], i-bt3)
            
            # If note_offset is last key press, all between is pause
          } else if(note_offset == bt3 | note_offset == bt3 - 1) {
            data$segment[bt3:(i-1)] <- rep('Pause', i-bt3)
            
            # Else, play until note_offset, then pause
          } else if(note_offset > bt3 & note_offset < i){
            data$segment[bt3:(note_offset-1)] <- rep(data$segment[i], note_offset-bt3)
            data$segment[note_offset:(i-1)] <- rep('Pause', i-note_offset)
          }
          
          # Next, if previous press is overlap
        } else if(i-(ov_offset+1) > 0 & data$segment[bt3-1] == 'Overlap') {
          
          # If note_offset is after overlap finishes
          if(note_offset - ov_offset > 0) {
            # If note_offset is after the next key press
            # All rows from ov_offset are 'Play
            if(note_offset >= i) {
              data$segment[ov_offset:(i-1)] <- rep(data$segment[i], i-ov_offset)
              
              # If note_offset is last key press, all between is pause
            } else if(note_offset == bt3 | note_offset == bt3 - 1) {
              data$segment[bt3:(i-1)] <- rep('Pause', i-bt3)
              
              # Else, play from ov_offset to note_offset, then pause
            } else if(note_offset > bt3 & note_offset < i){
              data$segment[ov_offset:(note_offset-1)] <- rep(data$segment[i], note_offset-ov_offset)
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
  
  output <<- data
  end <- Sys.time()
  print(start)
  print(end)
}


SegmentPB <- function(data) {
  
  start <- Sys.time()
  
  # order data by BallMillis (time),
  # create new columns for rows and segments
  data <- na.omit(data) # remove possible NAs
  data <- data[order(data$BallMillis), ] # order by BallMillis
  data <- cbind(1:nrow(data), data) # add column with row numbers
  data$segment <- rep('None', nrow(data)) # add column for segments
  data$segment <- factor(data$segment, levels = c('None', 'Control', 'Listen', 'Play',
                                                  'Prepare', 'Pause', 'Wait', 'Overlap'), 
                         labels = c('None', 'Control', 'Listen', 'Play',
                                    'Prepare', 'Pause', 'Wait', 'Overlap'))
  colnames(data) <- c('X', 'trial', 'midi_pitch', 'start_ms', 'duration_ms',
                      'velocity', 'channel', 'BallDist', 'BallMillis', 'segment')
  
  # Run through data and add trial numbers where 0's where before
  y <- data
  noZero <- data[-which(data$channel == 0), ]
  noZeroAgg <- unique(noZero$trial)
  for(i in noZeroAgg) {
    x <- data[which(data$trial == i), ]
    y$trial[x[1, 1]:nrow(y)] <- i
  }
  data$trial <- y$trial
  
  # Put in Listen, Play and Control
  # Listen and Play labels are entered only during key presses
  # Control added for whole trial 2
  for(i in which(data$channel != 0 & data$trial != 2)) {
    if(data$channel[i] == 2) {
      data$segment[i] <- 'Play'
    } else if(data$channel[i] == 1) {
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
      if(data$channel[i] == 2 & data$channel[bt3-1] == data$channel[i] & 
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
        
        # If other channel & previous key press is also other channel &
        # time inbetween is <= 1000ms, rows inbetween get 'Listen'
      } else if(data$channel[i] == 1 & data$channel[bt3-1] == data$channel[i] &
                data$BallMillis[i] - (data$BallMillis[bt3-1] + data$duration_ms[bt3-1]) <= 1000 & 
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
      } else if(data$channel[bt3-1] == data$channel[i] &
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
        
        # if last key press was not overlap, handle normally
        if(i-(ov_offset+1) > 0 & data$segment[bt3-1] != 'Overlap') {
          # If note_offset is after the next key press
          # all between are 'Play'
          if(note_offset >= i) {
            data$segment[bt3:(i-1)] <- rep(data$segment[i], i-bt3)
            
            # If note_offset is last key press, all between is pause
          } else if(note_offset == bt3 | note_offset == bt3 - 1) {
            data$segment[bt3:(i-1)] <- rep('Pause', i-bt3)
            
            # Else, 'Play' until note_offset, then pause
          } else if(note_offset > bt3 & note_offset < i){
            data$segment[bt3:(note_offset-1)] <- rep(data$segment[i], note_offset-bt3)
            data$segment[note_offset:(i-1)] <- rep('Pause', i-note_offset)
          }
          
          # if last press was 'Overlap'
        } else if(i-(ov_offset+1) > 0 & data$segment[bt3-1] == 'Overlap') {
          
          # If note_offset is after overlap finishes
          if(note_offset - ov_offset > 0) {
            # If note_offset is after the next key press
            # ov_offset to i are 'Play'
            if(note_offset >= i) {
              data$segment[ov_offset:(i-1)] <- rep(data$segment[i], i-ov_offset)
              
              # If note_offset is last key press, all between is pause
            } else if(note_offset == bt3 | note_offset == bt3 - 1) {
              data$segment[bt3:(i-1)] <- rep('Pause', i-bt3)
              
              # Else, 'Play' from ov_offset to note_offset, then 'Pause'
            } else if(note_offset > bt3 & note_offset < i){
              data$segment[ov_offset:(note_offset-1)] <- rep(data$segment[i], note_offset-ov_offset)
              data$segment[ov_offset:(i-1)] <- rep('Pause', i-ov_offset)
            }
            
            # if overlap is after note_offset, note_offset to i is 'Pause'
          } else if(note_offset - ov_offset <= 0) {
            data$segment[note_offset:(i-1)] <- rep('Pause', i-note_offset)
          }
        }
        
        # If own channel, but Last press was other channel, prepare
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
        
        # if last press was not overlap, handle normally
        if(i-(ov_offset+1) > 0 & data$segment[bt3-1] != 'Overlap') {
          
          # if note_offset is after i, then 'Listen' until i, then 'Overlap'
          if(note_offset > i) {
            data$segment[bt3:(i-1)] <- rep('Listen', i-bt3)
            data$segment[i:note_offset] <- rep('Overlap', note_offset-(i-1))
            
            # If note_offset IS the next key press
            # Time until next key press is 'Listen', then i is 'Overlap' 
          } else if(note_offset == i) {
            data$segment[bt3:(i-1)] <- rep('Listen', i-bt3)
            data$segment[i] <- 'Overlap'
            
            # Else, listen until note_offset, then prepare
          } else if(note_offset > bt3 & note_offset < i){
            data$segment[bt3:(note_offset-1)] <- rep('Listen', note_offset-bt3)
            data$segment[note_offset:(i-1)] <- rep('Prepare', i-note_offset)
          }
          
          # if last press was 'Overlap'
        } else if(i-(ov_offset+1) > 0 & data$segment[bt3-1] == 'Overlap') {
          # If note_offset is after overlap finishes
          if(note_offset - ov_offset > 0) {
            # If note_offset is after the next key press
            # ov_offset to i is 'Listen', then 'Overlap'
            if(note_offset > i) {
              data$segment[ov_offset:(i-1)] <- rep('Listen', i-ov_offset)
              data$segment[i:note_offset] <- rep('Overlap', note_offset-(i-1))
              
              # If note_offset IS the next key press
              # ov_offset to key press is 'Listen', then overlap
            } else if(note_offset == i) {
              data$segment[ov_offset:(i-1)] <- rep('Listen', i-ov_offset)
              data$segment[i] <- 'Overlap'
              
              # Else, ov_offset to note_offset is 'Listen', then 'Prepare'
            } else if(note_offset > bt3 & note_offset < i){
              data$segment[ov_offset:(note_offset-1)] <- rep('Listen', note_offset-ov_offset)
              data$segment[note_offset:(i-1)] <- rep('Prepare', i-note_offset)
            }
            
            # if overlap is after note_offset, note_offset to i is 'Prepare'
          } else if(note_offset - ov_offset <= 0) {
            data$segment[note_offset:(i-1)] <- rep('Prepare', i-note_offset)
          }
        }
        
        # If own channel, but Last press was other channel, wait
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
        
        # BIG START
        # if last press was not overlap, handle normally
        if(i-(ov_offset+1) > 0 & data$segment[bt3-1] != 'Overlap') {
          if(note_offset > i) {
            # if note_offset is after i, 'Play' until i, then 'Overlap'
            data$segment[bt3:(i-1)] <- rep('Play', i-bt3)
            data$segment[i:note_offset] <- rep('Overlap', note_offset-(i-1))
            
            # If note_offset IS the next key press
            # Time until next key press is 'Play', then overlap is next key press
          } else if(note_offset == i) {
            data$segment[bt3:(i-1)] <- rep('Play', i-bt3)
            data$segment[i] <- 'Overlap'
            
            # Else, 'Play' until note_offset, then prepare
          } else if(note_offset > bt3 & note_offset < i){
            data$segment[bt3:(note_offset-1)] <- rep('Play', note_offset-bt3)
            data$segment[note_offset:(i-1)] <- rep('Wait', i-note_offset)
          }
          
          # if last press was 'Overlap'
        } else if(i-(ov_offset+1) > 0 & data$segment[bt3-1] == 'Overlap') {
          # If note_offset is after overlap finishes
          if(note_offset - ov_offset > 0) {
            # If note_offset is after the next key press
            # ov_offset to i are 'Play', then prepare
            if(note_offset > i) {
              data$segment[ov_offset:(i-1)] <- rep('Play', i-ov_offset)
              data$segment[i:note_offset] <- rep('Overlap', note_offset-(i-1))
              
              # If note_offset IS the next key press
              # ov_offset to i is 'Play', then 'Overlap'
            } else if(note_offset == i) {
              data$segment[ov_offset:(i-1)] <- rep('Play', i-ov_offset)
              data$segment[i] <- 'Overlap'
              
              # Else, ov_offset to note_offset are 'Listen', then 'Wait'
            } else if(note_offset > bt3 & note_offset < i){
              data$segment[ov_offset:(note_offset-1)] <- rep('Play', note_offset-ov_offset)
              data$segment[note_offset:(i-1)] <- rep('Wait', i-note_offset)
            }
            
            # if ov_offset is after note_offset, note_offset to i is 'Wait'
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
  
  colnames(data)[10] <- 'segment'
  output <<- data
  end <- Sys.time()
  print(start)
  print(end)
}


# Make data frame with all participants
musicData <- S101A[1, ]
colnames(musicData)[1] <- 'X'
musicData$segment <- 'None'
musicData$segment <- factor(musicData$segment, levels = c('None', 'Control', 'Listen', 'Play',
                                                          'Prepare', 'Pause', 'Wait', 'Overlap'), 
                            labels = c('None', 'Control', 'Listen', 'Play',
                                       'Prepare', 'Pause', 'Wait', 'Overlap'))
musicData$Subject <- 123

# Label all data frames and combine them
# This loop uses SegmentPA and SegmentPB to run through list_a and list_b
# s defines the subject number, and is assigned to each subject before being added to musicData
s <- 1
for(i in 1:length(list_a)) {
  
  SegmentPA(list_a[[i]])
  output$Subject <- rep(s, nrow(output))
  musicData <- rbind(musicData, output)
  s <- s + 1
  
  SegmentPA(list_b[[i]])
  output$Subject <- rep(s, nrow(output))
  musicData <- rbind(musicData, output)
  s <- s + 1
  
  print(i)
}

# This removes the first, redundant, row of musicData
musicData <- musicData[2:nrow(musicData), ]
