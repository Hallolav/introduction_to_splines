#################################
# function for every distance   #
#################################
load_500m_data <- function(){
  df = read.csv("../data/500m data - changed by hand.csv")
  
  time2sec <- function(time = str){
    sec = strtoi(substr(time, 1, 2))
    if (nchar(time) == 5){hun = strtoi(substr(time, 4, 5)) / 100}
    else if (nchar(time) == 4){hun = strtoi(substr(time, 4, 4)) / 10}
    
    return(as.numeric(sec + hun))
  }
  
  for (i in 1:nrow(df)){
    time_character = df[i, 'time']
    if (nchar(time_character) > 10){
      df[i, c('time')] <- NA
    }
    else{
      df[i, c('time')] <- time2sec(df[i, c('time')])
    }
  }
  
  df$time <- as.numeric(df$time)
  colnames(df) <- c('year', 'time')
  
  return(df)
}

load_1500m_data <- function(){
  df = read.csv("../data/1500m data - changed by hand.csv")
  
  time2sec <- function(time = str){
    min = 60 * strtoi(substr(time, 1, 1))
    sec = strtoi(substr(time, 3, 4))
    if (nchar(time) == 7){hun = strtoi(substr(time, 6, 7)) / 100}
    else if (nchar(time) == 6){hun = strtoi(substr(time, 6, 6)) / 10}
    
    return(as.numeric(min + sec + hun))
  }
  
  for (i in 1:nrow(df)){
    time_character = df[i, 'time']
    if (nchar(time_character) > 10){
      df[i, c('time')] <- NA
    }
    else{
      df[i, c('time')] <- time2sec(df[i, c('time')])
    }
  }
  
  df$time <- as.numeric(df$time)
  colnames(df) <- c('year', 'time')
  
  return(df)
}

load_5000m_data <- function(){
  df = read.csv("../data/5000m data - changed by hand.csv")
  
  time2sec <- function(time = str){
    min = 60 * strtoi(strsplit(time, '[.]')[[1]][1])
    rest_time = strsplit(time, '[.]')[[1]][2]
    sec = strtoi(substr(rest_time, 1, 2))
    if (nchar(rest_time) == 5){hun = strtoi(substr(rest_time, 4, 5)) / 100}
    else if (nchar(rest_time) == 4){hun = strtoi(substr(rest_time, 4, 4)) / 10}
    
    return(as.numeric(min + sec + hun))
  }
  
  for (i in 1:nrow(df)){
    time_character = df[i, 'time']
    if (nchar(time_character) > 10){
      df[i, c('time')] <- NA
    }
    else{
      df[i, c('time')] <- time2sec(df[i, c('time')])
    }
  }
  
  df$time <- as.numeric(df$time)
  colnames(df) <- c('year', 'time')
  
  return(df)
}

load_10000m_data <- function(){
  df = read.csv("../data/10000m data - changed by hand.csv")
  
  time2sec <- function(time = str){
    min = 60 * strtoi(substr(time, 1, 2))
    sec = strtoi(substr(time, 4, 5))
    if (nchar(time) == 8){hun = strtoi(substr(time, 7, 8)) / 100}
    else if (nchar(time) == 7){hun = strtoi(substr(time, 7, 7)) / 10}
    
    return(as.numeric(min + sec + hun))
  }
  
  for (i in 1:nrow(df)){
    time_character = df[i, 'time']
    if (nchar(time_character) > 10){
      df[i, c('time')] <- NA
    }
    else{
      df[i, c('time')] <- time2sec(df[i, c('time')])
    }
  }
  
  df$time <- as.numeric(df$time)
  colnames(df) <- c('year', 'time')
  
  return(df)
}

#################################
# the one function to open all  #
#################################
load_data <- function(distance = 10000){
  if (!(distance %in% c(500, 1500, 5000, 10000))){
    stop('Invalid distance. Please enter one of the distances on the ISU speedskating
    allround world championship (i.e.: 500, 1000, 1500 or 10000)')
  }
  
  if (distance == 500){
    df = load_500m_data()
  } else if (distance == 1500){
    df = load_1500m_data()
  } else if (distance == 5000){
    df = load_5000m_data()
  } else if (distance == 10000){
    df = load_10000m_data()
  }
  
  return(df)
}

