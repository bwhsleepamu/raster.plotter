library(ROracle)

loadMainData <- function() {
  episodes <- as.data.table(read.csv('../data/episodes/episodes.csv'))
  sleep <- as.data.table(read.csv('../data/sleep/sleep.csv'))
  stages <- as.data.table(read.csv('../data/stages/stages.csv'))

  sleep[,label:='SLEEP']
  stages[,value:=as.numeric(stage)]
  
  list(episodes=episodes,sleep=sleep,stages=stages)
}

loadActigraphy <- function(subject_code) {
  act_file_path <- file.path('../data','actigraphy', paste(subject_code,'.csv',sep=''))
  if(file.exists(act_file_path)) {
    actigraphy <- as.data.table(read.csv(act_file_path))
    
    
    light <- actigraphy[,data.table(subject_code=subject_code,labtime=labtime_decimal,value=light_level)]
    activity <- actigraphy[,data.table(subject_code=subject_code,labtime=labtime_decimal,value=activity_count)]
  } else {
    light <- data.table(subject_code=character(0), labtime=numeric(0), value=numeric(0))
    activity <- data.table(subject_code=character(0), labtime=numeric(0), value=numeric(0))
  }
  

  list(light=light,activity=activity)  
}

loadForSubject <- function(sc, main_data) {
  actigraphy = loadActigraphy(sc)
  
  list(
    episodes=main_data$episodes[subject_code==sc],
    sleep=main_data$sleep[subject_code==sc], 
    stage=main_data$stages[subject_code==sc],
    light=actigraphy$light[subject_code==sc],
    activity=actigraphy$activity[subject_code==sc]
  )
}

getDayRange <- function(subject_data) {
  min_labtime <- min(unlist(sapply(subject_data, function(x){
    if(!is.null(x$labtime[1])) {x$labtime[1]} else {x$start_labtime[1]}
  })), na.rm=TRUE)
  max_labtime <- max(unlist(sapply(subject_data, function(x){
    if(!is.null(x$labtime[nrow(x)])) {x$labtime[nrow(x)]} else {x$end_labtime[nrow(x)]}
  })), na.rm=TRUE)

  min_day <- set_days(min_labtime)[[1]]
  max_day <- set_days(max_labtime)[[1]]

  list(min_day=min_day,max_day=max_day)
}