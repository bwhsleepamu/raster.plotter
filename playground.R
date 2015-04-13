## INPUT
# Title
# positions: order and length. for example: name,position,length
# colors: label, color
# data_list: list of data tables

## TEST:


episodes <- as.data.table(read.csv('/home/pwm4/Desktop/plot_test.csv'))
sleep <- as.data.table(read.csv('/home/pwm4/Desktop/sleep.csv'))
stages <- as.data.table(read.csv('/home/pwm4/Desktop/stages.csv'))
actigraphy <- as.data.table(read.csv('/home/pwm4/Desktop/3335GX.csv'))

light <- actigraphy[,data.table(subject_code=subject_code,labtime=labtime_decimal,value=log(light_level))]
activity <- actigraphy[,data.table(subject_code=subject_code,labtime=labtime_decimal,value=log(activity_count))]

light[value==-Inf, value:=0]
activity[value==-Inf, value:=0]

#episodes[,label:=episode_type]
sleep[,label:='SLEEP']
stages[,value:=as.numeric(stage)]

data_list <- list(episodes=episodes, sleep=sleep, stages=stages, light=light, activity=activity) 



colors <- data.table(data_label=c('SLEEP', 'WAKE', 'NREM','REM','UNDEF'), color=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"))
#colors <- data.table(data_label=c('SLEEP', 'WAKE', 'NREM','REM','UNDEF'), color=c("red", "aquamarine", "rosybrown", "wheat", "orange"))
positions <- data.table(name=c('activity','light', 'sleep','episodes','stages'), length=c(5,5,1,1,3))
#positions <- data.table(name=c('activity','sleep', 'light'), length=c(10,1,10))
#positions <- data.table(name=c('activity'), length=c(10))




#### LU

colors <- data.table(data_label=c('SLEEP'), color=c("darkblue"))
positions <- data.table(name=c('activity', 'sleep'), length=c(1000,100))
sleep_data <- as.data.table(read.csv('data/sleep/sleep.csv'))
fl <- list.files('data/actigraphy/', pattern="^\\w*\\.csv$")

sapply(fl, function(filename){
  m <- regexpr('^\\w+', filename)
  sc <- regmatches(filename,m)[1]

  file_name = file.path('outputs', paste(sc, '.svg', sep=''))
  
  if(!file.exists(file_name)) {
    actigraphy <- as.data.table(read.csv(file.path('data','actigraphy', filename)))
    sleep <- sleep_data[subject_code==sc]
    sleep[,label:="SLEEP"]
    light <- actigraphy[,data.table(subject_code=subject_code,labtime=labtime_decimal,value=light_level)]
    activity <- actigraphy[,data.table(subject_code=subject_code,labtime=labtime_decimal,value=activity_count)]
    
    data_list <- list(sleep=sleep,activity=activity)
    
    min_labtime <- min(activity$labtime)
    max_labtime <- max(activity$labtime)
    
    min_day <- set_days(min_labtime)[[1]]
    max_day <- set_days(max_labtime)[[1]]
    
    print(paste(sc, max_day-min_day))
    
    
    plot <- raster_plot(data_list,positions,colors)
    
    
    ggsave(plot=plot, file=file_name, height=((max_day-min_day)*1 + 0.5), width=7, scale=2.5, limitsize=FALSE)    
  }
  
  
})

