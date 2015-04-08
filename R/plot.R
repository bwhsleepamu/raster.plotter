library("grid")
library("ggplot2")
library(scales)
library(data.table)

source("R/constants.R")
source("R/util.R")
source("R/data.R")


## INPUT
# Title
# positions: order and length. for example: name,position,length
# colors: label, color
# data_list: list of data tables

## TEST:
epoch_length <- EPOCH_LENGTH

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

raster_plot <- function(data_list, positions, colors, days=NULL, draw_double=TRUE) {

  .e <- environment()
  
  min_labtime <- min(sapply(data_list, function(x){ if(!is.null(x$labtime[1])) {x$labtime[1]} else {x$start_labtime[1]}}))
  min_day <- set_days(min_labtime)[[1]]
  
  if(!is.null(days)) {
    days <- days + min_day
  }
    
  
  # Main Plot
  plot <- ggplot(environment = .e)
  
  # Labels and theming
  #plot <- plot + ggtitle(subject_code)
  plot <- plot + theme(axis.title.y=element_blank(), legend.title=element_blank())
  plot <- plot + xlab("Time (hours)")
  
  # Faceting
  plot <- plot + facet_grid(day ~ double_plot_pos)
  
  # Scaling and Margins
  #plot <- plot + theme(panel.margin = unit(0, "npc"))
  #y_breaks <- c(-5,-3,-1,0,.5,1.5,2,2.5,3,4)
  
  min_y <- 0
  max_y <- sum(positions$length)
  last_max_y <- max_y
  
  plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, 24 + epoch_length), expand=c(0,0), breaks=c(0,12,24), minor_breaks=c(3,6,9,15,18,21)) 
  plot <- plot + scale_y_continuous(limits=c(min_y, max_y))
  
  # Colors
  #plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  
  
  #grrr <- apply(positions, 1, function(pos_row){
  
  fill_colors <- colors$color
  names(fill_colors) <- colors$data_label
  
  plot <- plot + scale_fill_manual(values = fill_colors)  
  
  # Panel margins for double plots
  #plot <- plot + theme(panel.margin = unit(0.00, "npc"))
  
  
  for(i in 1:nrow(positions)) {  
    pos_row <- positions[i,]
    data <- copy(data_list[[pos_row$name]])
    data[,i:=.I]
    row_max_y <- last_max_y 
    row_min_y <- row_max_y - as.numeric(pos_row$length)
    
    if('value'%in%colnames(data)){
      # Points
      
      ## Re-Scale
      r <- range(data$value)
      data_min <- r[1]
      data_range <- diff(r)
      data[,value:=row_min_y+(value-data_min)*(as.numeric(pos_row$length)/data_range)]
      data <- setup_point_data(data)
  
      # Limit by days
      if(!is.null(days))
        data <- data[day %in% days]
      
      # Double-Plot
      data <- double_plot(data, draw_double)
      
      plot <- plot + geom_line(aes(x=labtime, y=value), data=data)
    } 
    else {
      # Blocks
      data <- setup_block_data(data)

      # Limit by days
      if(!is.null(days))
        data <- data[day %in% days]
      
      # Double-Plot
      data <- double_plot(data, draw_double)
      
      ## Set Colors
      plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_labtime, xmax = end_labtime + epoch_length, fill = label), ymin = row_min_y, ymax = row_max_y, data = data)
    }
    
    last_max_y <- row_min_y
  }
  plot
}
# 
# 
# raster_plot <- function(subject_code, #, epoch_length=EPOCH_LENGTH, output_dir="/home/pwm4/Desktop/", l="",
#                         number_of_days=NA, 
#                         first_day=1,
#                         cycle_types=c("NREM")
# ) {  
#   
#   epoch_length = EPOCH_LENGTH
#   
#   # Limit by subject
#   subject_list <- c(subject_code)
#   
#   # Limit by day
#   days_to_graph <- unique(sleep_data.v[subject_code %in% subject_list]$day_number)
#   if(!is.na(number_of_days))
#     days_to_graph <- days_to_graph[first_day:(first_day+number_of_days-1)]
#   
#   print(days_to_graph)
#   
#   graph_data <<- copy(sleep_data.v[subject_code %in% subject_list & day_number %in% days_to_graph])
#   graph_episodes <<- copy(episodes.v[subject_code %in% subject_list & day_number %in% days_to_graph & activity_or_bedrest_episode > 0])
#   graph_cycles <- copy(cycles.v[subject_code %in% subject_list & day_number %in% days_to_graph & activity_or_bedrest_episode > 0 & type %in% cycle_types])
#   #graph_bedrest_episodes <- copy(bedrest_episodes.v[subject_code %in% subject_list & day_number %in% days_to_graph])
#   
#   # Draw
#   .e <- environment()
#   
#   # Main Plot
#   plot <- ggplot(graph_data, aes(x=day_labtime, y=stage_for_raster, group=day_number), environment = .e)
#   
#   # Labels and theming
#   plot <- plot + ggtitle(subject_code)
#   plot <- plot + theme(axis.title.y=element_blank(), legend.title=element_blank())
#   plot <- plot + xlab("Time (hours)")
#   
#   # Faceting
#   plot <- plot + facet_grid(day_number ~ .)
#   
#   # Scaling and Margins
#   #plot <- plot + theme(panel.margin = unit(0, "npc"))
#   y_breaks <- c(-5,-3,-1,0,.5,1.5,2,2.5,3,4)
#   
#   plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, 24 + epoch_length), expand=c(0,0), breaks=c(0,12,24), minor_breaks=c(3,6,9,15,18,21)) 
#   plot <- plot + scale_y_continuous(limits=c(-8, 4), breaks=y_breaks, labels=lapply(y_breaks,y_axis_formatter))
#   
#   # Colors
#   plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
#   
#   
#   
#   #plot <- plot + scale_fill_manual(values=alpha(c("blue", "red", "black", "purple", "green", "yellow"), 0.8))
#   
#   ## Episodes and Cycles
#   #   methods <- c('raw')#, 'classic')#, 'changepoint')
#   #   r <- foreach(i=1:length(methods)) %do% {
#   #     end_pos <- i * -2    
#   #     text_y_pos <- end_pos + 0.5
#   #     
#   #     #for_this_graph <- graph_cycles[method==methods[i]]
#   #     #print(nrow(for_this_graph))
#   #     #print(end_pos)
#   #     plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = end_pos+.2, ymax = end_pos+1.8, data = graph_episodes[method==methods[i]])
#   #     #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = end_pos, ymax = end_pos+1, data=for_this_graph)    
#   #     #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=text_y_pos, data=for_this_graph)
#   #   }  
#   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -1, ymax = -0.2, data = graph_episodes[method=='classic'])
#   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -1.8, ymax = -1, data = graph_episodes[method=='classic' & keep==TRUE])
#   #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = -2.5, ymax = -2, data=graph_cycles[method=="classic"])    
#   #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=-2.25, data=graph_cycles[method=="classic"])
#   
#   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -3.5, ymax = -2.7, data = graph_episodes[method=='iterative'])
#   #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = -4.2, ymax = -3.7, data=graph_cycles[method=="iterative"])    
#   #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=-3.95, data=graph_cycles[method=="iterative"])
#   
#   
#   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = episode_type), ymin = -5.2, ymax = -4.4, data = graph_episodes[method=='changepoint'])
#   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -6, ymax = -5.2, data = graph_episodes[method=='changepoint'])
#   #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = -6.7, ymax = -6.2, data=graph_cycles[method=="changepoint"])    
#   #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=-6.45, data=graph_cycles[method=="changepoint"])
#   
#   
#   #  plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, y=-1.5, label=cycle_number), data=graph_cyles[method=='classic'])
#   #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -3, ymax = -2, data = graph_periods[method=="iterative"])
#   #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = -4, ymax = -3, data = graph_cyles[method=="iterative"])
#   #   
#   #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -5, ymax = -4, data = graph_periods[method=="changepoint"])
#   #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = -6, ymax = -5, data = graph_cyles[method=="changepoint"])
#   
#   ## Bedrest Episodes
#   #  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = 0, ymax = 4, alpha=.5, data = graph_bedrest_episodes)
#   
#   
#   
#   #plot <- plot + geom_point(shape='.', size=2)
#   plot <- plot + geom_line() #aes(colour=epoch_type)
#   
#   #file_name = file.path(output_dir, paste(subject_code, "_", l, '.svg', sep=''))
#   #print(file_name)
#   #print(length(days_to_graph))
#   #ggsave(plot=plot, file=file_name, height=(length(days_to_graph)*1 + 0.5), width=7, scale=2.5, limitsize=FALSE)
#   
#   plot
#   
# }





## Helpers





# 
# 
# convert_length_to_minutes <- function(lengths, epoch_length=EPOCH_LENGTH) {
#   lengths * epoch_length * 60
# } 
# 
# convert_to_labtimes <- function(indeces, sleep_data) {
#   sleep_data$labtime[indeces]
#   
# }
# 
# set_days <- function(labtimes, t_cycle=T_CYCLE) {
#   day_numbers <- floor(labtimes / t_cycle)
#   day_labtimes <- (labtimes - (day_numbers * t_cycle))
#   
#   list(day_numbers, day_labtimes)
# }
# 
# convert_stage_for_raster <- function(d) {
#   conv_map <- c(1.5,2,2.5,3,.5,4)
#   
#   d[epoch_type!='UNDEF', stage_for_raster:=conv_map[stage]]
#   d[epoch_type=='UNDEF', stage_for_raster:=0]
# }
# 
# y_axis_formatter <- function(x) {
#   if (x == .5) { res <- "WAKE" }
#   else if (x == 1.5) { res <- "Stage 1" }
#   else if (x == 2) { res <- "Stage 2" }
#   else if (x == 2.5) { res <- "Stage 3" }
#   else if (x == 3) { res <- "Stage 4" }
#   else if (x == 4) { res <- "REM" }
#   else if (x == 0) { res <- "UNDEF"}
#   else if (x == -1) { res <- ""}
#   else if (x == -3) { res <- ""}
#   else if (x == -5) { res <- ""}
#   else { res <- as.character(x) }
#   
#   res
# }