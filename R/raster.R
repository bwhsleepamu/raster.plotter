
plot_raster <- function(data, subject_code="Example", number_of_days=NA, first_day=1, epoch_length = EPOCH_LENGTH) {  
  # Limit by subject
  subject_list <- c(subject_code)
  
  # Limit by day
  days_to_graph <- unique(data$stages[subject_code %in% subject_list]$day_number)
  if(!is.na(number_of_days))
    days_to_graph <- days_to_graph[first_day:(first_day+number_of_days-1)]
  print(days_to_graph)
  
  # Get data subset
  graph_data <- copy(data$stages[subject_code %in% subject_list & day_number %in% days_to_graph])
  graph_sleep_episodes <- copy(data$sleep_episodes[subject_code %in% subject_list & day_number %in% days_to_graph])
  graph_state_episodes <<- copy(data$state_episodes[subject_code %in% subject_list & day_number %in% days_to_graph])
  
  # Draw
  .e <- environment()
  
  # Main Plot
  plot <- ggplot(graph_data, aes(x=day_labtime, y=stage_for_raster, group=day_number), environment = .e)
  
  # Labels and theming
  plot <- plot + ggtitle(subject_code)
  plot <- plot + theme(axis.title.y=element_blank(), legend.title=element_blank(), axis.line = element_blank(),panel.grid.minor=element_blank(),strip.text.x=element_blank())
  plot <- plot + xlab("Time (hours)")
  
  # Faceting
  plot <- plot + facet_grid(day_number ~ double_plot_pos)
  
  # Scaling and Margins
  y_breaks <- c(-8.5,-8, -7.5, -6, -4.5, -4, -3, -1, 0, 5, 10)
  
  plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, 24 + epoch_length), expand=c(0,0), breaks=c(0,4,8,12,16,20)) 
  plot <- plot + scale_y_continuous(limits=c(-8.6, 10), breaks=y_breaks, labels=lapply(y_breaks,y_axis_formatter))
  
  plot <- plot + theme(panel.margin.x = unit(0.00, "npc"))
  
  # Colors
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  
  # Plotting
  plot <- plot + geom_line(data=graph_data,mapping=(aes(group=activity_or_bedrest_episode))) #aes(colour=epoch_type)
  plot <- plot + geom_rect(aes(NULL, NULL, xmin=start_day_labtime, xmax=end_day_labtime+epoch_length), ymin=-2, ymax=0, fill=NA, color="black", data=graph_sleep_episodes)
  plot <- plot + geom_rect(aes(NULL, NULL, xmin=start_day_labtime, xmax=end_day_labtime+epoch_length, fill=label, color=label), ymin=-4, ymax=-2, data=graph_state_episodes)
  plot <- plot + geom_text(aes(x = (start_day_labtime+end_day_labtime)/2, label=activity_or_bedrest_episode), y=-1, size=2.5, data=graph_sleep_episodes)
  plot <- plot + geom_line(data=graph_data, aes(y=y), color='blue', size=1)
  
  
  plot
}





## Helpers
select_longer_split <- function(d) {
  max_length <- max(d$length)
  d[length==max_length]
}

split_day_spanning_blocks <- function(dt, t_cycle=T_CYCLE, epoch_length=EPOCH_LENGTH){
  first_division <- dt
  second_division <- copy(dt)
  
  new_end_day_labtime <- t_cycle-epoch_length
  
  first_division[,`:=`(end_day_number=start_day_number, end_day_labtime=new_end_day_labtime)]
  second_division[,`:=`(start_day_number=end_day_number, start_day_labtime=0)]
  
  rbindlist(list(first_division, second_division))
}


convert_length_to_minutes <- function(lengths, epoch_length=EPOCH_LENGTH) {
  lengths * epoch_length * 60
} 

convert_to_labtimes <- function(indeces, sleep_data) {
  sleep_data$labtime[indeces]
  
}

set_days <- function(labtimes, t_cycle=T_CYCLE) {
  day_numbers <- floor(labtimes / t_cycle)
  day_labtimes <- (labtimes - (day_numbers * t_cycle))
  
  list(day_numbers, day_labtimes)
}

convert_stage_for_raster <- function(d) {
  conv_map <- c(-7.5,-8,-8.5,-8.5,-4.5,-6)
  
  d[epoch_type!='UNDEF', stage_for_raster:=conv_map[stage]]
  d[epoch_type=='UNDEF', stage_for_raster:=-4.0]
}

y_axis_formatter <- function(x) {
  if (x == -4.5) { res <- "WAKE" }
  else if (x == -4) { res <- "" }
  else if (x == -6) { res <- "REM" }
  else if (x == -7.5) { res <- "" }
  else if (x == -8) { res <- "NREM" }
  else if (x == -8.5) { res <- "" }
  else if (x == -1) { res <- "Sleep Episode"}
  else if (x == -3) { res <- "Sleep Stage"}
  else if (x == 0) { res <- ""}
  else if (x == 5) { res <- "Continuous"}
  else if (x == 10) { res <- ""}
  else { res <- as.character(x) }
  
  res
}

# Continuous Data
transform_continuous <- function(values, cutoff=1) {
  max_v <- quantile(values, cutoff)
  min_v <- min(values)
  
  (values - min_v)*(10/(max_v-min_v))
}

# Double-Plotting
double_plot <- function(dataset, plot_double=TRUE) {
  ### DANGER ###
  # This function changes the date of the right double-plot, causing the actual dates to not match up with the times. 
  # The correct date for a given set of times is (day + double_plot_pos)
  ###
  if(plot_double) { 
    right_side <- copy(dataset)
    left_side <- copy(dataset)
    
    left_side[,double_plot_pos:=0]
    right_side[,double_plot_pos:=1]
    right_side[,day_number:=day_number-1]
    
    #     if(!is.null(right_side$day_s))
    #       r_df$day_s <- format(r_df$day, format="%Y-%m-%d")
    return(rbind(left_side, right_side))
  } else {
    dataset[, double_plot_pos:=0]    
  }
}