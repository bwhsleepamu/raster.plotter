## Main Function

raster_data <- function(x, ...) UseMethod("raster_data")

raster_data.default <- function(input_data, ...) {
}

raster_data.list <- function(input_list, ...) {
  
  rd <- list(title=input_list$title, save_path=input_list$save_path, file_name=input_list$filename, t_cycle=input_list$t_cycle, timescale=input_list$timescale, base_block_height=input_list$base_block_height, day_height=input_list$day_height)

  # Defaults
  if(is.null(rd$day_height)) rd$day_height<-.5
  if(is.null(rd$raster_width)) rd$raster_width<-12
  if(is.null(rd$double_plot)) rd$double_plot<-TRUE

  # Break up by event type
  e <- input_list$events

  # Process single timepoints
  single_e <- e[vapply(e, function(x) x$type == "single", FALSE)]
  rd$single_timepoints <- Reduce(function(...) merge(..., all=T), lapply(single_e, process_single_timepoint_event, rd$double_plot))   

  # Process Block Events
  block_e <- e[vapply(e, function(x) x$type == "block", FALSE)]
  rd$blocks <- Reduce(function(...) merge(..., all=T), lapply(block_e, process_block_event, rd$double_plot))   

  # Process Linear Events
  linear_e <- e[vapply(e, function(x) x$type == "linear", FALSE)]
  if(length(linear_e) > 0) rd$linear <- process_linear_event(linear_e[[1]], rd$double_plot)

  rd$number_of_days <- length(unique(c(rd$single_timepoints$day_s, rd$blocks$day_s, rd$linear$plot_data$day_s)))

  rd
}


## Block Events
process_block_event <- function(event_info, double_plot_switch) {

  df <- process_start_end_block_event(event_info)
  
  # Divide into parts that span two days, and those that don't
  non_spanning <- df[format(df$start_time, "%Y%m%d") == format(df$end_time, "%Y%m%d"),]
  spanning <- df[format(df$start_time, "%Y%m%d") != format(df$end_time, "%Y%m%d"),]
  
  first_parts <- mdply(spanning, first_div)
  middle_parts <- mdply(spanning, middle_div)
  last_parts <- mdply(spanning, last_div)

  df <- rbind(non_spanning, first_parts, middle_parts, last_parts)

  df$day = do.call(c, lapply(df$start_time, function(x) { as.Date(toString(x)) }))
  df$start_time <- do.call(c, lapply(df$start_time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  
  df$end_time <- do.call(c, lapply(df$end_time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  
  
  df <- double_plot(df, double_plot_switch)

  df$day_s <- do.call(c, lapply(df$day, toString))
  
  df
} 


process_start_end_block_event <- function(event_info) {
  st <- do.call(c, lapply(event_info$blocks, function(x) read_iso_time(x[1]) ))
  et <- do.call(c, lapply(event_info$blocks, function(x) read_iso_time(x[2]) ))

  if(is.null(event_info$color)) {
    if(length(event_info$blocks[[1]]) > 2) {
      colors <- do.call(c, lapply(event_info$blocks, function(x) x[3]))
    } else {
      colors <- "#000000"
    }
  } else {
    colors <- event_info$color
  }

  df <- data.frame(name=event_info$name, start_time=st, end_time=et, color=colors, position=event_info$position, height=event_info$height)
 
  df
}

first_div <- function(name, start_time, end_time, color, position, height) {
  new_end_time <- read_iso_time(format(start_time, "%Y-%m-%dT23:59:59"))
  data.frame(name=name, start_time=start_time, end_time = new_end_time, color=color, position=position, height=height)
}

middle_div <- function(name, start_time, end_time, color, position, height) {
  secs_in_day <- 3600*24
  new_end_time <- read_iso_time(format(start_time, "%Y-%m-%dT23:59:59"))
  new_start_time <- read_iso_time(format(end_time, "%Y-%m-%dT00:00:00"))
  
  gap_size <- as.numeric(round(difftime(new_start_time, new_end_time, units="days")))


  if(gap_size > 0) {
    sts <- vector()
    ets <- vector()
    for(day_num in 1:gap_size) {
      ref_time <- start_time+(secs_in_day*day_num)
      sts <- append(sts, read_iso_time(format(ref_time, "%Y-%m-%dT00:00:00")))
      ets <- append(sts, read_iso_time(format(ref_time, "%Y-%m-%dT23:59:59")))
    }
    data.frame(name=name, start_time=sts, end_time=ets, color=color, position=position, height=height) 
  } else {
    NULL
  }
}

last_div <- function(name, start_time, end_time, color, position, height) {
  new_start_time <- read_iso_time(format(end_time, "%Y-%m-%dT00:00:00"))
  data.frame(name=name, start_time=new_start_time, end_time = end_time, color=color, position=position, height=height)
}
## Single Timepoint Events
process_single_timepoint_event <- function(event_info) {
  t <- do.call(c, lapply(event_info$times, read_iso_time ))    

  df <- data.frame(name=event_info$name, time=t, color=event_info$color, group=event_info$group)

  df$day = as.Date(df$time, tz="EST")
  
  df$day_s <- do.call(c, lapply(df$day, toString))
  df$time <- do.call(c, lapply(df$time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  

  df
}

## Linear Events
process_linear_event <- function(event_info, double_plot_switch) {
  time <- do.call(c, lapply(event_info$points, function(x) x[[1]]))
  y <- do.call(c, lapply(event_info$points, function(x) if(is.null(x[[2]])) NA else as.numeric(x[[2]]) ))
  
  ft <- read_iso_time(time)
  day <- as.Date(ft, tz="EST")
  day_s <- do.call(c, lapply(day, toString))
  t <- read_time_of_day(time)



  linear_event_info$plot_data <- double_plot(data.frame(time=t, y=y, day=day, day_s=day_s, full_time=ft), double_plot_switch)



  # linear_event_info <- list(name=event_info$name, limits=event_info$limits, color=(if(is.null(event_info$color)) "#000000" else event_info$color)) #, limits=event_info$limits)
  # linear_event_info$plot_data <- data.frame(do.call(rbind, lapply(event_info$points, process_linear_data)))
  # colnames(linear_event_info$plot_data) <- c("year", "month", "day", "hour", "min", "sec", "y")

  # linear_event_info$plot_data$time <- do.call(c, lapply(firstof, list(1, 1, 1, linear_event_info$plot_data$hour, linear_event_info$plot_data$min, linear_event_info$plot_data$sec))
  # #linear_event_info$plot_data$day_s <- do.call(c, )
  # linear_event_info$plot_data$day <- as.Date.character(linear_event_info$plot_data$day_s, format="%Y-%m-%d")

  # linear_event_info$plot_data$day <- as.Date.character(linear_event_info$plot_data$day_s, format="%Y-%m-%d")

  # linear_event_info$plot_data <- double_plot(linear_event_info$plot_data, double_plot_switch)

  linear_event_info
}

read_time_of_day <- function(x) {
  hour <- as.numeric(substr(x, 12, 13))
  min <- as.numeric(substr(x, 15, 16))
  sec <- as.numeric(substr(x, 18, 19))
  
  do.call(firstof, list(1,1,1,hour, min, sec))
}


process_linear_data <- function(x) {
  time <- parse_timestring(x[[1]])
  val <- if(is.null(x[[2]])) NA else as.numeric(x[[2]])

#  t <- do.call(firstof, list(1, 1, 1, time$hour, time$min, time$sec))
  # y <- time$year
  # m <- time$month
  # d <- time$day

#  ds <- sprintf("%s-%s-%s", time$year, time$month, time$day)

  c(time$year, time$month, time$day, time$hour, time$min, time$sec, val)
}

## Helper Functions


parse_timestring <- function(time_s) {
  parsed_time <- list()
  parsed_time$year <- as.numeric(substr(time_s, 1, 4))
  parsed_time$month <- as.numeric(substr(time_s, 6, 7))
  parsed_time$day <- as.numeric(substr(time_s, 9, 10))
  parsed_time$hour <- as.numeric(substr(time_s, 12, 13))
  parsed_time$min <- as.numeric(substr(time_s, 15, 16))
  parsed_time$sec <- as.numeric(substr(time_s, 18, 19))
  
  parsed_time
}

read_iso_time <- function(x) {
  #  xts::.parseISO8601(x)$first.time

  time <- parse_timestring(x)
  f <- do.call(firstof, time)
  f
}

# Double-Plotting
double_plot <- function(df, should_do_it) {
  ### DANGER ###
  # This function changes the date of the right double-plot, causing the actual dates to not match up with the times. 
  # The correct date for a given set of times is (day + double_plot_pos)
  ###
  if(should_do_it) { 
    r_df <- df
    l_df <- df
    
    l_df$double_plot_pos <- 0
    r_df$double_plot_pos <- 1
    r_df$day <- r_df$day - 1
    if(!is.null(r_df$day_s))
      r_df$day_s <- format(r_df$day, format="%Y-%m-%d")
    return(rbind(l_df, r_df))
  } else {
    df$double_plot_pos <- 0
    df
  }
}





## Labtime Addition (In Progress)


process_start_end_block_event.labtime <- function(event_info) {
  st <- do.call(c, lapply(event_info$times, function(x) { read_iso_time(x[1]) }))
  et <- do.call(c, lapply(event_info$times, function(x) { read_iso_time(x[2]) }))

  df <- data.frame(name=event_info$name, start_time=st, end_time=et, color=event_info$color, group=event_info$group)  
 
  df
}

process_block_event.labtime <- function(event_info) {
  df <- process_start_end_block_event.labtime(event_info)
}



