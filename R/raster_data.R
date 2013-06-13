first_div <- function(name, start_time, end_time, color, group) {
  new_end_time <- xts::.parseISO8601(format(start_time, "%Y-%m-%dT23:59:59"))$last.time
  data.frame(name=name, start_time=start_time, end_time = new_end_time, color=color, group=group)
}

second_div <- function(name, start_time, end_time, color, group) {
  new_start_time <- xts::.parseISO8601(format(end_time, "%Y-%m-%dT00:00:00"))$first.time
  data.frame(name=name, start_time=new_start_time, end_time = end_time, color=color, group=group)
}


raster_data <- function(x, ...) UseMethod("raster_data")

raster_data.default <- function(input_data, ...) {
}

raster_data.list <- function(input_list, ...) {
  rd <- list(title=input_list$title, save_path=input_list$save_path, file_name=input_list$file_name)
  # Break up by event type

  # Process Block Events

  # Process Linear Events

  rd$single_timepoints <- Reduce(function(...) merge(..., all=T), lapply(input_list$single_timepoints, process_single_timepoint_event))   
  rd$blocks <- Reduce(function(...) merge(..., all=T), lapply(input_list$block_events, process_block_event))   
  rd$linear <- process_linear_event(input_list$linear_events[[1]])
  #Reduce(function(...) merge(..., all=T), lapply(input_list$linear_events, process_linear_event))

  rd
}


process_block_event <- function(event_info) {

  if(event_info$class == "start_end") {
    df <- process_start_end_block_event(event_info)
  } else if (event_info$class == "duration") {
    df <- process_block_duration_event(event_info)
  } 

  # Divide into parts that span two days, and those that don't
  non_spanning <- df[format(df$start_time, "%Y%m%d") == format(df$end_time, "%Y%m%d"),]
  spanning <- df[format(df$start_time, "%Y%m%d") != format(df$end_time, "%Y%m%d"),]
  
  first_parts <- mdply(spanning, first_div)
  second_parts <- mdply(spanning, second_div)

  df <- rbind(non_spanning, first_parts, second_parts)
  df$day = as.Date(df$start_time, tz="EST")

  df$day_s <- do.call(c, lapply(df$day, format_date_label))

  df$start_time <- do.call(c, lapply(df$start_time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  
  df$end_time <- do.call(c, lapply(df$end_time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  
 
  df
} 


process_start_end_block_event <- function(event_info) {
  st <- do.call(c, lapply(event_info$times, function(x) { read_iso_time(x[1]) }))
  et <- do.call(c, lapply(event_info$times, function(x) { read_iso_time(x[2]) }))

  df <- data.frame(name=event_info$name, start_time=st, end_time=et, color=event_info$color, group=event_info$group)  
 
  df
}

read_iso_time <- function(time_string) {
  xts::.parseISO8601(time_string)$first.time
}

process_single_timepoint_event <- function(event_info) {
  t <- do.call(c, lapply(event_info$times, read_iso_time ))    

  df <- data.frame(name=event_info$name, time=t, color=event_info$color, group=event_info$group)

  df$day = as.Date(df$time, tz="EST")

  df$day_s <- do.call(c, lapply(df$day, format_date_label))
  df$time <- do.call(c, lapply(df$time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  

  df
}


process_linear_event <- function(event_info) {

  linear_event_info <- list(name=event_info$name, y_range=event_info$y_range, color=event_info$color)

  x_times <- do.call(c, lapply(event_info$times, function(x) {read_iso_time(x[1])}))
  y_vals <- do.call(c, lapply(event_info$times, function(x) {as.numeric(x[2])}))

  linear_event_info$plot_data <- data.frame(time=x_times, y_value=y_vals)

  linear_event_info$plot_data$day <- as.Date(linear_event_info$plot_data$time, tz="EST")

  do.call(c, lapply(linear_event_info$plot_data$day, format_date_label))

  linear_event_info$plot_data$time <- do.call(c, lapply(linear_event_info$plot_data$time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  
  
  linear_event_info
}


format_date_label <- function(date) {
  format(date, format="%a %m/%d")
}

