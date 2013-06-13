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
  rd <- list(title=input_list$title, save_path=input_list$save_path, file_name=input_list$file_name, df=NULL, blocks=NULL)
  # Break up by event type

  # Process Block Events

  # Process Linear Events

  
  rd$block <- Reduce(function(...) merge(..., all=T), lapply(input_list$block_events, process_block_event))   
  rd$df <- Reduce(function(...) merge(..., all=T), lapply(input_list$linear_events, process_linear_event))
}


process_block_event <- function(event_info) {
  if(event_info$class = "block_start_end") {
    res <- process_full_block_event(event_info)
  } else if (event_info$class = "linear") {
    res <- process_linear_event
  } else if (event_info$class = "block_single_time") {
    res <- process_intantenous_event(event_info)
  }
  st <- do.call(c, lapply(event_info$times, function(x) { xts::.parseISO8601(x[1])$first.time }))
  et <- do.call(c, lapply(event_info$times, function(x) { xts::.parseISO8601(x[2])$first.time }))

  df <- data.frame(name=event_info$name, start_time=st, end_time=et, color=event_info$color, group=event_info$group)  

  # Divide into parts that span two days, and those that don't
  non_spanning <- df[format(df$start_time, "%Y%m%d") == format(df$end_time, "%Y%m%d"),]
  spanning <- df[format(df$start_time, "%Y%m%d") != format(df$end_time, "%Y%m%d"),]
  
  first_parts <- mdply(spanning, first_div)
  second_parts <- mdply(spanning, second_div)

  df <- rbind(non_spanning, first_parts, second_parts)
  df$day = as.Date(df$start_time, tz="EST")

  df$start_time <- do.call(c, lapply(df$start_time, function(x) { xts::.parseISO8601(format(x, "0001-01-01T%H:%M:%S"))$first.time }))  
  df$end_time <- do.call(c, lapply(df$end_time, function(x) { xts::.parseISO8601(format(x, "0001-01-01T%H:%M:%S"))$first.time }))  
 
  df
} 


process_full_block_event <- function(event_info) {
  st <- do.call(c, lapply(event_info$times, function(x) { xts::.parseISO8601(x[1])$first.time }))
  et <- do.call(c, lapply(event_info$times, function(x) { xts::.parseISO8601(x[2])$first.time }))

  df <- data.frame(name=event_info$name, start_time=st, end_time=et, color=event_info$color, group=event_info$group)  

  # Divide into parts that span two days, and those that don't
  non_spanning <- df[format(df$start_time, "%Y%m%d") == format(df$end_time, "%Y%m%d"),]
  spanning <- df[format(df$start_time, "%Y%m%d") != format(df$end_time, "%Y%m%d"),]
  
  first_parts <- mdply(spanning, first_div)
  second_parts <- mdply(spanning, second_div)

  df <- rbind(non_spanning, first_parts, second_parts)
  df$day = as.Date(df$start_time, tz="EST")

  df$start_time <- do.call(c, lapply(df$start_time, function(x) { xts::.parseISO8601(format(x, "0001-01-01T%H:%M:%S"))$first.time }))  
  df$end_time <- do.call(c, lapply(df$end_time, function(x) { xts::.parseISO8601(format(x, "0001-01-01T%H:%M:%S"))$first.time }))  
 
  df


}

process_intantenous_event <- function(event_info) {

}
