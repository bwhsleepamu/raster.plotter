library(RJSONIO)
library(xts)
library(ggplot2)
library(plyr)
library(scales)
library(grid)


raster_plot.first_div <- function(name, start_time, end_time, color, group) {
  new_end_time <- xts::.parseISO8601(format(start_time, "%Y-%m-%dT23:59:59"))$last.time
  data.frame(name=name, start_time=start_time, end_time = new_end_time, color=color, group=group)
}

raster_plot.second_div <- function(name, start_time, end_time, color, group) {
  new_start_time <- xts::.parseISO8601(format(end_time, "%Y-%m-%dT00:00:00"))$first.time
  data.frame(name=name, start_time=new_start_time, end_time = end_time, color=color, group=group)
}

json_input.read_single_event <- function(event_info) {
  st <- do.call(c, lapply(event_info$times, function(x) { xts::.parseISO8601(x[1])$first.time }))
  et <- do.call(c, lapply(event_info$times, function(x) { xts::.parseISO8601(x[2])$first.time }))

  df <- data.frame(name=event_info$name, start_time=st, end_time=et, color=event_info$color, group=event_info$group)  

  # Divide into parts that span two days, and those that don't
  non_spanning <- df[format(df$start_time, "%Y%m%d") == format(df$end_time, "%Y%m%d"),]
  spanning <- df[format(df$start_time, "%Y%m%d") != format(df$end_time, "%Y%m%d"),]
  
  first_parts <- mdply(spanning, raster_plot.first_div)
  second_parts <- mdply(spanning, raster_plot.second_div)

  df <- rbind(non_spanning, first_parts, second_parts)
  df$day = as.Date(df$start_time, tz="ETS")

  df$start_time <- do.call(c, lapply(df$start_time, function(x) { xts::.parseISO8601(format(x, "0001-01-01T%H:%M:%S"))$first.time }))  
  df$end_time <- do.call(c, lapply(df$end_time, function(x) { xts::.parseISO8601(format(x, "0001-01-01T%H:%M:%S"))$first.time }))  
 
  df
} 

json_input.read <- function(json_in) {
  Reduce(function(...) merge(..., all=T), lapply(json_in$events, json_input.read_single_event))
}


plot_raster <- function(json_path) {
  # Read input and create data frame
  json_input <- fromJSON(json_path)
  df<-json_input.read(json_input)

  # Set up day dates for correct faceting
  df$day_s <- do.call(c, lapply(df$day, toString))

  # Initialize Plot
  plot <- ggplot(data=df)

  # Add Rasters for events
  plot <- plot + geom_rect(aes(NULL, NULL, xmin=start_time, xmax=end_time, fill=name, ymin=as.numeric(group), ymax=as.numeric(group)+1))
  
  # Get rid of excess margins for double-plotting
  plot <- plot + theme(panel.margin = unit(0, "npc"))

  # Set Title
  plot <- plot + ggtitle(json_input$title) 

  # Set Up X Axis
  limits <- c(as.POSIXct(strptime("0001-01-01T00:00:00", "%Y-%m-%dT%H:%M:%S")), as.POSIXct(strptime("0001-01-02T00:00:00", "%Y-%m-%dT%H:%M:%S")))
  breaks <- pretty_breaks(24)(limits)
  plot <- plot + scale_x_datetime(breaks=breaks, limits=limits, name="Time of Day", expand=c(0,0), minor_breaks=NULL)

  # Set Up Y Axis
  plot <- plot + scale_y_continuous(labels=NULL, breaks=NULL, name="")

  # Set Up Fill Colors
  plot <- plot + scale_fill_manual(values=alpha(levels(df$color), 0.7))

  # Set Up Faceting by Day
  plot <- plot + facet_grid(day_s ~ .)
  
  ggsave(plot=plot, filename=json_input$filename, path=json_input$save_path, dpi=80, units="in", width=13, height=8)
  
}
