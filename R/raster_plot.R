library(RJSONIO)
library(xts)
library(ggplot2)
library(plyr)
library(scales)
library(grid)

set_up_plot <- function(rd) {
  # Initialize Plot
  plot <- ggplot()#rd$linear$plot_data)

  #rd Set Up Block Heights and Positions
  #rd$blocks <- rd$blocks[rd$blocks$day_s%in%sort(unique(rd$blocks$day_s))[1:1],]
  block_height <- base_height_for_blocks(rd)
  rd$blocks$ymin <- rd$block$position * block_height
  rd$blocks$ymax <- (rd$block$position * block_height) + rd$block$height
  #rd$blocks$color <- paste(rd$blocks$color, "CC", sep="")

  # Set Title
  plot <- plot + ggtitle(rd$title) 

  # Get rid of excess margins for double-plotting
  plot <- plot + theme(panel.margin = unit(0.00, "npc"))

  # Set Up X Axis
  limits <- c(as.POSIXct(strptime("0001-01-01T00:00:00", "%Y-%m-%dT%H:%M:%S")), as.POSIXct(strptime("0001-01-02T00:00:00", "%Y-%m-%dT%H:%M:%S")))
  breaks <- pretty_breaks(8)(limits)
  plot <- plot + scale_x_datetime(breaks=breaks, limits=limits, name="Time of Day", expand=c(0,0), minor_breaks=date_breaks("1 hour"))

  # # Set Up Y Axis
  if(is.null(rd$points)) {
    labels = NULL
    breaks = NULL
    plot <- plot + scale_y_continuous(labels=labels, breaks=breaks, name="")

  } else {
    # limits = c(as.numeric(rd$linear$limits[1]) - 10, as.numeric(rd$linear$limits[2]))
    # # if(length(rd$linear$plot_data$time > 0)) {
    # #   labels = waiver()
    # #   breaks = waiver()
    # # } else {
    # # }
  }

  # Add Rasters for block events
  plot <- plot + geom_rect(aes(NULL, NULL, xmin=start_time, fill=color, xmax=end_time, ymin=ymin, ymax=ymax), data=rd$blocks)

  # Add single timepoints
  #plot <- plot + geom_segment(aes(x=time, xend=time, y=-20, yend=0), data=rd$single_timepoints)

  # Add plots for linear data  
  #plot <- plot + geom_line(aes(time, y_value), data=rd$linear$plot_data)

  # Set Up Faceting by Day and Double Plot
  plot <- plot + facet_grid(day_s ~ double_plot_pos, labeller = format_facet_label) + theme(strip.text.y = element_text(angle=0)) 
  
  # # Set Up Fill Colors
  plot <- plot + scale_fill_identity("Events", labels=rd$blocks$name, breaks=rd$blocks$color, guide="legend")
  
  plot
}


plot_raster <- function(json_path) {
  # Read input and create data frame
  rd <- raster_data(fromJSON(json_path))

  # Initialize Plot
  plot <- set_up_plot(rd)

  # Save Plot
  ggsave(plot=plot, filename=rd$file_name, path=rd$save_path, dpi=80, units="in", width=rd$raster_width, height=((rd$number_of_days + rd$day_height)+1)) 
}


format_facet_label <- function(variable, value) {
  if(variable == 'day_s') format(as.Date(value), format="%a %m/%d") else sprintf("Day %d", value)
}

base_height_for_blocks <- function(df) {
  if(is.null(df$base_block_height)) max(df$blocks$height) else df$base_block_height
}
