library(RJSONIO)
library(xts)
library(ggplot2)
library(plyr)
library(scales)
library(grid)

source("/usr/local/htdocs/raster.plotter/R/raster_data.R")

set_up_plot <- function(rd) {
  # Initialize Plot
  plot <- ggplot(rd$df)

  # Add Rasters for block events
  plot <- plot + geom_rect(aes(NULL, NULL, xmin=start_time, xmax=end_time, fill=name, ymin=(-20 * (group-1)), ymax=((-20 * (group-1)) + 20)), data=rd$blocks)

  # Add single timepoints
  #plot <- plot + geom_segment(aes(x=time, xend=time, y=-20 * (group-1), yend=((-20))), data=rd$single_timepoints)

  # Add plots for linear data  
  #plot <- plot + geom_line(aes(time, y_value), data=rd$linear$plot_data)
  
  # Get rid of excess margins for double-plotting
  plot <- plot + theme(panel.margin = unit(0, "npc"))

  # Set Title
  plot <- plot + ggtitle(rd$title) 

  # Set Up X Axis
  limits <- c(as.POSIXct(strptime("0001-01-01T00:00:00", "%Y-%m-%dT%H:%M:%S")), as.POSIXct(strptime("0001-01-02T00:00:00", "%Y-%m-%dT%H:%M:%S")))
  breaks <- pretty_breaks(24)(limits)
  plot <- plot + scale_x_datetime(breaks=breaks, limits=limits, name="Time of Day", expand=c(0,0), minor_breaks=NULL)

  # Set Up Y Axis
  plot <- plot + scale_y_continuous(labels=NULL, breaks=NULL, name="")

  # Set Up Fill Colors
  plot <- plot + scale_fill_manual(values=alpha(levels(rd$blocks$color), 0.7))

  # Set Up Faceting by Day
  plot <- plot + facet_grid(day_s ~ .) + theme(strip.text.y = element_text(angle=0)) 

}


plot_raster <- function(json_path) {
  # Read input and create data frame
  rd <- raster_data(fromJSON(json_path))

  # Initialize Plot
  plot <- set_up_plot(rd)

  # Save Plot
  #ggsave(plot=plot, filename=json_input$filename, path=json_input$save_path, dpi=80, units="in", width=13, height=8)
  
  plot 
}
