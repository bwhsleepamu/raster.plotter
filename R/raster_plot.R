library(RJSONIO)
library(xts)
library(ggplot2)
library(plyr)
library(scales)
library(grid)


set_up_plot <- function(df, title) {
  # Initialize Plot
  plot <- ggplot(data=df)

  # Add Rasters for block events
  plot <- plot + geom_rect(aes(NULL, NULL, xmin=start_time, xmax=end_time, fill=name, ymin=as.numeric(group), ymax=as.numeric(group)+1))
  
  # Get rid of excess margins for double-plotting
  plot <- plot + theme(panel.margin = unit(0, "npc"))

  # Set Title
  plot <- plot + ggtitle(title) 

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

}


plot_raster <- function(json_path) {
  # Read input and create data frame
  json_input <- fromJSON(json_path)
  df <- json_input.read(json_input)

  # Set up day dates for correct faceting
  df$day_s <- do.call(c, lapply(df$day, toString))

  # Initialize Plot
  plot <- set_up_plot(df, json_input$title)

  # Save Plot
  ggsave(plot=plot, filename=json_input$filename, path=json_input$save_path, dpi=80, units="in", width=13, height=8)
  
}
