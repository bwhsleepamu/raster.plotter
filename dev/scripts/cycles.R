require("cpm")
library("ggplot2")
library(scales)
library(grid)
library(plyr)

setwd("/usr/local/htdocs/access/lib/data/etl/klerman_nrem_cycles/sleep")
epoch_seconds <- 30
epoch_length <- (epoch_seconds / 3600)

################## Helper Functions ####################
start_end_times <- function(df) { c(min(df$day_labtime), max(df$day_labtime)) }
map_epoch_type <- function(x) {
  if (x >= 1 & x <=4) { "NREM" }
  else if (x == 5) { "WAKE" }
  else if (x == 6) { "REM" }
  else { "UNDEF" }
}

calculate_bouts <- function(V1=NA, V2=NA, df=NA) {
  # get most frequent type
  c(names(sort(-table(df[V1:V2,]$epoch_type)))[1], df$labtime[V1], df$labtime[V2])
}

first_div <- function(x, t_cycle) {
	#c(I(x$bout_type), I(x$start), I(x$end), I(x$start_day_number), I(x$start_day_labtime), I(x$start_day_number), I(24.0-epoch_length))
  c(I(x$sleep_wake_period), I(x$start), I(x$end), I(x$length), I(x$start_day_number), I(x$start_day_labtime), I(x$start_day_number), I(t_cycle-epoch_length))  
}

second_div <- function(x) {
	#c(I(x$bout_type), I(x$start), I(x$end), I(x$end_day_number), I(0.00), I(x$end_day_number), I(as.numeric(x$end_day_labtime)))
  c(I(x$sleep_wake_period), I(x$start), I(x$end), I(x$length), I(x$end_day_number), I(0.00), I(x$end_day_number), I(as.numeric(x$end_day_labtime)))
}

double_plot <- function(df) {
  r_df <- df
  l_df <- df
  
  l_df$double_plot_pos <- 0
  r_df$double_plot_pos <- 1
  r_df$day_number <- r_df$day_number - 1
  
  return(rbind(l_df, r_df))
}

set_up_data_frame <- function(df, t_cycle) {
  df$day_number <- floor(df$labtime / t_cycle)
  df$day_labtime <- (df$labtime - (df$day_number * t_cycle))
  df$day_number <- df$day_number - (min(df$day_number) - 1)
  df$period_type <- factor(df$sleep_wake_period < 0, labels=c("sp", "wp")) 

  # Label NREM, REM, WAKE, UNDEF
  df$epoch_type <- factor(sapply(df$stage, map_epoch_type))
  df
}

get_periods <- function(df) {
  # Get period start and end times
  periods <- ddply(df, .(period_type, day_number, sleep_wake_period, double_plot_pos), start_end_times)
  colnames(periods) <- c("period_type", "day_number", "sleep_or_wake_period", "start", "end")
  periods
}

setup_intervals <- function(df, t_cycle) {
  
  # set day numbers + labtimes
  df$start_day_number <- floor(df$start / t_cycle)
  min_day_num <- min(df$start_day_number)  
  df$start_day_labtime <- (df$start - (df$start_day_number * t_cycle))
  df$start_day_number <- df$start_day_number - (min_day_num - 1)
  df$end_day_number <- floor(df$end / t_cycle)
  df$end_day_labtime <- (df$end - (df$end_day_number * t_cycle))
  df$end_day_number <- df$end_day_number - (min_day_num - 1)

  clean <- df[!df$start_day_number != df$end_day_number,]
  
  # clean bouts that span across days
  to_clean <- df[df$start_day_number != df$end_day_number,]
  # deal with bouts that start and end with day
  first_cleaned <- ddply(to_clean, .(start_day_number), first_div, t_cycle)
  second_cleaned <- ddply(to_clean, .(start_day_number), second_div)
  first_cleaned <- first_cleaned[, -1]
  second_cleaned <- second_cleaned[, -1]
  colnames(first_cleaned) <- colnames(clean)
  colnames(second_cleaned) <- colnames(clean)
  
  clean <- rbind(first_cleaned, second_cleaned, clean) #rbind(clean, first_cleaned, second_cleaned)
  # make numeric again =/
  # clean$start_day_number <- as.numeric(as.character(clean$start_day_number))
  clean$day_number <- clean$start_day_number
  # clean$day_number <- as.numeric(clean$day_number)
  # #clean$start <- as.numeric(clean$start)
  # #clean$end <- as.numeric(clean$end)
  # clean$start_day_labtime <- as.numeric(as.character(clean$start_day_labtime))
  # clean$end_day_labtime <- as.numeric(as.character(clean$end_day_labtime))
  clean
  #list(clean, first_cleaned, second_cleaned)
}
#########################################################
main_f <- function() {
  df <- read.csv("3232GXSlp.01.csv")
  colnames(df) <- c("subject_code", "sleep_wake_period", "labtime", "stage")
  odf <- df

  # Limit scope for now
  #df <- df[1:10000,]

  df <- set_up_data_frame(df)
  
  ####### Get Changepoints + Bouts
  cpm_res <- processStream(df$epoch_type, cpmType="Mann-Whitney", ARL0=10000, startup=20)
  changepoint_rows <- df[cpm_res$changePoints,]
  chunks <- as.data.frame(cbind(c(1, cpm_res$changePoints), c(cpm_res$changePoints, nrow(df))))
  bouts <- mdply(chunks, calculate_bouts, df=df)
  colnames(bouts) <- c("bout_type", "start", "end")
  # make numeric
  bouts$start <- as.numeric(bouts$start)
  bouts$end <- as.numeric(bouts$end)
  # set day numbers + labtimes
  bouts$start_day_number <- floor(bouts$start / 24)
  bouts$start_day_labtime <- (bouts$start - (bouts$start_day_number * 24))
  bouts$start_day_number <- bouts$start_day_number - (min_day_num - 1)
  bouts$end_day_number <- floor(bouts$end / 24)
  bouts$end_day_labtime <- (bouts$end - (bouts$end_day_number * 24))
  bouts$end_day_number <- bouts$end_day_number - (min_day_num - 1)
  # clean bouts that span across days
  clean <- bouts[!bouts$start_day_number != bouts$end_day_number,]
  to_clean <- bouts[bouts$start_day_number != bouts$end_day_number,]
  first_cleaned <- ddply(to_clean, .(start_day_number), first_div)
  second_cleaned <- ddply(to_clean, .(start_day_number), second_div)
  first_cleaned <- first_cleaned[, -1]
  second_cleaned <- second_cleaned[, -1]
  colnames(first_cleaned) <- colnames(clean)
  colnames(second_cleaned) <- colnames(clean)
  clean <- rbind(first_cleaned, second_cleaned, clean) #rbind(clean, first_cleaned, second_cleaned)
  # make numeric again =/
  # clean$start_day_number <- as.numeric(as.character(clean$start_day_number))
  clean$day_number <- clean$start_day_number
  clean$day_number <- as.numeric(clean$day_number)
  #clean$start <- as.numeric(clean$start)
  #clean$end <- as.numeric(clean$end)
  clean$start_day_labtime <- as.numeric(as.character(clean$start_day_labtime))
  clean$end_day_labtime <- as.numeric(as.character(clean$end_day_labtime))
  bouts <- clean

  # Double-Plot
  df <- double_plot(df)
  periods <- double_plot(periods)
  bouts <- double_plot(bouts)
  changepoint_rows <- double_plot(changepoint_rows)

  # Draw
  .e <- environment()
  # Main Plot
  plot <- ggplot(df, aes(day_labtime, stage, group=day_number), environment = .e)
  # Faceting
  plot <- plot + facet_grid(day_number ~ double_plot_pos)

  # Scaling and Margins
  plot <- plot + theme(panel.margin = unit(0, "npc"))
  plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, 24 + epoch_length), expand=c(0,0)) 
  plot <- plot + scale_y_continuous(limits=c(-2, 10))

  # Sleep/Wake Periods
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start-epoch_length, xmax = end + epoch_length, fill = period_type), ymin = -2, ymax = -1, data = periods)
  # Bouts
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = bout_type), ymin = 0, ymax = 10, data = bouts)
  # Changepoints
  #plot <- plot + geom_segment(aes(x = day_labtime, xend = day_labtime), y=0, yend=10, color="red", width=.2, data=changepoint_rows)
  # Stages
  plot <- plot + geom_point(aes(group=day_number), shape='.')

  # Coloring
  plot <- plot + scale_fill_manual(values=alpha(c("blue", "red", "black", "purple", "green", "yellow"), 0.8))
  plot <- plot + geom_vline(xintercept=c(0 - epoch_length, 24 + epoch_length), size=1)


  #ggsave(plot=plot, file="/home/pwm4/Documents/cpm_raster.pdf", scale=2, width=15, height=40, limitsize=FALSE)

}
