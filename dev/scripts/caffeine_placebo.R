source("/usr/local/htdocs/access/lib/scripts/cycles.R")
source("/usr/local/htdocs/access/lib/scripts/ultradian.R")

subject_list <- list(
  list("1637XX", "/home/pwm4/Windows/idrive/AMU Cleaned Data Sets/1637XX/Sleep/1637XXSlp.01.csv", 42.85),
  list("1819XX", "/home/pwm4/Windows/idrive/AMU Cleaned Data Sets/1819xx/Sleep/1819XXSlp.01.csv", 42.85),
  list("1888XX", "/home/pwm4/Windows/idrive/AMU Cleaned Data Sets/1888xx/Sleep/1888XXSlp.01.csv", 42.85),
  list("18B2XX", "/home/pwm4/Windows/idrive/AMU Cleaned Data Sets/18B2XX/Sleep/18B2xxSlp.01.csv", 42.85),
  list("18E3XX", "/home/pwm4/Windows/idrive/AMU Cleaned Data Sets/18E3xx/Sleep/18E3XXSlp.01.csv", 42.85),
  list("1963XX", "/home/pwm4/Windows/idrive/AMU Cleaned Data Sets/1963xx/Sleep/1963XXSlp.01.csv", 42.85),
  list("1708XX", "/home/pwm4/Windows/idrive/AMU Cleaned Data Sets/1708XX/Sleep/1708xxSlp.01.csv", 42.85),
  list("1772XXT2", "/home/pwm4/Windows/idrive/AMU Cleaned Data Sets/1772XXT2/Sleep/1772XXT2Slp.01.csv", 42.85)
)

map_state_type <- function(x) {
  if (x >= 1 & x <=4) { "SLEEP" }
  else if (x == 5) { "WAKE" }
  else if (x == 6) { "SLEEP" }
  else { "UNDEF" }
}

plot_subject <- function(subject_info) {
  subject_code <- subject_info[[1]]
  subject_path <- subject_info[[2]]
  t_cycle <- subject_info[[3]]

  df <- read.csv(subject_path)
  colnames(df) <- c("subject_code", "sleep_wake_period", "labtime", "stage")
  df$epoch_type <- factor(sapply(df$stage, map_state_type))
  
  plot_periods <- ddply(df, .(sleep_wake_period), labtime_range.f)
  plot_periods <- setup_intervals(plot_periods, t_cycle)
  plot_periods$period_type <- factor(plot_periods$sleep_wake_period < 0, labels=c("sp", "wp")) 
  plot_df <- set_day_labtime.f(df, t_cycle)
  
  # Limit Days
  limited_days <- lapply(list(plot_df, plot_periods), function(x){x$day_number <- x$day_number - 2exi; x[x$day_number > 0,]})
  plot_df <- limited_days[[1]]; plot_periods <- limited_days[[2]]

  # # Double Plot
  plot_periods <- double_plot.f(plot_periods)
  plot_df <- double_plot.f(plot_df)

  .e <- environment()
  plot <- ggplot(plot_df, aes(day_labtime, stage, group=day_number), environment = .e)

  # Title
  plot <- plot + ggtitle(subject_code)  
  # Faceting
  plot <- plot + facet_grid(day_number ~ double_plot_pos)

  # Scaling and Margins
  plot <- plot + theme(panel.margin = unit(0, "npc"))
  plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, t_cycle + epoch_length), expand=c(0,0), name="Time") 
  plot <- plot + scale_y_continuous(limits=c(-2, 10), breaks=NULL, name="")

  # Coloring
  plot <- plot + scale_fill_manual(values=alpha(c("red", "black", "blue", "white", NA), 1.0), name="Legend")
  
  # Sleep / Wake Periods
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime-epoch_length, xmax = end_day_labtime + epoch_length, fill = period_type), ymin = -2, ymax = 0, data = plot_periods)
  # Epochs
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = day_labtime, xmax = day_labtime + epoch_length, fill = epoch_type), ymin = 0, ymax = 10)
  
  ggsave(plot=plot, file=paste("/home/pwm4/Windows/idrive/People/Computer Programmers/Mankowski_P/Emily Rasters/", subject_code, "_raster.pdf", sep=""))#, scale=2, width=40, height=15, limitsize=FALSE)
  ggsave(plot=plot, file=paste("/home/pwm4/Windows/idrive/People/Computer Programmers/Mankowski_P/Emily Rasters/", subject_code, "_raster.png", sep=""))#, scale=2, width=40, height=15, limitsize=FALSE)
}


plot_all_subjects <- function(subject_list) { 
  lapply(subject_list, plot_subject)
}

subject_info <- subject_list[[1]]

