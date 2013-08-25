library("ggplot2")
library(scales)
library(grid)
library(plyr)

setwd("/usr/local/htdocs/access/lib/data/etl/klerman_nrem_cycles/cycles/")

start_end_times <- function(df) { c(min(df$day_labtime), max(df$day_labtime)) }
epoch_length <- (30.0 / 3600)

load_data <- function(df_path) {
  df <- read.csv(df_path)
  df$day_number <- floor(df$labtime / 24)
  df$day_labtime <- (df$labtime - (df$day_number * 24))
  df$day_number <- df$day_number - (min(df$day_number) - 1)
  df$period_type <- factor(df$sleep_or_wake_period < 0, labels=c("sp", "wp")) 
  df$bout_type[df$bout_type == "wake" & df$period_type == "wp"] <- NA
  return(df)
}       

create_ss <- function(df) {
  ss <- ddply(df, .(nrem_cycle, day_number, sleep_or_wake_period), start_end_times)
  ss <- subset(ss, nrem_cycle != 0)
  colnames(ss) <- c("nrem_cycle", "day_number", "sleep_or_wake_period", "start", "end")
  return(ss)
}

setup_data <- function(subject_code) {
  bout_path = paste(subject_code, "bouts.csv", sep="_")
  iter_path = paste(subject_code, "iter.csv", sep="_")
    
  bout <- load_data(bout_path)
  iter <- load_data(iter_path)
  
  bound_bouts <- cbind(as.character(bout$bout_type), as.character(iter$bout_type))
  bout$bout_diff <- apply(bound_bouts, 1, function(x) ifelse(x[1]==x[2], NA, x[1]))
  iter$bout_diff <- apply(bound_bouts, 1, function(x) ifelse(x[1]==x[2], NA, x[2]))
  
  bout_ss <- create_ss(bout)
  iter_ss <- create_ss(iter)
 
  return(list(iter=list(df=iter, ss=iter_ss), bout=list(df=bout, ss=bout_ss), subject_code=subject_code, days=max(bout$day_number)))
}

double_plot <- function(df) {
  r_df <- df
  l_df <- df
  
  l_df$position <- 0
  r_df$position <- 1
  r_df$day_number <- r_df$day_number - 1
  
  return(rbind(l_df, r_df))
}

limit_days <- function(df, days) {
  if(!is.null(days)) {
    return(rbind(subset(df, (position == 0 & day_number %in% days)), subset(df, (position == 1 & (day_number + 1) %in% days))))
  }
  else {
    return(df)
  }
}

plot_data <- function(data, days=NULL, compare=FALSE, type="bout") {
  iter_df <- limit_days(double_plot(data$iter$df), days)
  bout_df <- limit_days(double_plot(data$bout$df), days)
  iter_ss <- limit_days(double_plot(data$iter$ss), days)
  bout_ss <- limit_days(double_plot(data$bout$ss), days)
  # TOP IS ITERATIVE!!!
  
  .e <- environment()
  plot <- ggplot(bout_df, aes(day_labtime, stage, group=day_number), environment = .e)
  
  # Cycles
  if(type=="bout" || compare) {
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = factor(nrem_cycle %% 2)), ymin = ifelse(compare, -3, -2), ymax = ifelse(compare, -2, -1), data = bout_ss)
    plot <- plot + geom_text(aes(x=((start+end)/2), y=ifelse(compare, -2.5, -1.5), label=nrem_cycle), data = bout_ss)
  }
  if(type=="iter" || compare) {
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = factor(nrem_cycle %% 2)), ymin = -2, ymax = -1, data = iter_ss)
    plot <- plot + geom_text(aes(x=((start+end)/2), y = -1.5, label=nrem_cycle), data = iter_ss)
  }
  
  # Bouts
  if(type=="bout" || compare) {
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = day_labtime, xmax = day_labtime + epoch_length, fill = bout_type), ymin = 1, ymax = ifelse(compare, 4.5, 9), data=bout_df)
  }
  if(type=="iter" || compare) {
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = day_labtime, xmax = day_labtime + epoch_length, fill = bout_type), ymin = ifelse(compare, 5.5, 1), ymax = 9, data=iter_df)
  }
  if(compare) {
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = day_labtime, xmax = day_labtime + epoch_length, fill = bout_diff), ymin = 4.5, ymax = 5, data=bout_df)
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = day_labtime, xmax = day_labtime + epoch_length, fill = bout_diff), ymin = 5, ymax = 5.5, data=iter_df)
  }
    
  # Periods
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = day_labtime, xmax = day_labtime + epoch_length, fill = period_type), ymin = -1, ymax = 0)
  
  # Stages
  plot <- plot + geom_line(aes(group=sleep_or_wake_period), size=.2)

  # Facets and Coloring
  plot <- plot + facet_grid(day_number ~ position)
  plot <- plot + scale_fill_manual(values=alpha(c("0"="purple", "1"="yellow", "nrem"="blue", "rem"="red", "sp"="black", "wake"="green", "wp"="white"), 0.8))

  plot <- plot + theme(panel.margin = unit(0, "npc")) + scale_x_continuous(limits=c(0, 24), expand=c(0,0)) + geom_vline(xintercept=c(0,24))
  
  
  return(plot)
}

plot_all_subjects <- function() {
  subjects <- c("26N2GXT2", "26O2GXT2", "27D9GX", "27Q9GX", "1708XX", "1920MX", "2071DX", "2123W", "2419HM", "2823GX", "2844GX", "3232GX")

  sapply(subjects, plot_subject)  
}

plot_subject <- function(subject_code) {
  d <- setup_data(subject_code)
  
  bout_plot <- plot_data(d, type="bout")
  iter_plot <- plot_data(d, type="iter")
  compare_plot <- plot_data(d, compare=TRUE)
  
  #plot
  ggsave(plot=compare_plot, file=paste("Rasters/", d$subject_code, "_compare.pdf", sep=""), scale=2, width=15, height=40, limitsize=FALSE)
  ggsave(plot=iter_plot, file=paste("Rasters/", d$subject_code, "_iter.pdf", sep=""), scale=2, width=15, height=40, limitsize=FALSE)
  ggsave(plot=bout_plot, file=paste("Rasters/", d$subject_code, "_bout.pdf", sep=""), scale=2, width=15, height=40, limitsize=FALSE)  
}

stats <- read.csv("stats.csv", header=FALSE)
colnames(stats) <- c("subject_code", "method", "nrem", "rem", "wake")

nrem_t <- t.test(stats$nrem[stats$method == "bout"], stats$nrem[stats$method == "iter"], paired=TRUE)
rem_t <- t.test(stats$rem[stats$method == "bout"], stats$rem[stats$method == "iter"], paired=TRUE)
wake_t <- t.test(stats$wake[stats$method == "bout"], stats$wake[stats$method == "iter"], paired=TRUE)

nrem_w <- wilcox.test(stats$nrem[stats$method == "bout"], stats$nrem[stats$method == "iter"], paired=TRUE)
rem_w <- wilcox.test(stats$rem[stats$method == "bout"], stats$rem[stats$method == "iter"], paired=TRUE)
wake_w <- wilcox.test(stats$wake[stats$method == "bout"], stats$wake[stats$method == "iter"], paired=TRUE)

#d <- setup_data("3232GX")
#plot <- plot_data(d, compare=TRUE)
#plot
#ggsave(plot=plot, file=paste(d$subject_code, ".pdf", sep=""), scale=2, width=15, height=40, limitsize=FALSE)

