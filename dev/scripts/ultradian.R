require('signal')

source("/usr/local/htdocs/access/lib/scripts/cycles.R")

odf <- read.csv("2065DXSlp.01.csv")
colnames(odf) <- c("subject_code", "labtime_hour", "labtime_min", "labtime_sec", "labtime_year", "labtime", "realtime", "sleep_wake_period", "stage")


form.f <- function(x) {
	#x$nrem <- cumsum(ifelse(is.element(x$stage, c(1,2,3,4)), epoch_weight, -epoch_weight))
	#resid(lm(nrem ~ labtime, data=x))

}

cumsum.f <- function(x) {
	res <- cbind(cumsum(x$rem), cumsum(x$nrem), cumsum(x$wake))
	colnames(res) <- c("rem", "nrem", "wake")
	res
}

residual.f <- function(x) {
	res <- cbind(resid(lm(rem ~ labtime, data=x)), resid(lm(nrem ~ labtime, data=x)), resid(lm(wake ~ labtime, data=x)))
	colnames(res) <- c("rem_resid", "nrem_resid", "wake_resid")
	res
}

lm.f <- function(x) {
	rem.lm <- lm(rem ~ labtime, data=x) 
	nrem.lm <- lm(nrem ~ labtime, data=x)
	wake.lm <- lm(wake ~ labtime, data=x)
 	res <- cbind(rem.lm$coefficients[1], rem.lm$coefficients[2], nrem.lm$coefficients[1], nrem.lm$coefficients[2], wake.lm$coefficients[1], wake.lm$coefficients[2])
	colnames(res) <- c("rem_b", "rem_a", "nrem_b", "nrem_a", "wake_b", "wake_a")
	res
}

labtime_range.f <- function(x) {
	labtime_range <- range(x$labtime)
	res <- cbind(labtime_range[1], labtime_range[2], labtime_range[2]-labtime_range[1])
	colnames(res) <- c("start", "end", "length")
	res
}

day_labtime_range.f <- function(x) {
 	res <- cbind(min(x$day_labtime), max(x$day_labtime)) 
 	colnames(res) <- c("start", "end")
 	res
 }


double_plot.f <- function(df) {
  r_df <- df
  l_df <- df
  
  l_df$double_plot_pos <- 0
  r_df$double_plot_pos <- 1
  r_df$day_number <- r_df$day_number - 1
  
  return(rbind(l_df, r_df))
}

set_day_labtime.f <- function(df, t_cycle) {
	df$day_number <- floor(df$labtime / t_cycle)
  	min_day_num <- min(df$day_number)
  	df$day_labtime <- (df$labtime - (df$day_number * t_cycle))
  	df$day_number <- df$day_number - (min(df$day_number) - 1)
  	df
}

setup_for_plot.f <- function(df, t_cycle) {
	double_plot.f(set_day_labtime.f(df, t_cycle))	
}

main <- function() {
  epoch_weight = epoch_seconds / 60
  nyquist_f <- 1/60
  cutoff = 1/7200
  W = 1/120
  bf <- butter(1, W, "high", "z")


  ## FILL IN MISSING INFO (code as wake)
  # odf$labtime_in_seconds <- odf$labtime_hour * 3600 + odf$labtime_min * 60 + odf$labtime_sec
  # labtime_range <- range(odf$labtime_in_seconds)
  # full_labtimes <- seq(labtime_range[1], labtime_range[2], by=epoch_seconds)

  odf <- odf[odf$sleep_wake_period > 0,]


  sp <- odf

  ## SLEEP PERIOD INTERVALS
  sleep_periods <- ddply(sp, .(sleep_wake_period), labtime_range.f)

  #sp = odf #odf[abs(odf$sleep_wake_period) > 9 & abs(odf$sleep_wake_period) < 34,] #odf[odf$sleep_wake_period == 5,]
  ## CUMULATIVE BY SLEEP PERIOD
  sp$rem <- ifelse(sp$stage == 6, epoch_weight, -epoch_weight)
  sp$nrem <- ifelse(is.element(sp$stage, c(1,2,3,4)), epoch_weight, -epoch_weight)
  sp$wake <- ifelse(sp$stage == 5, epoch_weight, -epoch_weight)

  cumulative_results <- ddply(sp, .(sleep_wake_period), cumsum.f)
  sp$rem <- cumulative_results$rem
  sp$nrem <- cumulative_results$nrem
  sp$wake <- cumulative_results$wake

  ## RESIDUALS
  best_fit_lines <- ddply(sp, .(sleep_wake_period), lm.f)
  residual_results <- ddply(sp, .(sleep_wake_period), residual.f)
  residual_results$sleep_wake_period <- NULL
  sp <- cbind(sp, residual_results)

  ## PLOTTING
  ### Setup
  plot_df <- setup_for_plot.f(sp[5000:11000,])
  ult_plot <- ggplot(plot_df, aes(day_labtime, nrem_resid, group=day_number))

  ### Coloring
  ult_plot <- ult_plot + scale_fill_manual(values=alpha(c("black", "yellow", "blue", "red", "black", "purple", "green", "yellow"), 0.2))

  ### Faceting
  ult_plot <- ult_plot + facet_grid(day_number ~ double_plot_pos)

  ### Scaling
  ult_plot <- ult_plot + theme(panel.margin = unit(0, "npc"))
  ult_plot <- ult_plot + scale_x_continuous(limits=c(0 - epoch_length, 24 + epoch_length), expand=c(0,0)) 

  ## Raw Cumulative Sums
  # ult_plot <- ult_plot + geom_point(aes(labtime, rem), color="blue", shape='.')
  # ult_plot <- ult_plot + geom_point(aes(labtime, nrem), color="red", shape='.')
  # ult_plot <- ult_plot + geom_point(aes(labtime, wake), color="green", shape='.')



  ### Residuals
  # ult_plot <- ult_plot + geom_line(aes(labtime, rem_resid), color="blue")
  # ult_plot <- ult_plot + geom_line(aes(labtime, nrem_resid), color="red")
  # ult_plot <- ult_plot + geom_line(aes(labtime, wake_resid), color="green")

  ult_plot <- ult_plot + geom_point(aes(day_labtime, rem_resid), color="red", shape='.')
  ult_plot <- ult_plot + geom_point(aes(day_labtime, nrem_resid), color="blue", shape='.')
  ult_plot <- ult_plot + geom_point(aes(day_labtime, wake_resid), color="green", shape='.')

  # ult_plot <- ult_plot + geom_point(aes(day_labtime, filter(bf, rem_resid)), color="blue", shape='.')
  # ult_plot <- ult_plot + geom_point(aes(day_labtime, filter(bf, nrem_resid)), color="red", shape='.')
  # ult_plot <- ult_plot + geom_point(aes(day_labtime, filter(bf, wake_resid)), color="green", shape='.')


  ### Sleep-wake periods
  #divided_sleep_periods <- ddply(plot_df, .(day_number, sleep_wake_period), day_labtime_range.f)
  #ult_plot <- ult_plot + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = (sleep_wake_period < 0)), ymin = -10000, ymax = 10000, data=divided_sleep_periods)


  ######
  # form.baseline.rem <- sapply(4:17, function(x){sp$rem_resid[sp$sleep_wake_period == x]})
  # form.baseline.wake <- sapply(4:17, function(x){sp$wake_resid[sp$sleep_wake_period == x]})

  #form.baseline <- sp[sp$sleep_wake_period <= 3,]
  form.main <- sp[sp$sleep_wake_period >= 6 & sp$sleep_wake_period <= 17,]
  #form.recovery <- sp[sp$sleep_wake_period >= 18,]

  ## FORM ESTIMATE
  form.baseline.nrem <- sapply(4:17, function(x){filter(bf, sp$nrem_resid[sp$sleep_wake_period == x])})
  min.length <- min(sapply(form.baseline.nrem, length))
  form.baseline.nrem <- as.matrix(sapply(form.baseline.nrem, function(x) x[1:min.length]))
  form.wave <- apply(form.baseline.nrem, 1, mean)

  #form.resid <- daply(form.baseline, .(sleep_wake_period), form.f)

  # Filtering


  # filtered.results <- filter(bf, sp$nrem_resid)


  #acf(f_res, lag.max=length(sp$nrem_resid))
  #acf(filtered.results, lag.max=5760)

  #form.est <- form.est[abs(form.est$sleep_wake_period) > 9 & abs(form.est$sleep_wake_period) < 34,]


  # form.resid <- daply(form.est, .(sleep_wake_period), form.f)
  # form.wave <- apply(form.resid, 2, mean)

  # ult_plot <- ult_plot + geom_line(aes(labtime, f_res))
  # ult_plot <- ult_plot + geom_line(aes(labtime, filtered.results[1:10000]), color="brown")
  # ult_plot <- ggplot(form.est, aes(labtime, nrem_resid))


}
