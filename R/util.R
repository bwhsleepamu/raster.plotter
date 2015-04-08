set_days <- function(labtimes, t_cycle=T_CYCLE) {
  day_numbers <- floor(labtimes / t_cycle)
  day_labtimes <- (labtimes - (day_numbers * t_cycle))
  
  list(day_numbers, day_labtimes)
}



split_day_spanning_blocks <- function(dt, t_cycle=T_CYCLE, epoch_length=EPOCH_LENGTH){
  first_division <- dt
  second_division <- copy(dt)
  
  new_end_day_labtime <- t_cycle-epoch_length
  
  first_division[,`:=`(end_day_number=start_day_number, end_labtime=new_end_day_labtime)]
  second_division[,`:=`(start_day_number=end_day_number, start_labtime=0)]
  
  rbindlist(list(first_division, second_division))
}


# Double-Plotting
double_plot <- function(df, should_do_it) {
  ### DANGER ###
  # This function changes the date of the right double-plot, causing the actual dates to not match up with the times. 
  # The correct date for a given set of times is (day + double_plot_pos)
  ###
  if(should_do_it) { 
    r_df <- df
    l_df <- df
    
    l_df$double_plot_pos <- 0
    r_df$double_plot_pos <- 1
    r_df$day <- r_df$day - 1
    if(!is.null(r_df$day_s))
      r_df$day_s <- format(r_df$day, format="%Y-%m-%d")
    return(rbind(l_df, r_df))
  } else {
    df$double_plot_pos <- 0
    df
  }
}