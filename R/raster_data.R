

first_div <- function(name, start_time, end_time, color, group) {
  new_end_time <- read_iso_time(format(start_time, "%Y-%m-%dT23:59:59"))
  data.frame(name=name, start_time=start_time, end_time = new_end_time, color=color, group=group)
}

middle_div <- function(name, start_time, end_time, color, group) {
  secs_in_day <- 3600*24
  new_end_time <- read_iso_time(format(start_time, "%Y-%m-%dT23:59:59"))
  new_start_time <- read_iso_time(format(end_time, "%Y-%m-%dT00:00:00"))
  
  gap_size <- as.numeric(round(difftime(new_start_time, new_end_time, units="days")))


  if(gap_size > 0) {
    sts <- vector()
    ets <- vector()
    for(day_num in 1:gap_size) {
      ref_time <- start_time+(secs_in_day*day_num)
      sts <- append(sts, read_iso_time(format(ref_time, "%Y-%m-%dT00:00:00")))
      ets <- append(sts, read_iso_time(format(ref_time, "%Y-%m-%dT23:59:59")))
    }
    data.frame(name=name, start_time=sts, end_time=ets, color=color, group=group) 
  } else {
    NULL
  }
}

last_div <- function(name, start_time, end_time, color, group) {
  new_start_time <- read_iso_time(format(end_time, "%Y-%m-%dT00:00:00"))
  data.frame(name=name, start_time=new_start_time, end_time = end_time, color=color, group=group)
}




raster_data <- function(x, ...) UseMethod("raster_data")

raster_data.default <- function(input_data, ...) {
}

raster_data.list <- function(input_list, ...) {
  rd <- list(title=input_list$title, save_path=input_list$save_path, file_name=input_list$filename, t_cycle=input_list$t_cycle, timescale=input_list$timescale)
  
  # Break up by event type
  e <- input_list$events

  # Process single timepoints
  single_e <- e[vapply(e, function(x) x$type == "single", FALSE)]
  rd$single_timepoints <- Reduce(function(...) merge(..., all=T), lapply(single_e, process_single_timepoint_event))   

  # Process Block Events
  block_e <- e[vapply(e, function(x) x$type == "block", FALSE)]
  rd$blocks <- Reduce(function(...) merge(..., all=T), lapply(block_e, process_block_event))   

  # Process Linear Events
  linear_e <- e[vapply(e, function(x) x$type == "linear", FALSE)]
  rd$linear <- process_linear_event(linear_e[[1]])
  
  rd$number_of_days <- length(unique(c(rd$single_timepoints$day_s, rd$blocks$day_s, rd$linear$plot_data$day_s)))

  rd
}


process_block_event <- function(event_info) {

  # if(event_info$class == "start_end") {
  df <- process_start_end_block_event(event_info)
  # } else if (event_info$class == "duration") {
  #   df <- process_block_duration_event(event_info)
  # } 

  # Divide into parts that span two days, and those that don't
  non_spanning <- df[format(df$start_time, "%Y%m%d") == format(df$end_time, "%Y%m%d"),]
  spanning <- df[format(df$start_time, "%Y%m%d") != format(df$end_time, "%Y%m%d"),]
  
  first_parts <- mdply(spanning, first_div)
  middle_parts <- mdply(spanning, middle_div)
  last_parts <- mdply(spanning, last_div)

  df <- rbind(non_spanning, first_parts, middle_parts, last_parts)

  df$day = do.call(c, lapply(df$start_time, function(x) { as.Date(toString(x)) }))

  df$day_s <- do.call(c, lapply(df$day, toString))

  df$start_time <- do.call(c, lapply(df$start_time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  
  df$end_time <- do.call(c, lapply(df$end_time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  
 
  df
} 


process_start_end_block_event <- function(event_info) {
  st <- do.call(c, lapply(event_info$blocks, function(x) read_iso_time(x[1]) ))
  et <- do.call(c, lapply(event_info$blocks, function(x) read_iso_time(x[2]) ))

  if(is.null(event_info$color)) {
    colors <- do.call(c, lapply(event_info$blocks, function(x) x[3] ))
  } else {
    colors <- event_info$color
  }

  df <- data.frame(name=event_info$name, start_time=st, end_time=et, color=colors)
 
  df
}

read_iso_time <- function(x) {
  YYYY <- substr(x, 1, 4)
  MM <- substr(x, 6, 7)
  DD <- substr(x, 9, 10)
  H <- substr(x, 12, 13)
  M <- substr(x, 15, 16)
  S <- substr(x, 18, 19)
  tz="EST"


  f <- do.call(firstof, as.list(c(as.numeric(YYYY), as.numeric(MM), as.numeric(DD), as.numeric(H), as.numeric(M), as.numeric(S), tz)))
#  t <- do.call(firstof, as.list(c(1, 1, 1, as.numeric(H), as.numeric(M), as.numeric(S), tz)))
  #as.POSIXct(s)
 # data.frame(full_time=s, date=sprintf("%s-%s-%s", YYYY, MM, DD), time=t)
  f
}

function (x, start, end, tz = "") 
{
    as_numeric <- function(.x) {
        if (gsub(" ", "", .x) == "") 
            NULL
        else as.numeric(.x)
    }
    x <- gsub("NOW", format(Sys.time(), "%Y%m%dT%H%M%S"), x)
    x <- gsub("TODAY", format(Sys.Date(), "%Y%m%d"), x)
    if (identical(grep("/|(--)|(::)", x), integer(0))) {
        x <- paste(x, x, sep = "/")
    }
    intervals <- unlist(strsplit(x, "/|(--)|(::)"))
    DURATION <- ""
    if (length(intervals) == 2L) {
        if (substr(intervals[1], 0, 1) == "P") {
            DURATION <- intervals[1]
            DURATION_LHS <- TRUE
            intervals[1] <- ""
        }
        if (substr(intervals[2], 0, 1) == "P") {
            DURATION <- intervals[2]
            DURATION_LHS <- FALSE
            intervals <- intervals[1]
        }
    }
    parse.side <- function(x, startof) {
        if (is.na(x) || !nzchar(x)) 
            return(c(NULL))
        basic <- gsub(":|-", "", x, perl = TRUE)
        date.time <- unlist(strsplit(basic, " |T"))
        date <- date.time[1]
        if (!missing(startof) && nchar(basic) == 2L) {
            startof <- gsub(":|-", "", startof, perl = TRUE)
            if (nchar(startof) - nchar(date) >= 4) {
                sstartof <- substr(startof, 0, nchar(startof) - 
                  nchar(date))
                date <- paste(sstartof, date, sep = "")
            }
        }
        date <- sprintf("%-8s", date)
        YYYY <- substr(date, 0, 4)
        MM <- substr(date, 5, 6)
        DD <- substr(date, 7, 8)
        time <- date.time[2]
        if (!is.na(time)) {
            time <- sprintf("%-6s", time)
            H <- substr(time, 0, 2)
            M <- substr(time, 3, 4)
            S <- substr(time, 5, 10000L)
        }
        else H <- M <- S <- ""
        c(as.list(c(year = as_numeric(YYYY), mon = as_numeric(MM), 
            day = as_numeric(DD), hour = as_numeric(H), min = as_numeric(M), 
            sec = as_numeric(S))), tz = tz)
    }
    s <- e <- NA
    if (nzchar(intervals[1])) 
        s <- as.POSIXlt(do.call(firstof, parse.side(intervals[1])))
    if (length(intervals) == 2L) {
        e <- as.POSIXlt(do.call(lastof, parse.side(intervals[2], 
            intervals[1])))
        if (is.na(e)) 
            e <- as.POSIXlt(do.call(lastof, parse.side(intervals[2])))
    }
    if (!missing(start)) {
        start <- as.numeric(start)
        s <- as.POSIXlt(.POSIXct(max(start, as.numeric(s), na.rm = TRUE), 
            tz = tz))
    }
    if (!missing(end)) {
        end <- as.numeric(end)
        e <- as.POSIXlt(.POSIXct(min(end, as.numeric(e), na.rm = TRUE), 
            tz = tz))
    }
    if (nzchar(DURATION)) {
        parse_duration <- function(P) {
            P <- gsub("P", "", P)
            P <- gsub("T(.*)M", "\\1m", P)
            n <- unlist(strsplit(P, "[[:alpha:]]"))
            d <- unlist(strsplit(gsub("[[:digit:]]", "", P), 
                ""))
            dur.vec <- list(as.numeric(n), unname(c(Y = 6, M = 5, 
                D = 4, H = 3, m = 2, S = 1)[d]))
            init.vec <- rep(0, 9)
            init.vec[dur.vec[[2]]] <- dur.vec[[1]]
            init.vec
        }
        if (DURATION_LHS) {
            s <- as.POSIXct(structure(as.list(mapply(`-`, e, 
                parse_duration(DURATION))), class = c("POSIXt", 
                "POSIXlt"), tzone = attr(e, "tzone")))
        }
        else {
            e <- as.POSIXct(structure(as.list(mapply(`+`, s, 
                parse_duration(DURATION))), class = c("POSIXt", 
                "POSIXlt"), tzone = attr(e, "tzone")))
        }
    }
    list(first.time = as.POSIXct(s), last.time = as.POSIXct(e))
}



process_linear_data <- function(x) {
  time <- x[[1]]
  val <- x[[2]]

  YYYY <- substr(time, 1, 4)
  MM <- substr(time, 6, 7)
  DD <- substr(time, 9, 10)
  H <- substr(time, 12, 13)
  M <- substr(time, 15, 16)
  S <- substr(time, 18, 19)
  tz="EST"

  #f <- do.call(firstof, as.list(c(as.numeric(YYYY), as.numeric(MM), as.numeric(DD), as.numeric(H), as.numeric(M), as.numeric(S), tz)))
  t <- do.call(firstof, as.list(c(1, 1, 1, as.numeric(H), as.numeric(M), as.numeric(S), tz)))
  ds <- sprintf("%s-%s-%s", YYYY, MM, DD)
  d <- as.Date.character(ds, format="%Y-%m-%d")
  v <- as.numeric(val)

  


  #list(time=(if(length(t) != 1) NA else t), date=(if(length(d) != 1) NA else d), date_s=(if(length(ds) != 1) NA else ds), y_value=(if(length(v) != 1) NA else v))
  list(t, ds, d, v)
}

process_single_timepoint_event <- function(event_info) {
  t <- do.call(c, lapply(event_info$times, read_iso_time ))    

  df <- data.frame(name=event_info$name, time=t, color=event_info$color, group=event_info$group)

  df$day = as.Date(df$time, tz="EST")
  
  df$day_s <- do.call(c, lapply(df$day, toString))
  df$time <- do.call(c, lapply(df$time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  

  df
}


process_linear_event <- function(event_info) {

  linear_event_info <- list(name=event_info$name, limits=event_info$limits, color=event_info$color)#, limits=event_info$limits)
  
  # linear_event_info$plot_data <- data.frame(matrix(unlist(event_info$points), ncol=2, byrow=TRUE))
  # colnames(linear_event_info$plot_data) <-  c("time", "y_value")
  # linear_event_info$plot_data <- mdply(linear_event_reainfo$plot_data, function(time, y_value) { data.frame(time=read_iso_time(time), y_value=as.numeric(y_value)) })

  #x_times <- do.call(c, lapply(event_info$points, function(x) read_iso_time(x[1]) ))
  # y_vals <- do.call(c, lapply(event_info$points, function(x) {as.numeric(x[2])}))
  #y_vals <- unlist(lapply(event_info$points, function(x) x[2]))


  #linear_event_info$plot_data <- data.frame(time=x_times, y_value=y_vals)

  #linear_event_info$plot_data$day <- as.Date(linear_event_info$plot_data$time, tz="EST")

  #linear_event_info$plot_data$day_s <- do.call(c, lapply(linear_event_info$plot_data$day, toString))

  #linear_event_info$plot_data$time <- do.call(c, lapply(linear_event_info$plot_data$time, function(x) { read_iso_time(format(x, "0001-01-01T%H:%M:%S")) }))  
  

  linear_event_info$plot_data <- data.frame(do.call(rbind, lapply(event_info$points, process_linear_data)))
  colnames(linear_event_info$plot_data) <- c(day, day_s, time, y_value)
}

## Labtime Addition


process_start_end_block_event.labtime <- function(event_info) {
  st <- do.call(c, lapply(event_info$times, function(x) { read_iso_time(x[1]) }))
  et <- do.call(c, lapply(event_info$times, function(x) { read_iso_time(x[2]) }))

  df <- data.frame(name=event_info$name, start_time=st, end_time=et, color=event_info$color, group=event_info$group)  
 
  df
}

process_block_event.labtime <- function(event_info) {
  df <- process_start_end_block_event.labtime(event_info)
}



## Double-Plotting


