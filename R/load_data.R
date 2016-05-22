load_sleep_data <- function(subjects) {
  sleep_data <- rbindlist(lapply(subjects$file_path, load_sleep_file), fill=TRUE)
  
  setnames(sleep_data, c('subject_code', 'activity_or_bedrest_episode', 'labtime', 'stage'))
  setkey(sleep_data, subject_code, labtime)
  # Generate row indeces
  sleep_data[, pk:=.I]
  # Map stages to epoch types
  sleep_data[,epoch_type:=as.factor(as.character(lapply(stage, map_epoch_type))),]
  sleep_data
}


# Load a single sleep file
load_sleep_file <- function(file_path) {  
  use <- TRUE
  results <- data.table(character())
  
  if(file.exists(file_path)) {
    tryCatch(
      results<-fread(file_path), 
      error = function(e) { print(paste("ERROR!", file_path)); use <- FALSE }, 
      warning = function(w) {print(paste("WARNING!", file_path)); use <- FALSE}, 
      finally = {
        if(ncol(results) == 4)
          print(paste(file_path, ncol(results)))
        else
          use <- FALSE
      }
    )
    
  }
  
  if(use) {
    results
  } else {
    NULL
  }
  
}

map_epoch_type <- function(x) {
  ## Possibly speed up if x is a factor??
  if (x >= 1 & x <=4) { res <- "NREM" }
  else if (x == 5) { res <- "WAKE" }
  else if (x == 6) { res <- "REM" }
  else { res <- "UNDEF" }
  
  res
}