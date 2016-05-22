library("grid")
library("ggplot2")
library(scales)
library(data.table)
library(gdata)
source("R/raster.R")
source("R/load_data.R")

## Constants
T_CYCLE = 24.0
EPOCH_SECONDS <- 30
EPOCH_LENGTH <- (EPOCH_SECONDS / 3600)
## Color Palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#F0E442", "#888888", "#000000", "#D55E00",  "#CC79A7", "#555555")


## Load Example Data
sleep_data <- load_sleep_file("example_data/stages.csv")
sleep_episodes <- fread("example_data/sleep.csv")
state_episodes <- fread("example_data/episodes.csv")

setkey(sleep_data, subject_code, labtime)
sleep_data <- sleep_data[activity_or_bedrest_episode > 0]

# Generate row indeces
sleep_data[, pk:=.I]
# Map stages to epoch types
sleep_data[,epoch_type:=as.factor(as.character(lapply(stage, map_epoch_type))),]
##





# Sleep Stage Data Setup
sleep_data.v <- copy(sleep_data)
convert_stage_for_raster(sleep_data.v)
sleep_data.v[,c('day_number','day_labtime'):=set_days(labtime)]
sleep_data.v <- double_plot(sleep_data.v,TRUE)

# Sleep Episode Setup
sleep_episodes.v <- copy(sleep_episodes)
sleep_episodes.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
sleep_episodes.v <- data.table(rbindlist(list(sleep_episodes.v[start_day_number==end_day_number], split_day_spanning_blocks(sleep_episodes.v[start_day_number!=end_day_number]))))
sleep_episodes.v[,length:=end_day_labtime-start_day_labtime]
#sleep_episodes.v <- sleep_episodes.v[,select_longer_split(.SD),by='subject_code,activity_or_bedrest_episode']
sleep_episodes.v[,day_number:=start_day_number]
sleep_episodes.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
sleep_episodes.v <- double_plot(sleep_episodes.v,TRUE)

# State Episode Setup
state_episodes.v <- copy(state_episodes)
state_episodes.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
state_episodes.v <- data.table(rbindlist(list(state_episodes.v[start_day_number==end_day_number], split_day_spanning_blocks(state_episodes.v[start_day_number!=end_day_number]))))
state_episodes.v[,length:=end_day_labtime-start_day_labtime]
state_episodes.v[,day_number:=start_day_number]
state_episodes.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
state_episodes.v <- double_plot(state_episodes.v,TRUE)

# Plot Raster
raster_data <- list(sleep_episodes=sleep_episodes.v, stages=sleep_data.v, state_episodes=state_episodes.v)

plot_raster(raster_data)
