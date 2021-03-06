
# Pulling out monitoring time spans from csv files

library(zoo)
library(dplyr)
library(tools)
library(lubridate)

files.in <- list.files(path = "results",
                       pattern="*_15m.csv")

# Get station codes from file names
files.split <- strsplit(files.in, "_")
files.code <- unlist(files.split)[2*(1:length(files.in))-1]

files.fullpath <- paste("results", files.in, sep = "/")

# There is also a list of annotations to the data in .csv format
annotations.col <- c("character", "character", "character", "character", "character")
annotations <- read.csv(paste0("results/",
                              "annotations.csv"),
                        colClasses = annotations.col,
                        comment.char = "")

# And there is a list of offsets, where data need to be shifted by a number
#   of days to match NOAA.
offsets <- read_csv(paste0("results/",
                          "offsets.csv"))

#Function to read in data & do initial cleaning & calc
readTS <- function(filename) {
  data <- read.csv(filename)
  data$ts <- as.POSIXct(data$datetime,
                        format = "%Y-%m-%d %H:%M:%S")
  
  return(data)
}

# Function to fill in time series gaps
#   See https://bocoup.com/weblog/padding-time-series-with-r  
filledTS <- function(data) {
  
  timeseries <- seq(min(data$ts, na.rm = TRUE),
                    max(data$ts, na.rm = TRUE),
                    by="15 min")
  
  timeseries.df <- data.frame(list(ts = timeseries))
  
  timeseries.joined <- timeseries.df %>% left_join(data,
    by = 'ts')
  
  timeseries.trimmed <- timeseries.joined[,c("ts", "precip.in")]
  timeseries.trimmed$precip.in[which(is.na(timeseries.trimmed$precip.in))] <- 0
  
  return(timeseries.trimmed)
  
}

# Function to annotate timeseries data
#   Default timeseries column is "ts", default annotation column is "flag"
annotatets <- function(data, begin, end, text, ts = "ts", ann = "flag") {

  data[,ann][which(data[,ts] >= begin &
                      data[,ts] < end)] <- text
  
  return(data)
}


i <- 1
for (file in files.in) {
  
  print(files.code[i])
  
  data <- readTS(files.fullpath[i])
  
  # Move data back using matching offsets
  #   Find all the offsets for the station in question, and loop through them
  #   For each offset timespan, revise the dates forward or backward
  #   by x day within the timespan in question, then save back to data.filled.
  # Note that the days() function only accepts integers.
  
  offsets.filtered <- offsets %>% filter(station == files.code[i])
  
  j <- 1
  while (j <= nrow(offsets.filtered)) {
    
    ts.begin <- offsets.filtered$begin[j]
    ts.end <- offsets.filtered$end[j]
    offset.days <- offsets.filtered$offset.days[j]
    
    data <- data %>%
      mutate(condition = ts >= ts.begin & ts <= ts.end,
             ts = if_else(condition, ts + days(offset.days), ts)) %>% 
      select(-condition)
    
    j <- j+1
    
  }
  
  data.filled <- filledTS(data)
  
  print(paste(min(data$ts),
              max(data$ts),
              nrow(data.filled),
              sep = ", "))
  
  # Annotating the data, based on notes in the HSP file
  #   EST = estimated values
  #   FAIL = equipment failure (usually data overran chip capacity)
  #   UNK = unknown; manually flagged in response to comparison to NOAA

  data.filled$flag <- ""
  
  data.anno <- annotations[annotations$station == files.code[i],]
  
  # Could, and probably should, refactor this to a join/merge.
  for (j in c(1:nrow(data.anno))) {
    datum <- data.anno[j,]
    data.filled <- annotatets(data.filled, 
                              begin = datum$begin,
                              end = datum$end,
                              text = datum$flag)
  }
  
  
  
  # Write the final result
  write.csv(x = data.filled,
            file = paste0("results/",
                         files.code[i],
                         "_filled.csv"),
            row.names = FALSE)
  
  i <- i+1
}

