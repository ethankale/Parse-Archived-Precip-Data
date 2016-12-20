
# Convert .dat files to 15 minute .csv files

library(readr)
library(dplyr)
library(stringr)

files.in <- list.files(path = "data/dat",
                       pattern="*.DAT")

files.code <- substr(files.in, 1, 4)

files.df <- data.frame(path = files.in, code = files.code)
files.df$path.full <- paste("data/dat", files.df$path, sep="/")

# Given a path, pull in data from a .DAT file and spit out a dataframe
ReadDatFile <- function(filepath, station) {
  
  widths <- fwf_widths(c(2,3,5,5,9),
                       c("month", "day", "year", 
                         "hour.minute", "precip.hundreths"))
  types <- "cccci"
  
  precip.data <- read_fwf(filepath,
                           col_positions = widths,
                           skip = 14,
                           col_types = types)
  
  
  # add the station name and the filename as columns to the output,
  #   so that when we mash them together with mapply() and bind_rows()
  #   we can still tell where the data came from.
  if (nrow(precip.data) > 0) {
    precip.data$station <- station
    precip.data$filepath <- filepath
  } else {
    precip.data$station <- c()
    precip.data$filepath <- c()
  }
  
  # pad month, day, and hour.minute columns so that as.Date() reads them
  #   function from stringr
  precip.data$month <- str_pad(precip.data$month, 2, pad = "0")
  precip.data$day <- str_pad(precip.data$day, 2, pad = "0")
  precip.data$hour.minute <- str_pad(precip.data$hour.minute, 4, pad = "0")
  
  # get the date & time from the individual rows
  precip.data.final <- mutate(precip.data,
                              date.time.chr = paste(year,
                                                    month,
                                                    day,
                                                    hour.minute, 
                                                    sep = ""),
                              date.time = as.POSIXct(date.time.chr,
                                                      format = "%Y%m%d%H%M"))
  
  return(precip.data.final)
  
 }


# This function takes event data, where one row is created every time an
#   event occurs, and puts out 15 minute time data, where the number
#   of events per 15 minute interval is tracked.
EventTo15Minute <- function(data) {
  
  data.summed <- data %>%
    mutate(quarter.hour = as.POSIXct(round(as.numeric(data$date.time)/(15*60))
                                     * (15*60),
                                     origin = '1970-01-01'),
           datetime = format(quarter.hour, "%Y-%m-%d %H:%M:%S")) %>%
    group_by(station, datetime) %>%
    summarise(count = n()) %>%
    mutate(precip.in = count * 0.01) %>%
    select(datetime, precip.in)
    
  
  
  #data.15m <- mutate(data,
  #  quarter.hour = as.POSIXct(round(as.numeric(data$date.time)/(15*60)) 
  #                            * (15*60),
  #                            origin = '1970-01-01'))
  #data.grouped <- group_by(data.15m, station, quarter.hour)
  #data.summed <- summarise(data.grouped,
  #  count = n())
  
  return(data.summed)
}

GroupsToCSV <- function(df) {
  write_csv(df, paste0("results/", 
                       unique(df$station), 
                       "_15m.csv"))
  return(df)
}

# Returns a list of dataframes
precip.data.all <- mapply(ReadDatFile, 
                          filepath = files.df$path.full, 
                          station = files.code)

# From dplyr, puts the list of dataframes together into a single dataframe
precip.data <- bind_rows(precip.data.all)

test <- precip.data %>%
  do(EventTo15Minute(.)) %>%
  do(GroupsToCSV(.))

