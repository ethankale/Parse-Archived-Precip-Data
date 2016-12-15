

# Pulling out monitoring time spans from csv files

library(readr)
library(dplyr)

files.in <- list.files(path = "results",
                       pattern="*_filled.csv")

# Get station codes from file names
files.split <- strsplit(files.in, "_")
files.code <- unlist(files.split)[2*(1:length(files.in))-1]

files.fullpath <- paste("results", files.in, sep = "/")

# Converts data from a 15 minute timeseries to daily
#   Specifically for precip, so it SUMS rather than averages
MakeDailyTimeSeries <- function(filename) {
  data.15m <- read_csv(filename, na = character())

  # Order by date
  data.15m <- data.15m[order(data.15m$ts),]
  
  # Set up the flag factor correctly
  #   Note that R/readr don't do well with converting empty strings,
  #   i.e. "", to factors.
  data.15m$flag[data.15m$flag == ""] <- "Normal"
  levels <- c("Normal", "EST", "FAIL")
  data.15m$flag <- parse_factor(data.15m$flag, levels, 
                                ordered = TRUE)
  
  data.15m$days <- as.Date(data.15m$ts)
  data.daily.grouped <- group_by(data.15m, days)
  data.daily <- summarize(data.daily.grouped, 
                          total.precip = sum(precip.in),
                          max.flag = max(flag))
  
  return(data.daily)
  
}

# Do the following with daily data:
#   Save to a .csv
i <- 1
for (file in files.fullpath) {
  print(files.code[i])
  df <- MakeDailyTimeSeries(file)
  
  #Write the file
  write.csv(x = df,
            file = paste("results", 
                         "/",
                         files.code[i],
                         "_daily.csv",
                         sep = ""),
            row.names = FALSE)
  
  i <- i+1
}



