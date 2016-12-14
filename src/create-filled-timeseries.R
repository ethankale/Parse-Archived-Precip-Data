
# Pulling out monitoring time spans from csv files

library(zoo)
library(plyr)

curr.wd <- (paste("O:/TechnicalServices/Env Monitoring Program/MON DATA",
            "Archived Rainfall/2016-12 Processed HSP Files",
            sep = "/"))

files.in <- list.files(path = curr.wd,
                       pattern="*_15m.csv")

# Get station codes from file names
files.split <- strsplit(files.in, "_")
files.code <- unlist(files.split)[2*(1:length(files.in))-1]

files.fullpath <- paste(curr.wd, files.in, sep = "/")

# There is also a list of annotations to the data in .csv format
annotations.col <- c("character", "character", "character", "character", "character")
annotations <- read.csv(paste(curr.wd,
                              "annotations.csv",
                              sep = "/"),
                        colClasses = annotations.col,
                        comment.char = "")

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
  
  timeseries.joined <- join(x = timeseries.df,
                            y = data,
                            by = 'ts',
                            type = 'left',
                            match = 'first')
  
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
  
  data.filled <- filledTS(data)
  
  print(paste(min(data$ts),
              max(data$ts),
              nrow(data.filled),
              sep = ", "))
  
  # Annotating the data, based on notes in the HSP file
  #   EST = estimated values
  #   FAIL = equipment failure (usually data overran chip capacity)

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
  
  write.csv(x = data.filled,
            file = paste(curr.wd, 
                         "/",
                         files.code[i],
                         "_filled.csv",
                         sep = ""),
            row.names = FALSE)
  
  i <- i+1
}

