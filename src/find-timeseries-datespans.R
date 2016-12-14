
# Pulling out monitoring time spans from csv files

library(tools)

files.in <- list.files(path = "results",
                       pattern="*_15m.csv")

# Get station codes from file names
files.code <- file_path_sans_ext(files.in)

# Open file and pull out just the data we need
minmax <- function(filename) {
  data <- read.csv(filename)
  data$ts <- as.POSIXct(data$datetime,
                        format = "%Y-%m-%d %H:%M:%S")
  
  data.min <- min(data$ts)
  data.max <- max(data$ts)
  
  data.changes <- which(diff(as.numeric(data$flag))!=0) + 1
  
  return(c(data.min, data.max))
}

files.fullpath <- paste("results", files.in, sep = "/")

i <- 1
for (file in files.fullpath) {
  print(files.code[i])
  print(minmax(file))
  i <- i+1
}

