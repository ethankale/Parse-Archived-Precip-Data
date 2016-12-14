
# Pulling out monitoring time spans from csv files

curr.wd <- (paste("O:/TechnicalServices/Env Monitoring Program/MON DATA",
            "Archived Rainfall/2016-12 Processed HSP Files",
            sep = "/"))

#files.in <- list.files(path = curr.wd,
#                       pattern="*_15m.csv")
files.in <- list.files(path = curr.wd,
                       pattern="*_filled.csv")

# Get station codes from file names
files.split <- strsplit(files.in, "_")
files.code <- unlist(files.split)[2*(1:length(files.in))-1]

# Open file and pull out just the data we need
minmax <- function(filename) {
  data <- read.csv(filename)
  data$ts <- as.POSIXct(data$ts,
                        format = "%Y-%m-%d %H:%M:%S")
  
  data.min <- min(data$ts)
  data.max <- max(data$ts)
  
  data.changes <- which(diff(as.numeric(data$flag))!=0) + 1
  
  return(c(data.min, data.max))
}

files.fullpath <- paste(curr.wd, files.in, sep = "/")

i <- 1
for (file in files.fullpath) {
  print(files.code[i])
  print(minmax(file))
  i <- i+1
}

