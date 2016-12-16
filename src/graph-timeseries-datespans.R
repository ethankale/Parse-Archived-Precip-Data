

# Pulling out monitoring time spans from csv files

library(readr)
library(tools)
library(ggplot2)

files.in <- list.files(path = "results",
                       pattern="*_filled.csv")

# Get station codes from file names
files.split <- strsplit(files.in, "_")
files.code <- unlist(files.split)[2*(1:length(files.in))-1]

files.fullpath <- paste("results", files.in, sep = "/")

# Open file and pull out just the data we need
findevents <- function(filename) {
  data <- read_csv(filename, na = character())

  # Order by date
  data <- data[order(data$ts),]
  
  # Set up the flag factor correctly
  #   Note that R/readr don't do well with converting empty strings,
  #   i.e. "", to factors.
  data$flag[data$flag == ""] <- "Normal"
  levels <- c("Normal", "UNK", "EST", "FAIL")
  data$flag <- parse_factor(data$flag, levels, 
                                ordered = TRUE)
  levels(data$flag) <- c("Normal", "Unknown", "Questionable", "Bad")
  
  # Find where the flag changes; that is, a significant event
  #   occurs (begins and ends)
  data.changes <- which(diff(as.numeric(data$flag))!=0)
  data.changes <- c(data.changes, data.changes + 1)
  data.changes <- c(1, data.changes, nrow(data))
  data.changes <- data.changes[order(data.changes)]
  
  return(data[data.changes,])
  
}



events <- data.frame()
stations <- c()

# Pull together a list of all of the events in all of the files
i <- 1
for (file in files.fullpath) {
  print(files.code[i])
  df <- findevents(file)
  
  if (i == 1) {
    events <- df
  } else {
    events <- rbind(events, df)
  }

  stations <- c(stations, rep(files.code[i], nrow(df)))
  
  i <- i+1
}

events$station <- as.factor(stations)
events$datetime <- events$ts

# This plot is automatically generated with the current date & time.
ggplot(data = events) +
  geom_line(aes(x = datetime, y = stations, group = stations, color = flag),
            size = 3) +
  #geom_point(aes(x = ts, y = stations, color = flag), size = 2) +
  scale_color_manual(values = c("#CCCCCC","#ffff00", "#ffa600", "#ff0000")) +
  labs(title = "Precipitation Station Data Collection", 
       subtitle = paste("Created on", date()),
       x = "Date", 
       y = "Stations") +
  guides(color = guide_legend(title = "Data Quality")) +
  theme_bw()

ggsave("Rainfall Data Integrity.pdf", 
       path = "results/",
       plot = last_plot())


