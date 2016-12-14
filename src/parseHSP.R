
# Parsing .HSP rainfall files
# HSP files are fixed width.  Each row is a single "card",
#   which consists of the date, the card number (1-8), 
#   and twelve columns of 15-minute precip readings.
#
# 8 cards times 12 readings -> 96, which is exactly the 
#   number of possible 15 minute readings in a day.
#
# The card number is indexed to the time of day.
#   Card 1, value 1 is always 00:01 to 00:15;
#   Card 7, value 5 is always 19:16 to 19:30.
#
# If any given 12-reading (or 3 hour) card recorded no
#   precipitation at all, that card is skipped.

library(reshape2)

sitename = "PEA1"

directory.path <- paste("O:/TechnicalServices/Env Monitoring Program/MON DATA",
                        "Archived Rainfall/",
                        "Eaton Creek Basin - Meridian Road/Archived Data",
                        sep = "/")

filename = paste(sitename,
                 ".HSP",
                 sep = "")
filename.out.monthly = paste(sitename,
                             "_monthlySum",
                             ".csv",
                             sep ="")
filename.out.15min = paste(sitename,
                             "_15min",
                             ".csv",
                             sep ="")



filepath <- paste(directory.path,
                  "Archived Data",
                  filename,
                  sep = "/")

filepath.out.monthly <- paste(directory.path,
                              "Processed Data",
                              filename.out.monthly,
                              sep = "/")

filepath.out.15min <- paste(directory.path,
                            "Processed Data",
                            filename.out.15min,
                            sep = "/")

# For some reason there's a leading space in most of these files;
#   we drop that.  Next six characters are the date, then the
#   card number.  Following that are the 12 columns of 15-minute
#   observations, each 6 characters wide.
widths <- c(c(1,6,1), rep(6,12))
cnames <- c("ignore","date", "card",
            "C1.1","C1.2","C1.3","C1.4",
            "C2.1","C2.2","C2.3","C2.4",
            "C3.1","C3.2","C3.3","C3.4")

precip.raw <- read.fwf(filepath, 
                       widths = widths,
                       header = FALSE,
                       dec = ".",
                       col.names = cnames,
                       comment.char = "")


# Clean the data a little
#   NOTE THAT THERE ARE SOMETIMES NOTES IN ROWS IN LIEU OF DATA.
#   USING COMPLETE CASES REMOVES THESE NOTES.  MANUAL REVIEW OF
#   DATA IS NECESSARY.

for (row in cnames[3:length(cnames)]) {
  print(row)
  vals <- precip.raw[,row]
  if(typeof(vals) != "double") {
    precip.raw[,row] <- as.numeric(levels(vals))[vals]
  }
}

precip.raw.complete <- precip.raw[complete.cases(precip.raw),]

precip.raw.complete$date <- gsub(pattern = " ",
                                 replacement = "0",
                                 x = precip.raw.complete$date,
                                 fixed = TRUE)

# Note the two-digit year.
precip.raw.complete$ts.date <- as.Date(precip.raw.complete$date,
                                       format = "%y%m%d")

# Move from "wide" format to "long" format
precip.melted <- melt(precip.raw.complete, 
                      id.vars = c("card", "ts.date"),
                      measure.vars = cnames[4:length(cnames)])

# Get the time from the card & column
precip.melted$hour <- ((as.numeric(precip.melted$card) * 3) - 3) +
                      (as.numeric(substr(precip.melted$variable, 2, 2)) - 1)

precip.melted$minute <- as.numeric(substr(precip.melted$variable,4,4)) * 15

precip.melted$time <- paste(sprintf("%02d",precip.melted$hour),
                            precip.melted$minute, sep=":")

# Combine date & time
precip.melted$datetime.char <- paste(as.character(precip.melted$ts.date), 
                                     precip.melted$time, 
                                     sep = " ")

precip.melted$datetime <- as.POSIXct(precip.melted$datetime, 
                                     format = "%Y-%m-%d %R")

# Final dataset
precip <- precip.melted[,c("datetime", "value")]
colnames(precip) <- c("datetime", "precip.in")

# Monthly summary stats
precip.melted$month <- substr(precip.melted$datetime, 6, 7)
precip.melted$year <- substr(precip.melted$datetime, 1, 4)

ag <- aggregate(value ~ month + year,
                            data = precip.melted,
                            FUN = function(x) { c(count = length(x), 
                                                  sum = sum(x)) })

precip.monthly <- data.frame(ag$value)
precip.monthly$year <- ag$year
precip.monthly$month <- ag$month

# Export the data

# See earlier warning about notes.  Also look out for incomplete
#   data for the first & last months.
write.csv(precip.monthly, filepath.out.monthly)

write.csv(precip, filepath.out.15min)


