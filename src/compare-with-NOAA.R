

# Develop relationships between daily rainfall values
#   at TC sites and daily rainfall at NOAA's Olympia airport site.
# Possibly add annotations (flags) to the data based on outliers/
#   high residuals.

library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(broom)

files.in <- list.files(path = "results",
                       pattern="*_daily.csv")

# Get station codes from file names
files.split <- strsplit(files.in, "_")
files.code <- unlist(files.split)[2*(1:length(files.in))-1]


files.fullpath <- paste("results", files.in, sep = "/")

data.noaa <- read_csv(paste("data",
                            "11u_1988-2016_Day.csv",
                            sep = "/"),
                      na = character(),
                      col_types = "cccd")

data.noaa$days <- as.Date(data.noaa$DATE, format="%Y%m%d")

# What data to include in the analysis?  Just "Normal" flagged data,
#   or both "Normal" and "UNK" (unknown) flagged data?  This variable can be
#   overwritten in main.R.

if (!exists("flags.to.use")) {
  flags.to.use <- c("Normal")
}

# Lists of valuable stats, to be complied to a data frame
#   at the end of the loop.
rsquared <- c()
intercept <- c()
slope <- c()
days <- c()

# Do the following with each daily .csv:
#   Compare to the NOAA daily values
#   Find days with values that don't make sense; meaning, have
#     a significant deviation from the defined relationship w/ NOAA
i <- 1
for (file in files.fullpath) {
  
  sitecode <- files.code[i]
  
  print(sitecode)
  data.daily <- read_csv(file, na = character())
  
  # Remove flagged data
  data.filter <- data.daily[data.daily$max.flag %in% flags.to.use, ]
  
  # Create a merged NOAA-TC Site dataset
  data.combined <- data.noaa %>% inner_join(data.filter)
  
  # Build the linear model
  data.lm <- lm(data.combined$total.precip ~ data.combined$PRCP)
  
  df <- augment(data.lm)
  
  # Cook's distance - combined measure of leverage and residual.
  #   High values warrant further examination.
  data.combined$cooksd <- df$.cooksd
  cutoff <- quantile(df$.cooksd, 0.99)
  data.questionable <- data.combined[data.combined$cooksd >= cutoff,]
  data.questionable$tc.station <- files.code[i]
  
  histogram <- ggplot(df, aes(x = .resid)) +
                      geom_histogram(binwidth = 0.05) + 
                      labs(title = sitecode,
                           x = "Residuals",
                           y = "Count")
  
  fitted.vs.resid <- ggplot(df, aes(.fitted, .resid)) + 
                            geom_point() +
                            stat_smooth(method="loess") + 
                            geom_hline(yintercept=0, 
                                       col="red", 
                                       linetype="dashed") +
                            labs(x = "Fitted values",
                                 y = "Residuals")
  
  plots <- plot_grid(histogram, fitted.vs.resid, 
                     labels = c("A", "B"),
                     nrow = 2,
                     align = "v")
  
  save_plot(filename = paste("results/", sitecode, ".pdf"),
            plot = plots)
  
  # Make a table of the points worth investigating
  if (i == 1) {
    data.questionable.all <- data.questionable
  } else {
    data.questionable.all <- rbind(data.questionable.all, data.questionable)
  }

  # Record some vital stats from the model
  rsquared <- c(rsquared, summary(data.lm)$adj.r.squared)
  intercept <- c(intercept, coef(data.lm)[1])
  slope <- c(slope, coef(data.lm)[2])
  days <- c(days, nrow(data.combined))
  
  i <- i+1
}

stats.df <- data.frame(station = files.code,
                       rsquared = rsquared,
                       intercept = intercept,
                       slope = slope,
                       days = days)

write.csv(stats.df,
          file = paste("results", "modelStatistics.csv", sep = "/"))

write.csv(data.questionable.all, 
          file = paste("results", "questionableVals.csv", sep = "/"))

