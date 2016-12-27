

# Develop relationships between daily rainfall values
#   at TC sites and daily rainfall at NOAA's Olympia airport site.
# Possibly add annotations (flags) to the data based on outliers/
#   high residuals.

library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(broom)
library(lubridate)

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

# Start by emptying the directory of working plots.  Not great
#   to do this before creating the new plots, but they're not
#   intended to be final products, just review aids (they don't
#   even get checked into Git).

do.call(file.remove, list(list.files("./results/plots", full.names = TRUE)))

# Lists of valuable stats, to be complied to a data frame
#   at the end of the loop.
rsquared <- c()
intercept <- c()
slope <- c()
days <- c()
flag.ratio <- c()
years.good.data <- c()

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
  
  # Plots
  
  # What do the data look like around each of the questionable
  #   data points?
  
  j <- 1
  while (j <= nrow(data.questionable)) {
    
    .date <- data.questionable$days[j]
    .date.begin <- .date - months(1)
    .date.end <- .date + months(1)
    
    data.filtered <- data.combined %>%
      filter(days >= .date.begin & days <= .date.end)
    
    # This is a little more complicated than usual because the data are
    #   in wide format, instead of long - the NOAA and tc station values
    #   are in their own columns, instead of being in the same column with
    #   a separate column tagging which values are for which station.
    context.plot <- ggplot(data.filtered) + 
      geom_line(aes(x = days, y = PRCP), color = "red") +
      geom_line(aes(x = days, y = total.precip), color = "black") + 
      geom_point(aes(x = days, y = PRCP), color = "red") +
      geom_point(aes(x = days, y = total.precip), color = "black") +
      labs(title = paste0("Precipitation - NOAA vs. ", sitecode, " - ", .date),
           subtitle = now(),
           x = "Date",
           y = "Precipitation (inches)")
    
    ggsave(paste0("results/plots/", sitecode, " ", .date, ".png"),
           width = 7,
           height = 5)
    
    
    j <- j+1
  }
  
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
  
  days.good <- nrow(data.filter)
  days.total <- nrow(data.daily)
  days <- c(days, days.total)
  
  ratio <- 1 - (days.good/days.total)
  flag.ratio <- c(flag.ratio, ratio)
  
  years.good <- ((days.good/days.total) * days.total) / 365
  years.good.data <- c(years.good.data, years.good)
  
  
  i <- i+1
}

stats.df <- data.frame(station = files.code,
                       rsquared = rsquared,
                       intercept = intercept,
                       slope = slope,
                       days = days,
                       percent.bad = flag.ratio,
                       years.good = years.good.data)

write.csv(stats.df,
          file = paste("results", "modelStatistics.csv", sep = "/"))

write.csv(data.questionable.all, 
          file = paste("results", "questionableVals.csv", sep = "/"))

