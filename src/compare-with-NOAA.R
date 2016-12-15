

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

# Do the following with each daily .csv:
#   Compare to the NOAA daily values
#   Find days with values that don't make sense; meaning, have
#     a significant deviation from the defined relationship w/ NOAA
i <- 1
for (file in files.fullpath) {
  print(files.code[i])
  data.daily <- read_csv(file, na = character())
  
  # Remove flagged data
  data.filter <- data.daily[data.daily$max.flag == "Normal", ]
  
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
                      labs(title = files.code[i])
  
  fitted.vs.resid <- ggplot(df, aes(.fitted, .resid)) + 
                            geom_point() +
                            stat_smooth(method="loess") + 
                            geom_hline(yintercept=0, 
                                       col="red", 
                                       linetype="dashed") +
                            labs(x = "Fitted values",
                                 y = "Residuals")
  
  plots <- plot_grid(histogram, fitted.vs.resid, labels=c("A", "B"))
  
  save_plot(filename = paste("results/", files.code[i], ".pdf"),
            plot = plots,
            nrow = 2,
            align = "v")
  
  # Make a table of the points worth investigating
  if (i == 1) {
    data.questionable.all <- data.questionable
  } else {
    data.questionable.all <- rbind(data.questionable.all, data.questionable)
  }
  
  i <- i+1
}

write.csv(data.questionable.all, 
          file = paste("results", "questionableVals.csv", sep = "/"))

