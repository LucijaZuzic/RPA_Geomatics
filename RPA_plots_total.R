# Clear environment
rm(list=ls())

## The R script for Žužić et al. manuscript

# Set path to the working directory containing prepared data set

library(tidyverse)
getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = value, into = c("key", "value"), sep = "=", fill = 'right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file) == 0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())

# Load required libraries
library(fNonlinear)
library(crqa)
library(ggplot2)
library(patchwork)

myRPA<-function(station_name, num_days) {
  # set filenames
  start_filename <- paste0("afullframe_",station_name,"_new_",num_days)
  data_filename <- paste0(start_filename,".csv")
  
  # set parameters for RPA plots (same for all locations)
  delay = 1
  embed = 1
  rescale = 1
  radius = 1
  normalize = 0
  mindiagline = 2
  minvertline = 2
  tw = 0
  whiteline = FALSE
  recpt = FALSE
  side = "both"
  
  # load data
  data <- read_csv(data_filename, show_col_types = FALSE)
  horerr <- c()
  for (doy in 0:366) {
    data_day <- data[data$`doy` == doy,]
    for (hr in 0:23) {
      data_day_hr <- data_day[data_day$`hr` == hr,]
      if (length(data_day_hr$`horizontal(deg)`) != 0) {
        horerr <- c(horerr, mean(data_day_hr$`horizontal(deg)`))
      }
    }
  }
  
  # Prepare data series
  if (length(horerr) == 0) {
    return (c(FALSE, 0, 0, 0, 0, 0, 0, 0, 0))
  }
  ts_horerr <- as.ts(horerr)
  
  # calculate CRQA
  rpa_plot <- crqa(
    ts1         = ts_horerr,
    ts2         = ts_horerr,
    delay       = delay,
    embed       = embed,
    rescale     = rescale,
    radius      = radius,
    normalize   = normalize,
    mindiagline = mindiagline,
    minvertline = minvertline,
    tw          = tw,
    whiteline   = whiteline,
    recpt       = recpt,
    side        = side,
    method      = "crqa" # or "rqa" depending on your package version
  )
  
  # create plot as image in working dir
  RP <- rpa_plot$RP
  
  makeplot <- TRUE
  if (makeplot) {
    ts_filename <- paste0(start_filename,"_time_series.png")
    df_ts <- data.frame(
      Time  = as.numeric(time(ts_horerr)),
      Value = as.numeric(ts_horerr)
    )
    p_ts <- ggplot(df_ts, aes(x = Time, y = Value)) +
      geom_line(color = "red") +
      theme_minimal() +
      labs(x = "Time", y = "Value", title = paste0("Time series - Hourly means for ", station_name))
    ggsave(ts_filename, plot = p_ts, width = 4.75, height = 4.75, units = "in", dpi = 300)
    
    rpa_filename <- paste0(start_filename,"_RPA.png")
    p_rp <- plot_rp(
      rp_matrix = RP,
      xlabel    = "Time",
      ylabel    = "Time",
      pcolour   = "red",
      title     = paste0("Recurrence plot - Hourly means for ", station_name)
    )
    ggsave(rpa_filename, plot = p_rp, width = 4.75, height = 4.75, units = "in", dpi = 300)
    
    combined_filename <- paste0(start_filename,"_RPA_TS.png")
    p_combined <- p_ts + p_rp + plot_layout(ncol = 2)
    ggsave(combined_filename, plot = p_combined, width = 4.75 * 2, height = 4.75, units = "in", dpi = 300)
  }
  return(c(TRUE, rpa_plot$RR, rpa_plot$DET, rpa_plot$NRLINE, rpa_plot$L, rpa_plot$ENTR, rpa_plot$rENTR, rpa_plot$LAM, rpa_plot$TT))
}

num_days <- 120
station_names <- c("Iqaluit", "Darwin", "Matera")

rpa_plot_name <- c()
rpa_plot_RR <- c()
rpa_plot_DET <- c()
rpa_plot_NRLINE <- c()
rpa_plot_L <- c()
rpa_plot_ENTR <- c()
rpa_plot_rENTR <- c()
rpa_plot_LAM <- c()
rpa_plot_TT <- c()
for (numloc in 1:3) {
  station_name <- station_names[numloc]
  retval <- myRPA(station_name, num_days)
  if (retval[1] == TRUE) {
    rpa_plot_name <- c(rpa_plot_name, station_name)
    rpa_plot_RR <- c(rpa_plot_RR, retval[2])
    rpa_plot_DET <- c(rpa_plot_DET, retval[3])
    rpa_plot_NRLINE <- c(rpa_plot_NRLINE, retval[4])
    rpa_plot_L <- c(rpa_plot_L, retval[5])
    rpa_plot_ENTR <- c(rpa_plot_ENTR, retval[6])
    rpa_plot_rENTR <- c(rpa_plot_rENTR, retval[7])
    rpa_plot_LAM <- c(rpa_plot_LAM, retval[8])
    rpa_plot_TT <- c(rpa_plot_TT, retval[9])
    new_data <- cbind(rpa_plot_name, rpa_plot_RR, rpa_plot_DET, rpa_plot_NRLINE, rpa_plot_L, rpa_plot_ENTR, rpa_plot_rENTR, rpa_plot_LAM, rpa_plot_TT)
    colnames(new_data) <- c("name", "RR", "DET", "NRLINE", "L", "ENTR", "rENTR", "LAM", "TT")
    write_csv(data.frame(new_data), paste0("afullframe_mean_new_",num_days,".csv"))
  }
}