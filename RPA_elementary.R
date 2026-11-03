# Clear environment
rm(list=ls())

## The R script for Žužić et al. manuscript

# Lock RNG algorithm and seed for 100% reproducibility across runs and R versions
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
SEED <- 42
set.seed(SEED)

# Set path to the working directory containing prepared data set
library(tidyverse)
getCurrentFileLocation <- function()
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
library(compiler) ## requires R >= 2.13.0

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

all_ts <- c()
rpa_plot_name <- c()
rpa_plot_RR <- c()
rpa_plot_DET <- c()
rpa_plot_NRLINE <- c()
rpa_plot_L <- c()
rpa_plot_ENTR <- c()
rpa_plot_rENTR <- c()
rpa_plot_LAM <- c()
rpa_plot_TT <- c()

paste_data <- function(name, title, ts_y, all_ts, rpa_plot_name, rpa_plot_RR, rpa_plot_DET, rpa_plot_NRLINE, rpa_plot_L, rpa_plot_ENTR, rpa_plot_rENTR, rpa_plot_LAM, rpa_plot_TT) {
  rpa_plot <- crqa(
    ts1         = ts_y,
    ts2         = ts_y,
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
  RP <- rpa_plot$RP
  p_rp <- plot_rp(
    rp_matrix = RP,
    xlabel    = "Time",
    ylabel    = "Time",
    pcolour   = "red",
    title     = paste0("Recurrence plot - ", title)
  )
  ggsave(paste0(name,"_RPA_",num_days,".png"), plot = p_rp, width = 4.75, height = 4.75, units = "in", dpi = 300)
  
  df_ts <- data.frame(
    Time  = as.numeric(time(ts_y)),
    Value = as.numeric(ts_y)
  )
  p_ts <- ggplot(df_ts, aes(x = Time, y = Value)) +
    geom_line(color = "red") +
    theme_minimal() +
    labs(x = "Time", y = "Value", title = paste0("Time series - ", title))
  ggsave(paste0(name,"_time_series_",num_days,".png"), plot = p_ts, width = 4.75, height = 4.75, units = "in", dpi = 300)
  
  p_combined <- p_ts + p_rp + plot_layout(ncol = 2)
  ggsave(paste0(name,"_RPA_TS_",num_days,".png"), plot = p_combined, width = 4.75 * 2, height = 4.75, units = "in", dpi = 300)
  
  all_ts <- cbind(all_ts, ts_y)
  retval <- c(rpa_plot$RR, rpa_plot$DET, rpa_plot$NRLINE, rpa_plot$L, rpa_plot$ENTR, rpa_plot$rENTR, rpa_plot$LAM, rpa_plot$TT)
  rpa_plot_name <- c(rpa_plot_name, name)
  rpa_plot_RR <- c(rpa_plot_RR, retval[1])
  rpa_plot_DET <- c(rpa_plot_DET, retval[2])
  rpa_plot_NRLINE <- c(rpa_plot_NRLINE, retval[3])
  rpa_plot_L <- c(rpa_plot_L, retval[4])
  rpa_plot_ENTR <- c(rpa_plot_ENTR, retval[5])
  rpa_plot_rENTR <- c(rpa_plot_rENTR, retval[6])
  rpa_plot_LAM <- c(rpa_plot_LAM, retval[7])
  rpa_plot_TT <- c(rpa_plot_TT, retval[8])
  return(list(
    all_ts = all_ts,
    rpa_plot_name = rpa_plot_name,
    rpa_plot_RR = rpa_plot_RR,
    rpa_plot_DET = rpa_plot_DET,
    rpa_plot_NRLINE = rpa_plot_NRLINE,
    rpa_plot_L = rpa_plot_L,
    rpa_plot_ENTR = rpa_plot_ENTR,
    rpa_plot_rENTR = rpa_plot_rENTR,
    rpa_plot_LAM = rpa_plot_LAM,
    rpa_plot_TT = rpa_plot_TT
  ))
}

num_days <- 100

# 1. Sin function (Deterministic)
x <- seq(-num_days / 2, num_days / 2 - 1, by = 1/1)
y = sin(x)
ts_y <- as.ts(y)
retval <- paste_data("sin", "Sine", ts_y, all_ts, rpa_plot_name, rpa_plot_RR, rpa_plot_DET, rpa_plot_NRLINE, rpa_plot_L, rpa_plot_ENTR, rpa_plot_rENTR, rpa_plot_LAM, rpa_plot_TT)
all_ts <- retval$all_ts
rpa_plot_name <- retval$rpa_plot_name
rpa_plot_RR <- retval$rpa_plot_RR
rpa_plot_DET <- retval$rpa_plot_DET
rpa_plot_NRLINE <- retval$rpa_plot_NRLINE
rpa_plot_L <- retval$rpa_plot_L
rpa_plot_ENTR <- retval$rpa_plot_ENTR
rpa_plot_rENTR <- retval$rpa_plot_rENTR
rpa_plot_LAM <- retval$rpa_plot_LAM
rpa_plot_TT <- retval$rpa_plot_TT

# 2. Normal distribution - white noise (Stochastic -> lock seed)
set.seed(SEED + 1)
y = rnorm(num_days)
ts_y <- as.ts(y)
retval <- paste_data("normal", "White noise", ts_y, all_ts, rpa_plot_name, rpa_plot_RR, rpa_plot_DET, rpa_plot_NRLINE, rpa_plot_L, rpa_plot_ENTR, rpa_plot_rENTR, rpa_plot_LAM, rpa_plot_TT)
all_ts <- retval$all_ts
rpa_plot_name <- retval$rpa_plot_name
rpa_plot_RR <- retval$rpa_plot_RR
rpa_plot_DET <- retval$rpa_plot_DET
rpa_plot_NRLINE <- retval$rpa_plot_NRLINE
rpa_plot_L <- retval$rpa_plot_L
rpa_plot_ENTR <- retval$rpa_plot_ENTR
rpa_plot_rENTR <- retval$rpa_plot_rENTR
rpa_plot_LAM <- retval$rpa_plot_LAM
rpa_plot_TT <- retval$rpa_plot_TT

# 3. Auto-regressive time sequence (Stochastic -> lock seed)
set.seed(SEED + 2)
alpha = -0.99
# purely random process
Z <- rnorm(num_days, mean = 0, sd = 0.5)
# initial value
X <- rnorm(1)
# the process
for (i in 2:length(Z)) {
  X[i] <- alpha * X[i-1] + Z[i]
}
ts_y <- as.ts(X)
retval <- paste_data("ar", "Auto-regressive", ts_y, all_ts, rpa_plot_name, rpa_plot_RR, rpa_plot_DET, rpa_plot_NRLINE, rpa_plot_L, rpa_plot_ENTR, rpa_plot_rENTR, rpa_plot_LAM, rpa_plot_TT)
all_ts <- retval$all_ts
rpa_plot_name <- retval$rpa_plot_name
rpa_plot_RR <- retval$rpa_plot_RR
rpa_plot_DET <- retval$rpa_plot_DET
rpa_plot_NRLINE <- retval$rpa_plot_NRLINE
rpa_plot_L <- retval$rpa_plot_L
rpa_plot_ENTR <- retval$rpa_plot_ENTR
rpa_plot_rENTR <- retval$rpa_plot_rENTR
rpa_plot_LAM <- retval$rpa_plot_LAM
rpa_plot_TT <- retval$rpa_plot_TT

# 4. Brownian motion (Stochastic -> lock seed)
set.seed(SEED + 3)
t <- 0:(num_days - 1)  # time
sig2 <- 0.01
## first, simulate a set of random deviates
x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
## now compute their cumulative sum
x <- c(0, cumsum(x))
ts_y <- as.ts(x)
retval <- paste_data("brownian", "Brownian motion", ts_y, all_ts, rpa_plot_name, rpa_plot_RR, rpa_plot_DET, rpa_plot_NRLINE, rpa_plot_L, rpa_plot_ENTR, rpa_plot_rENTR, rpa_plot_LAM, rpa_plot_TT)
all_ts <- retval$all_ts
rpa_plot_name <- retval$rpa_plot_name
rpa_plot_RR <- retval$rpa_plot_RR
rpa_plot_DET <- retval$rpa_plot_DET
rpa_plot_NRLINE <- retval$rpa_plot_NRLINE
rpa_plot_L <- retval$rpa_plot_L
rpa_plot_ENTR <- retval$rpa_plot_ENTR
rpa_plot_rENTR <- retval$rpa_plot_rENTR
rpa_plot_LAM <- retval$rpa_plot_LAM
rpa_plot_TT <- retval$rpa_plot_TT

# 5. Logistic map (Deterministic)
logistic.map <- function(r, x, N, M){
  ## r: bifurcation parameter
  ## x: initial value
  ## N: number of iteration
  ## M: number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r * z[i] * (1 - z[i])
  }
  ## Return the last M iterations 
  z[c((N-M):N)]
}

logistic.map <- cmpfun(logistic.map)
lm <- logistic.map(3.56995, 0.01, 1000, num_days - 1)
ts_y <- as.ts(lm)
retval <- paste_data("logistic", "Logistic map", ts_y, all_ts, rpa_plot_name, rpa_plot_RR, rpa_plot_DET, rpa_plot_NRLINE, rpa_plot_L, rpa_plot_ENTR, rpa_plot_rENTR, rpa_plot_LAM, rpa_plot_TT)
all_ts <- retval$all_ts
rpa_plot_name <- retval$rpa_plot_name
rpa_plot_RR <- retval$rpa_plot_RR
rpa_plot_DET <- retval$rpa_plot_DET
rpa_plot_NRLINE <- retval$rpa_plot_NRLINE
rpa_plot_L <- retval$rpa_plot_L
rpa_plot_ENTR <- retval$rpa_plot_ENTR
rpa_plot_rENTR <- retval$rpa_plot_rENTR
rpa_plot_LAM <- retval$rpa_plot_LAM
rpa_plot_TT <- retval$rpa_plot_TT

# 6. Logistic map with linear trend (Deterministic)
logistic.map.linear.trend <- function(r, x, N, M, a){
  ## r: bifurcation parameter
  ## x: initial value
  ## N: number of iteration
  ## M: number of iteration points to be returned
  ## a: slope for the linear function to add
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r * z[i] * (1 - z[i])
  }
  for(i in c(1:N)){
    z[i] <- z[i] + a * i
  }
  ## Return the last M iterations 
  z[c((N-M):N)]
}
logistic.map.linear.trend <- cmpfun(logistic.map.linear.trend)
lm <- logistic.map.linear.trend(3.56995, 0.01, 1000, num_days - 1, 0.005)
ts_y <- as.ts(lm)
retval <- paste_data("logistic_linear", "Logistic map with a linear trend", ts_y, all_ts, rpa_plot_name, rpa_plot_RR, rpa_plot_DET, rpa_plot_NRLINE, rpa_plot_L, rpa_plot_ENTR, rpa_plot_rENTR, rpa_plot_LAM, rpa_plot_TT)
all_ts <- retval$all_ts
rpa_plot_name <- retval$rpa_plot_name
rpa_plot_RR <- retval$rpa_plot_RR
rpa_plot_DET <- retval$rpa_plot_DET
rpa_plot_NRLINE <- retval$rpa_plot_NRLINE
rpa_plot_L <- retval$rpa_plot_L
rpa_plot_ENTR <- retval$rpa_plot_ENTR
rpa_plot_rENTR <- retval$rpa_plot_rENTR
rpa_plot_LAM <- retval$rpa_plot_LAM
rpa_plot_TT <- retval$rpa_plot_TT

# Use tibble/data.frame directly instead of cbind() so numeric columns stay numeric
new_data <- tibble(
  name   = rpa_plot_name,
  RR     = rpa_plot_RR,
  DET    = rpa_plot_DET,
  NRLINE = rpa_plot_NRLINE,
  L      = rpa_plot_L,
  ENTR   = rpa_plot_ENTR,
  rENTR  = rpa_plot_rENTR,
  LAM    = rpa_plot_LAM,
  TT     = rpa_plot_TT
)
write_csv(new_data, paste0("examples_indicators_", num_days, ".csv"))

colnames(all_ts) <- c("sin", "normal", "ar", "brownian", "logistic", "logistic_linear")
write_csv(as.data.frame(all_ts), paste0("examples_", num_days, ".csv"))