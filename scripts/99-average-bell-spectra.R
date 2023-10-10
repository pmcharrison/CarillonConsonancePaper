# Averaging spectra
###################

# Here we try to create an average spectrum over all bells in the dataset.
# It doesn't work very well, the small peaks get dwarfed. This motivates
# the peak-picking approach.

library(tidyverse)
library(seewave)
library(readxl)


df <- 
  read_excel("../../Analysis/data/Frequency and Amplitude Data.xlsx", sheet = "ALL DATA - with Partials")


df_f0 <- df |> filter(Partial == 1) |> select(Bell, Frequency)
bells <- df$Bell |> unique()

get_spectrum <- function(bell) {
  file <- sprintf("input/bell-samples/%s.wav", bell)
  wav <- tuneR::readWave(file)
  f0 <- df_f0 |> filter(Bell == !!bell) |> pull(Frequency)
  stopifnot(length(f0) == 1)
  
  spec <- 
    seewave::meanspec(wav, dB = 'max0', main = 'Spectrum', wl = 2^14) |> 
    as_tibble() |> 
    transmute(Hz = x * 1000, dB = y) |> 
    transmute(
      bell = bell, 
      Hz = Hz, 
      freq_ratio = Hz / (f0 * 2 ), # seems necessary for expressing relative to prime
      dB = dB
    )
  
  spec
}

df_spectra <- 
  map_dfr(bells, get_spectrum, .progress = TRUE)


Rcpp::sourceCpp("../../Analysis/smooth_2d_gaussian.cpp")

smooth_x <- seq(from = 0, to = 7, by = 0.01)

smooth_y <- smooth_2d_gaussian(
  data_x = df_spectra$freq_ratio,
  data_y = rep(0, times = nrow(df_spectra)),
  data_val = df_spectra$dB,
  probe_x = smooth_x,
  probe_y = length(smooth_x),
  sigma_x = 0.01,
  sigma_y = 1
)

ggplot(
  tibble(smooth_x, smooth_y) |> na.omit(),
  aes(smooth_x, smooth_y)
) + 
  geom_line() + 
  scale_x_continuous(
    "Frequency ratio",
    sec.axis = sec_axis(
      ~ log2(.) * 12,  # 
      breaks = c(-12, 4 * 0:8),
      name = "Interval from the prime (semitones)"
    )
  ) + 
  scale_y_continuous("Level (dB)")
