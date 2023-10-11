library(tidyverse)
library(readxl)

source("src/consonance-utilities.R")
source("src/import-bell-peaks.R")

# The lower pitch in the chord was randomly sampled from [65, 67].
# The interval was sampled from [0, 15].
# This means we're using bells from the range [65, 82].
# For our modelling, we simplify as follows:
# - Lower bell's spectrum is the average of the bells within [65, 67]
# - Upper bell's spectrum is the average of the bells with [65, 82]
# - Lower bell's pitch is 66

df_bell_samples <- 
  read_csv("input/bell-samples.csv", col_types = cols()) |> 
  mutate(
    midi = hrep::freq_to_midi(f0)
  )

df_bell_peaks <- 
  import_bell_peaks()


get_representative_spectrum <- function(midi_range) {
  bells <- 
    df_bell_samples |> 
    filter(midi_range[1] <= midi, midi_range[2] >= midi) |> 
    pull(id)
  
  bell_spectra <-
    df_bell_peaks |> 
    filter(Bell %in% bells)
  
  representative_spectrum <- 
    bell_spectra |> 
    group_by(Partial) |> 
    dplyr::summarise(
      FrequencyRatio = weighted.mean(FrequencyRatio, w = Amplitude, na.rm = TRUE),
      Amplitude = mean(Amplitude),
    )
  
  representative_spectrum
}

lower_bell_spectrum <- get_representative_spectrum(c(65, 67)) |> na.omit()
upper_bell_spectrum <- get_representative_spectrum(c(65, 82)) |> na.omit()

timbres <- list()

timbres$Carillon <- list(
  lower = CustomTone$new(
    frequency_ratios = lower_bell_spectrum$FrequencyRatio, 
    amplitudes = lower_bell_spectrum$Amplitude
  ),
  upper = CustomTone$new(
    frequency_ratios = upper_bell_spectrum$FrequencyRatio, 
    amplitudes = upper_bell_spectrum$Amplitude
  )
)

timbres$Harmonic <- list(
  BasicHarmonicTone$new(decay_dB_per_octave = 3, n_harmonics = 10),
  BasicHarmonicTone$new(decay_dB_per_octave = 3, n_harmonics = 10)
)


model_profiles <- 
  expand_grid(
    interval = seq(from = 0, to = 15, by = 0.05),
    timbre_type = c("Carillon", "Harmonic"),
    model = c("roughness", "harmonicity")
)

model_profiles$value <- 
  pmap_dbl(model_profiles, function(interval, timbre_type, model) {
    midi <- c(66, 66 + interval)
    MODELS[[model]]$get_consonance(midi, timbre = timbres[[timbre_type]])
  }, .progress = TRUE)
  

normalise_to_unit_interval <- function(x) {
  range <- max(x) - min(x)
  (x - min(x)) / range
}

model_profiles |>  
  mutate(
    model = recode(
      model,
      harmonicity = "Harmonicity",
      roughness = "Interference between partials"
    ),
    # Reverse the minus sign for roughness
    value = if_else(
      model == "Interference between partials",
      - value,
      value
    ),
    timbre_type = recode(
      timbre_type,
      Carillon = "Carillon bell",
      Harmonic = "Harmonic tone"
    )
  ) |> 
  group_by(model, timbre_type) |> 
  dplyr::mutate(
    value = normalise_to_unit_interval(value),
    colour = case_when(
      timbre_type == "Harmonic tone" ~ "grey40",
      model == "Interference between partials" ~ "#B50000",
      model == "Harmonicity" ~ "#11A3FF",
    )
  ) |> 
  ggplot(aes(
    interval, 
    value,
    linetype = timbre_type,
    linewidth = timbre_type,
    colour = colour
  )) + 
  geom_line() + 
  facet_wrap(~ model, nrow = 1) +
  scale_colour_identity(NULL) + 
  scale_linewidth_manual(NULL, values = c(0.7, 0.4)) +
  scale_linetype_discrete(NULL) + 
  scale_x_continuous("Interval (semitones)", breaks = 0:15) + 
  scale_y_continuous("Value") +
  theme(
    legend.position = c(0.06, 1.3),
    legend.direction = "vertical",
    plot.margin = unit(c(60, 4, 4, 4), units = "pt")
  )

ggsave("output/figure-5.pdf", width = 10, height = 4)
