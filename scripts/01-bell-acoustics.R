library(ggpubr)
library(tidyverse)
library(readxl)
library(ggridges)

theme_set(theme_pubr())

df <- 
  read_excel("input/bell-peaks.xlsx", sheet = "ALL DATA - with Partials")

df <-
  df |> 
  left_join(
    df |> filter(Partial == "1") |> select(Bell = Bell, F0 = Frequency), 
    by = "Bell"
  ) |> 
  mutate(
    FrequencyRatio = Frequency / F0,
    Partial = recode(
      Partial,
      "~3.3" = "3.33",
      "~6.667" = "6.67",
      "~5.334" = "5.33",
      "4 - 4.2" = "4.0",
    )
  )

partial_labels <- tribble(
  ~ Partial,   ~ PartialLabel,
  "0.5", "Hum",
  "1", "Prime",
  "1.2", "Tierce",
  "1.5", "Quint",
  "2", "Nominal",
  "2.5", "Deciem",
  "2.61", "Undeciem",
  "3", "Duodeciem",
  "3.33", "III-4",
  "4.0", "Upper Octave",
  "5.33", "Upper Fourth",
  "6.67", "Upper Sixth",
) |> 
  add_column()

# Plotting an example spectrum

df_f0 <- df |> filter(Partial == 1) |> select(Bell, Frequency) |> mutate(Frequency = Frequency )

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

bell <- "40-e3"
df_spectrum <- get_spectrum(bell)

df_spectrum |> 
  filter(freq_ratio <= 7) |>
  ggplot(aes(Hz, dB)) + 
  geom_line() + 
  scale_x_continuous(
    "Frequency (Hz)",
    sec.axis = sec_axis(
      ~ . / df_f0 |> filter(Bell == !!bell) |> pull(Frequency) |> magrittr::multiply_by(2),
      name = "Frequency ratio",
    )
  ) + 
  scale_y_continuous("Level (dB)")


# Plotting frequencies ####
plot_freq <- 
  df |> 
  ggplot(aes(
    x = FrequencyRatio,
    y = factor(Partial),
    colour = factor(Partial),
    fill = factor(Partial),
    group = factor(Partial)
  )) + 
  geom_density_ridges(bandwidth = 0.01) +
  geom_text(
    aes(x = 7.15, y = Partial, label = PartialLabel), data = partial_labels, 
    inherit.aes = FALSE, hjust = "left"
    
  ) +
  scale_x_continuous(
    "Frequency ratio",
    limits = c(0.4, 8.5),
    breaks = seq(from = 0, to = 7, by = 1), 
    minor_breaks = seq(from = 0, to = 7, by = 0.5),
    sec.axis = sec_axis(
      ~ log2(.) * 12,
      breaks = 4 * 0:8,
      name = "Interval above the prime (semitones)"
    )
  ) + 
  scale_y_discrete(NULL) +
  scale_colour_viridis_d(option = "magma", end = 0.8) +
  scale_fill_viridis_d(option = "magma", end = 0.8) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_line(colour = "grey95"),
    panel.grid.minor.x = element_line(colour = "grey95"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plot_freq

# Plotting amplitudes ####
summarise_amplitudes <- function(data) {
  reference_amplitude <- 
    data |> 
    filter(Partial == "0.5") |> 
    pull(Amplitude) |> 
    mean(na.rm = TRUE)
  
  data |> 
    left_join(partial_labels, by = "Partial") |>
    mutate(
      Amplitude = Amplitude / reference_amplitude
    ) |> 
    group_by(Partial) |> 
    summarise(
      partial_label = unique(PartialLabel),
      amplitude_mean = mean(Amplitude, na.rm = TRUE),
      amplitude_n = sum(!is.na(Amplitude)),
      ampltitude_sd = sd(Amplitude, na.rm = TRUE),
      amplitude_se = ampltitude_sd / sqrt(amplitude_n)
    ) |> 
    ungroup() 
}

# Overall amplitudes
plot_amp <- 
  summarise_amplitudes(df) |> 
  ggplot(aes(x = Partial, y = amplitude_mean, ymin = amplitude_mean - amplitude_se, ymax = amplitude_mean + amplitude_se)) + 
  geom_bar(stat = "identity", fill = "white", colour = "black") + 
  geom_errorbar(width = 0.25) + 
  scale_x_discrete(labels = partial_labels$PartialLabel) +
  scale_y_continuous("Amplitude", breaks = seq(from = 0, to = 1.25, by = 0.25)) +
  coord_flip()


tmp_theme <- theme(
  text = element_text(size = 10), 
  axis.title = element_text(size = 12.5)
)

cowplot::plot_grid(
  plot_amp + tmp_theme,
  plot_freq + tmp_theme,
  nrow = 1
)

ggsave("output/figure-2.pdf", width = 10, height = 4)


# Splitting by frequency range
bind_rows(
  df |> filter(F0 < 320) |> summarise_amplitudes() |> add_column(range = "F0 < 320 Hz"),
  df |> filter(F0 > 320) |> summarise_amplitudes() |> add_column(range = "F0 > 320 Hz"),
) |> 
  ggplot(aes(x = Partial, y = amplitude_mean, ymin = amplitude_mean - amplitude_se, ymax = amplitude_mean + amplitude_se)) + 
  geom_bar(stat = "identity", fill = "white", colour = "black") + 
  geom_errorbar(width = 0.25) + 
  scale_x_discrete(labels = partial_labels$PartialLabel) +
  scale_y_continuous("Amplitude", breaks = seq(from = 0, to = 1.25, by = 0.25)) +
  coord_flip() + 
  facet_wrap(~ range, nrow = 1, scales = "free_x") +
  tmp_theme

ggsave("output/figure-3.pdf", width = 10, height = 4)

