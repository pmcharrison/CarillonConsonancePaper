library(ggpubr)
library(furrr)
library(tidyverse)
library(readxl)

theme_set(theme_pubr())

plan(multisession)

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
  
  f0_amplitude <- 
    representative_spectrum |> 
    filter(Partial == 1) |> 
    pull(Amplitude)
  
  partial_labels <- read_csv("output/partial_labels.csv", col_types = cols()) |> 
    mutate(Partial = as.numeric(Partial))
  
  representative_spectrum <-
    representative_spectrum |> 
    mutate(
      Amplitude = Amplitude / f0_amplitude,
      Partial = as.numeric(as.character(Partial))
    ) |> 
    left_join(partial_labels, by = "Partial")
}

lower_bell_spectrum <- get_representative_spectrum(c(65, 67)) |> na.omit()
upper_bell_spectrum <- get_representative_spectrum(c(65, 82)) |> na.omit()

write_csv(lower_bell_spectrum, "output/lower_bell_spectrum.csv")
write_csv(upper_bell_spectrum, "output/upper_bell_spectrum.csv")

full_join(
  lower_bell_spectrum |> select(- PartialLabel),
  upper_bell_spectrum,
  by = "Partial",
  suffix = c("Lower", "Upper")
) |> 
  select(- Partial) |> 
  select(PartialLabel, starts_with("FrequencyRatio"), starts_with("Amplitude")) |> 
  arrange(pmax(FrequencyRatioLower, FrequencyRatioUpper, na.rm = TRUE)) |> 
  mutate(across(is.numeric, ~ sprintf("%.3f", .))) |> 
  write_csv("output/idealised_bell_spectra_formatted.csv")


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
    interval = seq(from = 0, to = 15, by = 0.01),
    timbre_type = c("Carillon", "Harmonic"),
    model = c("roughness", "harmonicity")
)

model_profiles$value <- 
  future_pmap_dbl(model_profiles, function(interval, timbre_type, model) {
    if (!exists("COHERENT_WAVES")) source("src/consonance-utilities.R")
    midi <- c(66, 66 + interval)
    MODELS[[model]]$get_consonance(midi, timbre = timbres[[timbre_type]])
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))
  

write_csv(model_profiles, "output/model_profiles.csv")

model_profiles |> 
  filter(model == "roughness") |> 
  group_by(timbre_type, model) |> 
  summarise(
    interval = interval[which.max(value)],
    value = max(value)
  )

normalise_to_unit_interval <- function(x) {
  range <- max(x) - min(x)
  (x - min(x)) / range
}

plot_models <- 
  model_profiles |>  
  mutate(
    model = recode(
      model,
      harmonicity = "Harmonicity",
      roughness = "Interference between partials"
    ),
    timbre_type = recode(
      timbre_type,
      Carillon = "Carillon bell       ",
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
  facet_wrap(~ model, ncol = 1) +
  scale_colour_identity(NULL) + 
  scale_linewidth_manual(NULL, values = c(0.7, 0.4)) +
  scale_linetype_discrete(NULL) + 
  scale_x_continuous("Interval (semitones)", breaks = 0:15) + 
  scale_y_continuous("Value") +
  theme(
    legend.position = c(0.2, 1.140),
    legend.direction = "horizontal",
    plot.margin = unit(c(40, 4, 4, 4), units = "pt")
  )
plot_models

carillon_behavioural_profile <- read_csv("output/carillon-behavioural-profile.csv")

df_regression_input <- 
  model_profiles %>% 
  filter(timbre_type == "Carillon") %>% 
  select(- timbre_type) %>% 
  pivot_wider(id_cols = c("interval"), names_from = "model", values_from = "value") %>%
  left_join(carillon_behavioural_profile, by = c("interval" = "pitch_interval")) %>% 
  select(- pleasantness_se) %>% 
  mutate(
    pleasantness = as.numeric(scale(pleasantness)),
    roughness = as.numeric(scale(roughness)),
    harmonicity = as.numeric(scale(harmonicity))
  )

lm_formula <- as.formula("pleasantness ~ roughness + harmonicity")
mod <- lm(lm_formula, data = df_regression_input)
summary(mod)

df_regression_predictions <-
  df_regression_input %>%
  mutate(regression = predict(mod, newdata = df_regression_input))

plot_regression <- 
  df_regression_predictions %>% 
  select(interval, Participants = pleasantness, Model = regression) %>% 
  pivot_longer(cols = c("Participants", "Model"), names_to = "group") %>% 
  mutate(
    group = recode_factor(
      group,
      Participants = "Participants",
      Model = "Combined model",
    )
   ) %>% 
  ggplot(aes(interval, value, colour = group)) +#£, linetype = group)) + 
  geom_line() +
  scale_x_continuous("Interval (semitones)", breaks = 0:15) + 
  scale_y_continuous("Value") +
  scale_colour_manual(NULL, values = c("lightblue", "black")) +
  theme(
    legend.direction = "vertical",
    legend.position = c(0.2, 0.85)
  )
plot_regression

cowplot::plot_grid(
  plot_models,
  plot_regression,
  ncol = 1,
  labels = "AUTO"
)


ggsave("output/figure-5.pdf", width = 10, height = 10)
ggsave("output/figure-5.png", width = 10, height = 10, dpi = 300)


bootstrap_smooths <- readRDS("output/bootstrap_smooths.rds")
n_bootstrap <- ncol(bootstrap_smooths)

boot_coef <- 
  map_dfr(1:n_bootstrap, function(i) {
    tmp_df <- df_regression_input |> mutate(pleasantness = bootstrap_smooths[, i])
    boot_lm <- lm(lm_formula, data = tmp_df)
    boot_lm |> coef()
  })

roughness_coef <- coef(mod)["roughness"]
roughness_sem <- sd(boot_coef$roughness)
roughness_95_CI <- roughness_coef + 1.96 * roughness_sem * c(-1, 1)

harmonicity_coef <- coef(mod)["harmonicity"]
harmonicity_sem <- sd(boot_coef$harmonicity)
harmonicity_95_CI <- harmonicity_coef + 1.96 * harmonicity_sem * c(-1, 1)
