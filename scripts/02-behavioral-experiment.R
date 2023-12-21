library(tidyverse)
library(ggpubr())

theme_set(theme_pubr())

loess_span <- 0.1

trials <- 
  read_csv("input/behavioural-data/carillon-data/data/ConsonanceTrial.csv", col_types = cols())

trials <- 
  trials |> 
  bind_cols(
    map_dfr(trials$definition, ~ gsub("'", "\"", .) |> jsonlite::fromJSON() |> as_tibble())
  ) |> 
  select(
    id, participant_id, creation_time, answer, lower_pitch, upper_pitch, pitch_interval, is_repeat_trial
  ) |> 
  # z-score responses within participants, so that the mean is 0 and the standard deviation is 1
  group_by(participant_id) |> 
  mutate(answer = as.numeric(scale(answer))) |> 
  ungroup()

x_seq <- seq(from = 0, to = 15, by = 0.01)

stopifnot(!any(is.na(trials$answer)))

participants <-
  trials |> 
  group_by(participant_id) |> 
  summarise(
    n_trials = n(),
    n_not_repeat_trials = sum(!is_repeat_trial),
    n_repeat_trials = sum(is_repeat_trial),
    completed_all_trials = n_repeat_trials >= 4
  ) |>
  left_join(
    read_csv("input/behavioural-data/carillon-data/data/Participant.csv", col_types = cols()),
    by = c("participant_id" = "id")
  ) |> 
  mutate(
    vars = map(vars, reticulate::py_eval),
    age = map_chr(vars, pluck, "age", .default = NA) |> as.numeric(),
    gender = map_chr(vars, pluck, "gender", .default = NA),
    musical_training = map_dbl(vars, pluck, "gmsi", "mean_scores_per_scale", "Musical Training", .default = NA)
  )

participants |> nrow()
participants |> filter(completed_all_trials) |> nrow()

participants$age |> mean(na.rm = TRUE)
participants$age |> sd(na.rm = TRUE)

participants$gender |> table()

participants$base_payment
participants$bonus - participants$performance_bonus


mean(lubridate::as_datetime(participants$end_time) - lubridate::as_datetime(participants$creation_time), na.rm = TRUE)



# PsyNet reports these as mean scores, but according to the 
# Gold-MSI technical report these should be reported as sum scores.
# Since there are 7 questions we multiply these by 7
(participants$musical_training * 7) |> mean(na.rm = TRUE)
(participants$musical_training * 7) |> sd(na.rm = TRUE)

# Fits a smooth curve to the data.
# span dictates the degree of smoothing (see ?loess)
fit_loess <- function(trials, span = loess_span) {
  mod <- loess(answer ~ pitch_interval, data = trials, span = span)
  predict(mod, newdata = data.frame(pitch_interval = x_seq)) |> as.numeric()
}


smooth <- fit_loess(trials, span = loess_span)


# Creates bootstrapped resamples of these smoothed curves.
# The distribution of these curves tells us the uncertainty in our data.
bootstrap_fit_loess <- function(trials, n_boot = 1000) {
  by_participant <- split(
    trials |> select(pitch_interval, answer), 
    trials$participant_id
  )
  
  set.seed(12345)
  map(1:n_boot, function(i) {
    bootstrap_trials <- 
      sample(by_participant, size = length(by_participant), replace = TRUE) |> 
      bind_rows()
    fit_loess(bootstrap_trials)
  }, .progress = TRUE) |> 
    invoke(cbind, .x = _)
}

bootstrap_smooths <- bootstrap_fit_loess(trials)
bootstrap_se <- apply(bootstrap_smooths, 1, sd)

saveRDS(bootstrap_smooths, "output/bootstrap_smooths.rds")

# Major vs minor 3rd ####

major_3rd_index <- which(x_seq == 4)
minor_3rd_index <- which(x_seq == 3)

major_3rd_pleasantness <- smooth[major_3rd_index]
minor_3rd_pleasantness <- smooth[minor_3rd_index]

# How much more pleasant is the minor 3rd than the major 3rd?
minor_vs_major_3rd <- minor_3rd_pleasantness - major_3rd_pleasantness
minor_vs_major_3rd

bootstrap_minor_vs_major_3rd <- 
  bootstrap_smooths[minor_3rd_index, ] - 
  bootstrap_smooths[major_3rd_index, ]

mean(bootstrap_minor_vs_major_3rd)
se_minor_vs_major_3rd <- sd(bootstrap_minor_vs_major_3rd)

# What is the 95% confidence interval for the difference between the
# minor 3rd and the major 3rd?
ci_95_minor_vs_major_3rd <- 
  c(
    minor_vs_major_3rd - 1.96 * se_minor_vs_major_3rd,
    minor_vs_major_3rd + 1.96 * se_minor_vs_major_3rd
  )
ci_95_minor_vs_major_3rd

# Minor sixth vs tritone ####

minor_sixth_index <- which(x_seq == 8)
tritone_index <- which(x_seq == 6)

minor_sixth_pleasantness <- smooth[minor_sixth_index]
tritone_pleasantness <- smooth[tritone_index]

# How much more pleasant is the minor 6th than the major 6th?
tritone_vs_minor_sixth <- tritone_pleasantness - minor_sixth_pleasantness
tritone_vs_minor_sixth

bootstrap_tritone_vs_minor_sixth <- 
  bootstrap_smooths[tritone_index, ] - 
  bootstrap_smooths[minor_sixth_index, ]

mean(bootstrap_tritone_vs_minor_sixth)
se_tritone_vs_minor_sixth <- sd(bootstrap_tritone_vs_minor_sixth)

# What is the 95% confidence interval for the difference between the
# minor 6th and the major 6th?
ci_95_tritone_vs_minor_sixth <- 
  c(
    tritone_vs_minor_sixth - 1.96 * se_tritone_vs_minor_sixth,
    tritone_vs_minor_sixth + 1.96 * se_tritone_vs_minor_sixth
  )
ci_95_tritone_vs_minor_sixth

# Plotting ####
df_carillon_profile <- tibble(
  type = "Carillon",
  pitch_interval = x_seq,
  pleasantness = smooth,
  pleasantness_se = bootstrap_se
)

write_csv(df_carillon_profile, "output/carillon-behavioural-profile.csv")

df_harmonic_reference <- read_csv(
  "input/behavioural-data/reference-data-from-marjieh-et-al/1 - Harmonic dyads (3 dB roll-off).csv", 
  col_types = cols()
) |> 
  filter(measure == "Participants") |> 
  transmute(
    type = "Harmonic tone",
    pitch_interval = interval,
    pleasantness = value
  )

df_combined_consonance <- bind_rows(
  df_carillon_profile,
  df_harmonic_reference
)

plot_behavioural_profiles <- 
  df_combined_consonance |> 
  ggplot(aes(
    pitch_interval, 
    pleasantness,
    ymin = pleasantness - pleasantness_se,
    ymax = pleasantness + pleasantness_se,
    linetype = type,
    linewidth = type,
    colour = type
  )) +
  geom_ribbon(colour = NA, fill = "#e5e5ff") +
  geom_line() +
  scale_x_continuous("Pitch interval (semitones)", breaks = 0:15) + 
  scale_y_continuous("Pleasantness (z-scored)") + 
  scale_colour_manual(NULL, values = c("blue", "grey40")) + 
  scale_linewidth_manual(NULL, values = c(0.7, 0.4)) +
  scale_linetype_discrete(NULL) + 
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme(
    legend.position = c(0.2, 0.95),
    legend.direction = "vertical",
    plot.margin = unit(c(20, 4, 4, 4), units = "pt")
  )
plot_behavioural_profiles

# ggsave("output/behavioural-profiles.png", width = 6.5, height = 4.5, dpi = 200)


participants <- read_csv("input/behavioural-data/carillon-data/data/Participant.csv", col_types = cols())

(participants$progress == 1) |> table()

reliabilities <- 
  read_csv("input/behavioural-data/carillon-data/data/ChainTrialMakerState.csv", col_types = cols()) |> 
  select(module_id, performance_check) |> 
  filter(module_id == "consonance_main_experiment") |> 
  pull(performance_check) |> 
  na.omit() |> 
  gsub("'", "\"", x = _) |> 
  gsub("True", "true", x = _) |> 
  gsub("False", "false", x = _) |> 
  gsub("None", "null", x = _) |> 
  map(jsonlite::fromJSON) |> 
  map("score") |> 
  Filter(function(x) !is.null(x), x = _) |> 
  unlist()

median(reliabilities)

plot_reliabilities <- 
  tibble(reliability = reliabilities) |> 
  ggplot(aes(reliability)) + 
  geom_histogram(bins = 15, colour = "black", fill = "grey80") + 
  scale_x_continuous("Participant reliabilities") +
  scale_y_continuous("Count")


cowplot::plot_grid(
  plot_reliabilities,
  plot_behavioural_profiles,
  nrow = 1,
  rel_widths = c(4, 8),
  labels = "AUTO"
)

ggsave("output/figure-5.pdf", width = 10, height = 4)
ggsave("output/figure-5.png", width = 10, height = 4, dpi = 300)
