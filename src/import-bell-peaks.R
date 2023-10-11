library(tidyverse)

import_bell_peaks <- function() {
  # The original dataset has missing rows for partials that aren't present
  # in a given bell. We impute 0 for these rows.
  
  df <- read_excel("input/bell-peaks.xlsx", sheet = "ALL DATA - with Partials")
  
  # Get an ordered list of the possible partial values
  partials <- df$Partial |> unique()
  partials <- partials[gsub("~", "", partials) |> order()]

  df <- 
    df |> 
    mutate(Partial = factor(Partial, levels = partials)) |> 
    group_by(Bell, Partial, .drop = FALSE) |> 
    dplyr::summarise(
      Frequency = if (n() > 0) Frequency else NA_real_,
      Amplitude = if (n() > 0) Amplitude else 0,
      .groups = "drop"
    )
  
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
  
  df
}