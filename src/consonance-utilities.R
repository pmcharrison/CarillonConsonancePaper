# Here we define some wrapper functions for our consonance models that allow us
# to run them efficiently for different timbres. This code is excerpted 
# from supporting materials for the following paper:
# > "Reshaping musical consonance with timbral manipulations and massive 
# online experiments" by Marjieh, Harrison, Lee, Deligiannaki, & Jacoby (2022).

# remotes::install_github("pmcharrison/dycon")
# remotes::install_github("pmcharrison/har18")

library(hrep)
library(dycon)
library(har18)
library(R6)
library(tidyverse)


COHERENT_WAVES <- TRUE

Timbre <- R6Class(
  "Timbre",
  public = list(
    
    label = NULL,
    
    initialize = function(label) {
      self$label <- label
    },
    
    sparse_fr_spectrum = function(midi, coherent) {
      stop("needs to be implemented")
    }
    
  )
)

PureTone <- R6Class(
  "PureTone",
  inherit = Timbre,
  public = list(
    
    initialise = function(label = "Pure") {
      super$initialize(label = label)
    },
    
    sparse_fr_spectrum = function(midi, coherent) {
      hrep::sparse_fr_spectrum(as.numeric(midi),
                               num_harmonics = 1,
                               coherent = coherent)
    }
    
  )
)

CustomTone <- R6Class(
  "CustomTone",
  inherit = Timbre,
  public = list(
    frequency_ratios = NULL,
    amplitudes = NULL,
    
    initialize = function(
    frequency_ratios,
    amplitudes,
    label = "Custom"
    ) {
      super$initialize(label = label)
      
      stopifnot(
        length(frequency_ratios) == length(amplitudes)
      )
      
      self$frequency_ratios = frequency_ratios
      self$amplitudes = amplitudes
    },
    
    tone_sparse_fr_spectrum = function(pitch) {
      f0 <- hrep::midi_to_freq(pitch)
      df <- tibble::tibble(
        frequency = f0 * self$frequency_ratios,
        amplitude = self$amplitudes
      )
      spec <- df %>% as.list() %>% hrep::sparse_fr_spectrum()
      spec
    },
    
    sparse_fr_spectrum = function(midi, coherent) {
      args <- midi %>% map(self$tone_sparse_fr_spectrum)
      args$coherent <- coherent
      do.call(hrep::combine_sparse_spectra, args)
    }
  )
)

HarmonicTone <- R6Class(
  "HarmonicTone",
  inherit = Timbre,
  public = list(
    
    n_harmonics = NULL,
    harmonic_amplitudes = NULL, 
    octave_definition = NULL,
    
    initialize = function(harmonic_amplitudes,  
                          octave_definition = 2,
                          label = "Harmonic") {
      super$initialize(label = label)
      
      self$n_harmonics <- length(harmonic_amplitudes)
      self$harmonic_amplitudes <- harmonic_amplitudes
      self$octave_definition <- octave_definition
    },
    
    sparse_fr_spectrum = function(midi, coherent) {
      args <- midi %>% map(self$tone_sparse_fr_spectrum)
      args$coherent <- coherent
      do.call(hrep::combine_sparse_spectra, args)
    },
    
    tone_sparse_fr_spectrum = function(pitch) {
      f0 <- hrep::midi_to_freq(pitch)
      i <- seq_len(self$n_harmonics)
      frequency <- f0 * self$octave_definition ^ log2(i)
      amplitude <- self$harmonic_amplitudes
      hrep::sparse_fr_spectrum(
        list(frequency, amplitude)
      )
    }
  )
)

BasicHarmonicTone <- R6Class(
  "BasicHarmonicTone",
  inherit = HarmonicTone,
  public = list(
    
    decay_dB_per_octave = NULL,
    
    initialize = function(n_harmonics = 10L, 
                          decay_dB_per_octave = 12, 
                          octave_definition = 2,
                          label = sprintf("Harmonic (%i harmonics, %s dB/octave decay, octave ratio = %s",
                                          n_harmonics,
                                          decay_dB_per_octave,
                                          octave_definition)) {
      self$decay_dB_per_octave <- decay_dB_per_octave
      df <- tibble::tibble(
        i = seq_len(n_harmonics),
        dB = - decay_dB_per_octave * log2(i),
        amplitude = 1 * 10 ^ (dB / 20) 
      )
      super$initialize(harmonic_amplitudes = df$amplitude,
                       octave_definition = octave_definition,
                       label = label)
    }
  )
)

GamelanTone <- R6Class(
  "GamelanTone",
  inherit = Timbre,
  public = list(
    
    initialize = function(label = "Bonang") {
      super$initialize(label = label)
    },
    
    sparse_fr_spectrum = function(midi, coherent) {
      args <- midi %>% map(self$tone_sparse_fr_spectrum)
      args$coherent <- coherent
      do.call(hrep::combine_sparse_spectra, args)
    },
    
    tone_sparse_fr_spectrum = function(pitch) {
      f0 <- hrep::midi_to_freq(pitch)
      df <- tibble::tibble(
        frequency = f0 * c(1, 1.52, 3.46, 3.92),
        amplitude = 1
      )
      spec <- df %>% as.list() %>% hrep::sparse_fr_spectrum()
      spec
    }
  )
)

ConsonanceModel <- R6Class(
  "ConsonanceModel",
  public = list(
    
    vectorised = FALSE,
    allow_parallel = NULL,
    theory = NULL,
    label = NULL,
    plot_colour = NULL,
    options = NULL,
    
    initialize = function(
    theory, 
    label, 
    plot_colour = "black", 
    options = list()
    ) {
      self$theory <- theory
      self$label <- label
      self$plot_colour = plot_colour
      self$options = options
    },
    
    get_consonance = function(midi, timbre) {
      stop("not implemented")
    },
    
    get_consonance_list = function(midi_list, timbre) {
      stop("not implemented")
    },
    
    get_consonance_list_batched = function(midi_list, timbre, batch_size = 500L) {
      batch_ids <- ceiling(seq_along(midi_list) / batch_size)
      batches <- split(midi_list, batch_ids)
      results_by_batch <- plyr::llply(batches, 
                                      self$get_consonance_list, 
                                      timbre = timbre, 
                                      .progress = "time")
      do.call(c, unname(results_by_batch))
    },
    
    get_sparse_fr_spectrum = function(midi, timbre, coherent) {
      if (is(timbre, "Timbre")) {
        timbre$sparse_fr_spectrum(midi, coherent = coherent)
      } else if (is.list(timbre)) {
        stopifnot(length(timbre) == length(midi)) 
        spectra <- map2(timbre, midi, ~ .x$sparse_fr_spectrum(.y, coherent = coherent))
        args <- spectra
        args$coherent <- coherent
        do.call(hrep::combine_sparse_spectra, args)
      }
    }
  )
)

DyadInterferenceModel <- R6Class(
  "DyadInterferenceModel",
  inherit = ConsonanceModel,
  public = list(
    
    allow_parallel = TRUE,
    
    initialize = function(label, theory = "interference", plot_colour = "#B50000", ...) {
      super$initialize(
        label = label,
        theory = theory,
        plot_colour = plot_colour,
        ...
      )
    },
    
    get_consonance = function(midi, timbre) {
      spectrum <- self$get_sparse_fr_spectrum(midi, timbre, coherent = COHERENT_WAVES)
      - self$get_roughness_from_sparse_fr_spectrum(spectrum)
    },
    
    get_roughness_from_sparse_fr_spectrum = function(spectrum) {
      stop("not implemented")
    }
  )
)

Hutch78 <- R6Class(
  "Hutch78",
  inherit = DyadInterferenceModel,
  public = list(
    initialize = function(
    label = "Hutchinson & Knopoff (1978)",
    ...
    ) {
      super$initialize(
        label = label,
        ...
      )
    },
    
    get_roughness_from_sparse_fr_spectrum = function(spectrum) {
      dycon::roughness_hutch(spectrum)
    }
    
  )
)

Seth93 <- R6Class(
  "Seth93",
  inherit = DyadInterferenceModel,
  public = list(
    
    initialize = function(label = "Sethares (1993)", ...) {
      super$initialize(
        label = label, 
        ...
      )
    },
    
    get_roughness_from_sparse_fr_spectrum = function(spectrum) {
      dycon::roughness_seth(spectrum)
    }
    
  )
)

Vass01 <- R6Class(
  "Vass01",
  inherit = DyadInterferenceModel,
  public = list(
    
    initialize = function(label = "Vassilakis (2001)", ...) {
      super$initialize(
        label = label, ...
      )
    },
    
    get_roughness_from_sparse_fr_spectrum = function(spectrum) {
      dycon::roughness_vass(spectrum)
    }
    
  )
)

PCTemplateModel <- R6Class(
  "PCTemplateModel",
  inherit = ConsonanceModel,
  public = list(
    
    allow_parallel = TRUE,
    
    initialize = function(label, theory = "harmonicity", plot_colour = "#11A3FF", ...) {
      super$initialize(
        label = label,
        theory = theory,
        plot_colour = plot_colour,
        ...
      )
    },
    
    get_consonance = function(midi, timbre) {
      pc_spectrum <- 
        self$get_sparse_fr_spectrum(midi, timbre, coherent = COHERENT_WAVES) %>% 
        hrep::smooth_pc_spectrum(coherent = COHERENT_WAVES) # <--- 5 ms
      pitch_profile <- self$get_pitch_profile(pc_spectrum) # <--- 12 ms
      self$eval_profile(pitch_profile) # <--- v fast
    },
    
    get_pitch_profile = function(x) {
      stopifnot(is(x, "smooth_pc_spectrum"))
      x <- as.numeric(x)
      array_dim <- length(x)
      template <- hrep::smooth_pc_spectrum( # <--- 4 ms
        hrep::pi_chord(60),
        array_dim = array_dim,
        coherent = COHERENT_WAVES
      )
      res <- har18::sweep_template(x, template) # <--- 8 ms
      hrep::.smooth_pc_spectrum(res)
    },
    
    eval_profile = function(profile) {
      stop("not implemented")
    }
  )
)


Milne13 <- R6Class(
  "Milne13",
  inherit = PCTemplateModel,
  public = list(
    
    initialize = function(label = "Milne (2013)", ...) {
      super$initialize(label = label, ...)
    },
    
    eval_profile = function(profile) {
      max(as.numeric(profile))
    }
    
  )
)

Har18 <- R6Class(
  "Har18",
  inherit = PCTemplateModel,
  public = list(
    
    initialize = function(label = "Harrison & Pearce (2018)", ...) {
      super$initialize(label = label, ...)
    },
    
    eval_profile = function(profile) {
      self$kl_div_from_uniform(as.numeric(profile))
    },
    
    kl_div_from_uniform = function(x) {
      probs <- x / sum(x)
      n <- length(probs)
      uniform_probs <- 1 / n
      non_zero_probs <- probs[probs > 0]
      sum(
        non_zero_probs * log(non_zero_probs / uniform_probs, base = 2)
      )
    }
  )
)

MODELS <- list(
  roughness = Hutch78$new(),
  harmonicity = Har18$new()
)