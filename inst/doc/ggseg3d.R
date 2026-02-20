## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  eval = interactive()
)
library(ggseg3d)
library(dplyr)
dir.create("img", showWarnings = FALSE)

## -----------------------------------------------------------------------------
# library(ggseg3d)
# 
# ggseg3d(hemisphere = "left") |>
#   pan_camera("left lateral")

## -----------------------------------------------------------------------------
# ggseg3d(hemisphere = "left") |>
#   pan_camera("left lateral") |>
#   snapshot_brain("img/intro-basic.png")

## -----------------------------------------------------------------------------
# library(dplyr)
# 
# some_data <- tibble(
#   region = c("precentral", "postcentral", "insula", "superior parietal"),
#   p = c(0.01, 0.04, 0.2, 0.5)
# )
# 
# ggseg3d(.data = some_data, atlas = dk(), colour_by = "p", text_by = "p") |>
#   pan_camera("right lateral")

## -----------------------------------------------------------------------------
# some_data <- tibble(
#   region = c("precentral", "postcentral", "insula", "superior parietal"),
#   p = c(0.01, 0.04, 0.2, 0.5)
# )
# 
# ggseg3d(.data = some_data, atlas = dk(), colour_by = "p", text_by = "p") |>
#   pan_camera("right lateral") |>
#   snapshot_brain("img/intro-plot-data.png")

## -----------------------------------------------------------------------------
# subcort_data <- tibble(
#   region = c("Thalamus", "Caudate", "Hippocampus"),
#   p = c(0.2, 0.5, 0.8)
# )
# 
# ggseg3d(.data = subcort_data, atlas = aseg(), colour_by = "p", na_alpha = .5) |>
#   add_glassbrain()

## -----------------------------------------------------------------------------
# subcort_data <- tibble(
#   region = c("Thalamus", "Caudate", "Hippocampus"),
#   p = c(0.2, 0.5, 0.8)
# )
# 
# ggseg3d(.data = subcort_data, atlas = aseg(), colour_by = "p", na_alpha = .5) |>
#   add_glassbrain() |>
#   snapshot_brain("img/intro-subcortical.png")

## -----------------------------------------------------------------------------
# ggseg3d(hemisphere = "left") |>
#   pan_camera("left lateral") |>
#   set_background("black")

## -----------------------------------------------------------------------------
# ggseg3d(hemisphere = "left") |>
#   pan_camera("left lateral") |>
#   set_background("black") |>
#   snapshot_brain("img/intro-background.png")

