library(shiny)
library(shinydashboard)
library(shinyBS)
library(magrittr)
library(ggplot2)
library(plotly)
library(readxl)
library(googlesheets)
library(dplyr)
library(tidyr)
`%then%` <- shiny:::`%OR%`

# modules


# make sure base directory is set
if (!exists(".base_dir", env = .GlobalEnv))
  .GlobalEnv$.base_dir <- file.path(getwd(), "data")

# fixed settings
SIDEBAR_WIDTH <- 150 #px
SETTINGS_FILE <- "labware_c3_settings.xlsx"
DATA_DIR <- "cached_data"

# make sure base folder
if (!file.exists(.base_dir))
  dir.create(.base_dir)

# make sure data folder exists
if (!file.exists(file.path(.base_dir, DATA_DIR)))
  dir.create(file.path(.base_dir, DATA_DIR))

# copy default settings if needed
.settings_file <- file.path(.GlobalEnv$.base_dir, SETTINGS_FILE)
if (!file.exists(.settings_file)) {
  message("INFO: No settings file exists in this workspace yet. Copying default settings file.")
  .default_settings_file <- system.file("gui", "default_settings.xlsx", package = "labwareC3")
  file.copy(.default_settings_file, .settings_file)
}


