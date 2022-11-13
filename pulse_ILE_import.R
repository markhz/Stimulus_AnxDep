
rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Stimulus_Manuscript_DataAndCode/Stimulus_AnxDep"
setwd(path_wd)

library(tidyverse)
library(purrr)
source("Stim_Pulse_functions.R")

# -------------------------------------------------------------------------

# data path
dataPath <- "../Data_stim_manuscript/"
rawDataPath <- file.path(dataPath, "PUF_CSV")

# -------------------------------------------------------------------------



df_states <- read.csv(file.path(dataPath, "pulse_states.csv")) %>%
  mutate(REGION = as.factor(REGION))

# import table indicating variables to import
var_OI <- import_clean_var_OI( file.path(dataPath, "variables_of_interest.csv") )

# -------------------------------------------------------------------------
# import all survey data, filter and merge
# -------------------------------------------------------------------------

# get paths to each survey
fullFilePaths <- getFilePaths(rawDataPath)


# -------------------------------------------------------------------------

# import data
start_time = Sys.time()

df_import <- fullFilePaths %>%
  map_df(~ importPulseData(.x, var_OI ))

end_time = Sys.time()
time_import <- end_time - start_time

# -------------------------------------------------------------------------

# add State and Region name variables
df_import <- df_import %>%
  left_join(df_states, by = c("EST_ST" = "StateValue"))


# -------------------------------------------------------------------------

# save data frame with variables of interest
write.csv(df_import,
          file.path(dataPath, "National_Pulse_imported.csv"),
          row.names = F)


# write.csv(df_import %>% filter(WEEK <= 22),
#           file.path(dataPath, "National_Pulse_Week13_22_StimulusPayment.csv"),
#           row.names = F)

  


