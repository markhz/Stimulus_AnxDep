
rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Scripts_ILE"
setwd(path_wd)

library(tidyverse)
library(purrr)
library(mice)
source("../Scripts/pulse_process_functions.R")

# -------------------------------------------------------------------------

# data path
dataPath <- "../Data/"
rawDataPath <- file.path(dataPath, "PUF_CSV")

outputPath <- file.path(dataPath, "National_PulseData_ILE")
filePath_var_OI <- file.path(outputPath, "var_OI_ILE.csv")

# -------------------------------------------------------------------------

dir.create(outputPath, showWarnings = FALSE)

df_states <- read.csv(file.path(dataPath, "pulse_states.csv")) %>%
  mutate(REGION = as.factor(REGION))

# import table indicating variables to import
var_OI <- import_clean_var_OI(filePath_var_OI)

# -------------------------------------------------------------------------
# import all survey data, filter and merge
# -------------------------------------------------------------------------

# get paths to each survey
fullFilePaths_tmp <- getFilePaths(rawDataPath)

# only import select Weeks
fullFilePaths <- fullFilePaths_tmp[22:23]


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
          file.path(outputPath, "National_Pulse_imported.csv"),
          row.names = F)


# -------------------------------------------------------------------------

df <- construct_new_pulse_var(df_import) %>%
  mutate(EST_ST = as.character(EST_ST) )


freqTable<- function(df, by, varCalc){
  stratSym <- sym(by)
  varCalcSym <- sym(varCalc)
  
  df_out <- df %>%
    count(!!stratSym, !!varCalcSym) %>%
    group_by(!!stratSym) %>%
    mutate(prop = prop.table(n) )
    
  
  return(df_out)
}

freqTable(df, "LOST_WORK", "EIP")

# clean 
sum(df$EIP < 0) / nrow(df)


# number of those who applied for unemployment (=1)
table(df$UI_APPLY)
# number of those who applied and received unemployment (=1)
table(df$UI_RECV[df_import$df==1])

# number of those who lost income who applied for unemployment (=1)
table(df$UI_APPLY[df$LOST_WORK=="Household lost employment income"])
# number of those who lost income who received for unemployment (=1)
table(df$UI_RECV[df$LOST_WORK=="Household lost employment income"])


df_EIP <- df %>%
  filter(!is.na(EIP)) %>%
  filter(EIP > 0) %>%
  mutate(EIP_recv = EIP < 4 & EIP > 0) 



df_summ_EIP <- df %>%
  group_by(EIP, LOST_WORK) %>%
  summarize(N = n())
  
  


# get proportion of missing EIP responses
df_EIP_missing <- df_import_EIP %>%
  count(WEEK, EIP_missing) %>%
  group_by(WEEK) %>%
  mutate(prop = prop.table(n))

df_EIP_recv <- df_import_EIP %>%
  filter(!EIP_missing) %>%
  count(WEEK, EIP_recv) %>%
  group_by(WEEK) %>%
  mutate(prop = prop.table(n) )



# -------------------------------------------------------------------------


# seen, but not answered
sum(df_import$EIP == -99, na.rm = TRUE ) 
# missing (did not see?)
sum(df_import$EIP == -88, na.rm = TRUE )

# seen, but not answered
sum(df_import$FEWRTRIPS == -99 ) 
# missing (did not see?)
sum(df_import$FEWRTRIPS == -88 )

# seen, but not answered
sum(df_import$CURFOODSUF == -99 ) 
# missing (did not see?)
sum(df_import$CURFOODSUF == -88 )

# seen, but not answered
sum(df_import$INTEREST == -99 ) 
# missing (did not see?)
sum(df_import$INTEREST == -88 )






  


