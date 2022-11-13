rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Stimulus_Manuscript_DataAndCode/Stimulus_AnxDep"
setwd(path_wd)

library(tidyverse)
library(purrr)
library(tictoc)
library(survey)
library(margins)
library(RColorBrewer)
source("Stim_Pulse_functions.R")



# set input/output paths --------------------------------------------------
# -------------------------------------------------------------------------

# input data path
dataPath <- "../Data_stim_manuscript/"

file_pulse_imported <- file.path(dataPath, "National_Pulse_imported.csv")
file_weektime <- file.path(dataPath, "pulse_weeknum_to_date.csv")


# set output folder path
outputPath <- file.path(dataPath, "Output_2022_11_13")

dir.create(outputPath, showWarnings = FALSE)



# import pulse data -------------------------------------------------------
# -------------------------------------------------------------------------

# import merged pulse data (waves 13-22)
df_import <- read.csv(file_pulse_imported)

df_weektime <- import_clean_weektime( file_weektime )



# clean pulse data --------------------------------------------------------
# -------------------------------------------------------------------------

# list variables to keep in data frame
var_include <- c("SCRAM", "WEEK", "EST_ST", "PWEIGHT", "HWEIGHT", "Phase",
                 "midDate", "startDate", "endDate",
                 "PHQ4_missing","ANXIETY_3","ANXIETY_4","ANXIETY_5","ANXIETY_6",
                 "DEPRESSION_3","DEPRESSION_4","DEPRESSION_5","DEPRESSION_6",
                 "EIP_RECV", "UI", "SNAP",
                 "GENDER","AGE", "AGE_4", "RACE_5", "withCHILDREN", "MS",
                 "EDU_2", "INCOME_4", "LOST_WORK", "NOWORK_COVID")



# clean imported data...
df_clean <- 
  # this function generates standardized labels/categories for multiple variables in pulse survey
  construct_new_pulse_var(df_import) %>%
  
  # additional cleaning and re-labeling
  mutate(EST_ST = as.character(EST_ST),
         withCHILDREN = factor(THHLD_NUMKID > 0 ,
                               levels = c(FALSE, TRUE),
                               labels = c("No children in household", "Children in household") ),
         UI_APPLY = factor(UI_APPLY,
                           levels = c(1,2,-88,-99),
                           labels = c("UI_APPLY","No UI_APPLY",NA,NA) ),
         EIP = ifelse(is.na(EIP), -999, EIP ),
         EIP_RECV = factor(EIP,
                           levels = c(1,2,3,4,-999, -99,-88),
                           labels = c("EIP","EIP","EIP","NoEIP","Baseline","Missing","Missing") )
         )  %>%
  left_join(df_weektime, by = "WEEK") %>%
  
  # select specific variables
  select(var_include)





# filter data - inclusion criteria ----------------------------------------
# -------------------------------------------------------------------------


N_all <- nrow(df_clean)

# include respondents with complete data (no missing variables of interest)
df_nomissingPHQ <- df_clean %>%
  filter(!PHQ4_missing) 

N_nomissingPHQ <- nrow(df_nomissingPHQ)
N_all - N_nomissingPHQ # total with missing anx/dep outcome PHQ score
(N_all - N_nomissingPHQ) / N_all # percent with missing anx/dep outcome PHQ score

df_nomissing <- df_nomissingPHQ %>%
  filter(INCOME_4 != "Missing") %>%
  filter(LOST_WORK != "Missing") %>%
  filter(NOWORK_COVID != "Missing") %>%
  filter(EIP_RECV != "Missing")  %>%
  filter(EIP_RECV != "Missing")  %>%
  filter(UI != "Missing")  %>%
  filter(SNAP != "Missing")  

N_nomissing <- nrow(df_nomissing)
N_all - N_nomissing # total with any missing variables
(N_all - N_nomissing) / N_all # percent with any missing variables


# -------------------------------------------------------------------------

# include respondents who are working age
df_workingage <- df_nomissing %>%
  filter(AGE < 65)

# -------------------------------------------------------------------------



# data frame for prevalence plot...
# ---

df_forPrevPlot <- df_clean %>%
  filter(WEEK %in% c(18,19,20,21,22) ) 
  # mutate(EIP_RECV = replace_na(EIP_RECV, 0))
 
write.csv(df_forPrevPlot,
          file.path(outputPath,
                    "National_Pulse_clean_Wave18to22_forPrevPlot.csv" ),
          row.names = F)
 

# data frame for analysis and regression model...
# ---

df_forAnalysis <- df_workingage %>%
  # eligible income
  filter((INCOME_4%in%c("0 - 34,999", "35,000 - 74,999") |
            INCOME_4%in%c("0 - 34,999", "35,000 - 74,999", "75,000 - 149,999") & MS == 1 )) %>%
  filter(WEEK == 22)  

write.csv(df_forAnalysis,
          file.path(outputPath,
                    "National_Pulse_clean_Wave22_forAnalysis.csv" ),
          row.names = F)




# variable labels/levels --------------------------------------------------
# -------------------------------------------------------------------------

genderLevels <- c("Female",
                  "Male")

race5Levels <- rev(c("Non-hispanic White",
                     "Non-hispanic Black",
                     "Hispanic",
                     "Non-hispanic Asian",
                     "Other or multiple"))

incomeLevels <- rev(c("0 - 34,999", 
                      "35,000 - 74,999", 
                      "75,000 - 149,999"))

edu2Levels <- c("< College",
                ">= College")
edu2Levels_re <- c("Less than college degree",
                   "College degree or higher")


age4Levels <- rev(c("18 - 24",
                    "25 - 44",
                    "45 - 64"))


lostworkLevels <- c("Household lost employment income", 
                    "Household did not lose employment income")

noworkLevels <- c("No work in past 7 days due to pandemic",
                  "No work in past 7 days unrelated to pandemic",
                  "Had work in past 7 days")
noworkLevels_re <- c("No work due to pandemic",
                     "No work unrelated to pandemic",
                     "Had work in past 7 days")

childLevels <- c("Children in household",
                 "No children in household")

UILevels <- c("Household previously applied for UI",
              "Household has not applied for UI")

SNAPLevels <- c("Household receives SNAP",
                "Household does not receive SNAP")

levels_for_stratvar <- c("NoEIP","EIP")
labels_for_stratvar <- c("Did not receive stimulus payment\nin past 7 days",
                         "Received stimulus payment\nin past 7 days")



# analysis ----------------------------------------------------------------
# -------------------------------------------------------------------------

# full anlaytic sample
# relevel factors 
df <- df_forAnalysis %>% 
  mutate(RACE_5 = factor(RACE_5, 
                         levels = race5Levels  ) ) %>%
  mutate(INCOME_4 = factor(INCOME_4, 
                           levels = incomeLevels  ) ) %>%
  mutate(EDU_2 = factor(EDU_2, 
                        levels = edu2Levels,
                        labels = edu2Levels_re ) ) %>%
  mutate(LOST_WORK = factor(LOST_WORK, 
                            levels = lostworkLevels) ) %>%
  mutate(AGE_4 = factor(AGE_4, 
                        levels = age4Levels ) ) %>%
  mutate(NOWORK_COVID = factor(NOWORK_COVID, 
                               levels = noworkLevels,
                               labels = noworkLevels_re ) ) %>%
  mutate(UI = factor(UI, 
                     levels = UILevels) )  %>%
  mutate(SNAP = factor(SNAP, 
                       levels = SNAPLevels) )  %>%
  # mutate(EIP_RECV = factor(EIP_RECV,
  #                          levels = levels_for_stratvar,
  #                          labels = labels_for_stratvar) )  %>%
  mutate(EIP_RECV = EIP_RECV == "EIP") %>%
  mutate(WEEK = factor(WEEK)) %>%
  mutate(EST_ST = factor(EST_ST))





# table 1 -----------------------------------------------------------------
# -------------------------------------------------------------------------




# chi-square test

round( chisq.test(df$EIP_RECV, df$GENDER)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$AGE_4)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$RACE_5)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$EDU_2)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$INCOME_4)$p.value , digits=3)
# round( chisq.test(df$EIP_RECV, df$LOST_WORK)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$withCHILDREN)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$NOWORK_COVID)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$UI)$p.value , digits=3)
round( chisq.test(df$EIP_RECV, df$SNAP)$p.value , digits=3)



# survey weighted Table 1...

# groupVar <- "GENDER"

calc_var_n_prop <- function(df, groupVar){
  # get proportions of each group
  df_prop <- data.frame( round( ( prop.table(table(df$EIP_RECV, df[[groupVar]]),2) ) * 100 , digits = 1)) %>%
    rename(EIP = Var1 ,
           group = Var2,
           prop = Freq)
  
  # get counts of each group
  df_n <- df %>%
    count(!!sym(groupVar) )
  names(df_n) <- c("group", "n")
  
  
  df_long <- data.frame( table(df$EIP_RECV, df[[groupVar]])  ) %>%
    rename(EIP = Var1 ,
           group = Var2,
           n = Freq) %>%
    left_join(df_prop, by = c("EIP", "group")) %>%
    mutate(n_prop_str = paste0(n, " (", format(round(prop, 1), nsmall = 1) , "%)"))
  
  
  df_table1_formatted <- df_long %>%
    select(c("EIP", "group", "n_prop_str")) %>%
    filter(EIP == TRUE) %>%
    # group_by(group) %>%
    pivot_wider(names_from = EIP, values_from = n_prop_str) %>%
    right_join(df_n, by = "group") %>%
    arrange(desc(group))
  
  
  return(df_table1_formatted)
}


# "LOST_WORK",
df_table1 <- c("GENDER", "AGE_4", "RACE_5", "EDU_2", "INCOME_4",  "NOWORK_COVID", "withCHILDREN", "UI", "SNAP") %>%
  map_df( ~ calc_var_n_prop(df, .x) )


write.csv(df_table1, file.path(outputPath, "Table1.csv"), row.names = FALSE )




# regression --------------------------------------------------------------
# -------------------------------------------------------------------------



# -------------------------------------------------------------------------






run_poisson_mdl <- function(svydsn, outcomeStr, covars, metric = "aPR", limitMargins = TRUE){
  
  formula_reg <- paste(outcomeStr, "~", paste(covars, collapse="+"))
  
  
  mdl <- svyglm( as.formula(formula_reg) , design=svydsn,
                 # family=quasibinomial() ) 
                 family="poisson" ) 
  
  # -------------------------------------------------------------------------
  
  if(metric == "aAME"){
    # aAME
    
    if (limitMargins){
      marg_mdl <- margins(mdl, design = svydsn, variables = "EIP_RECV")
    } else {
      marg_mdl <- margins(mdl, design = svydsn)
    }
    
    
    df_mdl <- gen_AME_df(marg_mdl)
    
    df_mdl_out <- df_mdl %>%
      mutate(Outcome = outcomeStr)
    
  } else {
    # aPR
    
    df_mdl <- gen_mdl_df(mdl)
    
    df_mdl_out <- df_mdl %>%
      mutate(Outcome = outcomeStr) %>%
      filter(Variable != "(Intercept)")
  }
  
  
  # -------------------------------------------------------------------------
  
  
  
  # -------------------------------------------------------------------------
  
  return(df_mdl_out)
}





run_all_mdls <- function(svydsn, covars,
                         metric = "aPR", 
                         limitMargins = TRUE,
                         limitOutcomes = TRUE){
  if(limitOutcomes){
    outcomes_in <- c(
      "ANXIETY_3", "DEPRESSION_3"   )
  } else {
    outcomes_in <- c(
      "ANXIETY_3", "DEPRESSION_3", 
      "ANXIETY_4", "DEPRESSION_4",
      "ANXIETY_5", "DEPRESSION_5"  )
  }
  
  
  
  
  df_mdl_out <- outcomes_in %>%
    map_df(~ run_poisson_mdl(svydsn, 
                             .x, 
                             covars, 
                             metric = metric,
                             limitMargins = limitMargins) )
  
  
  return(df_mdl_out)
  
}


# function for subgroup models --------------------------------------------
# -------------------------------------------------------------------------


gen_mdl_strat<- function(df, covars, strat_varname, strat_levels, modelType) {
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df[ df[[strat_varname]] == strat_levels,   ])
  
  if(tolower(modelType) == "crude"){
    covars_strat <- c("EIP_RECV")
  } else {
    covars_strat <- setdiff(covars, strat_varname)
  }
  
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars_strat, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = strat_levels,
           varCategory = strat_varname)
  
  return(df_mdl)
}


# -------------------------------------------------------------------------
#functions for subgroup models
# -------------------------------------------------------------------------



gen_mdl_gender <- function(df, genderLevel, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(GENDER == genderLevel))
  
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV","AGE_4","RACE_5", "EDU_2", "INCOME_4","withCHILDREN","NOWORK_COVID", "UI", "SNAP", "EST_ST")
  }
  
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = genderLevel,
           varCategory = "Gender")
  
  return(df_mdl)
}

gen_mdl_age <- function(df, ageLevel, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(AGE_4 == ageLevel))
  
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "RACE_5", "EDU_2", "INCOME_4","withCHILDREN","NOWORK_COVID", "UI", "SNAP", "EST_ST")
  }
  
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = ageLevel,
           varCategory = "Age")
  
  return(df_mdl)
}


gen_mdl_race <- function(df, raceLevel, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(RACE_5 == raceLevel))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4", "EDU_2", "INCOME_4","withCHILDREN","NOWORK_COVID", "UI", "SNAP", "EST_ST")
  }
  
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = raceLevel,
           varCategory = "Race/Ethnicity")
  
  return(df_mdl)
}
gen_mdl_edu<- function(df, eduLevel, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(EDU_2 == eduLevel))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "INCOME_4","withCHILDREN","NOWORK_COVID", "UI", "SNAP", "EST_ST")
  }
  
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = eduLevel,
           varCategory = "Education")
  
  return(df_mdl)
}
gen_mdl_income<- function(df, incomeLevel, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(INCOME_4 == incomeLevel))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2","withCHILDREN", "NOWORK_COVID", "UI", "SNAP", "EST_ST")
  }
  
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = incomeLevel,
           varCategory = "Household\nIncome")
  
  return(df_mdl)
}

gen_mdl_lostwork<- function(df, lostworkLevels, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(LOST_WORK == lostworkLevels))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2", "INCOME_4","withCHILDREN", "EST_ST")
  }
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = lostworkLevels,
           varCategory = "Lost Income")
  
  return(df_mdl)
}

gen_mdl_nowork<- function(df, noworkLevels, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(NOWORK_COVID == noworkLevels))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2", "INCOME_4","withCHILDREN", "UI", "SNAP", "EST_ST")
  }
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = noworkLevels,
           varCategory = "Work Status")
  
  return(df_mdl)
}

gen_mdl_child<- function(df, childLevels, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(withCHILDREN == childLevels))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2", "INCOME_4","NOWORK_COVID", "UI", "SNAP", "EST_ST")
  }
  
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = childLevels,
           varCategory = "Children")
  
  return(df_mdl)
}





gen_mdl_UI<- function(df, UILevels, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(UI == UILevels))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2", "INCOME_4", "withCHILDREN", "NOWORK_COVID",  "SNAP", "EST_ST")
  }
  
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = UILevels,
           varCategory = "UI")
  
  return(df_mdl)
}

gen_mdl_SNAP<- function(df, SNAPLevels, modelType) {
  
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~PWEIGHT,
                      nest = T,
                      data = df %>% filter(SNAP == SNAPLevels))
  if(tolower(modelType) == "crude"){
    covars <- c("EIP_RECV")
  } else {
    covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2", "INCOME_4", "withCHILDREN", "NOWORK_COVID",  "UI", "EST_ST")
  }
  
  
  df_mdl <- run_all_mdls(svydsn, 
                         covars = covars, 
                         metric = "aAME",
                         limitMargins = TRUE,
                         limitOutcomes = TRUE) %>%
    mutate(stratVar = SNAPLevels,
           varCategory = "SNAP")
  
  return(df_mdl)
}


# -------------------------------------------------------------------------
# run impact models
# -------------------------------------------------------------------------


# survey design
svydsn_pweight <- svydesign(ids = ~SCRAM,
                            weights = ~PWEIGHT,
                            nest = T,
                            data = df)
svydsn_hweight <- svydesign(ids = ~SCRAM,
                            weights = ~HWEIGHT,
                            nest = T,
                            data = df)
svydsn_noweight <- svydesign(ids = ~SCRAM,
                             weights = NULL,
                             nest = T,
                             data = df)



covars <- c("EIP_RECV", "GENDER", "AGE_4","RACE_5", "EDU_2", "INCOME_4", "withCHILDREN", "NOWORK_COVID", "UI", "SNAP",  "EST_ST")
model_names <-  c("Fully Adjusted", "Partially Adjusted", "Unadjusted")




# average marginal effects ------------------------------------------------


# adjusted AME
tic()
df_mdl_adj <- run_all_mdls(svydsn_pweight, 
                           covars = covars, 
                           metric = "aAME",
                           limitMargins = TRUE,
                           limitOutcomes = TRUE) %>%
  mutate(stratVar = "Full Sample",
         varCategory = "", 
         weighting = "Person")
toc()

# unadjusted AME
tic()
df_mdl_unadj <- run_all_mdls(svydsn_pweight, 
                             covars = "EIP_RECV",
                             metric = "aAME",
                             limitMargins = TRUE,
                             limitOutcomes = TRUE) %>%
  mutate(stratVar = "Unadjusted",
         # mutate(stratVar = "Full Sample",
         varCategory = "",
         weighting = "Person")
toc()

# prevalence ratios -------------------------------------------------------

# adjusted PR
tic()
df_mdl_adj_aPR <- run_all_mdls(svydsn_pweight, 
                               covars = covars, 
                               metric = "aPR",
                               limitMargins = TRUE,
                               limitOutcomes = TRUE) %>%
  mutate(stratVar = "Full Sample",
         varCategory = "")
toc()

# unadjusted PR
tic()
df_mdl_unadj_aPR <- run_all_mdls(svydsn_pweight, 
                                 covars = "EIP_RECV",
                                 metric = "aPR",
                                 limitMargins = TRUE,
                                 limitOutcomes = TRUE) %>%
  mutate(stratVar = "Unadjusted",
         # mutate(stratVar = "Full Sample",
         varCategory = "")
toc()











# sensitivity analysis ----------------------------------------------------
# -------------------------------------------------------------------------



# no weighting
tic()
df_mdl_adj_noweight <- run_all_mdls(svydsn_noweight, 
                                    covars,
                                    metric = "aAME",
                                    limitMargins = TRUE,
                                    limitOutcomes = TRUE) %>%
  mutate(stratVar = "Full Sample",
         varCategory = "",
         weighting = "None")
toc()

# household weighting
tic()
df_mdl_adj_hweight <- run_all_mdls(svydsn_hweight, 
                                   covars,
                                   metric = "aAME",
                                   limitMargins = TRUE,
                                   limitOutcomes = TRUE) %>%
  mutate(stratVar = "Full Sample",
         varCategory = "",
         weighting = "Household")
toc()


# vary by PHQ score threshold
tic()
df_mdl_adj_pweight <- run_all_mdls(svydsn_pweight, 
                                    covars,
                                    metric = "aAME",
                                    limitMargins = TRUE,
                                    limitOutcomes = FALSE) %>%
  mutate(stratVar = "Full Sample",
         varCategory = "",
         weighting = "Person")
toc()
# tic()
# df_mdl_adj2_AME <- run_all_mdls(svydsn, covars, metric = "aAME") %>%
#   mutate(stratVar = "Full Sample",
#          varCategory = "")
# toc()


# sensitivity
df_mdl_sensitivity_all <- df_mdl_adj_pweight  %>%
  bind_rows(df_mdl_unadj ) %>%
  bind_rows(df_mdl_adj_noweight ) %>%
  bind_rows(df_mdl_adj_hweight ) %>%
  arrange(Variable, Outcome)

write.csv(df_mdl_sensitivity_all, file.path(outputPath, "ModelOutputs_SensitivityAnalysis.csv"), row.names = FALSE )



# subgroup analysis -------------------------------------------------------
# -------------------------------------------------------------------------


modelType <- "adjusted"


# stratify by unemployment insurance
tic()
df_mdl_UI <- UILevels %>%
  map_df(~ gen_mdl_strat(df, 
                         covars = covars, 
                         strat_varname = "UI",
                         strat_levels = .x, 
                         modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, 
                           levels = UILevels))
toc()



# stratify by work status
tic()
df_mdl_nowork <- noworkLevels_re %>%
  map_df(~ gen_mdl_strat(df, 
                         covars = covars, 
                         strat_varname = "NOWORK_COVID",
                         strat_levels = .x, 
                         modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, 
                           levels = noworkLevels_re))
toc()



# stratify by gender
tic()
df_mdl_gender <- genderLevels %>%
  map_df(~ gen_mdl_strat(df, 
                         covars = covars, 
                         strat_varname = "GENDER",
                         strat_levels = .x, 
                         modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, 
                           levels = genderLevels))
toc()

# stratify by age
tic()
df_mdl_age <- age4Levels %>%
  map_df(~ gen_mdl_strat(df, 
                         covars = covars, 
                         strat_varname = "AGE_4",
                         strat_levels = .x, 
                         modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, 
                           levels = age4Levels))
toc()

# stratify by race
tic()
df_mdl_race <- race5Levels %>%
  map_df(~ gen_mdl_strat(df, 
                         covars = covars, 
                         strat_varname = "RACE_5",
                         strat_levels = .x, 
                         modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, 
                           levels = race5Levels))
toc()

# stratify by income
tic()
df_mdl_income <- incomeLevels %>%
  map_df(~ gen_mdl_strat(df, 
                         covars = covars, 
                         strat_varname = "INCOME_4",
                         strat_levels = .x, 
                         modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, 
                           levels = incomeLevels))
toc()

# stratify by educational attainment
tic()
df_mdl_edu <- edu2Levels_re %>%
  map_df(~ gen_mdl_strat(df, 
                         covars = covars, 
                         strat_varname = "EDU_2",
                         strat_levels = .x, 
                         modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, 
                           levels = edu2Levels_re))
toc()


# stratify by children in household
tic()
df_mdl_child <- childLevels %>%
  map_df(~ gen_mdl_strat(df, 
                         covars = covars, 
                         strat_varname = "withCHILDREN",
                         strat_levels = .x, 
                         modelType = modelType)) %>%
  mutate(stratVar = factor(stratVar, 
                           levels = childLevels))
toc()

# # stratify by SNAP receipt
# tic()
# df_mdl_SNAP <- SNAPLevels %>%
#   map_df(~ gen_mdl_strat(df, 
#                          covars = covars, 
#                          strat_varname = "SNAP",
#                          strat_levels = .x, 
#                          modelType = modelType)) %>%
#   mutate(stratVar = factor(stratVar, 
#                            levels = SNAPLevels))
# toc()



# bind results from all models
# -------------------------------------------------------------------------

# rename groups
incomeLevels_re <- rev(c("Income $0-$34,999", 
                         "Income $35,000-$74,999", 
                         "Income $75,000-$149,999"))
age4Levels_re <- rev(c("Age 18-24 years",
                       "Age 25-44 years",
                       "Age 45-64 years"))

df_mdl_subgroup_full <- df_mdl_adj %>% select(-weighting) %>%
  bind_rows(df_mdl_gender) %>%
  bind_rows(df_mdl_age) %>%
  bind_rows(df_mdl_race) %>%
  bind_rows(df_mdl_edu) %>%
  bind_rows(df_mdl_income) %>%
  bind_rows(df_mdl_child) %>%
  bind_rows(df_mdl_nowork) %>%
  bind_rows(df_mdl_UI) %>%
  # bind_rows(df_mdl_SNAP) %>%
  mutate(varCategory = factor(varCategory, 
                              levels = c("", "NOWORK_COVID", "UI", "GENDER", "AGE_4", "RACE_5", "INCOME_4", "EDU_2", "withCHILDREN", "SNAP"),
                              labels = c("","Work\nStatus", "Unemp.\nInsurance\n(UI)", "Gender", "Age", "Race/Ethnicity", "Household\nIncome", "Education","Children",  "SNAP")  ) ) %>%
  mutate(stratVar = factor(stratVar, 
                           levels = c("Full Sample", noworkLevels_re, UILevels, genderLevels, age4Levels, race5Levels, incomeLevels, edu2Levels_re,  childLevels,  SNAPLevels),
                           labels = c("Full Sample", noworkLevels_re, UILevels, genderLevels, age4Levels_re, race5Levels, incomeLevels_re, edu2Levels_re,  childLevels,  SNAPLevels)   ) ) %>%
  # filter(Outcome %in% c("ANXIETY_3", "DEPRESSION_3") ) %>%
  
  arrange(Outcome)

write.csv(df_mdl_subgroup_full, 
          file.path(outputPath, "ModelOutputs_SubgroupAnalysis.csv"), 
          row.names = FALSE )


# plot subgroup analysis --------------------------------------------------
# -------------------------------------------------------------------------

# function for displaying results
displayModelResults <- function(mdl)
{
  
  if(is.data.frame(mdl) ){
    df <- mdl
  }
  else{
    df <- gen_mdl_df(mdl)
  }
  
  pal_custom <- brewer.pal(9, "Set1")
  pal_custom[6] <- "#000000"
  
  p1 <- df %>%
    ggplot(aes(x = stratVar, y = AME, ymin = CI_l, ymax = CI_u)) +
    geom_hline(aes(yintercept = 0), linetype = 'dashed') +
    
    geom_pointrange( aes(col = varCategory), size = 1) +
    theme_minimal() +
    coord_flip() +
    facet_grid(row = vars(varCategory), scales="free_y", space = "free_y") +
    theme(
      legend.position = "none",
      text = element_text(size = 16)
      # axis.text.x = element_text(angle = 45, hjust=1)
      # axis.text.x = element_text(angle = 45, hjust=1)
      # panel.spacing = unit(2, "lines")
    ) +
    # scale_color_brewer(palette = "Set1") +
    scale_color_manual(values = pal_custom)+
    xlab("")
  # scale_y_continuous(trans='log2') +
  # coord_flip()
  
  
  return(p1)
}



# both anx and depression
p_both3 <- df_mdl_subgroup_full %>%
  mutate(Outcome = factor(Outcome,
                          levels = c("ANXIETY_3", "DEPRESSION_3"),
                          labels = c("Association Between\nStimulus Payment and\nAnxiety Symptoms",
                                     "Association Between\nStimulus Payment and\nDepression Symptoms") ) ) %>%
  displayModelResults() +
  facet_grid(row = vars(varCategory), col = vars(Outcome), scales="free_y", space = "free_y") +
  theme(panel.spacing.x = unit(2, "lines"),
        axis.text.x = element_text(size = 16) ) +
  # scale_y_continuous(trans='log2', limits = c(.30, 1.6)) +
  ylab("Average marginal effect (95% CI), percentage points")
p_both3

ggsave(file.path(outputPath, "FIGURE2_ModelOutput_SubgroupAnalysis.jpg") ,
       plot = p_both3,
       width = 12,
       height = 12,
       device = "jpg")

