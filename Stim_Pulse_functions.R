
# -------------------------------------------------------------------------



getFilePaths <- function(dataPath){
# getFilePaths
#   Gets vector of full file paths to PUF .csv files
#
# input: 
#   - dataPath: file path to folder that contains all PUF Pulse data  
#               subfolders (extracted directly from census.gov)
#
# output:
#   - fullFilePaths: vector with full paths to each PUF .csv file
  
  
  # get subfolders in dataPath folder
  dataFolderPaths <- list.dirs(dataPath, recursive = FALSE)
  dataFolderNames = basename(dataFolderPaths)
  
  # extract week number from folder name
  weekNumStr <- sub("_PUF_CSV.*", "", dataFolderNames)  
  weekNumStr <- sub(".*HPS_Week", "", weekNumStr) 
  
  weekNum <- as.numeric(weekNumStr)
  
  
  # get PUF file names using name template 
  df_filePaths <- data.frame(weekNumStr, weekNum, dataFolderPaths) %>%
    rowwise() %>%
    mutate(prefixStr = ifelse(weekNum > 21, "pulse2021_puf_", "pulse2020_puf_") ,
           fileNames = paste0(prefixStr, weekNumStr, ".csv"),
           fullFilePaths = file.path(dataFolderPaths, fileNames) ) %>%
    ungroup()
  
  fullFilePaths <- df_filePaths$fullFilePaths
  
  return(fullFilePaths)
}


# -------------------------------------------------------------------------


importPulseData <- function(filePath, var_OI){
# function importPulseData: imports single Pulse data file
# To merge using filePaths %>% map_df(~ importPulseData(.x, var_OI ))
# df <- importPulseData(filePath, var_OI)
  print(filePath)
  
  # read csv file
  df <- read.csv(filePath) 
  
  # Create empty columns in data frame if variable not found
  cols_add<-setdiff(var_OI,names(df))
  for(i in cols_add){df[[i]]<-NA}
  
  
  df <- df %>%
    select(var_OI)
  
  
  return(df)  
}


# -------------------------------------------------------------------------



import_clean_weektime <- function(filePath){
# import_clean_weektime
#   import Week datetime .csv file and convert to appropriate date format
#   table enables translation of each Pulse Suvey "Week" or data collection 
#   period to dates
#
# Input:
#   - filePath: path to .csv table with week number-to-date translations
#
# Output: 
#   - df_weektime: data frame with converted date variables
  
  df_weektime <- read.csv(filePath) %>%
    mutate(startDate = as.Date(startDate, "%m/%d/%Y"),
           endDate = as.Date(endDate, "%m/%d/%Y"))%>%
    rowwise()%>%
    mutate(midDate = mean.Date(c(endDate, startDate), format = "%Y-%m-%d"),
           numDays = endDate - startDate,
           Phase = factor(Phase,
                          levels = c(1,2,3) )) 
  return(df_weektime)
}




import_clean_var_OI <- function(filePath){
# import_clean_var_OI
#   import variables of interest .csv file
#
# Input:
#   - filePath: path to .csv table with variables of interest
#
# Output: 
#   - var_OI: string vector with variables of interest
  
  df_var <- read.csv( filePath )
  
  df_var <- df_var %>% 
    filter(Include == 1) %>%
    mutate(Variable = as.character(Variable))
  
  var_OI <- df_var$Variable
  
  return(var_OI)
}

# -------------------------------------------------------------------------


# constructs variables in imported Pulse data for analysis
construct_new_pulse_var <- function(df_import){
  
  if(is.factor(df_import$ANXIOUS)){
    df_import <- df_import %>%
      mutate( ANXIOUS = as.numeric(as.character(ANXIOUS)) ,
              WORRY = as.numeric(as.character(WORRY)) ,
              INTEREST = as.numeric(as.character(INTEREST)) ,
              DOWN = as.numeric(as.character(DOWN)) )
  }
  
  # construct outcome variables
  df <- df_import %>%
    mutate(PHQ4_ANXIETY = (ANXIOUS-1) + (WORRY-1),
           PHQ4_DEPRESSION = (INTEREST-1) + (DOWN-1),
           PHQ4 = PHQ4_ANXIETY + PHQ4_DEPRESSION,
           PHQ4_missing = PHQ4 < 0,
           
           PHQ4_SEVERE = PHQ4 >= 9,
           ANXIETY = PHQ4_ANXIETY >= 5,
           ANXIETY_6 = PHQ4_ANXIETY >= 6,
           ANXIETY_5 = PHQ4_ANXIETY >= 5,
           ANXIETY_4 = PHQ4_ANXIETY >= 4,
           ANXIETY_3 = PHQ4_ANXIETY >= 3,
           DEPRESSION = PHQ4_DEPRESSION >= 5,
           DEPRESSION_6 = PHQ4_DEPRESSION >= 6,
           DEPRESSION_5 = PHQ4_DEPRESSION >= 5,
           DEPRESSION_4 = PHQ4_DEPRESSION >= 4,
           DEPRESSION_3 = PHQ4_DEPRESSION >= 3,
           
           ANXIETY_2 = (ANXIOUS-1) >= 2 & (WORRY-1) >= 2,
           DEPRESSION_2 = (INTEREST-1) >= 2 & (DOWN-1) >= 2 )
  
  
  # Add factor variable for recoded race/ethnicity data (5 levels)
  df <- df %>% mutate(RACE_5 = ifelse(RRACE == 1 & RHISPANIC == 1, 1,
                                        ifelse(RRACE == 2 & RHISPANIC == 1, 2,
                                               ifelse(RHISPANIC == 2, 3,
                                                      ifelse(RRACE == 3 & RHISPANIC == 1, 4, 5) ) ) ),
                      RACE_5 = factor(RACE_5,
                                        levels = c(1,2,3,4,5),
                                        labels = c("Non-hispanic White",
                                                   "Non-hispanic Black",
                                                   "Hispanic",
                                                   "Non-hispanic Asian",
                                                   "Other or multiple" ),
                                      ordered = FALSE) )
  
  # Add factor variable for recoded race/ethnicity data (white vs non-white)
  df <- df %>% mutate(RACE_2 = ifelse(RRACE == 1 & RHISPANIC == 1, 1, 2),
                      RACE_2 = factor(RACE_2,
                                      levels = c(1,2),
                                      labels = c("White",
                                                 "Non-white" ),
                                      ordered = FALSE) )
  
  
  # Add factor variable for recoded income (4 levels)
  df <- df %>% mutate(INCOME_4 = ifelse(INCOME %in% c(1,2), 1,
                                        ifelse(INCOME %in% c(3,4), 2,
                                               ifelse(INCOME %in% c(5,6), 3,
                                                      ifelse(INCOME %in% c(7,8), 4, -99) ) ) ),
                      INCOME_4 = factor(INCOME_4,
                                        levels = c(1,2,3,4, -99),
                                        labels = c("0 - 34,999",
                                                   "35,000 - 74,999",
                                                   "75,000 - 149,999",
                                                   "> 150,000",
                                                   "Missing" ),
                                        ordered = FALSE),
                      INCOME_missing = is.na( INCOME_4 ) )
  
  
  
  # Add factor variable for recoded education (4 levels)
  df <- df %>% mutate(EDU_4 = ifelse(EEDUC %in% c(1,2), 1,
                                        ifelse(EEDUC %in% c(3), 2,
                                               ifelse(EEDUC %in% c(4), 3,
                                                      ifelse(EEDUC %in% c(5, 6, 7), 4, NA) ) ) ),
                      EDU_4 = factor(EDU_4,
                                        levels = c(1,2,3,4),
                                        labels = c("< High school",
                                                   "High school graduate or GED",
                                                   "Some college",
                                                   ">= College"),
                                     ordered = FALSE) )

  
  
  # Add factor variable for recoded education (2 levels)
  df <- df %>% mutate(EDU_2 = ifelse(EEDUC %in% c(1,2,3,4), 1,
                                     ifelse(EEDUC %in% c(5,6,7), 2, NA) )  ,
                      EDU_2 = factor(EDU_2,
                                     levels = c(1,2),
                                     labels = c("< College",
                                                ">= College" ),
                                     ordered = FALSE) )
  
  
  # # Add factor variable for age (4 levels)
  # df <- df %>% mutate(AGE = ifelse(WEEK > 21, 2021 - TBIRTH_YEAR, 2020 - TBIRTH_YEAR) ,
  #                     AGE_4 = ifelse(between(AGE, 18, 39), 1,
  #                                    ifelse(between(AGE, 40, 49), 2,
  #                                           ifelse(between(AGE, 50, 64), 3,
  #                                                  ifelse(AGE >= 65, 4, NA) ) ) ),
  #                     AGE_4 = factor(AGE_4,
  #                                    levels = c(1,2,3,4),
  #                                    labels = c("18 - 39",
  #                                               "40 - 49",
  #                                               "50 - 64",
  #                                               ">= 65"),
  #                                    ordered = FALSE)  )
  # Add factor variable for age (4 levels)
  df <- df %>% mutate(AGE = ifelse(WEEK > 21, 2021 - TBIRTH_YEAR, 2020 - TBIRTH_YEAR) ,
                      AGE_4 = ifelse(between(AGE, 18, 24), 1,
                                     ifelse(between(AGE, 25, 44), 2,
                                            ifelse(between(AGE, 45, 64), 3,
                                                   ifelse(AGE >= 65, 4, NA) ) ) ),
                      AGE_4 = factor(AGE_4,
                                     levels = c(1,2,3,4),
                                     labels = c("18 - 24",
                                                "25 - 44",
                                                "45 - 64",
                                                ">= 65"),
                                     ordered = FALSE)  )
  
  # Add factor variable for age (3 levels)
  df <- df %>% mutate(AGE_3 = ifelse(between(AGE, 18, 39), 1,
                                     ifelse(between(AGE, 40, 64), 2,
                                            ifelse(AGE >= 65, 3, NA) ) ),
                      AGE_3 = factor(AGE_3,
                                     levels = c(1,2,3),
                                     labels = c("18 - 39",
                                                "40 - 64",
                                                ">= 65" ),
                                     ordered = FALSE) )
  
  
  
  
  # Add factor variable for lost employment income
  df <- df %>% mutate(LOST_WORK = ifelse(WRKLOSS %in% c(1), 1,
                                     ifelse(WRKLOSS %in% c(2), 2, -99) )  ,
                      LOST_WORK = factor(LOST_WORK,
                                     levels = c(1,2,-99),
                                     labels = c("Household lost employment income",
                                                "Household did not lose employment income",
                                                "Missing" ),
                                     ordered = FALSE),
                      LOST_WORK_missing = LOST_WORK == "Missing" )
  
  
  # Add factor variable for unemployment
  df <- df %>% mutate(NOWORK_COVID = ifelse(ANYWORK == 2 & RSNNOWRK %in% c(8,9,10,11), 1,
                                         ifelse(ANYWORK == 2 & RSNNOWRK %in% c(1,2,3,4,5,6,7,12), 2,
                                                ifelse(ANYWORK == 1 , 3, -99)) )  ,
                      NOWORK_COVID = factor(NOWORK_COVID,
                                         levels = c(1,2,3,-99),
                                         labels = c("No work in past 7 days due to pandemic",
                                                    "No work in past 7 days unrelated to pandemic",
                                                    "Had work in past 7 days",
                                                    "Missing" ),
                                         ordered = FALSE),
                      NOWORK_COVID_missing = NOWORK_COVID == "Missing" )
  
  # Add factor variable for gender
  df <- df %>% mutate(GENDER = factor(EGENDER,
                                     levels = c(1,2),
                                     labels = c("Male", "Female")  ) )
  
  # Add factor variable for UI
  df <- df %>% mutate(UI = ifelse(UI_APPLY %in% c(1), 1,
                                  ifelse(UI_APPLY %in% c(2), 2, -99) )  ,
                      UI = factor(UI,
                                  levels = c(1,2,-99),
                                  labels = c("Household previously applied for UI",
                                             "Household has not applied for UI",
                                             "Missing" ),
                                  ordered = FALSE),
                      UI_missing = UI == "Missing" )
  
  # Add factor variable for SNAP
  df <- df %>% mutate(SNAP = ifelse(SNAP_YN %in% c(1), 1,
                                    ifelse(SNAP_YN %in% c(2), 2, -99) )  ,
                      SNAP = factor(SNAP,
                                    levels = c(1,2,-99),
                                    labels = c("Household receives SNAP",
                                               "Household does not receive SNAP",
                                               "Missing" ),
                                    ordered = FALSE),
                      SNAP_missing = SNAP == "Missing" )
  
  
  return(df)
}

# -------------------------------------------------------------------------




get_counts_strat <- function(df, outcomeVar, stratVar, INCLUDE_FULL_SAMPLE = TRUE){
# get_counts_strat 

  stratSym <- as.symbol(stratVar)
  outcomeSym <- as.symbol(outcomeVar)
  
  # denominator for stratified sample
  count_week_groupvar <- df %>%
    group_by(WEEK, !!stratSym) %>%
    summarize(N = n(), .groups = 'drop') 
  
  # numerator for stratified sample
  count_week_groupvar_outcome <- df %>%
    group_by(WEEK, !!stratSym, !!outcomeSym) %>%
    summarize(n = n(), .groups = 'drop') %>%
    filter(!!outcomeSym == TRUE) %>%
    mutate(Outcome = outcomeVar) %>%
    left_join(count_week_groupvar, by = c("WEEK", stratVar)  )
  
  if(INCLUDE_FULL_SAMPLE){
    # denominator for non-stratified sample
    county_week <- df %>%
      group_by(WEEK) %>%
      summarize(N = n(), .groups = 'drop')
    
    # numerator for non-stratified sample
    count_week_outcome <- df %>%
      group_by(WEEK, !!outcomeSym) %>%
      summarize(n = n(), .groups = 'drop')   %>%
      filter(!!outcomeSym == TRUE) %>%
      mutate(Outcome = outcomeVar)
    count_week_outcome[[stratSym]] = "Full Sample"
    count_week_outcome <- count_week_outcome %>%
      left_join(county_week, by = "WEEK" )
    
    
    # bind to stratified table
    count_week_groupvar_outcome <- count_week_groupvar_outcome %>%
      bind_rows( select(count_week_outcome, names(count_week_groupvar_outcome) ) ) 
  }
  
  count_all <- count_week_groupvar_outcome %>%
    select( c("WEEK", stratVar, "Outcome", "N", "n") ) 
  return(count_all)
}



# -------------------------------------------------------------------------

map_svywt_prop_to_week <- function(df, WEEK_var, by = stratVar, outcomeVar = outcomeVar, BY_WEEK = TRUE, INCLUDE_FULL_SAMPLE = TRUE) {
# map_svywt_prop_to_week - 
#   calculates survey-weighted prevalence of several outcome variables
#   stratified by a user input variable(s) - calculated for individual data
#   collection periods
  
  print(paste("WEEK", WEEK_var))
  
  # filter data for specified data collection period
  df <- df %>%
    filter(WEEK == WEEK_var)
  
  # filter missing data (outcomes or variable for stratification)
  # df <- df %>%
  #   # filter(!LOST_WORK_missing) %>%
  #   filter(!PHQ4_missing  ) 

  
  # specify survey design (person weights) for collection period
  svydsn <- svydesign(ids = ~SCRAM,
                      weights = ~df$PWEIGHT,
                      nest = T,
                      data = df)
  

  
  
  # get numerators and denominators
  df_counts <- outcomeVar %>%
    map_df(~ get_counts_strat(df, .x, stratVar = stratVar, INCLUDE_FULL_SAMPLE = INCLUDE_FULL_SAMPLE) )
  
  
  # calculate survey-weighted prevalence by stratification variable for all outcomes
  # bind rows into one table with all outcomes
  df_prev_svtwt <- outcomeVar %>%
    map_df(~ get_svywt_prop_strat(svydsn, .x, by = by, BY_WEEK = TRUE, INCLUDE_FULL_SAMPLE = INCLUDE_FULL_SAMPLE) )
  
  
  # join numerators, denominators, prevalence, and time variables
  df_prev_svtwt <- df_counts %>%
    left_join(df_prev_svtwt, by = c("WEEK", stratVar, "Outcome") )
  
  return(df_prev_svtwt)
}



# -------------------------------------------------------------------------



get_svywt_prop_strat <- function(svydsn, outcomeStr, by, BY_WEEK = TRUE, INCLUDE_FULL_SAMPLE = TRUE ){
# get_svywt_prop_strat - 
#   calculates survey-weighted prevalence of specified outcome stratified
#   by a user input variable(s) 
#
# svydsn - survey design
# outcomeStr - character string with name of outcome variable
# by - character string with name of stratification variable(s)
# BY_WEEK - include collection period in stratification
# INCLUDE_FULL_SAMPLE - calculate survey weighted prevalence across all strata 
#
# Output: data frame with prevalence

  
  if(BY_WEEK) {
    # calculate survey-weighted prevalence by week (stratified samples)
    df_prev <- get_svywt_prop(svydsn, outcomeStr, c("WEEK", by) )
    
    
    if(INCLUDE_FULL_SAMPLE){
      # calculate survey-weighted prevalence by week (full sample)
      df_prev_full <- get_svywt_prop(svydsn, outcomeStr, c("WEEK") )
    }

    
    
  } else{
    # calculate survey-weighted prevalence all weeks (stratified samples)
    df_prev <- get_svywt_prop(svydsn, outcomeStr, c(by) )
    
    if(INCLUDE_FULL_SAMPLE){
      # calculate survey-weighted prevalence all weeks (full sample)
      df_prev_full <- get_svywt_prop(svydsn, outcomeStr, c("PLACEHOLDER") )
    }
    
  }
  
  if(INCLUDE_FULL_SAMPLE){
    df_prev_full[[by]] <- "Full Sample"
    
    # merge data frames for stratified samples and full sample into one data frame
    df_prev <- df_prev %>%
      bind_rows(select(df_prev_full, names(df_prev) ))
  }
  
  
  

  return(df_prev)
  
  
}


# -------------------------------------------------------------------------



# get survey-weighted proportion
get_svywt_prop <- function(svydsn, depVarStr, indVarStr){
# get_svywt_prop - 
#   calculates survey-weighted prevalence of specified dependent variable
#   (outcome) stratified by a user input variable  
#
# svydsn - survey design
# depVarStr - character string with name of dependent variable
# indVarStr - character string with name of stratification variable(s)
#
# Output: data frame with prevalence

  indVarStr_form <- paste(indVarStr,collapse="+")
  
  eval( parse( text = paste0("df_prop <- svyby(~", depVarStr,
                             ", ~", indVarStr_form, ", svydsn,",
                             "FUN = svymean, na.rm=T, vartype=\"ci\")" )  )  )  
  
  df_prop <- df_prop %>%
    clean_svy_output(depVarStr, indVarStr) %>%
    mutate(Outcome = depVarStr)
  
  return(df_prop)
}


# -------------------------------------------------------------------------




clean_svy_output<- function(df_svy, depVarStr, indVarStr){
# clean_svy_output
#   convert table output by svy packages to standardized data frame with 
#   prevalence, 95% CIs, and output sting for prevalence (95% CI)
#
# Input:
#   - df_svy: data frame with svymean/svyby output
#   - depVarStr: character string with name of dependent variable (outcome)
#   - indVarStr: character string with name of stratification variable(s)
#
# Output:
#   - df_out: data frame with prevalence, 95% CI, and output string
#       [indVarStr]: all existing columns in input data frame
#       prop: prevalence
#       ci_l: lower 95% CI value
#       ci_u: upper 95% CI value
#       propStr: cleaned string of prevalence (95% CI) for published table
    

  roundDigit <- 1
  
  varOut <- c(indVarStr, "prop", "ci_l", "ci_u", "propStr")
  
  df_out <- df_svy %>%
    mutate(Outcome = depVarStr) %>%
    rename( "prop" = paste0(depVarStr, "TRUE"),
            "ci_l" = paste0("ci_l.", depVarStr, "TRUE"),
            "ci_u" = paste0("ci_u.", depVarStr, "TRUE")   ) %>%
    mutate(prop = prop * 100 ,
           ci_l = ci_l * 100 ,
           ci_u = ci_u * 100 ,
           propStr = paste0(format(round(prop, digits = roundDigit), nsmall = roundDigit), " (",
                            format(round(ci_l, digits = roundDigit), nsmall = roundDigit), ", ",
                            format(round(ci_u, digits = roundDigit), nsmall = roundDigit), ")") ) %>%
    select(varOut)
  
  
  return(df_out)
  
}



# -------------------------------------------------------------------------



clean_prev_df_import <- function(df_prev, stratVar, outcomeVarLevels, stratVarLevels, IS_TIMESERIES = TRUE){
# clean_prev_df_import
#   clean prevalence table import / refactor variables
  
  df_prev_out <- df_prev %>%
    filter(Outcome %in% outcomeVarLevels) %>%
    
    mutate(Outcome = factor(Outcome, levels = outcomeVarLevels),
           PHASE2 = factor(Phase, levels = c(1,2,3), labels = c("Phase 1", "Phase 2 & 3", "Phase 2 & 3")   )) 
  
  if(IS_TIMESERIES){
    df_prev_out <- df_prev_out %>%
      clean_import_dates() 
  }
    
  df_prev_out[[stratVar]] <- factor(df_prev_out[[stratVar]], levels = stratVarLevels)
  
  return(df_prev_out)
}



import_clean_df_prev <- function(prevFilePath, stratVar, outcomeLevels){
  
  
  regionLevels <- c("Full Sample", 
                    "Northeast",
                    "South",
                    "Midwest",
                    "West")
  regionLevels_rename <- c("United States", 
                           "Northeast",
                           "South",
                           "Midwest",
                           "West")
  
  genderLevels <- c("Female",
                    "Male")
  
  race2Levels <- c("Non-white",
                   "White")
  
  race5Levels <- c("Non-hispanic White",
                   "Non-hispanic Black",
                   "Hispanic",
                   "Non-hispanic Asian",
                   "Other or multiple")
  
  edu2Levels <- c("< College",
                  ">= College")
  
  
  age3Levels <- c("18 - 39",
                  "40 - 64",
                  ">= 65")
  
  age4Levels <- c("18 - 24",
                  "25 - 44",
                  "45 - 64",
                  ">= 65")
  
  lostworkLevels <- c("Household lost employment income", 
                      "Household did not lose employment income")
  noworkLevels <- c("No work in past 7 days due to pandemic",
                    "No work in past 7 days unrelated to pandemic",
                    "Had work in past 7 days")
# -------------------------------------------------------------------------

  
  
  if(stratVar == "REGION"){
    
    # load region stratified prevalence table (pulse)
    df_prev <- read.csv(prevFilePath) %>%
      clean_prev_df_import("REGION", 
                           outcomeVarLevels = outcomeLevels, 
                           stratVarLevels = regionLevels) %>%
      mutate(REGION = factor(REGION, levels = regionLevels, labels = regionLevels_rename))
    
    
  } else if (stratVar == "GENDER"){
    # load gender stratified prevalence table (pulse)
    df_prev <- read.csv(prevFilePath) %>%
      clean_prev_df_import("GENDER", 
                           outcomeVarLevels = outcomeLevels, 
                           stratVarLevels = genderLevels) 
    
  } else if (stratVar == "RACE_2"){
    
    # load race/ethnicity (5 level) stratified prevalence table (pulse)
    df_prev <- read.csv(prevFilePath) %>%
      clean_prev_df_import("RACE_2", 
                           outcomeVarLevels = outcomeLevels, 
                           stratVarLevels = race2Levels) 
    
  } else if (stratVar == "RACE_5"){
    
    # load race/ethnicity (5 level) stratified prevalence table (pulse)
    df_prev <- read.csv(prevFilePath) %>%
      clean_prev_df_import("RACE_5",
                           outcomeVarLevels = outcomeLevels,
                           stratVarLevels = race5Levels) 
  } else if (stratVar == "EDU_2") {
    
    # load education (2 level) stratified prevalence table (pulse)
    df_prev <- read.csv(prevFilePath) %>%
      clean_prev_df_import("EDU_2", 
                           outcomeVarLevels = outcomeLevels, 
                           stratVarLevels = edu2Levels) %>% 
      mutate( EDU_2 = recode(EDU_2, ">= College" = paste("Completed college degree or higher")),
              EDU_2 = recode(EDU_2, "< College" = "Completed less than college degree") )
    
  } else if (stratVar == "AGE_3") {
    
    
    # load age (3 level) stratified prevalence table (pulse)
    df_prev <- read.csv(prevFilePath) %>%
      clean_prev_df_import("AGE_3", 
                           outcomeVarLevels = outcomeLevels, 
                           stratVarLevels = age3Levels) %>%
      mutate( AGE_3 = recode(AGE_3, ">= 65" = paste("\u2265", "65 years old")),
              AGE_3 = recode(AGE_3, "18 - 39" = "18 - 39 years old"),
              AGE_3 = recode(AGE_3, "40 - 64" = "40 - 64 years old"))
    
  } else if (stratVar == "AGE_4") {
    
    
    # load age (4 level) stratified prevalence table (pulse)
    df_prev <- read.csv(prevFilePath) %>%
      clean_prev_df_import("AGE_4", 
                           outcomeVarLevels = outcomeLevels, 
                           stratVarLevels = age4Levels) %>%
      mutate( AGE_4 = recode(AGE_4, ">= 65" = paste("\u2265", "65 years old")),
              AGE_4 = recode(AGE_4, "18 - 24" = "18 - 24 years old"),
              AGE_4 = recode(AGE_4, "25 - 44" = "25 - 44 years old"),
              AGE_4 = recode(AGE_4, "45 - 64" = "45 - 64 years old") )
    
  } else if (stratVar == "LOST_WORK"){
    
    # income4Levels <- c("0 - 34,999",
    #                    "35,000 - 74,999",
    #                    "75,000 - 149,999",
    #                    "> 150,000")
    # # load income stratified prevalence table (pulse)
    # df_prev_income4 <- read.csv(prevFilePath_income4) %>%
    #   clean_prev_df_import("INCOME_4", 
    #                        outcomeVarLevels = outcomeLevels, 
    #                        stratVarLevels = income4Levels) %>%
    #   left_join(df_covid_summ_us, by = c("startDate" = "date")) %>%
    #   # mutate( INCOME_4 = paste("Income: $", INCOME_4) ) %>%
    #   mutate(Outcome = factor(Outcome, levels = outcomeLevels, labels = outcomeLevels_rename) )
    

    
    # load work lost stratified prevalence table (pulse)
    df_prev <- read.csv(prevFilePath) %>%
      clean_prev_df_import("LOST_WORK", 
                           outcomeVarLevels = outcomeLevels, 
                           stratVarLevels = lostworkLevels) 
  } else if (stratVar == "NOWORK_COVID"){
    
    # load work lost stratified prevalence table (pulse)
    df_prev <- read.csv(prevFilePath) %>%
      clean_prev_df_import("NOWORK_COVID", 
                           outcomeVarLevels = outcomeLevels, 
                           stratVarLevels = noworkLevels) 
  } else if (stratVar == "STATE"){
    
    # 
    df_prev <- read.csv(prevFilePath) %>%
      clean_prev_df_import("STATE", 
                           outcomeVarLevels = outcomeLevels, 
                           stratVarLevels =  c("Connecticut", "Maine", "Massachusetts", 
                                               "New Hampshire", "Rhode Island", "Vermont")) 
  } else {
    
    df_prev <- NULL
  }

  
  # add percent change var
  df_prev <- df_prev %>%
    # arrange(WEEK) %>%
    group_by(!!sym(stratVar), Outcome) %>%
    mutate(pctchg_prop = (prop / prop[WEEK == 1] - 1) * 100) %>%
    ungroup()
  
  return(df_prev)
}
  


# -------------------------------------------------------------------------
  

clean_import_dates<- function(df){
  df_out <- df %>%
    mutate(startDate = as.Date(startDate),
           endDate = as.Date(endDate),
           midDate = as.Date(midDate),
           Phase = as.factor(Phase) )
  
  return(df_out)
}



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# correlations



# function for calculating correlation
calcCovidCorr <- function(df, var_strat){
  var_sym <- as.symbol(var_strat)
  
  # calculate correlations phase 1 and phase 2&3
  df_corr  <- df %>%
    group_by(Outcome, PHASE2, !!var_sym) %>%
    # do(mod = lm(prop ~ cases_per_mean, data = .)) %>%
    summarize(corr_covid_prop= round( cor(prop, cases_per_mean, method = "pearson"), digits = 2),
              .groups = 'drop') %>%
    rename(stratVar = !!var_sym) %>%
    mutate(varCategory = var_strat)
  
  
  
  df_slope <- get_lm_metrics(df, var_strat)
  
  df_corr <- df_corr %>%
    left_join(df_slope, by = c("Outcome", "PHASE2", "stratVar") )
  
  # # calculate correlations full sample (both phases)
  # df_corr_full  <- df %>%
  #   group_by(Outcome, !!var_sym) %>%
  #   # do(mod = lm(prop ~ cases_per_mean, data = .)) %>%
  #   # mutate(Slope = summary(mod)$coeff[2]) %>%
  #   summarize(corr_covid= round( cor(prop, cases_per_mean, method = "pearson"), digits = 2),
  #             .groups = 'drop') %>%
  #   do(mod = lm(prop ~ cases_per_mean, data = .)) %>%
  #   mutate(Slope = summary(mod)$coeff[2]) %>%
  #   rename(startVar = !!var_sym) %>%
  #   mutate(varCategory = var_strat,
  #          PHASE2 = "Full Sample")
  #
  # # join both phases
  # df_corr <- df_corr %>%
  #   bind_rows( select(df_corr_full, names(df_corr))  ) %>%
  #   mutate(PHASE2 = factor(PHASE2, levels = c("Full Sample", "Phase 1", "Phase 2 & 3") ))
  
  return(df_corr)
}



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
 # regression models
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# convert model output to formatted data frame
gen_mdl_df <- function(mdl){
  
  # add p-value
  pvalue<- round(  summary(mdl)$coefficients[,4] , digits = 4)
  significant <- pvalue < 0.05
  
  # add confidence intervals and create data frame
  df<-cbind( round (  exp(cbind(OR = coef(mdl), confint(mdl) ) )  , digits = 3),
             data.frame(pvalue, significant )   )
  
  
  df[ "Variable" ] <- rownames(df)
  rownames(df)=NULL
  colnames(df) <- c("OR", "CI_l", "CI_u", "pvalue", "significant", "Variable")
  
  
  df <- df %>%
    # round numbers
    mutate(OR = round( OR, digits = 2),
           CI_l = round( CI_l, digits = 2),
           CI_u = round( CI_u, digits = 2)) %>%
    # string for p-value
    mutate(  pvalue = round(pvalue, digits = 3), 
             p_str = format(pvalue, nsmall = 3),
             p_str = ifelse(pvalue >= 0.05, paste0(p_str, " ") ,
                            ifelse(pvalue >= 0.01, paste0( p_str, " *"),
                                   ifelse(pvalue >= 0.001, paste0( p_str, " **"), "< 0.001 ***" ) ) ) )  %>%
    # string for OR and CI
    mutate(OR_CI_str = paste0(format(OR, nsmall = 2), " (", 
                              format(CI_l, nsmall = 2), ", ", 
                              format(CI_u, nsmall = 2), ")") ) %>%
    
    
    
    select( c("Variable", "OR_CI_str", "p_str", "OR", "CI_l", "CI_u", "pvalue", "significant") )
  
  
  return(df)
}



# -------------------------------------------------------------------------



# convert Average Marginal Effects model output to formatted data frame
gen_AME_df <- function(marg_mdl){
  
  df_AME_mdl <- summary(marg_mdl)
  
  df <- df_AME_mdl %>%
    mutate(significant = p <0.05) %>%
    rename(Variable = factor,
           CI_l = lower,
           CI_u = upper,
           pvalue = p,
           ) 
    
  
  df <- df %>%
    # round numbers
    mutate(AME = round( AME * 100, digits = 1),
           CI_l = round( CI_l * 100, digits = 1),
           CI_u = round( CI_u * 100, digits = 1)) %>%
    # string for p-value
    mutate(  pvalue = round(pvalue, digits = 3), 
             p_str = format(pvalue, nsmall = 3),
             p_str = ifelse(pvalue >= 0.05, paste0(p_str, " ") , 
                            ifelse(pvalue >= 0.01, paste0( p_str, " *"),
                                   ifelse(pvalue >= 0.001, paste0( p_str, " **"), "< 0.001 ***" ) ) ) )  %>%
    # string for OR and CI
    mutate(AME_CI_str = paste0(format(AME, nsmall = 1), "% (", 
                              format(CI_l, nsmall = 1), "%, ", 
                              format(CI_u, nsmall = 1), "%)") ) %>%
    
    
    
    select( c("Variable", "AME_CI_str", "p_str", "AME", "CI_l", "CI_u", "pvalue", "significant") )
  
  
  return(df)
}


