
rm(list=ls())

path_wd <- "C:/Users/markh/OneDrive/Documents/BUSPH_Research/Covid_MentalHealth/Scripts_ILE"
setwd(path_wd)

library(tidyverse)
library(purrr)
library(tictoc)
library(survey)
source("../Scripts/pulse_process_functions.R")

# -------------------------------------------------------------------------

# data path
dataPath <- "../Data/"

importDataPath <- file.path(dataPath, "National_PulseData_ILE")
filePath_weektime <- file.path(dataPath, "pulse_weeknum_to_date.csv")


# save data frame with variables of interest
# df_import <- read.csv(file.path(importDataPath, "National_Pulse_imported_imputed_ILE22.csv") )
df_import <- read.csv(file.path(importDataPath, "National_Pulse_imported.csv") )

df_cusp <- read.csv(file.path(importDataPath, "CUSP_data_20210310.csv")) %>%
  filter(!is.na(FIPS)) %>%
  select(c("FIPS","STATE", "POSTCODE", "EBSTART", "EBEND", "X20EBSTART", "X20EBEND")) %>%
  mutate(FIPS = as.character(FIPS),
         EBEND = as.Date(EBEND, format = "%m/%d/%Y"),
         ENDED_EB = !is.na(EBEND))

df_weektime <- import_clean_weektime( filePath_weektime )

# saveFilePrefix <- "National_Pulse_wtPrev_by"

# -------------------------------------------------------------------------

df_all <- construct_new_pulse_var(df_import) %>%
  mutate(EST_ST = as.character(EST_ST),
         withCHILDREN = factor(THHLD_NUMKID > 0 ,
                               levels = c(FALSE, TRUE),
                               labels = c("No children in household", "Children in household") ),
         ANYWORK = factor(ANYWORK,
                          levels = c(1,2,-88,-99),
                          labels = c("Work","No work",NA, NA) ),
         UI_RECV = factor(UI_RECV,
                          levels = c(1,2,-88,-99),
                          labels = c("UI_RECV","No UI_RECV",NA,NA) ),
         UI_APPLY = factor(UI_APPLY,
                          levels = c(1,2,-88,-99),
                          labels = c("UI_APPLY","No UI_APPLY",NA,NA) ),
         EIP = ifelse(is.na(EIP), -999, EIP ),
         EIP_RECV = factor(EIP,
                           levels = c(1,2,3,4,-999, -99,-88),
                           labels = c("EIP","EIP","EIP","NoEIP","Baseline","Missing","Missing") ),
         FOODINSUF = CURFOODSUF %in% c(3,4),
         FOODINSUF_missing = CURFOODSUF < 0,
         RENT_BHND = RENTCUR == 2, 
         RENT_BHND_missing = RENTCUR < 0,
         MONEYINSUF = EXPNS_DIF %in% c(3,4),
         MONEYINSUF_missing = EXPNS_DIF < 0 )  %>%
  left_join(df_cusp, by = c("EST_ST" = "FIPS")) %>%
  left_join(df_weektime, by = "WEEK") 



var_include <- c("SCRAM", "WEEK", "EST_ST", "PWEIGHT", "HWEIGHT", "Phase",
                 "midDate", "startDate", "endDate",
                 "ANXIETY_3","ANXIETY_4","ANXIETY_5","ANXIETY_6",
                 "DEPRESSION_3","DEPRESSION_4","DEPRESSION_5","DEPRESSION_6",
                 "MONEYINSUF", "FOODINSUF", "RENT_BHND", 
                 "PHQ4_missing", "MONEYINSUF_missing", "FOODINSUF_missing", "RENT_BHND_missing", 
                 "EIP_RECV", "UI", "SNAP",
                 "GENDER","AGE", "AGE_4", "RACE_5", "withCHILDREN", "MS",
                 "EDU_2", "INCOME_4", "LOST_WORK", "NOWORK_COVID")


df_stim_sub <- df_all %>%
  # filter(WEEK %in% c(13,14,15,16,17,18,19,20,21,22) )  %>%
  # mutate(EIP_RECV = replace_na(EIP_RECV, 0))
  
  # filter(WEEK == 22)  %>%
  select(var_include)

write.csv(df_stim_sub,
          file.path(importDataPath,
                    paste0("National_Pulse_Week13_22_StimulusPayment.csv") ),
          row.names = F)









# saveFilePrefix <- "National_Pulse_wtPrev_UIrecipients_by"
# saveFilePrefix <- "National_Pulse_FoodInsuf_EligibleIncomeWorkingAge_by"
# saveFilePrefix <- "National_Pulse_AnxDepFoodMoney_EligibleIncomeWorkingAge_by"
saveFilePrefix <- "National_Pulse_AnxDep3FoodMoney_EligibleIncomeWorkingAge_by"
# saveFilePrefix <- "National_Pulse_AnxDep3FoodMoney_IneligibleIncomeWorkingAge_18_22_by"
# saveFilePrefix <- "National_Pulse_BehindRentImputed_EligibleIncomeWorkingAge_by"
# saveFilePrefix <- "National_Pulse_Expenses_EligibleIncomeWorkingAge_by"
# saveFilePrefix <- "National_Pulse_FoodMoneyInsuf_EligibleIncomeWorkingAge_by"


# plotTitle <- "Working Age & Ineligible Income"
# plotTitle <- "Anxiety and Depression Symptoms\nUI Recipients"
plotTitle <- "Anxiety and Depression Symptoms\nWorking Age & Eligible Income"
# plotTitle <- "Food and Money Insufficiency\nWorking Age & Eligible Income"
# plotTitle <- "Behind on Rent\nWorking Age Renters & Eligible Income"
# plotTitle <- "Difficulty with Expenses\nWorking Age & Eligible Income"
# plotTitle <- "Anxiety and Depression Symptoms\nWorking Age & Not Eligible Income"

hetStratVar <- NULL
# hetStratVar <- "EDU_2"
# hetStratVar <- "INCOME_4"
# hetStratVar <- "GENDER"
# hetStratVar <- "RACE_5"
# hetStratVar <- "AGE_4"
# hetStratVar <- "LOST_WORK"


df <- df_all %>% 
  # filter(!FOODINSUF_missing) %>%
  # filter(!RENT_BHND_missing) %>%
  # filter(!MONEYINSUF_missing) %>%
  filter(!PHQ4_missing) %>%
  
  filter(LOST_WORK != "Missing") %>%
  filter(EIP_RECV != "Missing") %>%
  mutate(EIP_RECV_bin = EIP_RECV == "EIP") %>%
  # filter( (INCOME_4%in%c("> 150,000") |
  #           INCOME_4%in%c("75,000 - 149,999") & MS != 1 )  &
  #          AGE < 65) %>%
  # filter( (INCOME_4%in%c("> 150,000") & MS != 1 )  &
  #           AGE < 65) %>%
  filter( (INCOME_4%in%c("0 - 34,999", "35,000 - 74,999") |
            INCOME_4%in%c("0 - 34,999", "35,000 - 74,999", "75,000 - 149,999") & MS == 1 )  &
           AGE < 65) %>%
  
  # mutate(EIP_RECV = "Baseline") %>%
  
  # filter(WEEK %in% c(13,14,15,16,17,18,19,20,21,22))
  filter(WEEK %in% c(18, 19, 20, 21, 22))
  # filter(WEEK >= 22) 
  # filter(WEEK == 22)
  
  # filter(Phase %in% c("2","3")) 

# -------------------------------------------------------------------------

  
  # filter(INCOME_4%in%c("> 150,000")  &
  #          AGE < 65) %>%
  # filter((INCOME_4%in%c("0 - 34,999", "35,000 - 74,999") |  
  #           INCOME_4%in%c("0 - 34,999", "35,000 - 74,999", "75,000 - 149,999") & MS == 1 ) ) %>%
  
  # filter(UI_RECV=="UI_RECV") %>%
  # filter(INCOME_4%in%c("0 - 34,999", "35,000 - 74,999")) %>%
  # filter(LOST_WORK == "Household lost employment income") %>%
  # filter(LOST_WORK == "Household did not lose employment income") %>%
  # filter(INCOME_4%in%c("35,000 - 74,999")) %>%
  # filter(RACE_2%in%c("Non-white")) %>%
  # filter(RACE_2%in%c("White")) %>%
  # filter(RACE_5%in%c("Non-hispanic Black")) %>%
  # filter(EDU_2%in%c(">= College") & AGE < 65) %>%
# filter(INCOME_4%in%c("75,000 - 149,999") & AGE < 65) %>%
  # filter(INCOME_4%in%c("0 - 34,999", "35,000 - 74,999", "75,000 - 149,999") & AGE < 65 & GENDER == "Male") %>%
  # filter(INCOME_4%in%c("0 - 34,999", "35,000 - 74,999", "75,000 - 149,999") & AGE < 65 & THHLD_NUMKID > 0) %>%
  # filter(withCHILDREN) %>%
  # filter(EBSTART != 0) %>% # exclude states that never extended UI
  # mutate(UI_EB = endDate <= EBEND | is.na(EBEND),
  #        UI_justEnded = ( (startDate - EBEND) > 0 & (endDate - EBEND) < 30) & !is.na(EBEND)  )


svydsn <- svydesign(ids = ~SCRAM,
                    weights = ~PWEIGHT,
                    nest = T,
                    data = df)
df_prop<-get_svywt_prop_strat(svydsn, outcomeStr = "EIP_RECV_bin", by = "INCOME_4", BY_WEEK = TRUE, INCLUDE_FULL_SAMPLE = FALSE )
df_prop %>%
  ggplot(aes(x=WEEK, y = prop, col = INCOME_4) ) + geom_line() +geom_point()+
  ylab("Household Received Stimulus Payment\nSurvey-weighted Prevalence (%)") +
  xlab("Survey Wave")+
  theme_bw()+
  ylim(0, NA)
# prop.table( table(df$EIP_RECV) )
# prop.table( table(df$ANYWORK) )
# prop.table( table(df$UI_APPLY) )
# prop.table( table(df$UI_RECV[df$UI_APPLY=="UI_APPLY"] ) )
# # compare EIP and no EIP populations
# prop.table( table(df$AGE_4, df$EIP_RECV),2 )
# prop.table( table(df$RACE_5, df$EIP_RECV),2 )
# prop.table( table(df$GENDER, df$EIP_RECV),2 )
# prop.table( table(df$EDU_4, df$EIP_RECV),2 )
# prop.table( table(df$INCOME_4, df$EIP_RECV),2 )
# prop.table( table(df$REGION, df$EIP_RECV),2 )
# prop.table( table(df$LOST_WORK, df$EIP_RECV),2 )
# prop.table( table(df$StateAbbr, df$EIP_RECV),2 )

  # weeks since end of extended benefits
  # mutate(WEEK = round((midDate - EBEND)/ 7) ) %>%
  # filter(WEEK >= -8 & WEEK <= 8)

# mdl<-glm(ANXIETY ~ factor(WEEK) + UI_EB,
#     data = df,
#     family = "binomial")
# summary(mdl)

# -------------------------------------------------------------------------

# identify outcome variables
# outcomeVar <- c("FOODINSUF",
# "MONEYINSUF")
outcomeVar <- c("MONEYINSUF",
                "FOODINSUF",
                "ANXIETY_3",
                "DEPRESSION_3")

# outcomeVar <- c("RENT_BHND")

# rename outcome factors
outcomeRename <- c("Difficulty with Expenses",
                   "Food Insufficient",
                   "Anxiety Symptoms (GAD-2 >= 3)",
                   "Depression Symptoms (PHQ-2 >= 3)" )
# outcomeRename <- c("Food Insufficient",
#                    "Difficulty with Expenses")
# outcomeRename <- c("Behind on Rent")

# # by state
# strat_variables <- "EST_ST"
# levels_for_stratvar <- as.character(unique(sort(as.numeric(df$EST_ST))))
# 
# stratified by states that extended vs those that didn't
# strat_variables <- "ENDED_EB"
# levels_for_stratvar <- c(TRUE,FALSE)

#
# strat_variables <- "UI_APPLY"
# levels_for_stratvar <- c("UI_APPLY","No UI_APPLY")

strat_variables <- "EIP_RECV"
levels_for_stratvar <- c("EIP", "NoEIP","Baseline")
labels_for_stratvar <- c("Received stimulus payment",
                         "Did not receive stimulus payment",
                         "Baseline (pre-stimulus payment)")

for(i in 1:length(strat_variables) ){
  # for(i in 2) {
  
  tic()
  
  stratVar <- strat_variables[i]
  levels_stratVar <- levels_for_stratvar[[i]]
  
  print(stratVar)
  
  
  if(stratVar == "REGION") {
    include_fullsample = TRUE
  } else {
    include_fullsample = FALSE
  }
  
  # -------------------------------------------------------------------------
  # survey-weighted prevalence
  # -------------------------------------------------------------------------
  
  
  # get survey weighted prevalence -- map to each data collection period (WEEK)
  df_prev_svtwt <- unique(df$WEEK) %>%
    map_df(~ map_svywt_prop_to_week(df, 
                                    .x, 
                                    by = c(stratVar, hetStratVar), 
                                    outcomeVar = outcomeVar, 
                                    # BY_WEEK = TRUE, 
                                    BY_WEEK = FALSE, 
                                    INCLUDE_FULL_SAMPLE = include_fullsample) )
  
  df_prev_svtwt <- df_prev_svtwt %>%
    left_join(df_weektime, by = "WEEK")
  
  
  # rename outcome variable levels  
  df_prev_svtwt <- df_prev_svtwt %>%
    mutate(Outcome = factor(Outcome,
                            levels = outcomeVar,
                            labels = outcomeRename ) ) %>%
    arrange(Outcome, !!sym(stratVar), WEEK) 

  
  # save prevalence table
  write.csv(df_prev_svtwt,
            file.path(importDataPath,
                      paste0(saveFilePrefix, stratVar, ".csv") ),
            row.names = F)
  
  
  toc()
  
}





# -------------------------------------------------------------------------

# # EIP plots
# # pos_d <- position_dodge( width=3 )
# pos_d <- position_dodge( width=0 )
# df_prev <- df_prev_svtwt%>%
#   mutate(d_prop = c(NA, diff(prop)))
# 
# p1 <- df_prev %>%
#   mutate(EIP_RECV = factor(EIP_RECV, 
#                            levels = levels_for_stratvar,
#                            labels = labels_for_stratvar) ) %>%
#   # filter(LOST_WORK != "Missing") %>%
#   ggplot(aes(x =midDate, y = prop, col = EIP_RECV, group = EIP_RECV) ) +
#   geom_line(position = pos_d) +
#   geom_point(position = pos_d) +
#   geom_ribbon(aes(ymin = ci_l, ymax = ci_u, fill = EIP_RECV), 
#               col = NA, 
#               alpha = 0.2,
#               position = pos_d) +
#   # geom_errorbar(aes(ymin = ci_l, ymax = ci_u, width = 0),position = pos_d) +
#   facet_wrap(vars(Outcome), ncol= 1) +
#   # facet_grid(cols = vars(!!sym(hetStratVar)), rows = vars(Outcome)) +
#   theme_bw() +
#   theme(legend.title = element_blank()
#     # legend.position = "bottom",
#         )+
#   # guides(col=guide_legend(ncol=1, order = 2), 
#          # fill=guide_legend(ncol=1, order = 2) ) +
#   scale_color_brewer(palette = "Dark2")+
#   scale_fill_brewer(palette = "Dark2")+
#   xlab("Date") +
#   ylab("Prevalence, % (95% CI)") +
#   ggtitle(plotTitle) +
#   ylim(0, NA)
# p1
# 
# ggsave(file.path(importDataPath,
#                  paste0(saveFilePrefix, stratVar, ".png")),
#        plot = p1,
#        device = "png",
#        width = 6,
#        height = 4)




# -------------------------------------------------------------------------



# for single plots
# width = 5,
# height = 3.5)
# for 2x1
# width = 5,
# height = 6)
# for 2x2
# width = 7,
# height = 6)
# for 2x3
# width = 9,
# height = 6)
# for 2x5
# width = 13,
# height = 6)

# 
# 
# # -------------------------------------------------------------------------
# # plot for end of UI
# p1 <- df_cusp%>%count(EBEND)  %>%
#   ggplot(aes(x = EBEND, y = n)) + geom_bar(stat = "identity")+
#   xlim(as.Date("2020-08-01"), as.Date("2021-03-01") ) +
#   ylab("Number of States\nEnding Extended UI benefits")
#     
# 
# pos_d <- position_dodge( width=3 )
# df_prev <- df_prev_svtwt%>%
#            mutate(d_prop = c(NA, diff(prop)))
# 
# 
# 
# table(as.Date(df_cusp$EBEND,format = "%m/%d/%Y") )
# p2 <- df_prev %>%
#   ggplot(aes(x =midDate, y = prop, col = ENDED_EB, group = ENDED_EB) ) +
#   geom_line(position = pos_d) +
#   geom_point(position = pos_d) +
#   geom_errorbar(aes(ymin = ci_l, ymax = ci_u, width = 0), position = pos_d) +
#   facet_wrap(vars(Outcome), nrow = 2) +
#   xlim(as.Date("2020-08-01"), as.Date("2021-03-01") )
# p1
# p2
# library(egg)
# ggarrange(p2,p1, heights = c(1,.5))
#
# # -------------------------------------------------------------------------
# # time since plots
# 
# table(as.Date(df_cusp$EBEND,format = "%m/%d/%Y") )
# df_prev %>%
#   ggplot(aes(x =WEEK, y = prop) ) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = ci_l, ymax = ci_u, width = 0)) +
#   facet_wrap(vars(Outcome), nrow = 1) +
#   xlab("Time since end of extended UI benefits")
# 
# 
# # -------------------------------------------------------------------------
# # Unemployment insurance plots by state
# 
# 
# # df_prev <- df_prev_svtwt %>%
# #   left_join(df_cusp, by = c("EST_ST" = "FIPS")) %>%
# #   mutate(ENDED_EB = EBEND == "0",
# #          EBEND = as.Date(EBEND, format = "%m/%d/%Y"),
# #          t_since_EBEND = midDate - EBEND,
# #          d_prop = c(NA, diff(prop)))
# 
# # by state
# df_prev %>%
# ggplot() +
#   geom_line(aes(x =midDate, y =prop, col = ENDED_EB, group = POSTCODE) ) +
#   facet_wrap(vars(Outcome), nrow = 1)
# 
# # prevalence - time since end
# df_prev %>%
# ggplot() +
#   geom_line(aes(x =t_since_EBEND, y =prop, col = ENDED_EB, group = POSTCODE), alpha = .4 ) +
#   facet_wrap(vars(Outcome), nrow = 1)
# # delta prevalence - time since end
# df_prev %>%
#   ggplot() +
#   geom_line(aes(x =t_since_EBEND, y =d_prop, col = ENDED_EB, group = POSTCODE), alpha = .4 ) +
#   facet_wrap(vars(Outcome), nrow = 1)
# # -------------------------------------------------------------------------
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# 
# 
# freqTable<- function(df, by, varCalc){
#   stratSym <- sym(by)
#   varCalcSym <- sym(varCalc)
#   
#   df_out <- df %>%
#     count(!!stratSym, !!varCalcSym) %>%
#     group_by(!!stratSym) %>%
#     mutate(prop = prop.table(n) )
#     
#   
#   return(df_out)
# }
# 
# freqTable(df, "LOST_WORK", "EIP_recv")
# 
# # clean 
# sum(df$EIP < 0) / nrow(df)
# 
# 
# # number of those who applied for unemployment (=1)
# table(df$UI_APPLY)
# # number of those who applied and received unemployment (=1)
# table(df$UI_RECV[df_import$df==1])
# 
# # number of those who lost income who applied for unemployment (=1)
# table(df$UI_APPLY[df$LOST_WORK=="Household lost employment income"])
# # number of those who lost income who received for unemployment (=1)
# table(df$UI_RECV[df$LOST_WORK=="Household lost employment income"])
# 
# 
# df_EIP <- df %>%
#   filter(!is.na(EIP)) %>%
#   filter(EIP > 0) %>%
#   mutate(EIP_recv = EIP < 4 & EIP > 0) 
# 
# 
# 
# df_summ_EIP <- df %>%
#   group_by(EIP_recv, LOST_WORK) %>%
#   summarize(N = n())
#   
#   
# 
# 
# # get proportion of missing EIP responses
# df_EIP_missing <- df_import_EIP %>%
#   count(WEEK, EIP_missing) %>%
#   group_by(WEEK) %>%
#   mutate(prop = prop.table(n))
# 
# df_EIP_recv <- df_import_EIP %>%
#   filter(!EIP_missing) %>%
#   count(WEEK, EIP_recv) %>%
#   group_by(WEEK) %>%
#   mutate(prop = prop.table(n) )
# 
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# # seen, but not answered
# sum(df_import$EIP == -99, na.rm = TRUE ) 
# # missing (did not see?)
# sum(df_import$EIP == -88, na.rm = TRUE )
# 
# # seen, but not answered
# sum(df_import$FEWRTRIPS == -99 ) 
# # missing (did not see?)
# sum(df_import$FEWRTRIPS == -88 )
# 
# # seen, but not answered
# sum(df_import$CURFOODSUF == -99 ) 
# # missing (did not see?)
# sum(df_import$CURFOODSUF == -88 )
# 
# # seen, but not answered
# sum(df_import$INTEREST == -99 ) 
# # missing (did not see?)
# sum(df_import$INTEREST == -88 )






  


