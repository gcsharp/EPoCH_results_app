library(shiny)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(shinyTree)
library(RCurl)
library(DT)
library(tableone)
library(markdown)
library(ggh4x)
library(ggnewscale)


format_loaded_data <- function(global_data, loaded_data){
  # a bit of tidying will happen (stick it all together, make lists of unique exposure and outcome classes, make a tibble of unique combinations, put everything in a list and name the objects)
  global_data$exp_classes <- unique(loaded_data$exposure_class)
  global_data$out_classes <- unique(loaded_data$outcome_class)
  loaded_data$exposure_dose_type <- loaded_data$exposure_dose
  loaded_data$exposure_dose_type[which(loaded_data$exposure_type%in%c("binary","continuous"))]<-  loaded_data$exposure_type[which(loaded_data$exposure_type%in%c("binary","continuous"))]
  loaded_data$exposure_dose_type[loaded_data$exposure_dose_type=="binary"]<-"any vs none"
  loaded_data$exposure_dose_type[loaded_data$exposure_dose_type=="continuous"]<-"continuous"
  loaded_data$exposure_dose_type[loaded_data$exposure_dose_type=="heavy"]<-"heavy vs none"
  loaded_data$exposure_dose_type[loaded_data$exposure_dose_type=="moderate"]<-"moderate vs none"
  loaded_data$exposure_dose_type[loaded_data$exposure_dose_type=="light"]<-"light vs none"
  loaded_data$exposure_dose_type[loaded_data$exposure_class=="low socioeconomic position"]<-"lowest vs other"
  formatted_data <- list(loaded_data,global_data$exp_classes,global_data$out_classes)
  names(formatted_data) <- c("all_res","exp_classes","out_classes")

  formatted_data
}

# import_data_dropbox <- function(global_data){
#   dropbox_links <- c("https://www.dropbox.com/s/amagzojd4xmj66l/metaphewas_model1a_extracted.RDS?dl=1",
#                      "https://www.dropbox.com/s/3ey11hxpqlo6i2h/metaphewas_model1b_extracted.RDS?dl=1"
#   )
#   all_res <- lapply(dropbox_links,function(x) readRDS(url(x)))
#   imported_data <- format_loaded_data(global_data, all_res)
# 
#   return(imported_data)
# }

import_data_local <- function(global_data){
  # file_paths <- c("data/rds/metaphewas_model1a_extracted.RDS",
  #                 "data/rds/metaphewas_model1b_extracted.RDS"
  # )
  # all_res <- lapply(file_paths,function(x) readRDS(x))
  all_res <- readRDS("data/rds/all_results_reduced.rds")
  imported_data <- format_loaded_data(global_data, all_res)

  return(imported_data)
}

import_table_ones <- function(global_data){
  file_paths <- sort(paste0("data/cohorts/",list.files("data/cohorts/")))
  all_tables <- lapply(file_paths,read.csv)
  all_tables
}

import_triangulation_summaries <- function(global_data){
  file_paths <- sort(paste0("data/triangulation/",list.files("data/triangulation/")))
  all_triang <- lapply(file_paths,readRDS)
  all_triang
}

create_exposure_dfs <- function(exposureclass, dat){

  if (exposureclass == "all") {
    df <- dat[dat$person_exposed!="child",]
  } else {
    df <- dat[dat$exposure_class==exposureclass&dat$person_exposed!="child",]
  }

  df$exposure_dose_ordered <- factor(df$exposure_dose,ordered=T,levels=c("light","moderate","heavy"))
  df <- df[order(df$exposure_subclass,df$exposure_time,df$exposure_dose_ordered),]
  df$exposure_subclass_time_dose <- paste(df$exposure_subclass,df$exposure_time,ifelse(is.na(df$exposure_dose),"",df$exposure_dose))
  df$exposure_subclass_time_dose<-factor(df$exposure_subclass_time_dose,ordered=T,levels=unique(df$exposure_subclass_time_dose))
  # to convert ln(OR) to cohen's d (SDM), multiply ln(OR) by sqrt(3)/pi (which is 0.5513)
  # to convert SDM to ln(OR), multiply SDM by pi/sqrt(3) (which is 1.814)
  # https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
  # https://onlinelibrary.wiley.com/doi/abs/10.1002/1097-0258(20001130)19:22%3C3127::AID-SIM784%3E3.0.CO;2-M
  df$est_SDM <- df$est
  df$est_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$est[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df$se_SDM <- df$se
  df$se_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$se[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df$lci <- df$est-(1.96*df$se) #lower 95% CI
  df$uci <- df$est+(1.96*df$se) #upper 95% CI
  df$or <- exp(df$est)
  df$or_lci <- exp(df$lci)
  df$or_uci <- exp(df$uci)
  df$p_sig <- ifelse(df$p<0.05,"#1B9E77","#D95F02")
  df$p_rank <- rank(-log10(df$p))
  df
}

create_outcome_dfs <- function(outcomeclass, dat){

  if (outcomeclass == "all") {
    df <- dat[dat$person_exposed!="child",]
  } else {
    df <- dat[dat$outcome_class==outcomeclass&dat$person_exposed!="child",]
  }

  df$outcome_time_ordered <- factor(df$outcome_time,ordered=T,levels=c("pregnancy","delivery","first year", "age 1 to 2","age 3 to 4","age 5 to 7","age 8 to 11","any time in childhood"))
  df <- df[order(df$outcome_subclass1,df$outcome_subclass2,df$outcome_time_ordered),]
  df$outcome_subclass_time <- paste(df$outcome_subclass1,df$outcome_subclass2,df$outcome_time)
  df$outcome_subclass_time<-factor(df$outcome_subclass_time,ordered=T,levels=unique(df$outcome_subclass_time))
  df$outcome_subclass2_time <- paste(df$outcome_subclass2,df$outcome_time)
  # to convert ln(OR) to cohen's d (SDM), multiply ln(OR) by sqrt(3)/pi (which is 0.5513)
  # to convert SDM to ln(OR), multiply SDM by pi/sqrt(3) (which is 1.814)
  # https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
  # https://onlinelibrary.wiley.com/doi/abs/10.1002/1097-0258(20001130)19:22%3C3127::AID-SIM784%3E3.0.CO;2-M
  df$est_SDM <- df$est
  df$est_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$est[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df$se_SDM <- df$se
  df$se_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$se[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df$lci <- df$est-(1.96*df$se) #lower 95% CI
  df$uci <- df$est+(1.96*df$se) #upper 95% CI
  df$or <- exp(df$est)
  df$or_lci <- exp(df$lci)
  df$or_uci <- exp(df$uci)
  df$p_sig <- ifelse(df$p<0.05,"#1B9E77","#D95F02")
  df$p_rank <- rank(-log10(df$p))
  df
}

create_exposure_dfs <- function(exposureclass, dat){

  if (exposureclass == "all") {
    df <- dat[dat$person_exposed!="child",]
  } else {
    df <- dat[dat$exposure_class==exposureclass&dat$person_exposed!="child",]
  }

  df$exposure_dose_ordered <- factor(df$exposure_dose,ordered=T,levels=c("light","moderate","heavy"))
  df <- df[order(df$exposure_subclass,df$exposure_time,df$exposure_dose_ordered),]
  df$exposure_subclass_time_dose <- paste(df$exposure_subclass,df$exposure_time,ifelse(is.na(df$exposure_dose),"",df$exposure_dose))
  df$exposure_subclass_time_dose<-factor(df$exposure_subclass_time_dose,ordered=T,levels=unique(df$exposure_subclass_time_dose))
  # to convert ln(OR) to cohen's d (SDM), multiply ln(OR) by sqrt(3)/pi (which is 0.5513)
  # to convert SDM to ln(OR), multiply SDM by pi/sqrt(3) (which is 1.814)
  # https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
  # https://onlinelibrary.wiley.com/doi/abs/10.1002/1097-0258(20001130)19:22%3C3127::AID-SIM784%3E3.0.CO;2-M
  df$est_SDM <- df$est
  df$est_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$est[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df$se_SDM <- df$se
  df$se_SDM[df$outcome_type=="binary"|df$outcome_type=="ordinal"]<-df$se[df$outcome_type=="binary"|df$outcome_type=="ordinal"]*0.5513
  df$lci <- df$est-(1.96*df$se) #lower 95% CI
  df$uci <- df$est+(1.96*df$se) #upper 95% CI
  df
}

create_forest_dfs <- function(dat, exp_linker, out_linker){
  # which cohorts contributed to the meta-analysis 
  cohorts <- unlist(strsplit(dat$cohorts,split=","))
  cohorts <-as.character(str_remove(cohorts," "))

  #prepare data:
  df <- data.frame(exposure=rep(dat$exposure_linker, length(cohorts)+1),
                   outcome=rep(dat$outcome_linker, length(cohorts)+1),
                   cohort=c("meta",cohorts),
                   binary=c(dat$outcome_type=="binary"),
                   est=c(dat$est,#meta-analysis estimate
                         unlist(dat[,paste0("est_",cohorts)])), #estimates for each cohort
                   se=c(dat$se,#meta-analysis standard errror
                         unlist(dat[,paste0("se_",cohorts)])), #standard errors for each cohort
                   n=c(dat$total_n,#meta-analysis sample size
                        unlist(dat[,paste0("n_",cohorts)])), #sample size for each cohort
                   p=c(dat$p,#meta-analysis P-value
                       unlist(dat[,paste0("p_",cohorts)])) #P-value for each cohort
                   )
  df$lci <- df$est-(1.96*df$se) #lower 95% CI
  df$uci <- df$est+(1.96*df$se) #upper 95% CI
  df$or <- exp(df$est)
  df$or_lci<-exp(df$lci)
  df$or_uci <-exp(df$uci)

  df$cohort<-factor(df$cohort,ordered=T,levels=c("meta",sort(cohorts,decreasing = T))) #ordering so that meta-analysis statistic comes bottom
  df$point_size <- (1/df$se)/(1/df$se[1]) #then all other cohorts will have points sized smaller than 1, but proportionate to their contribution to the meta-analysis (i.e. the inverse of the variance)
  df$point_symbol <- "square"
  df$point_symbol[1] <-"diamond"
  df
}

# Interpret the results in words

interpret <- function(res,out1,exp1,model){
  exp_binary <- ifelse(grepl("binary|ordinal",exp1),T,F)
  out_binary <- ifelse(grepl("binary",out1),T,F)
  
  dose <- res$exposure_dose[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  n_cohorts <- res$cohorts_n[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  cohorts <- res$cohorts[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  I2 <- res$i2[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  hetP <- res$hetp[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  n_total <- res$total_n[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  n_exp <- res$total_n_exposure[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  n_out <- res$total_n_outcome[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  name_out <- res$outcome_subclass2[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  time_out <- res$outcome_time[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  or <- exp(res$est[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1])
  or_lci <- exp(res$est[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]-(1.96*res$se[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]))
  or_uci <- exp(res$est[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]+(1.96*res$se[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]))
  P <- res$p[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  lci <- res$est[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]-(1.96*res$se[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1])
  uci <- res$est[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]+(1.96*res$se[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1])
  est <- res$est[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]
  parent <- ifelse(res$person_exposed[res$model==model & tolower(res$exposure_linker)==exp1 & tolower(res$outcome_linker)==out1]=="mother",
                   "mothers","partners")
  if (grepl("active smok", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "smoked"
  } else if (grepl("passive smok", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "were exposed to passive smoke"
  } else if (grepl("any drinking", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "drank alcohol"
  } else if (grepl("binge", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "binged on alcohol"
  } else if (grepl("any source", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "consumed caffeine"
  } else if (grepl("basic", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "exercised"
  } else  if (grepl("tea", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "consumed caffeine from tea"
  } else if (grepl("coffee", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "consumed caffeine from coffee"
  } else if (grepl("cola", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "consumed caffeine from cola"
  } else if (grepl("education", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "had a low social class (based on education)"
  } else if (grepl("occupa", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    name_exp <- "had a low social class (based on occupation)"
  }
  if (grepl("at study recruitment", res$exposure_time[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    time_exp <- NA
  } else if (grepl("early onset", res$exposure_time[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    time_exp <- "before age 12"
  } else if (grepl("ever in life", res$exposure_time[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    time_exp <- "at some point in their lives"
  } else if (grepl("ever in pregnancy", res$exposure_time[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    time_exp <- "at any point during pregnancy"
  } else if (grepl("first trimester", res$exposure_time[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    time_exp <- "in the first trimester of pregnancy"
  } else if (grepl("second trime", res$exposure_time[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    time_exp <- "in the second trimester of pregnancy"
  } else if (grepl("third trime", res$exposure_time[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    time_exp <- "in the third trimester of pregnancy"
  } else if (grepl("first two", res$exposure_time[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    time_exp <- "in the first two years after the baby's birth"
  } else if (grepl("preconcep", res$exposure_time[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])) {
    time_exp <- "in the months before pregnancy"
  }

  reference <- " that did not."
  
  if(tolower(dose) %in% c("heavy","light","moderate")){
    name_exp <-paste0(name_exp," a ",dose," amount")
    if(grepl("smok",name_exp)){
      reference <-" that did not smoke at all."
    }
    if(grepl("caff",name_exp)){
      reference <-" that did not consume any caffeine."
    }
    if(grepl("drank",name_exp)){
      reference <-" that did not consume any alcohol."
    }
    if(grepl("binge",name_exp)){
      reference <-" that did not binge drink."
    }
  }  
  
  if(grepl("score", res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])){
   if(grepl("smok", res$exposure_class[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])){
   name_exp <-paste0("genetic risk score for smoking (",
                     res$exposure_subclass[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1],
                     ")")
   }    
     if(grepl("caff", res$exposure_class[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])){
       name_exp <-"signif( risk score for caffeine consumption"
     }
       if(grepl("alc", res$exposure_class[res$model == model & tolower(res$exposure_linker) == exp1 & tolower(res$outcome_linker) == out1])){
         name_exp <-"genetic risk score for alcohol consumption"
       }
     }

     
    if(grepl("smok",name_exp)){
      reference <-" that did not smoke at all."
    }
    if(grepl("caff",name_exp)){
      reference <-" that did not consume any caffeine."
    }
    if(grepl("drank",name_exp)){
      reference <-" that did not consume any alcohol."
    }
  if(grepl("binge",name_exp)){
    reference <-" that did not binge drink."
  }
  
  if(exp_binary==T & out_binary==T){
    prev_exp <- paste0("In this model, there were ", n_total," ",parent,
                       ". Of those, ", n_exp," (",signif(100*(n_exp/n_total),1),"%) ", name_exp," ", ifelse(is.na(time_exp),"", time_exp),".")
    prev_out <- paste0("In this model, ", n_out, " out of ",n_total," children (", signif(100*(n_out/n_total),1),"%) had ", name_out, " at ", time_out,".")
    or_est_described <- paste0("The odds ratio for the association between the exposure and the outcome is ", signif(or,2),
                               ". This means that the odds of ", name_out," at ", time_out, " are ", signif(ifelse(or>1,or-1,1-or)*100,2),"% ",ifelse(or>1,"higher", "lower"), " for children of ",
                               parent, " that ", name_exp, " ", ifelse(is.na(time_exp),"", time_exp), " relative to children of ",parent, reference)
    ci_described <- paste0("The confidence intervals of the odds ratio suggest that we can be 95% sure that the true value of the odds ratio lies between ",signif(or_lci,2)," and ",signif(or_uci,2),".")
    p_described <- paste0("The P-value is ", ifelse(P<0.0009,scientific(P,2),ifelse(P<0.0009,scientific(P,2),signif(P,2)))," which suggests we can", ifelse(P<1E-4," be quite","not be"), " confident that the association did not occur by chance alone. It's important to note that the interpretation of p-values is somewhat subjective and depends on the context.")
  }
  
  if(exp_binary==T & out_binary==F){
    prev_exp <- paste0("In this model, there were ", n_total," ",parent,
                       ". Of those, ", n_exp," (",signif(100*(n_exp/n_total),1),"%) ", name_exp," ", ifelse(is.na(time_exp),"", time_exp),".")
    prev_out <- NA
    or_est_described <- paste0("The standardised mean difference for the association between the exposure and the outcome is ", signif(est,2),
                               ". In other words, the mean value of ", name_out," at ", time_out, " is ", abs(signif(est,2)), " standard deviations ", ifelse(est>0,"higher","lower")," for children of ",
                               parent, " that ", name_exp, " ", ifelse(is.na(time_exp),"", time_exp), " relative to children of ",parent,reference)
    ci_described <- paste0("The confidence intervals of the effect estimate suggest that we can be 95% sure that the true value of the standardised mean difference lies between ",signif(lci,2)," and ",signif(uci,2),".")
    p_described <- paste0("The P-value is ", ifelse(P<0.0009,scientific(P,2),signif(P,2))," which suggests we can", ifelse(P<1E-4," be quite","not be very"), " confident that the association did not occur by chance alone. It's important to note that the interpretation of p-values is somewhat subjective and depends on the context.")
  }
  if(exp_binary==F & grepl("score",name_exp)==F & out_binary==T){ 
    prev_exp <- paste0("In this model, there were ", n_total," ",parent,".")
    prev_out <- paste0("In this model, ", n_out, " out of ",n_total," children (", signif(100*(n_out/n_total),1),"%) had ", name_out, " at ", time_out,".")
    or_est_described <- paste0("The odds ratio for the association between the exposure and the outcome is ", signif(or,2),
                               ". This means that for 1mg/day increase in caffeine consumption, there is a ", signif(ifelse(or>1,or-1,1-or)*100,2),"% ", ifelse(or>1,"higher", "lower")," odds of a child having ", name_out," at ", time_out,".")
    ci_described <- paste0("The confidence intervals of the odds ratio suggest that we can be 95% sure that the true value of the odds ratio lies between ",signif(or_lci,2)," and ",signif(or_uci,2),".")
    p_described <- paste0("The P-value is ", ifelse(P<0.0009,scientific(P,2),signif(P,2))," which suggests we can", ifelse(P<1E-4," be quite","not be"), " confident that the association did not occur by chance alone. It's important to note that the interpretation of p-values is somewhat subjective and depends on the context.")
    
  } 
  if(exp_binary==F & grepl("score",name_exp)==T & out_binary==T){ 
    prev_exp <- paste0("In this model, there were ", n_total," ",parent,".")
    prev_out <- paste0("In this model, ", n_out, " out of ",n_total," children (", signif(100*(n_out/n_total),2),"%) had ", name_out, " at ", time_out,".")
    or_est_described <- paste0("The odds ratio for the association between the exposure and the outcome is ", signif(or,2),
                               ". This means that for every 1 standard deviation increase in the genetic risk score, there is a ", signif(ifelse(or>1,or-1,1-or)*100,2),"% ", ifelse(or>1,"higher", "lower")," odds of a child having ", name_out," at ", time_out,".")
    ci_described <- paste0("The confidence intervals of the odds ratio suggest that we can be 95% sure that the true value of the odds ratio lies between ",signif(or_lci,2)," and ",signif(or_uci,2),".")
    p_described <- paste0("The P-value is ", ifelse(P<0.0009,scientific(P,2),signif(P,2))," which suggests we can", ifelse(P<1E-4," be quite","not be"), " confident that the association did not occur by chance alone. It's important to note that the interpretation of p-values is somewhat subjective and depends on the context.")
    
  } 
  if(exp_binary==F & grepl("score",name_exp)==F & out_binary==F){
    prev_exp <- paste0("In this model, there were ", n_total," ",parent, " and children.")
    prev_out <-NULL
    or_est_described <- paste0("The standardised mean difference for the association between the exposure and the outcome is ", signif(est,2),
                               ". In other words, for every 1mg/day increase in caffeine consumption, there is ",ifelse(est>0,"an increase in ","a decrease in "), name_out," at ", time_out, " of ", abs(signif(est,2)), " standard deviations.")
    ci_described <- paste0("The confidence intervals of the effect estimate suggest that we can be 95% sure that the true value for the standardised mean difference lies between ",signif(lci,2)," and ",signif(uci,2),".")
    p_described <- paste0("The P-value is ", ifelse(P<0.0009,scientific(P,2),signif(P,2))," which suggests we can", ifelse(P<1E-4," be quite","not be very"), " confident that the association did not occur by chance alone. It's important to note that the interpretation of p-values is somewhat subjective and depends on the context.")
    
  } #This means that, on average, for every one-unit increase in maternal caffeine intake (measured in milligrams per day), the BMI of the child is estimated to increase by 0.2 standard deviations, assuming all other variables in the model remain constant.
  
  if(exp_binary==F & grepl("score",name_exp)==T & out_binary==F){
    prev_exp <- paste0("In this model, there were ", n_total," ",parent, " and children.")
    prev_out <- NULL
    or_est_described <- paste0("The standardised mean difference for the association between the exposure and the outcome is ", signif(est,2),
                               ". In other words, for every 1 standard deviation increase in the genetic risk score, there is ",ifelse(est>0,"an increase in ","a decrease in "), name_out," at ", time_out, " of ", abs(signif(est,2)), " standard deviations.")
    ci_described <- paste0("The confidence intervals of the effect estimate suggest that we can be 95% sure that the true value for the standardised mean difference lies between ",signif(lci,2)," and ",signif(uci,2),".")
    p_described <- paste0("The P-value is ", ifelse(P<0.0009,scientific(P,2),signif(P,2))," which suggests we can", ifelse(P<1E-4," be quite","not be very"), " confident that the association did not occur by chance alone. It's important to note that the interpretation of p-values is somewhat subjective and depends on the context.")
    
  } 
  cohorts_described <-paste0(n_cohorts," cohort(s) contributed results to this finding: ",cohorts,".")
  if(n_cohorts>1){
    het_described <- paste0("The I-squared value is ",signif(I2,3)," and the Q test heterogeneity P-value is ",ifelse(hetP<0.0009,scientific(hetP,2),ifelse(hetP<0.0009,scientific(hetP,2),signif(hetP,2))),
                            ". This means that an estimated ",signif(I2,3),
                            "% of the variation across studies is due to true heterogeneity (i.e. differences between the studies) rather than chance. The P-value can be interpreted as a measure of confidence in that assertion, with P<0.05 being a common threshold for accepting the hypothesis that different studies have different effects, and therefore variations in effect estimates between studies are NOT due to chance alone. High levels of heterogeneity (i.e. a large I-squared and a low Q-test P-value) mean that the meta-analysis effect estimate may be biased, because the assumptions of fixed effects meta-analysis are unlikely to have been met. ")
    
  }else{
    het_described <-NA
  }
  
  paste(na.omit(c(prev_exp,
       prev_out,
       or_est_described,
       ci_described,
       p_described,
       cohorts_described,
       het_described)),collapse="<br><br>")
  
}


create_triang_DFs <-function(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=F,oe=F){
  
  outctype <- tolower(df$outcome_type[tolower(df$outcome_subclass2)==tolower(outc)][1])
 
   if(expclass=="smoking"){
    expsubclass <- "active smoking"
    exptime <- "ever in pregnancy"
    exptype <-"binary"
  }
  
  if(expclass=="alcohol consumption"){
    expsubclass <- "any drinking"
    exptime <- "ever in pregnancy"
    exptype <- "binary"
  }
  
  if(expclass == "caffeine consumption"){
    expsubclass <- "any source"
    exptime <- "second trimester" #as a proxy for ever in pregnancy, chosen because it had the most amount of data and it's in the middle of preg so likely to reflect what happened in the first and third trimester
    exptype <- "continuous"
  }
  
  # calculate LCI, UCI and change to OR scale if necessary
  df$lci <- df$est - (df$se * 1.96)
  df$uci <- df$est + (df$se * 1.96)
  if(outctype == "binary"){
    df$est <- exp(df$est)
    df$lci <- exp(df$lci)
    df$uci <- exp(df$uci)
  }
  
  # set up factors
  df$mutualadj <- ifelse(grepl("1a|2a|3a|4a",df$model),"unadjusted for co-parent's exposure","adjusted for co-parent's exposure")
  df$modelnumber <- NA
  df$modelnumber[grepl("model1",df$model)]<-"Minimal"
  df$modelnumber[grepl("model2",df$model)]<-"Standard"
  df$modelnumber[grepl("model3",df$model)]<-"Standard + previous timepoints"
  df$modelnumber[grepl("model4",df$model)]<-"Standard + mediators"
  
  df$outcome_time <- factor(df$outcome_time, ordered=T, levels=c("delivery","first year","age 1 to 2","age 3 to 4","age 5 to 7", "age 8 to 11","any time in childhood"))
  
  df$mutual_parent <- NA
  df$mutual_parent[df$mutualadj=="unadjusted for co-parent's exposure" & df$person_exposed=="mother"]<-"mother (no mutual adj)"
  df$mutual_parent[df$mutualadj=="adjusted for co-parent's exposure" & df$person_exposed=="mother"]<-"mother (mutual adj)"
  df$mutual_parent[df$mutualadj=="unadjusted for co-parent's exposure" & df$person_exposed=="partner"]<-"partner (no mutual adj)"
  df$mutual_parent[df$mutualadj=="adjusted for co-parent's exposure" & df$person_exposed=="partner"]<-"partner (mutual adj)"
  df$mutual_parent <- droplevels(factor(df$mutual_parent,ordered=T,levels=c("mother (no mutual adj)","mother (mutual adj)","partner (no mutual adj)","partner (mutual adj)")))
  
  df$exposure_time <- factor(df$exposure_time,ordered=T,levels=c("at study recruitment","initiation","age at initiation","heaviness","cessation","alcohol","caffeine","early onset","ever in life","preconception","ever in pregnancy","first trimester","second trimester","third trimester", "first two postnatal years"))
  
  df$modelname <- ifelse(df$model=="model2b","Without adjustment for exposure in previous time-point","With adjustment for exposure in previous time-point")
  df$modelname <- factor(df$modelname,ordered=T,levels=c("Without adjustment for exposure in previous time-point","With adjustment for exposure in previous time-point"))
  
  df$exposure_dose <- factor(df$exposure_dose,ordered=T,levels=c("light","moderate","heavy"))
  
  df$mutual_model_grs <- NA
  df$mutual_model_grs[grepl("model2a",df$model)] <- "Unadj for co-parent's GRS + unadj for child's GRS"
  df$mutual_model_grs[grepl("model2b",df$model)] <- "Adj for co-parent's GRS + unadj for child's GRS"
  df$mutual_model_grs[grepl("model4a",df$model)] <- "Unadj for co-parent's GRS + adj for child's GRS"
  df$mutual_model_grs[grepl("model4b",df$model)] <- "Adj for co-parent's GRS + adj for child's GRS"
  
  df$exposure_dose2 <- as.character(df$exposure_dose)
  df$exposure_dose2[df$exposure_type=="binary"]<-"any"
  df$exposure_dose2[df$exposure_type=="continuous"]<-"continuous"
  df$exposure_dose2 <- factor(df$exposure_dose2,ordered=T,levels=c("continuous","any","light","moderate","heavy"))
  
  df$exposure_time_sep <- as.character(df$exposure_time)
  df$exposure_time_sep[df$exposure_time_sep=="at study recruitment"] <- df$exposure_subclass[df$exposure_time_sep=="at study recruitment"]
  df$exposure_time_sep <- factor(df$exposure_time_sep,ordered=T,levels=c("early onset","ever in life","ever in pregnancy", "preconception","first trimester","second trimester","third trimester", "first two postnatal years", "low SEP (occupation)", "low SEP (education)"))
  
  #set nullvalue value and x axis title for plots based on data type of the outcome
  nullvalue <- 1
  xtitle <- "odds ratio"
  if(outctype=="continuous"){
    nullvalue <- 0
    xtitle <- "standardised mean difference"
  }
  
  df<-df[order(df$outcome_time),]
  df$outcome_subclass2_time <- paste0(df$outcome_subclass2," at ",df$outcome_time)
  df$outcome_subclass2_time <- factor(df$outcome_subclass2_time,ordered=T,levels=unique(df$outcome_subclass2_time))
  
  if(dose==T){
    exptype <- "ordinal"
  }
  
  if(grs==T){
    exptype <- "continuous"
    expsubclass<-"genetic risk score"
  }
  
  if(sep==T){
  sep_df <- df[which(df$exposure_class %in% c(expclass, "low socioeconomic position") &
                       df$person_exposed==parent &
                       df$outcome_time==outtime &
                       df$exposure_subclass %in% c(expsubclass,"low SEP (education)","low SEP (occupation)") &
                       df$exposure_type == "binary" &
                       tolower(df$outcome_subclass2)==tolower(outc)&
                       df$outcome_type==outctype),]
  sep_df <- droplevels(sep_df[-grep("FEMALE|MALE|2a|3a|4a|1b|2b|3b|4b",sep_df$model),])
  }else{
    sep_df <- NULL
  }
  
  
  if(oe==T){
    sc_df <- df[which(df$exposure_class== expclass&
                        df$outcome_time==outtime &
                        df$person_exposed==parent &
                        tolower(df$outcome_subclass2)==tolower(outc)&
                        df$outcome_type==outctype),]
    sc_df <- sc_df[-grep("FEMALE|MALE|1a|2a|3a|4a|1b|3b|4b",sc_df$model),]
    sc_df <- sc_df[-grep("genetic",sc_df$exposure_subclass),]
    
    if("continuous" %in% sc_df$exposure_dose2){
      sc_df <- sc_df[sc_df$exposure_dose2=="continuous",]
    }else{
      sc_df <- sc_df[sc_df$exposure_dose2=="any",]
    }
    sc_df <- droplevels(sc_df[sc_df$person_exposed == parent,])
  }else{
    sc_df <-NULL
  }
  
  bdf <- df[which(df$exposure_class==expclass &
                    df$exposure_subclass == expsubclass &
                    df$outcome_time==outtime &
                    df$person_exposed %in% c("mother","partner") &
                    df$exposure_type == exptype &
                    tolower(tolower(df$outcome_subclass2))==tolower(outc)&
                    df$outcome_type==outctype),]
  bdf <- bdf[-grep("FEMALE|MALE",bdf$model),]
  
  if(dose==F&grs==F&time==F&sep==F&oe==F){
  bdf <- bdf[which(bdf$exposure_time%in%exptime),]
  }
  
  if(time==T){
    bdf <- bdf[which(bdf$exposure_time %in% c("preconception","first trimester","second trimester","third trimester", "first two postnatal years")),]
    bdf <- bdf[bdf$model%in%(c("model2b","model3b")),] # main model with parental adjustment only, with and without adjustment for previous timepoints
  }
  
  df <- droplevels(bdf[bdf$person_exposed == parent,])
  
  res <- list(df,bdf,expsubclass,exptime,exptype,outctype,nullvalue,xtitle,sc_df,sep_df)
  names(res)<-c("df","bdf","expsubclass","exptime","exptype","outctype","nullvalue","xtitle","sc_df","sep_df")
  res
}

