# @file functions
#
# Copyright 2017 Observational Health Data Sciences and Informatics
#
# This file is part of:
#  ----------------------------------------------
#  DiabetesTxPath
#  ----------------------------------------------
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# @author Stanford University Center for Biomedical Informatics - Shah Lab
# @author Rohit Vashisht
#
#' @title runStudy
#'
#' @author Rohit Vashisht
#'
#' @details This function can be used to perform the DiabetesTxPathway analysis
#' for a given outcome of interest. Briefly, for a given drug combinations, the
#' function will construct a treatment, a comparator and an outcome cohort. Will
#' use the CohortMethod and perform the analysis.
#'
#' @param connectionDetails The connection details of the database.
#' @param cdmDatabaseSchema The name of cdm database schema.
#' @param resultsDatabaseSchema The name of results database schema.
#' @param cdmVersion The name of cdm version, should be 5
#' @param outComeId The outcome Id for which study need to be executed (3 = HbA1c, 4 = MI, 5 = KD and 6 = ED)
#' @param outComeName Name of the outcome.
#' @param numThread Number of threads.
#'
#' @export
runStudy <- function(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     resultsDatabaseSchema = resultsDatabaseSchema,
                     cdmVersion = cdmVersion,
                     outComeId = outComeId,
                     outComeName = outComeName,
                     numThread = numThread,
                     results_path = results_path){
  tcComb <- read.csv(system.file(paste("settings/","treatmentComparator.csv",sep=""), package = "DiabetesTxPath"), stringsAsFactors = FALSE, header = TRUE)
  tcComb <- tcComb[c(29,33,38),]
  conn <- DatabaseConnector::connect(connectionDetails)
  drugComparision <- data.frame()
  for(i in 1:nrow(tcComb)){
    buildCohort(connectionDetails = connectionDetails,
                cdmDatabaseSchema = cdmDatabaseSchema,
                resultsDatabaseSchema = resultsDatabaseSchema,
                treatment = paste(tcComb$treatmentCohort[i],".sql",sep=""),
                comparator = paste(tcComb$comparatorCohort[i],".sql",sep=""),
                outComeId = outComeId)
    #Computing total number of patients in both treatment and comparator cohorts.
    #The study will not be performed if each treatment and comparator cohorts have lesss than 250 patients.
    sql <- paste("SELECT COUNT (COHORT_DEFINITION_ID) AS PID FROM @results_database_schema.ohdsi_t2dpathway WHERE COHORT_DEFINITION_ID = 1",sep="")
    sql <- SqlRender::renderSql(sql,results_database_schema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    numPidOne <- as.numeric(as.character((querySql(conn, sql))))
    sql <- paste("SELECT COUNT (COHORT_DEFINITION_ID) AS PID FROM @results_database_schema.ohdsi_t2dpathway WHERE COHORT_DEFINITION_ID = 2",sep="")
    sql <- SqlRender::renderSql(sql,results_database_schema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    numPidTwo <- as.numeric(as.character((querySql(conn, sql))))
    if(numPidOne < 250 || numPidTwo < 250){
      treatment <- tcComb$treatmentCohort[i]
      comparator <- tcComb$comparatorCohort[i]
      drugRR_raw <- cbind(treatment,comparator,outComeName,NA,NA,NA)
      colnames(drugRR_raw) <- c("Treatment","Comparator","outCome","RR","lowCI","upCI")
    }else
    {
      treatment <- tcComb$treatmentCohort[i]
      comparator <- tcComb$comparatorCohort[i]
      cid2Rm <- read.csv(system.file(paste("settings/","conceptIdToRemove.csv",sep=""), package = "DiabetesTxPath"), stringsAsFactors = FALSE, header = FALSE)
      cid2Rm <- as.numeric(as.character(unique(cid2Rm$V1)))
      results <- drugEfficacyAnalysis(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      resultsDatabaseSchema = resultsDatabaseSchema,
                                      cid2Rm = cid2Rm,
                                      outCome = outComeId,
                                      cdmVersion = cdmVersion,
                                      treatment = treatment,
                                      comparator = comparator,
                                      numThread = numThread)

      if(length(results)<1){
        drugRR_raw <- cbind(treatment,comparator,outComeName,NA,NA,NA)
        colnames(drugRR_raw) <- c("Treatment","Comparator","outCome","RR","lowCI","upCI")
      }else if(is.numeric(results[[6]]$outcomeModelTreatmentEstimate$logRr)==TRUE & is.numeric(results[[6]]$outcomeModelTreatmentEstimate$logLb95)==TRUE & is.numeric(results[[6]]$outcomeModelTreatmentEstimate$logUb95)==TRUE & is.numeric(results[[6]]$outcomeModelTreatmentEstimate$seLogRr)==TRUE){
        drugRR_raw <- cbind(treatment,comparator,outComeName,exp(coef(results[[6]])),exp(confint(results[[6]]))[1],exp(confint(results[[6]]))[2])
        colnames(drugRR_raw) <- c("Treatment","Comparator","outCome","RR","lowCI","upCI")
        pdf(file=paste(results_path,treatment,"-and-",comparator,"_",outComeName,".pdf",sep=""))
        plot(results[[2]]) #Ps score before matching.
        plot(results[[3]]) #Ps score after matching.
        plot(results[[5]]) #Cov balance.
        plot(results[[4]]) #Attr diagram.
        plot(results[[7]]) #Km without CI
        plot(results[[8]]) #Km with CI
        plot.new()
        grid.table(results[[1]]) #PsAUC
        dev.off()
        write.csv(results[[9]],file=paste(results_path,"ageBeforeMatching-",treatment,"-and-",comparator,"_",outComeName,".csv",sep=""))
        write.csv(results[[10]],file=paste(results_path,"ageAfterMatching-",treatment,"-and-",comparator,"_",outComeName,".csv",sep=""))
        write.csv(results[[11]],file=paste(results_path,"genderBeforeMatching-",treatment,"-and-",comparator,"_",outComeName,".csv",sep=""))
        write.csv(results[[12]],file=paste(results_path,"genderAfterMatching-",treatment,"-and-",comparator,"_",outComeName,".csv",sep=""))
        write.csv(results[[13]],file=paste(results_path,"stat-",treatment,"-and-",comparator,"_",outComeName,".csv",sep=""))
      }else
      {
        drugRR_raw <- cbind(treatment,comparator,outComeName,NA,NA,NA)
        colnames(drugRR_raw) <- c("Treatment","Comparator","outCome","RR","lowCI","upCI")
      }
    }
    drugComparision <- rbind(drugComparision,drugRR_raw)
    remove(drugRR_raw)
  }
  write.csv(drugComparision, file = paste(results_path,"drugComparision","_",outComeName,".csv",sep=""))
}

