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
#' end-to-end. Just supply the details and leave it running overnight. Please note
#' the function will perform analysis if there are atleast 100 patients for each
#' of the drug group considered in the study.
#'
#' @param connectionDetails The connection details of the database.
#' @param cdm_database_schema The name of your cdm database schema.
#' @param results_database_schema The name of your results database schema.
#' @param target_database_schema The name of the target database schema.
#' @param target_cohort_table The name of target cohort table.
#' @param numThread Number of threads to be used.
#' @param idOne Treatment cohort ID
#' @param idTwo Comparator Cohort ID
#' @param idThree Outcome cohort ID
#' @param idFour Outcome cohort ID
#' @param idFive Outcome cohort ID
#' @param idSix Outcome cohort ID
#'
#' @export
runStudy <- function(connectionDetails,cdmDatabaseSchema,resultsDatabaseSchema,cdmVersion,outComeId,outComeName){
  tcComb <- read.csv(system.file(paste("settings/","treatmentComparator.csv",sep=""), package = "DiabetesTxPath"), stringsAsFactors = FALSE, header = TRUE)
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
  #The study will not be performed if each treatment and comparator cohorts have lesss than 100 patients.
    sql <- paste("SELECT COUNT (COHORT_DEFINITION_ID) AS PID FROM @results_database_schema.ohdsi_t2dpathway WHERE COHORT_DEFINITION_ID = 1",sep="")
    sql <- SqlRender::renderSql(sql,results_database_schema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    numPidOne <- as.numeric(as.character((querySql(conn, sql)))) #In some cases the 6th outcome is not being generated.
    sql <- paste("SELECT COUNT (COHORT_DEFINITION_ID) AS PID FROM @results_database_schema.ohdsi_t2dpathway WHERE COHORT_DEFINITION_ID = 2",sep="")
    sql <- SqlRender::renderSql(sql,results_database_schema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    numPidTwo <- as.numeric(as.character((querySql(conn, sql))))
    if(numPidOne < 150 || numPidTwo < 150){
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
                             comparator = comparator)

        if(length(results)>1){
          drugRR_raw <- cbind(treatment,comparator,outComeName,exp(coef(results[[7]])),exp(confint(results[[7]]))[1],exp(confint(results[[7]]))[2])
          colnames(drugRR_raw) <- c("Treatment","Comparator","outCome","RR","lowCI","upCI")
          write.csv(results[[2]],file=paste(results_path,"impFeature",treatment,"-and-",comparator,".csv",sep=""))
          pdf(file=paste(results_path,treatment,"-and-",comparator,".pdf",sep=""))
          plot(results[[3]]) #Ps score before matching.
          plot(results[[4]]) #Ps score after matching.
          plot(results[[6]]) #Cov balance.
          plot(results[[5]]) #Attr diagram.
          plot(results[[8]]) #Km without CI
          plot(results[[9]]) #Km with CI
          plot.new()
          grid.table(results[[1]]) #PsAUC
          dev.off()
        }else
        {
          drugRR_raw <- cbind(treatment,comparator,outComeName,NA,NA,NA)
          colnames(drugRR_raw) <- c("Treatment","Comparator","outCome","RR","lowCI","upCI")
        }
    }
    drugComparision <- rbind(drugComparision,drugRR_raw)
    remove(drugRR_raw)
  }
  write.csv(drugComparision, file = paste(results_path,"drugComparision.csv",sep=""))
}
