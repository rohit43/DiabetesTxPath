# @file functions
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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
runStudy <- function(connectionDetails,cdm_database_schema,results_database_schema,target_database_schema,target_cohort_table,numThread,idOne, idTwo, idThree, idFour, idFive, idSix){
  drugs <- read.csv(system.file(paste("csv/","drugComb.csv",sep=""), package = "TreatmentPath"), stringsAsFactors = FALSE, header = FALSE)
  colnames(drugs) <- c("drugName")
  for(i in 1:nrow(drugs)){
    sqlFileName <- paste(as.character(drugs$drugName[i]),sep="")
    drugCombName <- gsub(".sql","",sqlFileName)
    getCohort(connectionDetails, cdmDatabaseSchema, resultsDatabaseSchema, sqlFileName, targetDatabaseSchema, targetCohortTable,idOne,idTwo,idThree,idFour,idFive,idSix)
    sql <- paste("SELECT COUNT (COHORT_DEFINITION_ID) AS PID FROM @results_database_schema.studyCohort WHERE COHORT_DEFINITION_ID = 1",sep="")
    sql <- renderSql(sql, results_database_schema = resultsDatabaseSchema)$sql
    sql <- translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    connection <- connect(connectionDetails)
    numPidOne <- as.numeric(as.character(querySql(connection, sql)))
    remove(sql)
    sql <- paste("SELECT COUNT (COHORT_DEFINITION_ID) AS PID FROM @results_database_schema.studyCohort WHERE COHORT_DEFINITION_ID = 2",sep="")
    sql <- renderSql(sql, results_database_schema = resultsDatabaseSchema)$sql
    sql <- translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    numPidTwo <- as.numeric(as.character(querySql(connection, sql)))
    remove(sql)
    if((numPidOne < 100) || (numPidTwo < 100)){
      cat(file = paste(results_path,"notPerformedFor.txt",sep=""), paste("Analysis can not be done for - ",i," - ",drugCombName," as one of them has less than 100 patients",sep=""), append = TRUE, "\n")
    }else
    {
      outCome <- c(3,4,5,6)
      outComeName <- c("hbA1c","MI","KD","eyes")
      for(j in 1:length(outCome)){
        cid2Rm <- read.csv(system.file(paste("csv/","conceptIdToRemove.csv",sep=""), package = "TreatmentPath"), stringsAsFactors = FALSE, header = FALSE)
        cid2Rm <- as.numeric(as.character(unique(cid2Rm$V1)))
        drugEfficacyAnalysis(drugCombName,numThread,outCome[j],cid2Rm,outComeName[j])
      }
    }
  }
}
