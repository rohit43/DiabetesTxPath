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
#' @title getTxPath
#'
#' @author Rohit Vashisht
#'
#' @details This function can be used to construct treatment pathways for T2D
#'
#' @param connectionDetails The connection details of the database.
#' @param cdmDatabaseSchema The name of cdm database schema.
#' @param resultsDatabaseSchema The name of results database schema.
#' @param cdmVersion The name of cdm version, should be 5
#'
#' @export
getTxPath <- function(connectionDetails = connectionDetails,
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      resultsDatabaseSchema = resultsDatabaseSchema,
                      cdmVersion = cdmVersion,
                      results_path = results_path){
  #Get all the Sqls
  sqlFiles <- list.files(system.file(paste("sql/txSql/",sep=""), package = "DiabetesTxPath"))
  targetDatabaseSchema <- resultsDatabaseSchema
  targetCohortTable <- "ohdsi_t2dpathway"
  conn <- DatabaseConnector::connect(connectionDetails)
  txPathFinal <- data.frame()
  for(i in 1:length(sqlFiles)){
    txPathName <- sqlFiles[i]
    txPathName <- gsub(".sql","",txPathName)
    sql <- "IF OBJECT_ID('@results_database_schema.@targetCohortTable', 'U') IS NOT NULL\n  DROP TABLE @results_database_schema.@target_cohort_table;\n    CREATE TABLE @results_database_schema.@target_cohort_table (cohort_definition_id INT, subject_id BIGINT, cohort_start_date DATE, cohort_end_date DATE);"
    sql <- SqlRender::renderSql(sql,
                              results_database_schema = resultsDatabaseSchema,
                              target_cohort_table = targetCohortTable)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
    # Constructing treatment cohort - Treatment Cohort Id will always be 1
    sql <- readSql(system.file(paste("sql/txSql/", sqlFiles[i], sep = ""),
                             package = "DiabetesTxPath"))

    sql <- SqlRender::renderSql(sql,
                              cdm_database_schema = cdmDatabaseSchema,
                              target_database_schema = targetDatabaseSchema,
                              target_cohort_table = targetCohortTable,
                              target_cohort_id = 1)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
    sql <- paste("SELECT COUNT (COHORT_DEFINITION_ID) AS PID FROM @results_database_schema.ohdsi_t2dpathway WHERE COHORT_DEFINITION_ID = 1",sep="")
    sql <- SqlRender::renderSql(sql,results_database_schema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    numPidOne <- as.numeric(as.character((querySql(conn, sql))))
    txPathPidRaw <- as.data.frame(cbind(txPathName, numPidOne))
    colnames(txPathPidRaw) <- c("txPath","numPid")
    txPathFinal <- rbind(txPathFinal,txPathPidRaw)
    remove(sql,txPathName,txPathPidRaw)
    print(paste("Done computing for txPath ",i, " off the total ", length(sqlFiles)," txPaths ",sep=""))
  }
  write.csv(txPathFinal, paste(results_path,"/","txPathPid.csv",sep=""))
}
