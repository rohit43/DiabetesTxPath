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
# @author Stanford University Center for Biomedical Informatics - Nigam Shah Lab
# @author Rohit Vashisht
#
#' @title Get Cohort
#'
#' @author Rohit Vashisht
#'
#' @details The function get cohort can be used to generate the required cohort for
#' a given drug combination. You can use this function to generate your own cohort
#' based on given SQL query or use one of the SQL query provided. For DiabetesTxPathway
#' it is highly recomended to use the SQLs provided in this package.
#'
#' @param connectionDetails The details of database schema.
#' @param cdmDatabaseSchema The details of CDM schema.
#' @param resultsDatabaseSchema The details of results schema.
#' @param sqlFileName Name of SQL file that will used to construct the cohort.
#' @param targetDatabaseSchema Name of target database schema.
#' @param targetCohortTable Name of target cohort table.
#' @param idOne Numeric value = 1 represents one of the drug combination.
#' This is representative of Treatment Cohort.
#' @param idTwo Numeric value = 2 representing one of the drug combination.
#' This is representative of the Comparator Cohort.
#' @param idThree Numeric value = 3 representing one of the outcome measure.
#' In case of DiabetesTxPathway this represents HbA1c
#' @param idFour Numeric value = 4 representing one of the outcome measure.
#' In case of DiabetesTxPathway this represents Myocardial Infraction
#'
#' @export
getCohort <- function(connectionDetails, cdmDatabaseSchema, resultsDatabaseSchema, sqlFileName, targetDatabaseSchema, targetCohortTable, idOne, idTwo, idThree, idFour, idFive, idSix){
  connection <- connect(connectionDetails)
  sql <- readSql(system.file(paste("sql/",sqlFileName,sep=""), package="TreatmentPath"))
  sql <- renderSql(sql, cdm_database_schema = cdmDatabaseSchema,
                   results_database_schema = resultsDatabaseSchema,
                   target_database_schema = targetDatabaseSchema,
                   target_cohort_table = targetCohortTable,
                   target_cohort_id_one = idOne,
                   target_cohort_id_two = idTwo,
                   target_cohort_id_three = idThree,
                   target_cohort_id_four = idFour,
                   target_cohort_id_five = idFive,
                   target_cohort_id_six = idSix)$sql
  sql <- translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  executeSql(connection, sql)
  remove(sql)
}
