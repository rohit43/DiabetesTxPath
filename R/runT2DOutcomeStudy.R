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
#' @title
#' runStudy
#'
#' @author
#' Rohit Vashisht
#'
#' @details
#' This function can be used to execute the T2D study. The function create and stores the study outcome in the
#' provided results folder.
#'
#' @param connectionDetails       The connection details of the database.
#' @param cdmDatabaseSchema       The name of cdm database schema.
#' @param resultsDatabaseSchema   The name of results database schema.
#' @param cdmVersion              The name of cdm version, should be 5
#' @param results_path            The results path
#' @param maxCores                Number of cores.
runT2DOutcomeStudy <- function(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     resultsDatabaseSchema = resultsDatabaseSchema,
                     cdmVersion = cdmVersion,
                     results_path = results_path,
                     maxCores = maxCores) {
  #Building Exposure and Outcome Cohorts
  createExposureCohorts(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        resultsDatabaseSchema = resultsDatabaseSchema)
  buildOutComeCohort(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     resultsDatabaseSchema = resultsDatabaseSchema)
  #----- Need to work on this -----
  #Computing the number of patients in each of the exposure cohort. The exposure
  #cohorts with a minimum number of 250 patients will be considered for the analysis.
  conn <- DatabaseConnector::connect(connectionDetails)
  sql <- paste("SELECT cohort_definition_id as outComeId, COUNT(DISTINCT SUBJECT_ID) AS PID FROM @results_database_schema.ohdsi_t2dpathway GROUP BY COHORT_DEFINITION_ID",
               sep = "")
  sql <- SqlRender::renderSql(sql, results_database_schema = resultsDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  numPid <- (querySql(conn, sql))
  #---------------------------------

  #Performing the analysis -----
  #Settings
  covariateSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender          = TRUE,
    useDemographicsAge             = TRUE,
    useDemographicsAgeGroup        = TRUE,
    useConditionOccurrenceLongTerm = TRUE,
    useDrugExposureLongTerm        = TRUE,
    useProcedureOccurrenceLongTerm = TRUE,
    excludedCovariateConceptIds    = c(1529331,1510202,1503297,43013884,40239216,40166035,1580747,19122137,44816332,45774435,
                                       1583722,40170911,1502826,1516766,44785829,43526465,45774751,1597756,19059796,1560171,
                                       1559684,1594973,1502809,1502855,1547504,1525215,253182,86009,51428,274783,139825,5856,
                                       352385,314684,400008,314683),
    addDescendantsToExclude        = TRUE
  )
  getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
    studyStartDate             = "",
    studyEndDate               = "",
    excludeDrugsFromCovariates = FALSE,
    firstExposureOnly          = TRUE,
    removeDuplicateSubjects    = TRUE,
    restrictToCommonPeriod     = FALSE,
    washoutPeriod              = 0,
    maxCohortSize              = 0,
    covariateSettings          = covariateSettings
  )
  createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
    firstExposureOnly              = FALSE,
    restrictToCommonPeriod         = FALSE,
    washoutPeriod                  = 0,
    removeDuplicateSubjects        = FALSE,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback           = 99999,
    minDaysAtRisk                  = 0,
    riskWindowStart                = 0,
    addExposureDaysToStart         = FALSE,
    riskWindowEnd                  = 0,
    addExposureDaysToEnd           = TRUE
  )
  createPsArgs <- CohortMethod::createCreatePsArgs(
    excludeCovariateIds = c(),
    includeCovariateIds = c(),
    maxCohortSizeForFitting = 250000,
    errorOnHighCorrelation = TRUE,
    stopOnError            = TRUE,
    prior                  = Cyclops::createPrior(priorType          = "laplace",
                                                  useCrossValidation = TRUE),
    control                = Cyclops::createControl(cvType           = "auto",
                                                    startingVariance = 0.01,
                                                    tolerance        = 2e-07,
                                                    cvRepetitions    = 10,
                                                    noiseLevel       = "quiet")
  )
  matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
    caliper      = 0.25,
    caliperScale = "standardized logit",
    maxRatio     = 1
  )
  fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
    modelType           = "cox",
    stratified          = TRUE,
    useCovariates       = FALSE,
    excludeCovariateIds = c(),
    includeCovariateIds = c(),
    prior               = Cyclops::createPrior(priorType          = "laplace",
                                               useCrossValidation = TRUE),
    control             = Cyclops::createControl(cvType           = "auto",
                                                 startingVariance = 0.01,
                                                 tolerance        = 2e-07,
                                                 cvRepetitions    = 10,
                                                 noiseLevel       = "quiet")
  )
  cmAnalysis1 <- CohortMethod::createCmAnalysis(
    analysisId                    = 2,
    description                   = "T2D Tx Path Analysis",
    targetType                    = NULL,
    comparatorType                = NULL,
    getDbCohortMethodDataArgs     = getDbCmDataArgs,
    createStudyPopArgs            = createStudyPopArgs,
    createPs                      = TRUE,
    createPsArgs                  = createPsArgs,
    trimByPs                      = FALSE,
    trimByPsArgs                  = NULL,
    trimByPsToEquipoise           = FALSE,
    trimByPsToEquipoiseArgs       = NULL,
    matchOnPs                     = TRUE,
    matchOnPsArgs                 = matchOnPsArgs,
    matchOnPsAndCovariates        = FALSE,
    matchOnPsAndCovariatesArgs    = NULL,
    stratifyByPs                  = FALSE,
    stratifyByPsArgs              = NULL,
    stratifyByPsAndCovariates     = FALSE,
    stratifyByPsAndCovariatesArgs = NULL,
    computeCovariateBalance       = TRUE,
    fitOutcomeModel               = TRUE,
    fitOutcomeModelArgs           = fitOutcomeModelArgs
  )
  cmAnalysisList <- list(cmAnalysis1)
  #OutCome
  allOutComeId <- c(4,5,6,7,8)
  #comparision
  cohortsToCreate <- cbind(c(1:3), read.csv(system.file("settings/CohortsToCreate.csv", package = "DiabetesTxPath"))[1:3, ])
  comparisons <- base::t(utils::combn(x = cohortsToCreate[, 1], m = 2))
  drugComparatorOutcomesList <- list()
  for (i in 1:nrow(comparisons))
  {
    drugComparatorOutcomesList[[i]] <- CohortMethod::createDrugComparatorOutcomes(
    targetId                    = comparisons[i, 1],
    comparatorId                = comparisons[i, 2],
    excludedCovariateConceptIds = c(),
    includedCovariateConceptIds = c(),
    outcomeIds                  = allOutComeId
    )
  }
  #Performing the analysis ...
  #All the intermediate results from the cohortMethod will be saved in the
  #deleteMeBeforeSharing folder. Please make sure to delete this folder
  #before sharing the results.
  outputFolder <- paste(results_path,"deleteMeBeforeSharing/",sep="")
  result <- CohortMethod::runCmAnalyses(
    connectionDetails            = connectionDetails,
    cdmDatabaseSchema            = cdmDatabaseSchema,
    exposureDatabaseSchema       = resultsDatabaseSchema ,
    exposureTable                = "ohdsi_t2dpathway",
    outcomeDatabaseSchema        = resultsDatabaseSchema,
    outcomeTable                 = "ohdsi_t2dpathway",
    cdmVersion                   = 5,
    outputFolder                 = outputFolder,
    cmAnalysisList               = cmAnalysisList,
    drugComparatorOutcomesList   = drugComparatorOutcomesList,
    getDbCohortMethodDataThreads = 1,
    createPsThreads              = 1,
    psCvThreads                  = min(16, maxCores),
    computeCovarBalThreads       = min(3, maxCores),
    createStudyPopThreads        = min(3, maxCores),
    trimMatchStratifyThreads     = min(10, maxCores),
    fitOutcomeModelThreads       = max(1, round(maxCores/4)),
    outcomeCvThreads             = min(4, maxCores),
    refitPsForEveryOutcome       = FALSE,
    outcomeIdsOfInterest         = NULL
  )
  analysisSummary <- CohortMethod::summarizeAnalyses(result)
  write.csv(x = analysisSummary, file = file.path(results_path, "T2DStudyOutcome.csv"), row.names = FALSE)
}
