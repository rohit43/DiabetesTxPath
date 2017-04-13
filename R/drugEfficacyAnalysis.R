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
# @author Stanford University Center for Biomedical Informatics - Nigam Shah Lab
# @author Rohit Vashisht
#
#' @title
#' drugEfficacyAnalysis
#'
#' @author
#' Rohit Vashisht
#'
#' @details
#' This function can be used to perform the drug efficacy analysis. Very briefly, this function a)
#' obtain treatment and comparator cohorts from target schema b) perform patient-level propensity
#' score matching c) perform cox-proportional hazard modeling of the given outcome. Please note this
#' function can only performe analysis if there are >= 100 patients in both treatment and comparator
#' cohorts. Right now I've hard coded these values. May be in the future we'll see how to generalize
#' this.
#'
#' @param drugCombName   Name of the drug combination for which the analysis need to be performed.
#' @param numThread      Number of threads to be used for parallel processing. Give it as much as you
#'                       can :)
#' @param outCome        The outcome Id for which the analysis need to be performed.
#' @param cid2Rm         List of concept IDs to be removed before patient-level matching.
#' @param outComeName    Name of the outcome measure. This will be used for naming the files.
#'
#' @export
drugEfficacyAnalysis <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 resultsDatabaseSchema,
                                 cid2Rm,
                                 outCome,
                                 cdmVersion,
                                 treatment,
                                 comparator) {
  exposureTable <- "ohdsi_t2dpathway"
  outcomeTable <- "ohdsi_t2dpathway"
  covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                               useCovariateDemographicsAge = TRUE,
                                               useCovariateDemographicsGender = TRUE,
                                               useCovariateDemographicsRace = FALSE,
                                               useCovariateDemographicsEthnicity = FALSE,
                                               useCovariateConditionOccurrence = TRUE,
                                               useCovariateConditionOccurrence365d = TRUE,
                                               useCovariateDrugExposure = TRUE,
                                               useCovariateDrugExposure365d = TRUE,
                                               useCovariateProcedureOccurrence = TRUE,
                                               useCovariateProcedureOccurrence365d = TRUE,
                                               excludedCovariateConceptIds = cid2Rm,
                                               deleteCovariatesSmallCount = 0)
  cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            oracleTempSchema = resultsDatabaseSchema,
                                            targetId = 1,
                                            comparatorId = 2,
                                            outcomeIds = outCome,
                                            studyStartDate = "",
                                            studyEndDate = "",
                                            exposureDatabaseSchema = resultsDatabaseSchema,
                                            exposureTable = exposureTable,
                                            outcomeDatabaseSchema = resultsDatabaseSchema,
                                            outcomeTable = outcomeTable,
                                            cdmVersion = cdmVersion,
                                            excludeDrugsFromCovariates = FALSE,
                                            firstExposureOnly = TRUE,
                                            removeDuplicateSubjects = TRUE,
                                            washoutPeriod = 0,
                                            covariateSettings = covariateSettings)
  studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                    outcomeId = outCome,
                                    firstExposureOnly = FALSE,
                                    washoutPeriod = 0,
                                    removeDuplicateSubjects = FALSE,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 0,
                                    riskWindowStart = 0,
                                    addExposureDaysToStart = FALSE,
                                    riskWindowEnd = 0,
                                    addExposureDaysToEnd = TRUE)

  # For some of the treatment and comparator cohort combinations I noticed that there are hardly any
  # patients. Therefore putting a constaint here on the study requiring it proceed if an only if there
  # are at-least 100 patients in both treatment and comparator cohort after creating study population.
  tPid <- as.data.frame(table(studyPop$treatment))
  colnames(tPid) <- c("treatment", "pid")
  if ((tPid$pid[1] < 150) || tPid$pid[2] < 150) {
    results <- list()
  } else {
    psScore <- createPs(cohortMethodData = cohortMethodData,
                        population = studyPop,
                        prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
                        control = createControl(cvType = "auto",
                                                startingVariance = 0.01,
                                                noiseLevel = "quiet",
                                                tolerance = 2e-07,
                                                cvRepetitions = 10,
                                                threads = numThread))
    # Getting AUC of propensity score with CI
    psAUC <- computePsAuc(psScore,
                          confidenceIntervals = TRUE)  #This is a data frame, see how to integrate it later. Important
    impFeatures <- getPsModel(psScore, cohortMethodData)
    psScoreBeforeMatching <- plotPs(psScore,
                                    scale = "preference",
                                    treatmentLabel = treatment,
                                    comparatorLabel = comparator)
    # Perfome Matching on PS
    matchedPop <- matchOnPs(psScore, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)
    psScoreAfterMatching <- plotPs(matchedPop,
                                   psScore,
                                   treatmentLabel = treatment,
                                   comparatorLabel = comparator)
    finalAttDiag <- drawAttritionDiagram(matchedPop,
                                         treatmentLabel = treatment,
                                         comparatorLabel = comparator)
    balance <- computeCovariateBalance(matchedPop, cohortMethodData)
    covariateBalance <- plotCovariateBalanceScatterPlot(balance)
    # Cox Proportional hazard model without covariate
    modelFit <- fitOutcomeModel(population = matchedPop,
                                cohortMethodData = cohortMethodData,
                                modelType = "cox",
                                stratified = TRUE,
                                useCovariates = FALSE)
    kmPlotWithoutCI <- plotKaplanMeier(matchedPop,
                                       includeZero = FALSE,
                                       confidenceIntervals = FALSE,
                                       treatmentLabel = treatment,
                                       comparatorLabel = comparator)
    kmPlotWithCI <- plotKaplanMeier(matchedPop,
                                    includeZero = FALSE,
                                    confidenceIntervals = TRUE,
                                    treatmentLabel = treatment,
                                    comparatorLabel = comparator)
    results <- list(psAUC,
                    impFeatures,
                    psScoreBeforeMatching,
                    psScoreAfterMatching,
                    finalAttDiag,
                    covariateBalance,
                    modelFit,
                    kmPlotWithoutCI,
                    kmPlotWithCI)
  }
  return(results)
}
