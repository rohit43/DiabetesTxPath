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
#' @title drugEfficacyAnalysis
#'
#' @author Rohit Vashisht
#'
#' @details This function can be used to perform the drug efficacy analysis.
#' Very briefly, this function a) obtain treatment and comparator cohorts from
#' target schema b) perform patient-level propensity score matching c) perform
#' cox-proportional hazard modeling of the given outcome. Please note this function
#' can only performe analysis if there are >= 100 patients in both treatment and
#' comparator cohorts. Right now I've hard coded these values. May be in the
#' future we'll see how to generalize this.
#'
#' @param drugCombName Name of the drug combination for which the analysis need to be
#' performed.
#' @param numThread Number of threads to be used for parallel processing. Give it as much
#' as you can :)
#' @param outCome The outcome Id for which the analysis need to be performed.
#' @param cid2Rm List of concept IDs to be removed before patient-level matching.
#' @param outComeName Name of the outcome measure. This will be used for naming the files.
#'
#' @export
drugEfficacyAnalysis <- function(drugCombName,numThread,outCome,cid2Rm,outComeName){
  exposureTable <- "studyCohort"
  outcomeTable <- "studyCohort"
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
                                    firstExposureOnly = FALSE, #it was false
                                    washoutPeriod = 0,
                                    removeDuplicateSubjects = FALSE, #it was false
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 0,
                                    riskWindowStart = 0,
                                    addExposureDaysToStart = FALSE,
                                    riskWindowEnd = 0,
                                    addExposureDaysToEnd = TRUE)
  tPid <- as.data.frame(table(studyPop$treatment))
  colnames(tPid) <- c("treatment","pid")
  if((tPid$pid[1] < 100) || tPid$pid[2] < 100){
    cat(file = paste(results_path,"NoPropensityScore.txt",sep=""), paste(drugCombName, " has fewer than 100 patients. Can not fit PS, therefore can not compute further.",sep=""), append = TRUE, "\n")
  }else
  {
    p1 <- drawAttritionDiagram(studyPop, treatmentLabel = "Target", comparatorLabel = "Comparator")
    psScore <- createPs(cohortMethodData = cohortMethodData,
                        population = studyPop,
                        prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
                        control = createControl(cvType = "auto",
                                                startingVariance = 0.01,
                                                noiseLevel = "quiet",
                                                tolerance  = 2e-07,
                                                cvRepetitions = 10,
                                                threads = numThread))
    impFeatures <- getPsModel(psScore,cohortMethodData)
    write.csv(impFeatures, file = paste(results_path,"ImpFeatures-",drugCombName,"_",outComeName,".csv",sep=""))
    p2 <- plotPs(psScore, scale = "preference")
    matchedPop<- matchOnPs(psScore, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)
    p3 <- plotPs(matchedPop, psScore)
    p4 <- drawAttritionDiagram(matchedPop)
    balance <- computeCovariateBalance(matchedPop, cohortMethodData)
    p5 <- plotCovariateBalanceScatterPlot(balance)
    #Cox Proportional hazard model without covariate
    om <- fitOutcomeModel(population = matchedPop,
                          cohortMethodData = cohortMethodData,
                          modelType = "cox",
                          stratified = TRUE,
                          useCovariates = FALSE)
    #modelVal <- rbind(modelVal,as.data.frame(exp(omWithCovariate$outcomeModelTreatmentEstimate)))
    save(om, file = paste(results_path,"CoxModel-",drugCombName,"_",outComeName,".RData",sep=""))
    dat <- as.data.frame(exp(om$outcomeModelTreatmentEstimate))
    dat <- signif(dat,8)
    rownames(dat) <- c("treatment")
    p6 <- plotKaplanMeier(studyPop, includeZero = FALSE, confidenceIntervals = FALSE)
    p7 <- plotKaplanMeier(matchedPop, includeZero = FALSE, confidenceIntervals = FALSE)
    p8 <- plotKaplanMeier(studyPop, includeZero = FALSE, confidenceIntervals = TRUE)
    p9 <- plotKaplanMeier(matchedPop, includeZero = FALSE, confidenceIntervals = TRUE)
    pdf(file=paste(results_path,drugCombName,"_",outComeName,".pdf",sep=""))
    plot(p1)
    plot(p2)
    plot(p3)
    plot(p4)
    plot(p5)
    plot(p6)
    plot(p7)
    plot(p8)
    plot(p9)
    plot.new()
    grid.table(dat)
    dev.off()
    remove(studyPop,psScore,impFeatures,matchedPop,balance,om,p1,p2,p3,p4,p5,p6,p7,p8,p9,dat)
  }
  remove(tPid)
}
