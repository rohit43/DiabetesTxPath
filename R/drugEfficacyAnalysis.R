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
#' function can only performe analysis if there are > 250 patients in both treatment and comparator
#' cohorts. Right now I've hard coded these values. May be in the future we'll see how to generalize
#' this.
#'
#' @param connectionDetails The connection details.
#' @param cdmDatabaseSchema Name of CDM database schema.
#' @param resultsDatabaseSchema Name of results database schema.
#' @param cid2Rm Concept Ids to remove before matching.
#' @param outCome Outcome Id of interest, for which the analysis need to be performed.
#' @param cdmVersion Version of cdm, should be 5 only.
#' @param treatment Name of treatment drug.
#' @param comparator Name of comparator drug.
#' @param numThread Number of threads.
#'
#' @export
drugEfficacyAnalysis <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 resultsDatabaseSchema,
                                 cid2Rm,
                                 outCome,
                                 cdmVersion,
                                 treatment,
                                 comparator,
                                 numThread) {
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
  tPid <- as.data.frame(table(studyPop$treatment))
  colnames(tPid) <- c("treatment", "pid")
  if ((tPid$pid[1] < 250) || tPid$pid[2] < 250) {
    results <- list()
  }else
  {
    psScore <- createPs(cohortMethodData = cohortMethodData,
                        population = studyPop,
                        prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
                        control = createControl(cvType = "auto",
                                                startingVariance = 0.01,
                                                noiseLevel = "quiet",
                                                tolerance = 2e-07,
                                                cvRepetitions = 10,
                                                threads = numThread))
    allPsScore <- unique(psScore$propensityScore)
    if(length(allPsScore)==1){
      results <- list()
    }else{
      psAUC <- computePsAuc(psScore,
                            confidenceIntervals = TRUE)
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
      if (!is.null(cohortMethodData$metaData$deletedCovariateIds)) {
        idx <- is.na(ffbase::ffmatch(cohortMethodData$covariateRef$covariateId, ff::as.ff(cohortMethodData$metaData$deletedCovariateIds)))
        removedCovars <- ff::as.ram(cohortMethodData$covariateRef[ffbase::ffwhich(idx, idx == FALSE), ])
        #Age before matching.
        ageBeforeMatching <- balance[grep("Age group:", balance$covariateName), ]
        ageBeforeMatching <- data.frame(group = ageBeforeMatching$covariateName,
                                        countTreated = ageBeforeMatching$beforeMatchingSumTreated,
                                        countComparator = ageBeforeMatching$beforeMatchingSumComparator,
                                        fractionTreated = ageBeforeMatching$beforeMatchingMeanTreated,
                                        fractionComparator = ageBeforeMatching$beforeMatchingMeanComparator)
        removedAgeGroup <- removedCovars[grep("Age group:", removedCovars$covariateName), ]
        if (nrow(removedAgeGroup) == 1) {
          totalTreated <- ageBeforeMatching$countTreated[1] / ageBeforeMatching$fractionTreated[1]
          missingFractionTreated <- 1 - sum(ageBeforeMatching$fractionTreated)
          missingFractionComparator <- 1 - sum(ageBeforeMatching$fractionComparator)
          removedAgeGroup <- data.frame(group = removedAgeGroup$covariateName,
                                        countTreated = round(missingFractionTreated * totalTreated),
                                        countComparator = round(missingFractionComparator * totalTreated),
                                        fractionTreated = missingFractionTreated,
                                        fractionComparator = missingFractionComparator)
          ageBeforeMatching <- rbind(ageBeforeMatching, removedAgeGroup)
        }
        ageBeforeMatching$start <- gsub("Age group: ", "", gsub("-.*$", "", ageBeforeMatching$group))
        ageBeforeMatching$start <- as.integer(ageBeforeMatching$start)
        ageBeforeMatching <- ageBeforeMatching[order(ageBeforeMatching$start), ]
        ageBeforeMatching$start <- NULL
        #Age after matching ...
        ageAfterMatching <- balance[grep("Age group:", balance$covariateName), ]
        ageAfterMatching <- data.frame(group = ageAfterMatching$covariateName,
                                       countTreated = ageAfterMatching$afterMatchingSumTreated,
                                       countComparator = ageAfterMatching$afterMatchingSumComparator,
                                       fractionTreated = ageAfterMatching$afterMatchingMeanTreated,
                                       fractionComparator = ageAfterMatching$afterMatchingMeanComparator)
        # Add removed age group (if any):
        removedAgeGroup <- removedCovars[grep("Age group:", removedCovars$covariateName), ]
        if (nrow(removedAgeGroup) == 1) {
          totalTreated <- ageAfterMatching$countTreated[1] / ageAfterMatching$fractionTreated[1]
          missingFractionTreated <- 1 - sum(ageAfterMatching$fractionTreated)
          missingFractionComparator <- 1 - sum(ageAfterMatching$fractionComparator)
          removedAgeGroup <- data.frame(group = removedAgeGroup$covariateName,
                                        countTreated = round(missingFractionTreated * totalTreated),
                                        countComparator = round(missingFractionComparator * totalTreated),
                                        fractionTreated = missingFractionTreated,
                                        fractionComparator = missingFractionComparator)
          ageAfterMatching <- rbind(ageAfterMatching, removedAgeGroup)
        }
        ageAfterMatching$start <- gsub("Age group: ", "", gsub("-.*$", "", ageAfterMatching$group))
        ageAfterMatching$start <- as.integer(ageAfterMatching$start)
        ageAfterMatching <- ageAfterMatching[order(ageAfterMatching$start), ]
        ageAfterMatching$start <- NULL
        ## Gender before matching
        genderBeforeMatching <- balance[grep("Gender", balance$covariateName), ]
        x <- grep("during 365d", genderBeforeMatching$covariateName)
        if(length(x)==0){
          genderBeforeMatching <- genderBeforeMatching
        }else
        {
          genderBeforeMatching <- genderBeforeMatching[-x,]
        }
        remove(x)
        genderBeforeMatching <- data.frame(group = genderBeforeMatching$covariateName,
                                           countTreated = genderBeforeMatching$beforeMatchingSumTreated,
                                           countComparator = genderBeforeMatching$beforeMatchingSumComparator,
                                           fractionTreated = genderBeforeMatching$beforeMatchingMeanTreated,
                                           fractionComparator = genderBeforeMatching$beforeMatchingMeanComparator)
        removedGender <- removedCovars[grep("Gender", removedCovars$covariateName), ]
        if (nrow(removedGender) == 1) {
          totalTreated <- genderBeforeMatching$countTreated[1] / genderBeforeMatching$fractionTreated[1]
          missingFractionTreated <- 1 - sum(genderBeforeMatching$fractionTreated)
          missingFractionComparator <- 1 - sum(genderBeforeMatching$fractionComparator)
          removedGender <- data.frame(group = removedGender$covariateName,
                                      countTreated = round(missingFractionTreated * totalTreated),
                                      countComparator = round(missingFractionComparator * totalTreated),
                                      fractionTreated = missingFractionTreated,
                                      fractionComparator = missingFractionComparator)
          genderBeforeMatching <- rbind(genderBeforeMatching, removedGender)
        }
        genderBeforeMatching$group <- gsub("Gender = ", "", genderBeforeMatching$group)
        #Gender After Matching
        genderAfterMatching <- balance[grep("Gender", balance$covariateName), ]
        x <- grep("during 365d", genderAfterMatching$covariateName)
        if(length(x)==0){
          genderAfterMatching <- genderAfterMatching
        }else
        {
          genderAfterMatching <- genderAfterMatching[-x,]
        }
        remove(x)
        genderAfterMatching <- data.frame(group = genderAfterMatching$covariateName,
                                          countTreated = genderAfterMatching$afterMatchingSumTreated,
                                          countComparator = genderAfterMatching$afterMatchingSumComparator,
                                          fractionTreated = genderAfterMatching$afterMatchingMeanTreated,
                                          fractionComparator = genderAfterMatching$afterMatchingMeanComparator)
        # Add removed gender (if any):
        removedGender <- removedCovars[grep("Gender", removedCovars$covariateName), ]
        if (nrow(removedGender) == 1) {
          totalTreated <- genderAfterMatching$countTreated[1] / genderAfterMatching$fractionTreated[1]
          missingFractionTreated <- 1 - sum(genderAfterMatching$fractionTreated)
          missingFractionComparator <- 1 - sum(genderAfterMatching$fractionComparator)
          removedGender <- data.frame(group = removedGender$covariateName,
                                      countTreated = round(missingFractionTreated * totalTreated),
                                      countComparator = round(missingFractionComparator * totalTreated),
                                      fractionTreated = missingFractionTreated,
                                      fractionComparator = missingFractionComparator)
          genderAfterMatching <- rbind(genderAfterMatching, removedGender)
        }
        genderAfterMatching$group <- gsub("Gender = ", "", genderAfterMatching$group)
      }else
      {
        x <- grep("Age group:", balance$covariateName)
        if(length(x)!=0){
          #Before Matching
          ageBeforeMatching <- balance[grep("Age group:", balance$covariateName), ]
          ageBeforeMatching <- data.frame(group = ageBeforeMatching$covariateName,
                                          countTreated = ageBeforeMatching$beforeMatchingSumTreated,
                                          countComparator = ageBeforeMatching$beforeMatchingSumComparator,
                                          fractionTreated = ageBeforeMatching$beforeMatchingMeanTreated,
                                          fractionComparator = ageBeforeMatching$beforeMatchingMeanComparator)
          ageBeforeMatching$start <- gsub("Age group: ", "", gsub("-.*$", "", ageBeforeMatching$group))
          ageBeforeMatching$start <- as.integer(ageBeforeMatching$start)
          ageBeforeMatching <- ageBeforeMatching[order(ageBeforeMatching$start), ]
          ageBeforeMatching$start <- NULL
          #after matching
          ageAfterMatching <- balance[grep("Age group:", balance$covariateName), ]
          ageAfterMatching <- data.frame(group = ageAfterMatching$covariateName,
                                         countTreated = ageAfterMatching$afterMatchingSumTreated,
                                         countComparator = ageAfterMatching$afterMatchingSumComparator,
                                         fractionTreated = ageAfterMatching$afterMatchingMeanTreated,
                                         fractionComparator = ageAfterMatching$afterMatchingMeanComparator)
          ageAfterMatching$start <- gsub("Age group: ", "", gsub("-.*$", "", ageAfterMatching$group))
          ageAfterMatching$start <- as.integer(ageAfterMatching$start)
          ageAfterMatching <- ageAfterMatching[order(ageAfterMatching$start), ]
          ageAfterMatching$start <- NULL
        }else
        {
          ageBeforeMatching <- data.frame(NA)
          ageAfterMatching <- data.frame(NA)
        }
        r <- grep("Gender", balance$covariateName)
        if(length(r)!=0){
          #Before Matching
          genderBeforeMatching <- balance[grep("Gender", balance$covariateName), ]
          x <- grep("during 365d", genderBeforeMatching$covariateName)
          if(length(x)==0){
            genderBeforeMatching <- genderBeforeMatching
          }else
          {
            genderBeforeMatching <- genderBeforeMatching[-x,]
          }
          remove(x)
          genderBeforeMatching <- data.frame(group = genderBeforeMatching$covariateName,
                                             countTreated = genderBeforeMatching$beforeMatchingSumTreated,
                                             countComparator = genderBeforeMatching$beforeMatchingSumComparator,
                                             fractionTreated = genderBeforeMatching$beforeMatchingMeanTreated,
                                             fractionComparator = genderBeforeMatching$beforeMatchingMeanComparator)
          genderBeforeMatching$group <- gsub("Gender = ", "", genderBeforeMatching$group)
          #Gender after matching
          genderAfterMatching <- balance[grep("Gender", balance$covariateName), ]
          x <- grep("during 365d", genderAfterMatching$covariateName)
          if(length(x)==0){
            genderAfterMatching <- genderAfterMatching
          }else
          {
            genderAfterMatching <- genderAfterMatching[-x,]
          }
          remove(x)
          genderAfterMatching <- data.frame(group = genderAfterMatching$covariateName,
                                            countTreated = genderAfterMatching$afterMatchingSumTreated,
                                            countComparator = genderAfterMatching$afterMatchingSumComparator,
                                            fractionTreated = genderAfterMatching$afterMatchingMeanTreated,
                                            fractionComparator = genderAfterMatching$afterMatchingMeanComparator)
          genderAfterMatching$group <- gsub("Gender = ", "", genderAfterMatching$group)
      }else
      {
        genderBeforeMatching <- data.frame(NA)
        genderAfterMatching <- data.frame(NA)
      }
      }

      #------ Getting mean HbA1c before matching for all the patients
      if(outCome==3){
        conn <- DatabaseConnector::connect(connectionDetails)
        studyPopHba1c <- subset(studyPop,outcomeCount>0)
        studyPopHba1cTreatment <- subset(studyPopHba1c, treatment==1)
        studyPopHba1cComparator <- subset(studyPopHba1c, treatment==0)
        HbA1cBefTx_treatment <- data.frame()
        HbA1cAftTx_treatment <- data.frame()
        for(i in 1:nrow(studyPopHba1cTreatment)){
          sqlOne <- paste("SELECT AVG(VALUE_AS_NUMBER) AS AVG FROM @cdmDatabaseSchema.MEASUREMENT WHERE PERSON_ID = ",studyPopHba1cTreatment$subjectId[i], " AND MEASUREMENT_CONCEPT_ID IN (3004410,3007263,3003309,3005673,40762352,40758583,3034639,4197971) AND MEASUREMENT_DATE < ","'",studyPopHba1cTreatment$cohortStartDate[i],"'",sep="")
          sqlOne <- SqlRender::renderSql(sqlOne,cdmDatabaseSchema = cdmDatabaseSchema)$sql
          sqlOne <- SqlRender::translateSql(sqlOne, targetDialect = connectionDetails$dbms)$sql
          sqlTwo <- paste("SELECT AVG(VALUE_AS_NUMBER) AS AVG FROM @cdmDatabaseSchema.MEASUREMENT WHERE PERSON_ID = ",studyPopHba1cTreatment$subjectId[i], " AND MEASUREMENT_CONCEPT_ID IN (3004410,3007263,3003309,3005673,40762352,40758583,3034639,4197971) AND MEASUREMENT_DATE > ","'",studyPopHba1cTreatment$cohortStartDate[i],"'",sep="")
          sqlTwo <- SqlRender::renderSql(sqlTwo,cdmDatabaseSchema = cdmDatabaseSchema)$sql
          sqlTwo <- SqlRender::translateSql(sqlTwo, targetDialect = connectionDetails$dbms)$sql
          avgHbA1cBefTx <- querySql(conn, sqlOne)
          avgHbA1cAftTx <- querySql(conn, sqlTwo)
          HbA1cBefTx_treatment <- rbind(HbA1cBefTx_treatment,avgHbA1cBefTx)
          HbA1cAftTx_treatment <- rbind(HbA1cAftTx_treatment,avgHbA1cAftTx)
          remove(avgHbA1cBefTx, avgHbA1cAftTx, sqlOne, sqlTwo)
        }
        #making sure we are not considering lab values that are miss reported.
        if(nrow(HbA1cBefTx_treatment)!=0&nrow(HbA1cBefTx_treatment)!=0){
          HbA1cAftTx_treatment <- subset(HbA1cAftTx_treatment,AVG<100)
          HbA1cBefTx_treatment <- subset(HbA1cBefTx_treatment,AVG<100)
        }else
        {
          HbA1cAftTx_treatment <- data.frame()
          HbA1cBefTx_treatment <- data.frame()
        }
        #Getting for the comparator
        HbA1cBefTx_comparator <- data.frame()
        HbA1cAftTx_comparator <- data.frame()
        for(i in 1:nrow(studyPopHba1cComparator)){
          sqlOne <- paste("SELECT AVG(VALUE_AS_NUMBER) AS AVG FROM @cdmDatabaseSchema.MEASUREMENT WHERE PERSON_ID = ",studyPopHba1cComparator$subjectId[i], " AND MEASUREMENT_CONCEPT_ID IN (3004410,3007263,3003309,3005673,40762352,40758583,3034639,4197971) AND MEASUREMENT_DATE < ","'",studyPopHba1cComparator$cohortStartDate[i],"'",sep="")
          sqlOne <- SqlRender::renderSql(sqlOne,cdmDatabaseSchema = cdmDatabaseSchema)$sql
          sqlOne <- SqlRender::translateSql(sqlOne, targetDialect = connectionDetails$dbms)$sql
          sqlTwo <- paste("SELECT AVG(VALUE_AS_NUMBER) AS AVG FROM @cdmDatabaseSchema.MEASUREMENT WHERE PERSON_ID = ",studyPopHba1cComparator$subjectId[i], " AND MEASUREMENT_CONCEPT_ID IN (3004410,3007263,3003309,3005673,40762352,40758583,3034639,4197971) AND MEASUREMENT_DATE > ","'",studyPopHba1cComparator$cohortStartDate[i],"'",sep="")
          sqlTwo <- SqlRender::renderSql(sqlTwo,cdmDatabaseSchema = cdmDatabaseSchema)$sql
          sqlTwo <- SqlRender::translateSql(sqlTwo, targetDialect = connectionDetails$dbms)$sql
          avgHbA1cBefTx <- querySql(conn, sqlOne)
          avgHbA1cAftTx <- querySql(conn, sqlTwo)
          HbA1cBefTx_comparator <- rbind(HbA1cBefTx_comparator,avgHbA1cBefTx)
          HbA1cAftTx_comparator <- rbind(HbA1cAftTx_comparator,avgHbA1cAftTx)
          remove(avgHbA1cBefTx, avgHbA1cAftTx, sqlOne, sqlTwo)
        }
        #making sure we are not considering lab values that are miss reported.
        if(nrow(HbA1cBefTx_comparator)!=0&nrow(HbA1cAftTx_comparator)!=0){
          HbA1cAftTx_comparator <- subset(HbA1cAftTx_comparator,AVG<100)
          HbA1cBefTx_comparator <- subset(HbA1cBefTx_comparator,AVG<100)
        }else
        {
          HbA1cAftTx_comparator <- data.frame()
          HbA1cBefTx_comparator <- data.frame()
        }

        if(nrow(HbA1cBefTx_treatment)!=0&nrow(HbA1cAftTx_treatment)!=0&nrow(HbA1cBefTx_comparator)!=0&nrow(HbA1cAftTx_comparator)!=0){
          meanTreatBefIndex <- as.numeric(as.character(mean(HbA1cBefTx_treatment$AVG)))
          sdTreatBefIndex <- as.numeric(as.character(sd(HbA1cBefTx_treatment$AVG)))
          meanTreatAftIndex <- as.numeric(as.character(mean(HbA1cAftTx_treatment$AVG)))
          sdTreatAftIndex <- as.numeric(as.character(sd(HbA1cAftTx_treatment$AVG)))
          meanCompBefIndex <- as.numeric(as.character(mean(HbA1cBefTx_comparator$AVG)))
          sdCompBefIndex <- as.numeric(as.character(sd(HbA1cBefTx_comparator$AVG)))
          meanCompAftIndex <- as.numeric(as.character(mean(HbA1cAftTx_comparator$AVG)))
          sdCompAftIndex <- as.numeric(as.character(sd(HbA1cAftTx_comparator$AVG)))
          unMatchedHbA1cMeanSd <- data.frame(cbind(meanTreatBefIndex,sdTreatBefIndex,meanTreatAftIndex,sdTreatAftIndex,meanCompBefIndex,sdCompBefIndex,meanCompAftIndex,sdCompAftIndex))
          colnames(unMatchedHbA1cMeanSd) <- c("meanTreatmentBefIndex","sdTreatmentBefIndex","meanTreatmentAftIndex","sdTreatmentAftIndex","meanComparatorBefIndex","sdComparatorBefIndex","meanComparatorAftIndex","sdComparatorAftIndex")
          remove(HbA1cAftTx_comparator,HbA1cAftTx_treatment,HbA1cBefTx_comparator,HbA1cBefTx_treatment,studyPopHba1c,studyPopHba1cComparator,studyPopHba1cTreatment)
        } else
        {
          unMatchedHbA1cMeanSd <- data.frame(NA,NA,NA,NA,NA,NA,NA,NA)
          colnames(unMatchedHbA1cMeanSd) <- c("meanTreatmentBefIndex","sdTreatmentBefIndex","meanTreatmentAftIndex","sdTreatmentAftIndex","meanComparatorBefIndex","sdComparatorBefIndex","meanComparatorAftIndex","sdComparatorAftIndex")
        }

        #Matched Cohort
        studyPopHba1c <- subset(matchedPop,outcomeCount>0)
        studyPopHba1cTreatment <- subset(studyPopHba1c, treatment==1)
        studyPopHba1cComparator <- subset(studyPopHba1c, treatment==0)
        HbA1cBefTx_treatment <- data.frame()
        HbA1cAftTx_treatment <- data.frame()
        for(i in 1:nrow(studyPopHba1cTreatment)){
          sqlOne <- paste("SELECT AVG(VALUE_AS_NUMBER) AS AVG FROM @cdmDatabaseSchema.MEASUREMENT WHERE PERSON_ID = ",studyPopHba1cTreatment$subjectId[i], " AND MEASUREMENT_CONCEPT_ID IN (3004410,3007263,3003309,3005673,40762352,40758583,3034639,4197971) AND MEASUREMENT_DATE < ","'",studyPopHba1cTreatment$cohortStartDate[i],"'",sep="")
          sqlOne <- SqlRender::renderSql(sqlOne,cdmDatabaseSchema = cdmDatabaseSchema)$sql
          sqlOne <- SqlRender::translateSql(sqlOne, targetDialect = connectionDetails$dbms)$sql
          sqlTwo <- paste("SELECT AVG(VALUE_AS_NUMBER) AS AVG FROM @cdmDatabaseSchema.MEASUREMENT WHERE PERSON_ID = ",studyPopHba1cTreatment$subjectId[i], " AND MEASUREMENT_CONCEPT_ID IN (3004410,3007263,3003309,3005673,40762352,40758583,3034639,4197971) AND MEASUREMENT_DATE > ","'",studyPopHba1cTreatment$cohortStartDate[i],"'",sep="")
          sqlTwo <- SqlRender::renderSql(sqlTwo,cdmDatabaseSchema = cdmDatabaseSchema)$sql
          sqlTwo <- SqlRender::translateSql(sqlTwo, targetDialect = connectionDetails$dbms)$sql
          avgHbA1cBefTx <- querySql(conn, sqlOne)
          avgHbA1cAftTx <- querySql(conn, sqlTwo)
          HbA1cBefTx_treatment <- rbind(HbA1cBefTx_treatment,avgHbA1cBefTx)
          HbA1cAftTx_treatment <- rbind(HbA1cAftTx_treatment,avgHbA1cAftTx)
          remove(avgHbA1cBefTx, avgHbA1cAftTx, sqlOne, sqlTwo)
        }
        if(nrow(HbA1cBefTx_treatment)!=0&nrow(HbA1cBefTx_treatment)!=0){
          HbA1cAftTx_treatment <- subset(HbA1cAftTx_treatment,AVG<100)
          HbA1cBefTx_treatment <- subset(HbA1cBefTx_treatment,AVG<100)
        }else
        {
          HbA1cAftTx_treatment <- data.frame()
          HbA1cBefTx_treatment <- data.frame()
        }
        #Getting for the comparator
        HbA1cBefTx_comparator <- data.frame()
        HbA1cAftTx_comparator <- data.frame()
        for(i in 1:nrow(studyPopHba1cComparator)){
          sqlOne <- paste("SELECT AVG(VALUE_AS_NUMBER) AS AVG FROM @cdmDatabaseSchema.MEASUREMENT WHERE PERSON_ID = ",studyPopHba1cComparator$subjectId[i], " AND MEASUREMENT_CONCEPT_ID IN (3004410,3007263,3003309,3005673,40762352,40758583,3034639,4197971) AND MEASUREMENT_DATE < ","'",studyPopHba1cComparator$cohortStartDate[i],"'",sep="")
          sqlOne <- SqlRender::renderSql(sqlOne,cdmDatabaseSchema = cdmDatabaseSchema)$sql
          sqlOne <- SqlRender::translateSql(sqlOne, targetDialect = connectionDetails$dbms)$sql
          sqlTwo <- paste("SELECT AVG(VALUE_AS_NUMBER) AS AVG FROM @cdmDatabaseSchema.MEASUREMENT WHERE PERSON_ID = ",studyPopHba1cComparator$subjectId[i], " AND MEASUREMENT_CONCEPT_ID IN (3004410,3007263,3003309,3005673,40762352,40758583,3034639,4197971) AND MEASUREMENT_DATE > ","'",studyPopHba1cComparator$cohortStartDate[i],"'",sep="")
          sqlTwo <- SqlRender::renderSql(sqlTwo,cdmDatabaseSchema = cdmDatabaseSchema)$sql
          sqlTwo <- SqlRender::translateSql(sqlTwo, targetDialect = connectionDetails$dbms)$sql
          avgHbA1cBefTx <- querySql(conn, sqlOne)
          avgHbA1cAftTx <- querySql(conn, sqlTwo)
          HbA1cBefTx_comparator <- rbind(HbA1cBefTx_comparator,avgHbA1cBefTx)
          HbA1cAftTx_comparator <- rbind(HbA1cAftTx_comparator,avgHbA1cAftTx)
          remove(avgHbA1cBefTx, avgHbA1cAftTx, sqlOne, sqlTwo)
        }
        #making sure we are not considering lab values that are miss reported.
        if(nrow(HbA1cBefTx_comparator)!=0&nrow(HbA1cAftTx_comparator)!=0){
          HbA1cAftTx_comparator <- subset(HbA1cAftTx_comparator,AVG<100)
          HbA1cBefTx_comparator <- subset(HbA1cBefTx_comparator,AVG<100)
        }else
        {
          HbA1cAftTx_comparator <- data.frame()
          HbA1cBefTx_comparator <- data.frame()
        }
        if(nrow(HbA1cBefTx_treatment)!=0&nrow(HbA1cAftTx_treatment)!=0&nrow(HbA1cBefTx_comparator)!=0&nrow(HbA1cAftTx_comparator)!=0){
          meanTreatBefIndex <- as.numeric(as.character(mean(HbA1cBefTx_treatment$AVG)))
          sdTreatBefIndex <- as.numeric(as.character(sd(HbA1cBefTx_treatment$AVG)))
          meanTreatAftIndex <- as.numeric(as.character(mean(HbA1cAftTx_treatment$AVG)))
          sdTreatAftIndex <- as.numeric(as.character(sd(HbA1cAftTx_treatment$AVG)))
          meanCompBefIndex <- as.numeric(as.character(mean(HbA1cBefTx_comparator$AVG)))
          sdCompBefIndex <- as.numeric(as.character(sd(HbA1cBefTx_comparator$AVG)))
          meanCompAftIndex <- as.numeric(as.character(mean(HbA1cAftTx_comparator$AVG)))
          sdCompAftIndex <- as.numeric(as.character(sd(HbA1cAftTx_comparator$AVG)))
          matchedHbA1cMeanSd <- data.frame(cbind(meanTreatBefIndex,sdTreatBefIndex,meanTreatAftIndex,sdTreatAftIndex,meanCompBefIndex,sdCompBefIndex,meanCompAftIndex,sdCompAftIndex))
          colnames(matchedHbA1cMeanSd) <- c("meanTreatmentBefIndex","sdTreatmentBefIndex","meanTreatmentAftIndex","sdTreatmentAftIndex","meanComparatorBefIndex","sdComparatorBefIndex","meanComparatorAftIndex","sdComparatorAftIndex")
          remove(HbA1cAftTx_comparator,HbA1cAftTx_treatment,HbA1cBefTx_comparator,HbA1cBefTx_treatment,studyPopHba1c,studyPopHba1cComparator,studyPopHba1cTreatment)
        } else
        {
          matchedHbA1cMeanSd <- data.frame(NA,NA,NA,NA,NA,NA,NA,NA)
          colnames(matchedHbA1cMeanSd) <- c("meanTreatmentBefIndex","sdTreatmentBefIndex","meanTreatmentAftIndex","sdTreatmentAftIndex","meanComparatorBefIndex","sdComparatorBefIndex","meanComparatorAftIndex","sdComparatorAftIndex")
        }
        hbA1cStat <- rbind(unMatchedHbA1cMeanSd,matchedHbA1cMeanSd)
        rownames(hbA1cStat) <- c("unMatched","matched")
        remove(unMatchedHbA1cMeanSd,matchedHbA1cMeanSd)
        results <- list(psAUC,
                        psScoreBeforeMatching,
                        psScoreAfterMatching,
                        finalAttDiag,
                        covariateBalance,
                        modelFit,
                        kmPlotWithoutCI,
                        kmPlotWithCI,
                        ageBeforeMatching,
                        ageAfterMatching,
                        genderBeforeMatching,
                        genderAfterMatching,
                        hbA1cStat)
      }else if(outCome==4){
        #unMatched
        studyPopMITreatment <- subset(studyPop, treatment == 1)
        studyPopMIComparator <- subset(studyPop, treatment == 0)
        pcTreatmentWithMI <- round(as.numeric(as.character(nrow(subset(studyPopMITreatment,outcomeCount>0))/nrow(studyPopMITreatment))),3)
        pcComparatorWithMI <- round(as.numeric(as.character(nrow(subset(studyPopMIComparator,outcomeCount>0))/nrow(studyPopMIComparator))),3)
        remove(studyPopMITreatment,studyPopMIComparator)
        #matched
        studyPopMITreatment <- subset(matchedPop, treatment == 1)
        studyPopMIComparator <- subset(matchedPop, treatment == 0)
        pcTreatmentWithMIMatched <- round(as.numeric(as.character(nrow(subset(studyPopMITreatment,outcomeCount>0))/nrow(studyPopMITreatment))),3)
        pcComparatorWithMIMatched <- round(as.numeric(as.character(nrow(subset(studyPopMIComparator,outcomeCount>0))/nrow(studyPopMIComparator))),3)
        MiStat <- data.frame(cbind(pcTreatmentWithMI, pcComparatorWithMI, pcTreatmentWithMIMatched, pcComparatorWithMIMatched))
        colnames(MiStat) <- c("pidFracTreatmentBeforeMatch","pidFracComparatorBeforeMatch","pidFracTreatmentAfterMatch","pidFracComparatorAfterMatch")
        results <- list(psAUC,
                        psScoreBeforeMatching,
                        psScoreAfterMatching,
                        finalAttDiag,
                        covariateBalance,
                        modelFit,
                        kmPlotWithoutCI,
                        kmPlotWithCI,
                        ageBeforeMatching,
                        ageAfterMatching,
                        genderBeforeMatching,
                        genderAfterMatching,
                        MiStat)
      }else if(outCome==5){
        studyPopKDTreatment <- subset(studyPop, treatment == 1)
        studyPopKDComparator <- subset(studyPop, treatment == 0)
        pcTreatmentWithKD <- round(as.numeric(as.character(nrow(subset(studyPopKDTreatment,outcomeCount>0))/nrow(studyPopKDTreatment))),3)
        pcComparatorWithKD <- round(as.numeric(as.character(nrow(subset(studyPopKDComparator,outcomeCount>0))/nrow(studyPopKDComparator))),3)
        remove(studyPopKDTreatment,studyPopKDComparator)
        #matched
        studyPopKDTreatment <- subset(matchedPop, treatment == 1)
        studyPopKDComparator <- subset(matchedPop, treatment == 0)
        pcTreatmentWithKDMatched <- round(as.numeric(as.character(nrow(subset(studyPopKDTreatment,outcomeCount>0))/nrow(studyPopKDTreatment))),3)
        pcComparatorWithKDMatched <- round(as.numeric(as.character(nrow(subset(studyPopKDComparator,outcomeCount>0))/nrow(studyPopKDComparator))),3)
        KDStat <- data.frame(cbind(pcTreatmentWithKD, pcComparatorWithKD, pcTreatmentWithKDMatched, pcComparatorWithKDMatched))
        colnames(KDStat) <- c("pidFracTreatmentBeforeMatch","pidFracComparatorBeforeMatch","pidFracTreatmentAfterMatch","pidFracComparatorAfterMatch")
        results <- list(psAUC,
                        psScoreBeforeMatching,
                        psScoreAfterMatching,
                        finalAttDiag,
                        covariateBalance,
                        modelFit,
                        kmPlotWithoutCI,
                        kmPlotWithCI,
                        ageBeforeMatching,
                        ageAfterMatching,
                        genderBeforeMatching,
                        genderAfterMatching,
                        KDStat)
      }else if(outCome==6){
        studyPopEDTreatment <- subset(studyPop, treatment == 1)
        studyPopEDComparator <- subset(studyPop, treatment == 0)
        pcTreatmentWithED <- round(as.numeric(as.character(nrow(subset(studyPopEDTreatment,outcomeCount>0))/nrow(studyPopEDTreatment))),3)
        pcComparatorWithED <- round(as.numeric(as.character(nrow(subset(studyPopEDComparator,outcomeCount>0))/nrow(studyPopEDComparator))),3)
        remove(studyPopEDTreatment,studyPopEDComparator)
        #matched
        studyPopEDTreatment <- subset(matchedPop, treatment == 1)
        studyPopEDComparator <- subset(matchedPop, treatment == 0)
        pcTreatmentWithEDMatched <- round(as.numeric(as.character(nrow(subset(studyPopEDTreatment,outcomeCount>0))/nrow(studyPopEDTreatment))),3)
        pcComparatorWithEDMatched <- round(as.numeric(as.character(nrow(subset(studyPopEDComparator,outcomeCount>0))/nrow(studyPopEDComparator))),3)
        EDStat <- data.frame(cbind(pcTreatmentWithED, pcComparatorWithED, pcTreatmentWithEDMatched, pcComparatorWithEDMatched))
        colnames(EDStat) <- c("pidFracTreatmentBeforeMatch","pidFracComparatorBeforeMatch","pidFracTreatmentAfterMatch","pidFracComparatorAfterMatch")
        results <- list(psAUC,
                        psScoreBeforeMatching,
                        psScoreAfterMatching,
                        finalAttDiag,
                        covariateBalance,
                        modelFit,
                        kmPlotWithoutCI,
                        kmPlotWithCI,
                        ageBeforeMatching,
                        ageAfterMatching,
                        genderBeforeMatching,
                        genderAfterMatching,
                        EDStat)
      }
    }
    }
  return(results)
}
