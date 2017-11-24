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
#' getAgeGender
#'
#' @author
#' Rohit Vashisht
#'
#' @details
#' This function can be used to compute the age and gender of patients for each outcome and for each treatment and
#' comparator cohort.
  print(paste("Plotting all the results. This might take few minutes ... "))
  resFiles <- list.files(paste(results_path,"/deleteMeBeforeSharing/",sep=""))
  #---------------------------------------------------------------------
  #For outCome 4 representing HbA1c <= 7%, represented as HbA1c7Good
  x <- grep("_o4",resFiles)
  resFilesOutCome4 <- resFiles[x]
  #Get files sorted for t and c comparisions
  #bigToSulf and bigToDpp4 (1,2)
  tcOne <- grep("_t1_c2",resFilesOutCome4)
  #bigToSulf and bigToThia
  tcTwo <- grep("_t1_c3",resFilesOutCome4)
  #bigToDpp4 and bigToThia
  tcThree <- grep("_t2_c3",resFilesOutCome4)
  #---- For tcOne
  if(length(tcOne)!=0){
    resFilesOutCome4tcOne <- resFilesOutCome4[tcOne]
    #load the data
    ps <- grep("Ps",resFilesOutCome4tcOne)
    psScore <- readRDS(paste(results_path,"deleteMeBeforeSharing/",resFilesOutCome4tcOne[ps],sep=""))
    matchPop <- grep("StratPop",resFilesOutCome4tcOne)
    matchedPop <- readRDS(paste(results_path,"deleteMeBeforeSharing/",resFilesOutCome4tcOne[matchPop],sep=""))
    studPop <- grep("StudyPop", resFilesOutCome4tcOne)
    studyPop <- readRDS(paste(results_path,"deleteMeBeforeSharing/",resFilesOutCome4tcOne[studPop],sep=""))
    bl <- grep("Bal",resFilesOutCome4tcOne)
    balance <- readRDS(paste(results_path,"deleteMeBeforeSharing/",resFilesOutCome4tcOne[bl],sep=""))
    remove(ps,matchPop,studPop,bl)
    loadCohortMethodData(paste(results_path,"deleteMeBeforeSharing/CmData_l1_t1_c2",sep=""))
    # getting age gender information for all the patients before and after matching.
    if (!is.null(cohortMethodData$metaData$deletedCovariateIds)) {
      idx <- is.na(ffbase::ffmatch(cohortMethodData$covariateRef$covariateId,
                                   ff::as.ff(cohortMethodData$metaData$deletedCovariateIds)))
      removedCovars <- ff::as.ram(cohortMethodData$covariateRef[ffbase::ffwhich(idx, idx == FALSE),
                                                                ])
      # Age before matching.
      ageBeforeMatching <- balance[grep("age group:", balance$covariateName), ]
      ageBeforeMatching <- data.frame(group = ageBeforeMatching$covariateName,
                                      countTreated = ageBeforeMatching$beforeMatchingSumTreated,
                                      countComparator = ageBeforeMatching$beforeMatchingSumComparator,
                                      fractionTreated = ageBeforeMatching$beforeMatchingMeanTreated,
                                      fractionComparator = ageBeforeMatching$beforeMatchingMeanComparator)
      removedAgeGroup <- removedCovars[grep("age group:", removedCovars$covariateName), ]
      if (nrow(removedAgeGroup) == 1) {
        totalTreated <- ageBeforeMatching$countTreated[1]/ageBeforeMatching$fractionTreated[1]
        missingFractionTreated <- 1 - sum(ageBeforeMatching$fractionTreated)
        missingFractionComparator <- 1 - sum(ageBeforeMatching$fractionComparator)
        removedAgeGroup <- data.frame(group = removedAgeGroup$covariateName,
                                      countTreated = round(missingFractionTreated *
                                                             totalTreated), countComparator = round(missingFractionComparator * totalTreated), fractionTreated = missingFractionTreated, fractionComparator = missingFractionComparator)
        ageBeforeMatching <- rbind(ageBeforeMatching, removedAgeGroup)
      }
      ageBeforeMatching$start <- gsub("age group: ", "", gsub("-.*$", "", ageBeforeMatching$group))
      ageBeforeMatching$start <- as.integer(ageBeforeMatching$start)
      ageBeforeMatching <- ageBeforeMatching[order(ageBeforeMatching$start), ]
      ageBeforeMatching$start <- NULL
      # Age after matching ...
      ageAfterMatching <- balance[grep("age group:", balance$covariateName), ]
      ageAfterMatching <- data.frame(group = ageAfterMatching$covariateName,
                                     countTreated = ageAfterMatching$afterMatchingSumTreated,
                                     countComparator = ageAfterMatching$afterMatchingSumComparator,
                                     fractionTreated = ageAfterMatching$afterMatchingMeanTreated,
                                     fractionComparator = ageAfterMatching$afterMatchingMeanComparator)
      # Add removed age group (if any):
      removedAgeGroup <- removedCovars[grep("age group:", removedCovars$covariateName), ]
      if (nrow(removedAgeGroup) == 1) {
        totalTreated <- ageAfterMatching$countTreated[1]/ageAfterMatching$fractionTreated[1]
        missingFractionTreated <- 1 - sum(ageAfterMatching$fractionTreated)
        missingFractionComparator <- 1 - sum(ageAfterMatching$fractionComparator)
        removedAgeGroup <- data.frame(group = removedAgeGroup$covariateName,
                                      countTreated = round(missingFractionTreated *
                                                             totalTreated), countComparator = round(missingFractionComparator * totalTreated), fractionTreated = missingFractionTreated, fractionComparator = missingFractionComparator)
        ageAfterMatching <- rbind(ageAfterMatching, removedAgeGroup)
      }
      ageAfterMatching$start <- gsub("age group: ", "", gsub("-.*$", "", ageAfterMatching$group))
      ageAfterMatching$start <- as.integer(ageAfterMatching$start)
      ageAfterMatching <- ageAfterMatching[order(ageAfterMatching$start), ]
      ageAfterMatching$start <- NULL
      ## gender before matching
      genderBeforeMatching <- balance[grep("gender", balance$covariateName), ]
      x <- grep("during 365d", genderBeforeMatching$covariateName)
      if (length(x) == 0) {
        genderBeforeMatching <- genderBeforeMatching
      } else {
        genderBeforeMatching <- genderBeforeMatching[-x, ]
      }
      remove(x)
      genderBeforeMatching <- data.frame(group = genderBeforeMatching$covariateName,
                                         countTreated = genderBeforeMatching$beforeMatchingSumTreated,
                                         countComparator = genderBeforeMatching$beforeMatchingSumComparator,
                                         fractionTreated = genderBeforeMatching$beforeMatchingMeanTreated,
                                         fractionComparator = genderBeforeMatching$beforeMatchingMeanComparator)
      removedGender <- removedCovars[grep("gender", removedCovars$covariateName), ]
      if (nrow(removedGender) == 1) {
        totalTreated <- genderBeforeMatching$countTreated[1]/genderBeforeMatching$fractionTreated[1]
        missingFractionTreated <- 1 - sum(genderBeforeMatching$fractionTreated)
        missingFractionComparator <- 1 - sum(genderBeforeMatching$fractionComparator)
        removedGender <- data.frame(group = removedGender$covariateName,
                                    countTreated = round(missingFractionTreated *
                                                           totalTreated), countComparator = round(missingFractionComparator * totalTreated), fractionTreated = missingFractionTreated, fractionComparator = missingFractionComparator)
        genderBeforeMatching <- rbind(genderBeforeMatching, removedGender)
      }
      genderBeforeMatching$group <- gsub("gender = ", "", genderBeforeMatching$group)
      # Gender After Matching
      genderAfterMatching <- balance[grep("gender", balance$covariateName), ]
      x <- grep("during 365d", genderAfterMatching$covariateName)
      if (length(x) == 0) {
        genderAfterMatching <- genderAfterMatching
      } else {
        genderAfterMatching <- genderAfterMatching[-x, ]
      }
      remove(x)
      genderAfterMatching <- data.frame(group = genderAfterMatching$covariateName,
                                        countTreated = genderAfterMatching$afterMatchingSumTreated,
                                        countComparator = genderAfterMatching$afterMatchingSumComparator,
                                        fractionTreated = genderAfterMatching$afterMatchingMeanTreated,
                                        fractionComparator = genderAfterMatching$afterMatchingMeanComparator)
      # Add removed gender (if any):
      removedGender <- removedCovars[grep("gender", removedCovars$covariateName), ]
      if (nrow(removedGender) == 1) {
        totalTreated <- genderAfterMatching$countTreated[1]/genderAfterMatching$fractionTreated[1]
        missingFractionTreated <- 1 - sum(genderAfterMatching$fractionTreated)
        missingFractionComparator <- 1 - sum(genderAfterMatching$fractionComparator)
        removedGender <- data.frame(group = removedGender$covariateName,
                                    countTreated = round(missingFractionTreated *
                                                           totalTreated), countComparator = round(missingFractionComparator * totalTreated), fractionTreated = missingFractionTreated, fractionComparator = missingFractionComparator)
        genderAfterMatching <- rbind(genderAfterMatching, removedGender)
      }
      genderAfterMatching$group <- gsub("gender = ", "", genderAfterMatching$group)
    } else {
      x <- grep("age group:", balance$covariateName)
      if (length(x) != 0) {
        # Before Matching
        ageBeforeMatching <- balance[grep("age group:", balance$covariateName), ]
        ageBeforeMatching <- data.frame(group = ageBeforeMatching$covariateName,
                                        countTreated = ageBeforeMatching$beforeMatchingSumTreated,
                                        countComparator = ageBeforeMatching$beforeMatchingSumComparator,
                                        fractionTreated = ageBeforeMatching$beforeMatchingMeanTreated,
                                        fractionComparator = ageBeforeMatching$beforeMatchingMeanComparator)
        ageBeforeMatching$start <- gsub("age group: ",
                                        "",
                                        gsub("-.*$", "", ageBeforeMatching$group))
        ageBeforeMatching$start <- as.integer(ageBeforeMatching$start)
        ageBeforeMatching <- ageBeforeMatching[order(ageBeforeMatching$start), ]
        ageBeforeMatching$start <- NULL
        # after matching
        ageAfterMatching <- balance[grep("age group:", balance$covariateName), ]
        ageAfterMatching <- data.frame(group = ageAfterMatching$covariateName,
                                       countTreated = ageAfterMatching$afterMatchingSumTreated,
                                       countComparator = ageAfterMatching$afterMatchingSumComparator,
                                       fractionTreated = ageAfterMatching$afterMatchingMeanTreated,
                                       fractionComparator = ageAfterMatching$afterMatchingMeanComparator)
        ageAfterMatching$start <- gsub("age group: ", "", gsub("-.*$", "", ageAfterMatching$group))
        ageAfterMatching$start <- as.integer(ageAfterMatching$start)
        ageAfterMatching <- ageAfterMatching[order(ageAfterMatching$start), ]
        ageAfterMatching$start <- NULL
      } else {
        ageBeforeMatching <- data.frame(NA)
        ageAfterMatching <- data.frame(NA)
      }
      r <- grep("gender", balance$covariateName)
      if (length(r) != 0) {
        # Before Matching
        genderBeforeMatching <- balance[grep("gender", balance$covariateName), ]
        x <- grep("during 365d", genderBeforeMatching$covariateName)
        if (length(x) == 0) {
          genderBeforeMatching <- genderBeforeMatching
        } else {
          genderBeforeMatching <- genderBeforeMatching[-x, ]
        }
        remove(x)
        genderBeforeMatching <- data.frame(group = genderBeforeMatching$covariateName,
                                           countTreated = genderBeforeMatching$beforeMatchingSumTreated,
                                           countComparator = genderBeforeMatching$beforeMatchingSumComparator,
                                           fractionTreated = genderBeforeMatching$beforeMatchingMeanTreated,
                                           fractionComparator = genderBeforeMatching$beforeMatchingMeanComparator)
        genderBeforeMatching$group <- gsub("gender = ", "", genderBeforeMatching$group)
        # gender after matching
        genderAfterMatching <- balance[grep("gender", balance$covariateName), ]
        x <- grep("during 365d", genderAfterMatching$covariateName)
        if (length(x) == 0) {
          genderAfterMatching <- genderAfterMatching
        } else {
          genderAfterMatching <- genderAfterMatching[-x, ]
        }
        remove(x)
        genderAfterMatching <- data.frame(group = genderAfterMatching$covariateName,
                                          countTreated = genderAfterMatching$afterMatchingSumTreated,
                                          countComparator = genderAfterMatching$afterMatchingSumComparator,
                                          fractionTreated = genderAfterMatching$afterMatchingMeanTreated,
                                          fractionComparator = genderAfterMatching$afterMatchingMeanComparator)
        genderAfterMatching$group <- gsub("gender = ", "", genderAfterMatching$group)
      } else {
        genderBeforeMatching <- data.frame(NA)
        genderAfterMatching <- data.frame(NA)
      }
    }
  }
