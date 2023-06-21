# Experiences and outcomes of Alternative Education participants

IDI analysis conducted by SWA to support an evaluation of Alternative Education.

## Overview

The Social Wellbeing Agency partnered with the Education Review Office to undertake an evaluation of Alternative Education. This code represents the SWA component of this evaluation, which involved statistical analysis of the Integrated Data Infrastructure (IDI). 

This code contributed to two documents. The first was a technical report published by SWA: **Experiences and outcomes of Alternative Education participants: An IDI analysis supporting an evaluation of Alternative Education** (available at http://swa.govt.nz). The second was the final evaluation report summarising key results both from SWA and ERO activities: **An Alternative Education? Support for our most disengaged young people** (available at http://ero.govt.nz). This code should be read and used alongside these two documents.

## Dependencies

It is necessary to have an IDI project if you wish to run the code. Visit the Stats NZ website for more information about this.

This analysis has been developed for the IDI_Clean_202210 refresh of the IDI. As changes in database structure can occur between refreshes, the initial preparation of the input information may require updating to run the code in other refreshes. This code also reads and writes to a project-specific sandpit tables. It will be necessary for others to change these project references when running the code.

The R code makes use of several publicly available R packages, we also use the [Dataset Assembly Tool](https://github.com/nz-social-wellbeing-agency/dataset_assembly_tool) and associated resources. Stats NZ who maintain the IDI have already installed in the IDI the all the key packages that this analysis depends on. 

## Instructions to run the project

Prior to running the project be sure to review the associated report and documentation.

To prepare the necessary supporting tables that create some necessary indicators, first run the following pieces of code:

- Alt Ed Input - Parents
- Alt Ed Input - ASD indicator 
- Alt Ed Input - MHA indicators 
- Alt Ed Input - ADHD indicator
- Alt Ed Input - ID indicator
- Alt Ed Input - YJ FGC indicator

Then the main analysis can be run sequentially, alternating between SQL (to build datasets) and R (to conduct statistical modelling).

## Citation

Social Wellbeing Agency (2022). *Experiences and outcomes of Alternative Education participants.* Source code. https://github.com/nz-social-wellbeing-agency/alternative_education 

## Getting Help

If you have any questions email info@swa.govt.nz
