# Long-term Physical Activity Trajectories, Depressive Symptoms, and Risk of Dementia: Consistent and Divergent Patterns among Aging Populations of Western Countries, 2004–2022

# Overview

This project leverages harmonized longitudinal data from three aging cohort studies, including the Health and Retirement Study (HRS), the English Longitudinal Study of Ageing (ELSA), and the Survey of Health, Ageing and Retirement in Europe (SHARE), to investigate the association between long-term physical activity trajectories and incident dementia among adults aged 50 years and older.

Physical activity trajectories were constructed using repeated measures of physical activity status across three waves in each cohort. The study further evaluated whether depressive symptoms partially mediated the association between physical activity trajectories and dementia risk. Cohort-specific analyses were first conducted separately, and estimates were subsequently pooled using meta-analysis to compare findings across settings.

# Requirement

R version 4.2 or higher

# Scripts descriptions

* The folder "HRS" includes all analysis code developed for the HRS cohort.
* The folder "ELSA" includes all analysis code developed for the ELSA cohort.
* The folder "SHARE" includes all analysis code developed for the SHARE cohort.
* The folder "Pooled analysis" includes code for cross-cohort harmonization, pooled descriptive analyses, meta-analysis, and figure generation.
* The folder "Sensitivity analysis" includes code for sensitivity analyses, including inverse probability treatment weighting, inverse probability censoring weighting, complete case analysis, and bootstrap mediation analysis.

# Main analyses

The main analyses include:

1. Construction of harmonized physical activity status across three waves.
2. Classification of physical activity trajectories:
   * Consistently active
   * Increasingly active
   * Intermittently active
   * Decreasingly active
   * Consistently inactive
3. Descriptive analyses of baseline characteristics and trajectory distributions.
4. Cohort-specific Cox proportional hazards models estimating the association between physical activity trajectories and incident dementia.
5. Meta-analysis of cohort-specific hazard ratios.
6. Mediation analysis assessing depressive symptoms as a potential mediator.
7. Sensitivity analyses to evaluate the robustness of the findings.

# About

This project analyzes harmonized longitudinal data from HRS, ELSA, and SHARE to examine long-term physical activity trajectories, depressive symptoms, and incident dementia risk across the United States, England, and European countries. The study aims to clarify whether less favorable physical activity trajectories are associated with higher dementia risk and whether depressive symptoms may partially explain these associations.
