**This repo is hosted by an anonymous account and will be deleted after the double-anonymous peer review process. It will be replaced with an identical Git repository hosted by the author.**

__________________


## Welcome!
This repository contains code and data used in the following research: 

`{Hidden for double-anonymous peer review}. Blending census and paleolimnological data allows for tracking the establishment and growth of a major gannet colony over several centuries. Proc. R. Soc. B [in-review].`


You can parse through the original R files individually by cloning this repo onto your local computer

### Description of R files:

- **Breakpoint_analysis_Metals.R**: contains code to conduct a breakpoint analysis on the metal(loid) data, including constrained cluster analysis (CONISS), and a broken stick plot. Corresponds to Figure S3 of the manuscript's Supplementary materials

- **Dating_profiles.R**: contains code to plot 210-Pb activity profiles, includes data for sedimentation rate, midpoint depth, and year. Corresponds to Figure S2 of the manuscript's Supplementary materials

- **GAM-Impact(or Reference)_core.R**: contains code to plot a generalized additive model (GAM) using a mixed model approach via restricted maximum likelihood (REML). Corresponds to Figure S4 and S5 of the manuscript's Supplementary materials

- **Paleostratigraphy.R**: contains code to plot multiple stratigraphies (two cores total) on the same figure using the package patchwork. Corresponds to Figure 1 of the manuscript.

- **Zscore-v-NestingPairs.R**: contains code to plot the z-score values of all proxies against a seabird colony's historical census data. Corresponds to Figure 2 of the manuscript.


### Description of data files:

* **FileS1_ColonialSeabirdDatabase_WilhelmSI.csv** : Historical population reports collected for seabird species nesting in Cape St. Mary’s Ecological Reserve from 1883-2018. A blank cell indicates that population counts were not collected for that year.

* **FileS2_MonitoringData.csv** : Raw monitoring data from File S1 used to align population data to the proxies.

* **FileS3_Dating_CSM-IMP.csv** and File S4_Dating_CSM-REF.csv : Table containing 210Pb dating profiles over the depth of the impact core (CSM-IMP) and reference core (CSM-REF).

* **FileS5_ProxyData_CSM-IMP.csv** and **FileS6_ProxyData_CSM-REF.csv** : Table containing the isotope, metal(loid), chlorophyll a, and diatom count data for the depths of the sediment core collected from the reference pond.

* **File S7_Metalloids_CSM-IMP.csv** and **File S8_Metalloids_CSM-REF.csv** : Table of all metal(loid)s analyzed for the impact core and reference core.

* **FileS9_ZScores_CSM-IMP.csv** : Z-score data for the isotope, metal(loid)s, chlorophyll a, and diatom count for the depths of the sediment core collected from the impact pond.
