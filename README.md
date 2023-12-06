## Welcome!

This repository contains code and data used in the following research:

NA. [nd] Combining census data and paleoecology to track the establishment and growth of a major seabird colony in Newfoundland [unpublished].

### Description of R files:

- **Breakpoint_analysis_Metals.R**: contains code to conduct a breakpoint analysis on the metal(loid) data, including constrained cluster analysis (CONISS), and a broken stick plot. Corresponds to Figure S3 of the manuscript's Supplementary materials

- **Dating_profiles.R**: contains code to plot 210-Pb activity profiles, includes data for sedimentation rate, midpoint depth, and year. Corresponds to Figure S2 of the manuscript's Supplementary materials

- **GAM-Impact(or Reference)_core.R**: contains code to plot a generalized additive model (GAM) using a mixed model approach via restricted maximum likelihood (REML). Corresponds to Figure S4 and S5 of the manuscript's Supplementary materials

- **Paleostratigraphy.R**: contains code to plot multiple stratigraphies (two cores total) on the same figure using the package patchwork. Corresponds to Figure 1 of the manuscript.

- **Zscore-v-NestingPairs.R**: contains code to plot the z-score values of all proxies against a seabird colony's historical census data. Corresponds to Figure 2 of the manuscript.


### Description of data files:

- File S1. Historical population reports collected for seabird species nesting in Cape St. Mary’s Ecological Reserve, 1883-2018, provided by the Canadian Wildlife Service, Environment and Climate Change Canada, Newfoundland. A blank cell indicates that population counts were not collected for that year. Hover over each cell to view the citation associated with the census data.

- File S2. A table containing 210Pb dating profiles over depth of the impact core.

- File S3. A table containing 210Pb dating profiles over depth of the reference core.

- File S4. A table containing the isotope, metal(loid), chlorophyll a, and diatom count data for the depths of the sediment core collected from the impact pond, including z-score values denoted by “z_” in column names.

- File S5. A table containing the isotope, metal(loid), chlorophyll a, and diatom count data for the depths of the sediment core collected from the reference pond, including z-score values denoted by “z_” in column names.


