# Overview

Cooperations between species, aka mutualisms, occur on a pairwise basis or in a more complex network and often play integral and foundational roles in ecological communities. 
Mutualisms are species interactions where all participants receive net benefits, leading to higher individual fitness and increased population growth rates. 
They are widespread species interactions but can deteriorate into commensalism or parasitism under conditions that elevate costs or dampen benefits. 
Mutualisms are considered more context dependent than other species interactions, meaning the magnitude and sign of interaction strength are often determined by environmental conditions and species’ identities.
Mutualisms were historically studied as pairwise interactions despite often being embedded within multi-species communities. 
Growing evidence suggests that pairwise interactions arepoor predictors of the net effects of multi-species mutualism, often resulting in greater benefits than predicted by pairwise models.
These differences can be explained by several mechanisms: portfolio effect, complementarity, and sampling effect. 
Our study provides a wholistic and diversity-centered approach to studying multi-species mutualisms.

We integrate long-term field obserational data and sophisticated statistical modeling techniques to create a multi-level integral projection model (IPM).
We use this IPM to provide estimates of the fitness of our focal population (_Clyindropuntia imbricata_) to determine if there are benefits to partner diversity in this system and what mechanisms may explain this.
Beyond the population level, we also used the statistical models to analyze the partner turnover dynamics and the impacts of each partner on individual vital rates (i.e. survival, growth, reproduction).
We found that there were differences in ant impact on vital rates with some partners offering apparent boosts to growth and survival for small plants and others offering boosts to floral viability for reproducing plants.
Despite these differences, which led us to believe we may find evidence of complementarity, we found that there was actually no benefit of diversity within this system, and actually there appears to be a small cost as the number of partners increases from two to three (three being what we observe in the field).
We believe this is due to the differences primarily occurring at sizes where partner interactions are rare or in vital rates which do not significantly impact the long term population growth.
These results differ significantly from both similar systems and completely unrelated multi-species mutualisms.
Our wholistic and integrative approach allowed us to see not only a lack of diversity benefits, but also pin down why we likely see this while learning more about the dynamics of this system across the entire life cycle of the focal mutualist. 

The corresponding data for this repository can be found [here](https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sev.323.1). 

# Files

*Note: due to the large volume of files not all are described here.*

## Data

The folder Data contains all original data files needed to parameterize the models for this study and the cleaned data file for the primary analysis.

**cholla_demography_20042023** *(.csv)* -- Spreadsheet containing 2004-2023 census data.
**cholla_demography_20042023_cleaned** *(.csv)* -- Spreadsheet containing the cleaned 2004-2023 census data.
**FruitSurvival** *(.csv)* -- Spreadsheet containing data on the proportion of flowers which survival to produce fruits.
**Germination** *(.csv)* -- Spreadsheet containing data from a germination observational field experiment.
**JO_fruit_data_final_dropplant0** *(.csv)* -- Spreadsheet containing data from an ant exclusion seed production field experiment.
**PrecensusSurvival** *(.csv)* -- Spreadsheet containing data on seedlings which survive from germination to the annual May census.
**seed_counts** *(.csv)* -- Spreadsheet containing data on the number of seeds produced by a flower.

## Scripts

**01_cholla_ant_IPM_setup** *(.R)* -- Loads in all needed packages and data and cleans up the data so it is ready to be run through our models.
**02_cholla_ant_IPM_vital_rates** *(.R)* -- Loads in the "cleaned" data, subset it properly, runs the data through the stan models, and exports the outputs as *(.RDS)* files.
**03_cholla_ant_IPM_params_functions** *(.R)* -- Loads all model fits, loads the IPM functions, and loads any values which needed to run the IPM.
**04_cholla_ant_IPM_analysis** *(.R)* -- Runs the IPM under all conditions and saves the outputs as *(.csv)* files.
**05_cholla_ant_IPM_figures_tables** *(.R)* -- Create all visuals of model outputs for publications and related talks.

## Others

**STAN Models** *(folder)* -- This folder contains all vital rate models written in STAN code to be run through the second script.
**Model Outputs** *(folder)* -- This folder contains all IPM model outputs (estimates of lambda).
**Manuscript** *(folder)* -- This folder contains the *(.tex, .pdf, .bib, etc.)* files needed to compile the manuscript in LATEX. It also contains a sub folder called **Figures** which contains all figures created by the fifth script. 


