# Crop Wild Relative of the USA

## Code base for "[Crop wild relatives of the United States require urgent conservation action](https://www.pnas.org/content/117/52/33351)"

## Primary Authors : Colin Khoury, Dan Carver

## maintainer: dan - carverd@colostate.edu

### Contributors : Harold Armando Achicanoy Estrella, Chrystiam Sossa, Maria Victoria Diaz

Code base for a reproducible maxnet modeling methodology written in R.

If you are interest in using this material for your own analysis, three folders are significant

### dataPrep
- datePrep : Functions that are called by run_linear.R for individual species model runs.
- dataPrep/dataBaseTransform folder: a series of scripts that covert data from various sources into a common schema. The input datasets for these scripts are not provded in this repo. The final dataset used in the analysis for the paper is available on the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BV4I06).

### Modeling
- alternative : a script called by run_linear.R for species with not enough valid occurrence data to run the Maxnet modeling process.
- maxnet : scripts called by run_linear.R that conduct a variable section, model fitting, model project, and a series of model evaluations.

### gapAnalysis
The gap analysis methodology can be applied to existing species distribution models. We recommend utilizing the [GapAnalysis R package](https://github.com/CIAT-DAPA/GapAnalysis) if you are applying these assessment methodology for your own work. 
- Combinded: a script called by run_linear.R for generate final conservation metric
- exsitu: scripts called by run_linear.R forgenerating the three exsitu conservation metrics  
- insitu: scripts called by run_linear.R for generating the three insitu conservation metrics  
- redlist: a script called by run_linear.R for evaluting the EOO and AOO red list conservation metrics. Utilizes the function provided by [redlisr package](https://github.com/red-list-ecosystem/redlistr)

### run_lineal.R
Primary script for declaring all input variables and output locations
