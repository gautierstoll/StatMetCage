StatMetCage: a package for statistical analysis of metabolic cage data.
====================================================================
Introduction
==============
StatMetCage performs statistical analysis of data produced by metabolic cages.

## Installation

StatMetCage is installed from Github:
```R
install.packages("devtools")
devtools::install_git("https://github.com/gautierstoll/StatMetCage")
```

Some dependent packages may not be available directly. They have to be installed first:

[lubridate](https://lubridate.tidyverse.org/):
```R
install.packages("lubridate")
```
[lme](https://cran.r-project.org/web/packages/nlme/index.html):
```R
install.packages("nlme")
```
[shiny](https://cran.r-project.org/web/packages/shiny/index.html):
```R
install.packages("shiny")
```
[shinyFiles](https://cran.r-project.org/web/packages/shinyFiles/index.html):
```R
install.packages("shinyFiles")
```
[fs](https://cran.r-project.org/web/packages/fs/index.html):
```R
install.packages("fs")
```
[readxl](https://cran.r-project.org/web/packages/readxl/index.html):
```R
install.packages("readxl")
```

## HowTo
1. Activate the package:
```R
library(StatMetCage)
```
2. Download your data stored in a .csv file (eg `myData.csv`):
```R
RawMetaData = new("RawMetaboData",fileName = "myData.csv")
```
3. Create an analysis object that contains the observation of interests(eg "VO2" and "VCO2""). An annotation data frame can be included (eg `myAnnotation`) that must contains "Animal", "Date" and "Time" column, with qualitative annotation (eg "Treatment", "Mutation"), the other columns will be treated as numeric data, with a time dependence evaluated by spline:
```R
AnalysisMeta = new("AnalysisMetaboData",rawData = RawMetaData,obs = c("VO2","VCO2"),annotation = myAnnotation,annotGroups = c("Treatment","Mutation"))
```
4. Create an statistical result object:
```R
ResStatMeta = new("ResStatMetabo",anMetData = AnalysisMeta,observation = "VO2",model = "quadratic",group = "Treatment")
```

The results can be plotted:
```R
metaboPlot(ResStatMeta,type = "data.model")
```
and statistical results can be exctracted:
```R
ResStatMeta@lmeRes$tTable
```
```

