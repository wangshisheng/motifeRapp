# motifeRapp<img src="motifeRlogo.png" align="right" height="200" width="200"/>
This is a R package for motifeR software. 
The detailed information can be found at [https://github.com/wangshisheng/motifeR](https://github.com/wangshisheng/motifeR). 
The online version is available here: [https://www.omicsolution.org/wukong/motifeR](https://www.omicsolution.org/wukong/motifeR).

## Preparation
This tool is developed with R, so if you want to run it locally, you may do some preparatory work:
1. **Install R.** You can download R from here: [https://www.r-project.org/](https://www.r-project.org/).
2. **Install RStudio.** (Recommendatory but not necessary). You can download RStudio from here: [https://www.rstudio.com/](https://www.rstudio.com/).
3. **Check packages.** After installing R and RStudio, you should check whether you have installed these packages (shiny, shinyBS, shinyjs, DT, gdata, knitr, ggplot2, ggsci, openxlsx, data.table, Biostrings, stringi, stringr, ggrepel, igraph, ggraph, graphlayouts, scales). You may run the codes below to check them:

```r
if(!require(pacman)) install.packages("pacman")
p_load(devtools,shiny,shinyBS,shinyjs,DT,gdata,knitr,ggplot2,ggsci,openxlsx,data.table,Biostrings,stringi,stringr,ggrepel,igraph,ggraph,graphlayouts,scales)
```


## Run it locally
If the preparatory work has been done, you can run this tool locally as below:
```r
if(!require(motifeR)) devtools::install_github("wangshisheng/motifeRapp")
library(motifeR)
motifeR_app()
```
Then motifeR will be started as below:
<img src="openfig.jpg" align="right" height="425" width="900"/>


**Bravo!** You are successful to run motifeR locally, then you can analyze your own data.


## Friendly suggestion
1. Open motifeR with Chrome or Firefox.
2. The minimum operating system specifications are: **RAM 4GB, Hard drive 100 GB.**
