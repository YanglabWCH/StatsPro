# StatsPro<img src="StatsProhome.png" align="right" height="145" width="145"/>
StatsPro: systematic integration and evaluation of statistical approaches for detecting differential expression in label-free quantitative proteomics

## Citation
Yang Y, Cheng J, Wang S, Yang H. StatsPro: Systematic integration and evaluation of statistical approaches for detecting differential expression in label-free quantitative proteomics. J Proteomics. 2021 Sep 30;250:104386. doi: [10.1016/j.jprot.2021.104386](https://doi.org/10.1016/j.jprot.2021.104386).

## Brief Description
**<font size='5'> StatsPro </font>** integrates 12 common statistical methods and 6 P-value combination strategies, and then provides three evaluation criteria to assess the performance of each method or strategy (described in Software Manual). This tool is expected to help scientists detect the differentially expressed proteins and realize the ability of different statistical methods in a systematic view. Here we present the detailed introduction and operation of StatsPro, by which users can follow to analyze their own data freely and conveniently. The online version can be accessed from here: [https://www.omicsolution.com/wukong/StatsPro/](https://www.omicsolution.com/wukong/StatsPro/) or [https://www.omicsolution.org/wukong/StatsPro](https://www.omicsolution.org/wukong/StatsPro).

***Please Note:*** If the online version does not work, which means you cannot open the link above, that is probably because our server is down and we will fix it very soon. We also recommend users to install this tool locally.

## Software Manual
A detailed introduction of this software can be found in [StatsPro_Manual.pdf](https://github.com/YanglabWCH/StatsPro/blob/main/StatsPro_Manual.pdf) file.

## Table of contents graphic

<img src="StatsPro_TOC.png" align="center" height="429" width="800"/>

## Preparation for local installation
This tool is developed with R, so if you want to run it locally, you may do some preparatory work:  
**1. Install R.** You can download R from here: [https://www.r-project.org/](https://www.r-project.org/).  
**2. Install RStudio.** (Recommendatory but not necessary). You can download RStudio from here: [https://www.rstudio.com/](https://www.rstudio.com/).  
**3. Check packages.** After installing R and RStudio, you should check whether you have installed these packages (shiny, shinyjs, shinyBS, rlang, htmltools, shinyWidgets, gdata, ggplot2, ggsci, DT, tidyverse, ggExtra, cowplot, readxl, writexl, data.table, Amelia, impute, coin, exactRankTests, genefilter, geneplotter, limma, DESeq, samr, RankProd, ROTS, MSnbase, DEqMS, plgem, effsize, patchwork, parallelly, listenv, globals, future, future.apply, survcomp, msqrobsum, EDASeq, NOISeq, baySeq, NBPSeq, log4r, metaseqR). You may run the codes below to check them:

```r
if(!require(pacman)) install.packages("pacman")
if(!require(BiocManager)) install.packages('BiocManager')
pacman::p_load(devtools, shiny, shinyjs, shinyBS, rlang, htmltools, shinyWidgets, gdata, gplots, ggplot2, ggsci, DT, tidyverse, ggExtra, cowplot, readxl, writexl, data.table, Amelia, impute, coin, exactRankTests, genefilter, geneplotter, qvalue, limma, samr, RankProd, ROTS, MSnbase, DEqMS, plgem, effsize, patchwork, parallelly, listenv, globals, future, future.apply, survcomp, EDASeq, NOISeq, baySeq, NBPSeq, log4r, raster, corrplot)
install.packages("https://www.bioconductor.org/packages//2.10/bioc/src/contrib/DESeq_1.8.3.tar.gz", repos = NULL, type = "source")
devtools::install_github("statOmics/MSqRobSum")
install.packages("https://www.bioconductor.org/packages/3.9/bioc/src/contrib/metaseqR_1.24.0.tar.gz", repos = NULL, type = "source")
devtools::install_github("YanglabWCH/StatsPro")
```

## Run it locally
If the preparatory work has been done, you can run this tool locally as below:
```r
library(StatsPro)
StatsPro_app()
```

Then NAguideR will be started as below:

<img src="homepage.png" align="center" height="612" width="500"/>


Enjoy yourself^_^


## Friendly suggestion
1. Open StatsPro with Chrome or Firefox.
2. The minimum operating system specifications are: **RAM 4GB, Hard drive 100 GB.**


## Contact
You could push an issue on this github. And optionally, please feel free to sent me an e-mail if you have any question or find a bug about this tool. Thank you^_^
Email: shishengwang@wchscu.cn.

## My R Info
```r
> sessionInfo()
R version 4.1.3 (2022-03-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)
```
