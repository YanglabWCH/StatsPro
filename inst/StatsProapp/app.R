library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(readxl)
library(gdata)
library(ggplot2)
library(ggsci)
library(DT)
library(tidyverse)
library(ggExtra)
library(cowplot)
options(warn=-1)
colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10),pal_nejm("default")(8),pal_lancet("lanonc")(9),
                      pal_jama("default")(7),pal_jco("default")(10),pal_ucscgb("default")(26),pal_d3("category10")(10),
                      pal_locuszoom("default")(7),pal_igv("default")(51),
                      pal_uchicago("default")(9),pal_startrek("uniform")(7),
                      pal_tron("legacy")(7),pal_futurama("planetexpress")(12),pal_rickandmorty("schwifty")(12),
                      pal_simpsons("springfield")(16),pal_gsea("default")(12)))
#
ui<-renderUI(
  fluidPage(
    title="StatsPro",
    shinyjs::useShinyjs(),
    fluidRow(
      div(
        HTML(
          "<div style='text-align:center;margin-top:5px;margin-right:0px'>
            <a href='#' target=''><img src='statsproti.png' width='200px'>
            </a>
            </div>"
        )
      )
    ),
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="busystyle.css"),
        tags$script(type="text/javascript", src = "busy.js"),
        tags$style(type="text/css", "
                           #loadmessage {
                     position: fixed;
                     top: 0px;
                     left: 0px;
                     width: 100%;
                     height:100%;
                     padding: 250px 0px 5px 0px;
                     text-align: center;
                     font-weight: bold;
                     font-size: 100px;
                     color: #000000;
                     background-color: #D6D9E4;
                     opacity:0.6;
                     z-index: 105;
                     }
                     "),
        tags$script('
                            var dimension = [0, 0];
                    $(document).on("shiny:connected", function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    $(window).resize(function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    '),
        tags$style(type="text/css", "
                   #tooltip {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip2 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip3 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip4 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltipx5 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   ")
      )
    ),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(h2(strong("Calculating......")),img(src="rmd_loader.gif"),id="loadmessage")),
    tabsetPanel(
      id="maintab",
      tabPanel(
        "Welcome",
        uiOutput("welcomeui"),
        icon = icon("home")
      ),
      tabPanel(
        "Import Data",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 1: Upload Original Data",
              tags$span(
                id = 'span1',
                `data-toggle` = "tooltip",
                title = '
                In this part, users can upload their own proteomics expression data and type in sample information. The example data can be found when users click "Load example data" below. Detailed descriptions are provided in the supplementary notes.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            selectInput(
              "datatypex",
              h4("Data type:"),
              choices = c("Normal"=1,"MaxQuant"=2,"Proteome Discoverer"=3,"Spectronaut"=4),
              selected = 1
            ),
            tags$hr(style="border-color: grey;"),
            radioButtons(
              "loaddatatype",
              label = "",
              choices = list("Upload experimental data" = 1,"Load example data"=2),
              selected = 1,
              inline = TRUE
            ),
            tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = "input.loaddatatype==1",
              h4("1. Expression data:"),
              conditionalPanel(
                condition = "input.datatypex==1",
                fileInput('file1', h5('1.1.1 Import your data：'),
                          accept=c('.csv')),
                checkboxInput('header', 'First row as column names ?', TRUE),
                checkboxInput('firstcol', 'First column as row names ?', FALSE)
              ),
              conditionalPanel(
                condition = "input.datatypex==2",
                fileInput('file2', h5('1.1.2 Import your data：'),
                          accept=c('.txt'))
              ),
              conditionalPanel(
                condition = "input.datatypex==3",
                fileInput('file3', h5('1.1.3 Import your data：'),
                          accept=c('.xlsx'))
              ),
              conditionalPanel(
                condition = "input.datatypex==4",
                fileInput('file4', h5('1.1.4 Import your data：'),
                          accept=c('.csv'))
              ),
              tags$hr(style="border-color: #B2B2B2;"),
              h4("2. Samples information:"),
              textInput("grnums",h5("2.1 Group and replicate number:"),value = ""),
              bsTooltip("grnums",'Type in the group number and replicate number here. Please note, the group number and replicate number are linked with ";", and the replicate number of each group is linked with "-". For example, if you have two groups, each group has three replicates, then you should type in "2;3-3" here. Similarly, if you have 3 groups with 5 replicates in every groups, you should type in "3;5-5-5".',
                        placement = "right",options = list(container = "body")),
              textInput("grnames",h5("2.2 Group names:"),value = ""),
              bsTooltip("grnames",'Type in the group names of your samples. Please note, the group names are linked with ";". For example, there are two groups, you can type in "Control;Experiment".',
                        placement = "right",options = list(container = "body"))
            ),
            conditionalPanel(
              condition = "input.loaddatatype==2",
              conditionalPanel(
                condition = "input.datatypex==1",
                downloadButton("loaddatadownload1","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              ),
              conditionalPanel(
                condition = "input.datatypex==2",
                downloadButton("loaddatadownload2","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              ),
              conditionalPanel(
                condition = "input.datatypex==3",
                downloadButton("loaddatadownload3","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              ),
              conditionalPanel(
                condition = "input.datatypex==4",
                downloadButton("loaddatadownload4","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              ),
              #downloadButton("loaddatadownload1","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED"),
              tags$hr(style="border-color: grey;"),
              #downloadButton("loaddatadownload2","Download example sample group data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              h4("Samples information:"),
              conditionalPanel(
                condition = "input.datatypex!=4",
                textInput("examgrnums",h5("2.1 Group and replicate number:"),value = "2;3-3"),
                textInput("examgrnames",h5("2.2 Group names:"),value = "U5000;U12500")
              ),
              conditionalPanel(
                condition = "input.datatypex==4",
                textInput("examgrnums2",h5("2.1 Group and replicate number:"),value = "2;10-10"),
                textInput("examgrnames2",h5("2.2 Group names:"),value = "Cyc;Noco")
              )
            )
          ),
          mainPanel(
            width = 9,
            hr(),
            h4("1. Expression data："),
            hr(),
            dataTableOutput("peaksdata")#,
            #h4("2. Samples information data："),
            #dataTableOutput("samplesdata")
          )
        ),
        icon = icon("upload")
      ),
      tabPanel(
        "Data Preprocessing",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 2: Data Pre-processing",
              tags$span(
                id = 'span2',
                `data-toggle` = "tooltip2",
                title = '
                Here "Data Pre-processing" means this tool will process data normalization (Median normalization by default), coefficient of variation calculation, missing value statsx (KNN method by default) for the uploaded expression data.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            textInput("natype",h5("1. Missing value type:"),value = "NA"),
            bsTooltip("natype",'The type of missing value in the expression data.',
                      placement = "right",options = list(container = "body")),
            #textInput("natypecol",h5("2. Colors for plot by row:"),value = "#A11315;#8C86FF"),
            hr(),
            checkboxInput('fenzukaolvif', '2. Count NA by each group or not?', FALSE),
            bsTooltip("fenzukaolvif",'If true, StatsPro will count the NA number in every group, otherwise, it will count the NA number across all groups.',
                      placement = "right",options = list(container = "body")),
            #conditionalPanel(
            #  condition = 'input.fenzukaolvif==true',
            #  checkboxInput("keepzeroif","2.1. Keep NA as zero (0) or not?",FALSE),
            #  bsTooltip("keepzeroif",'If true, that means when all of the values of one protein/peptide are NA in one group, StatsPro will keep these NA as 0.',
            #            placement = "right",options = list(container = "body"))
            #),
            numericInput('naratio', h5('3. NA ratio:'), 0.5,max = 1,min = 0,step = 0.1),
            bsTooltip("naratio",'The threshold of NA ratio. One protein/peptide with NA ratio above this threshold will be removed.',
                      placement = "right",options = list(container = "body")),
            checkboxInput('mediannormif', '4. Median normalization or not?', TRUE),
            bsTooltip("mediannormif",'If true, the values in expression matrix will be devided by its column median value to make the samples to have the same median. (Please note, StatsPro was not designed to perform sophisticated normalization analysis. Any normalized datasets with NA can be accepted for analysis).',
                      placement = "right",options = list(container = "body")),
            checkboxInput('logif', '5. Log or not?', TRUE),
            bsTooltip("logif",'If true, the values in expression matrix will be log-transformed with base 2.',
                      placement = "right",options = list(container = "body")),
            numericInput('cvyuzhi', h5('6. CV threshold (raw scale):'), 0.3,max = 1,min = 0,step = 0.1),
            bsTooltip("cvyuzhi",'The threshold of coefficient of variation (CV). One protein/peptide with CV above this threshold will be removed. "raw scale" here means the values without log-transformation are used to calculate the CV.',
                      placement = "right",options = list(container = "body")),
            tags$hr(style="border-color: grey;"),
            numericInput("preheight",h5("Height for figure:"),value = 900)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              id="subnav",
              tabPanel(
                "NA Distribution",
                #hr(),
                div(
                  id="nafenbuid",
                  radioButtons(
                    "nafenbu",
                    label = h4(""),
                    choices = list("NA data"=1,"Plot by column"=2,"Plot by row"=3),
                    selected = 1,
                    inline = TRUE
                  )
                ),
                bsTooltip("nafenbuid",'1. NA data: the result that StatsPro recognize the missing values in the expression matrix; 2. Plot by column: the figure shows the distribution of missing values in each sample; 3. Plot by row: the figure show the distribution of missing values in every protein/peptide.',
                          placement = "left",options = list(container = "body")),
                actionButton("mcsbtn_nafenbu","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #CD853F; border-color: #CD853F"),
                tags$hr(style="border-color: grey;"),
                hidden(
                  div(
                    id="mcsbtn_nafenbu_hid",
                    conditionalPanel(
                      condition = "input.nafenbu==1",
                      downloadButton("nadatadfdl","Download"),
                      dataTableOutput("nadatadf")
                    ),
                    conditionalPanel(
                      condition = "input.nafenbu==2",
                      downloadButton("naplotbycolumndl","Download"),
                      plotOutput("naplotbycolumn")
                    ),
                    conditionalPanel(
                      condition = "input.nafenbu==3",
                      downloadButton("naplotbyrowdl","Download"),
                      plotOutput("naplotbyrow")
                    )
                  )
                )
              ),
              tabPanel(
                "NA and CV Filter",
                hr(),
                #radioButtons(
                #  "nafilterradiobutton",
                #  label = h4(""),
                #  choices = list("Filtered data"=1,"Data without NA"=2,"Data with NA"=3),
                #  selected = 1,
                #  inline = TRUE
                #),
                actionButton("mcsbtn_nafilter","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #CD853F; border-color: #CD853F"),
                tags$hr(style="border-color: grey;"),
                hidden(
                  div(
                    id="mcsbtn_nafilter_hid",
                    downloadButton("filtereddatadfdl","Download"),
                    dataTableOutput("filtereddatadf")
                  )
                )
                #conditionalPanel(
                #  condition = "input.nafilterradiobutton==1",
                #  downloadButton("filtereddatadfdl","Download"),
                #  dataTableOutput("filtereddatadf")
                #),
                #conditionalPanel(
                #  condition = "input.nafilterradiobutton==2",
                #  downloadButton("datanonadl","Download"),
                #  dataTableOutput("datanona")
                #),
                #conditionalPanel(
                #  condition = "input.nafilterradiobutton==3",
                #  downloadButton("datahasnadl","Download"),
                #  dataTableOutput("datahasna")
                #)
              ),
              tabPanel(
                "Imputation Result",
                hr(),
                downloadButton("knnimputeresdl","Download"),
                dataTableOutput("knnimputeres")
              )
              #tabPanel(
              #  "Input data check",
              #  hr(),
              #  uiOutput("inputdatacheck1"),
              #  uiOutput("inputdatacheck2")
              #)
            )
          )
        ),
        icon = icon("binoculars")
      ),
      tabPanel(
        "Statistical Methods",
        hr(),
        div(
          style="text-align:center;margin-left:200px;margin-right:200px",
          h3(
            "Step 3: Twelve statistical methods have been integrated here and classified into two categories (Parametric and Non-Parametric). Please select the test methods you want, then click the 'Calculate' button.",
            tags$span(
              id = 'span3',
              `data-toggle` = "tooltip3",
              title = '
                Here, users can select the statistical methods. Detailed instruction of each methods can be found in the references below.
                ',
              tags$span(class = "glyphicon glyphicon-question-sign")
            )# or in the "Help" part
          ),
          actionButton("mcsbtn_statsxII","Calculate",icon("paper-plane"),width ='200px',
                       style="color: #fff; background-color: #CD853F; border-color: #CD853F")
        ),
        hr(),
        mainPanel(
          id="methodids",
          width = 12,
          fixedRow(
            column(
              width = 12,
              panel(
                "",
                heading = "A. Parametric tests",
                status = "info",
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("ttestif", "Using Student's t test or not?",TRUE),
                      tags$hr(style="border-color: grey80;"),
                      checkboxInput("ttestpairedif", "M1.1 Paired or not?",FALSE),
                      #checkboxInput("ttestvarequalif", "Using Student's t test method or not?",FALSE),
                      heading = "Method 1: Student's t test (ttest)",
                      status = "success",
                      footer = a(href="https://en.wikipedia.org/wiki/Student%27s_t-test",h6("Student's t test introduction from Wikipedia"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("aovif", "Using One-Way ANOVA or not?",TRUE),
                      heading = "Method 2: One-Way ANOVA (aov)",
                      status = "success",
                      footer = a(href="https://en.wikipedia.org/wiki/Analysis_of_variance",h6("Analysis of variance from Wikipedia"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("limmaif", "Using Limma method or not?",TRUE),
                      heading = "Method 3: Linear Models for Microarray Data (limma)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1093/nar/gkv007",h6("DOI: 10.1093/nar/gkv007"),target="_blank")
                    )
                  ),
                  column(
                    6,
                    panel(
                      "",
                      checkboxInput("samif", "Using sam method or not?",TRUE),
                      tags$hr(style="border-color: grey80;"),
                      selectInput("samtype",h5("M7.1 Problem type:"),choices = c("Two class unpaired","Multiclass","Two class paired")),
                      heading = "Method 4: Significance analysis of microarrays (sam)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1073/pnas.091062498",h6("DOI: 10.1073/pnas.091062498"),target="_blank")
                    )
                  )
                )
              )
            ),
            column(
              width = 12,
              panel(
                "",
                heading = "B. Non-parametric tests",
                status = "warning",
                fixedRow(
                  column(
                    4,
                    panel(
                      "",
                      checkboxInput("wiltestif", "Using Wilcoxon rank sum test or not?",TRUE),
                      tags$hr(style="border-color: grey80;"),
                      checkboxInput("wiltestpairedif", "M1.1 Paired or not?",FALSE),
                      heading = "Method 5: Wilcoxon rank sum test (wiltest)",
                      status = "success",
                      footer = a(href="https://www.tandfonline.com/doi/abs/10.1080/01621459.1972.10481279",h6("DOI: 10.1080/01621459.1972.10481279"),target="_blank")
                    )
                  ),
                  column(
                    4,
                    panel(
                      "",
                      checkboxInput("kwtestif", "Using Kruskal-Wallis Rank Sum Test or not?",TRUE),
                      heading = "Method 6: Kruskal-Wallis Rank Sum Test (kwtest)",
                      status = "success",
                      footer = a(href="https://onlinelibrary.wiley.com/doi/book/10.1002/9781119196037",h6("DOI: 10.1002/9781119196037"),target="_blank")
                    )
                  ),
                  column(
                    4,
                    panel(
                      "",
                      checkboxInput("permif", "Using Permutation test or not?",TRUE),
                      heading = "Method 7: Permutation test (perm)",
                      status = "success",
                      footer = a(href="https://cran.r-project.org/web/packages/exactRankTests/index.html",h6("Package: exactRankTests"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    4,
                    panel(
                      "",
                      checkboxInput("rankproif", "Using Rank Product method or not?",TRUE),
                      heading = "Method 8: Rank Product analysis (rankpro)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1016/j.febslet.2004.07.055",h6("DOI: 10.1016/j.febslet.2004.07.055"),target="_blank")
                    )
                  ),
                  column(
                    4,
                    panel(
                      "",
                      checkboxInput("rotsif", "Using Reproducibility-Optimized Test Statistic or not?",TRUE, width ="100%"),
                      heading = "Method 9: Reproducibility-Optimized Test Statistic (rots)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1109/tcbb.2007.1078",h6("DOI: 10.1109/tcbb.2007.1078"),target="_blank")
                    )
                  ),
                  column(
                    4,
                    panel(
                      "",
                      checkboxInput("msqrobsumif", "Using Robust differential abundance analysis method or not?",TRUE, width ="100%"),
                      heading = "Method 10: Robust differential abundance analysis (msqrobsum)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1074/mcp.RA119.001624",h6("DOI: 10.1074/mcp.RA119.001624"),target="_blank")
                    )
                  )
                ),
                fixedRow(
                  column(
                    4,
                    panel(
                      "",
                      checkboxInput("deqmsif", "Using DEqMS method or not?",TRUE),
                      heading = "Method 11: DEqMS (deqms)",
                      status = "success",
                      footer = a(href="https://doi.org/10.1074/mcp.TIR119.001646",h6("DOI: 10.1074/mcp.TIR119.001646"),target="_blank")
                    )
                  ),
                  column(
                    4,
                    panel(
                      "",
                      checkboxInput("plgemif", "Using Power Law Global Error Model or not?",TRUE, width ="100%"),
                      heading = "Method 12: Power Law Global Error Model (plgem)",
                      status = "success",
                      footer = a(href="https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-5-203",h6("DOI: 10.1186/1471-2105-5-203"),target="_blank")
                    )
                  )
                )
              )
            )
          )
        ),
        icon = icon("cogs")
      ),
      tabPanel(
        "Results and Assessments",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 4: Results and Assessments",
              tags$span(
                id = 'span4',
                `data-toggle` = "tooltip4",
                title = '
                In this part, users can obtain (1) the statistical test results (p values without and with correction, BH-adjusted p-values by default), (2) the p value combination results, and (3) the ranks under every criterion (Detection number, Correlation between p values and effect sizes, Areas under the ROC curves).
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )#Detailed descriptions are provided in the "Help" part.
            ),
            #h4("1. Parameters for 'Results'"),
            h5(
              "1. Select one method: ",
              tags$span(
                id = 'spanx5',
                `data-toggle` = "tooltipx5",
                title = '
                The Data obtained in Step 2 are processed individually with each statistical method chosen by users in Step 3. Therefore, users can check every statistical result by selecting relative method here.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            uiOutput(
              "statsIIresui"
            ),
            numericInput("pvaluethreshold",h5("2. Threshold for p value:"),value = 0.05),
            numericInput("fcthreshold",h5("3. Threshold for fold change:"),value = 1.5),
            tags$hr(style="border-color: grey;"),
            fileInput('aucfile1', h5('4. Upload standard/spiked protein IDs/names:'),accept=c('.csv'))
            #numericInput("pinjiafigheight",h5("Figure height:"),value = 800)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              id="subnav",
              tabPanel(
                "P value results",
                div(
                  id="pvalueres1id",
                  radioButtons(
                    "pvalueres1",
                    label = h4(""),
                    choices = list("1. Tables"=1,"2. Plots"=2),
                    selected = 1,
                    inline = TRUE
                  )
                ),
                bsTooltip("pvalueres1id",'1. Tables: the statistical test results (p values without and with correction, BH-adjusted p-values by default); 2. Plots: the figures show the distribution of p values, volcano plot of p value and fold change, volcano plot of adjusted p value and fold change.',
                          placement = "left",options = list(container = "body")),
                hr(),
                conditionalPanel(
                  condition = "input.pvalueres1==1",
                  downloadButton("pvalueresdfdl","Download"),
                  dataTableOutput("pvalueresdf")
                ),
                conditionalPanel(
                  condition = "input.pvalueres1==2",
                  downloadButton("pvalueresplotdl","Download"),
                  plotOutput("pvalueresplot",height = "1200")
                )
              ),
              tabPanel(
                "P value combination",
                hr(),
                downloadButton("pcombinresdl","Download"),
                dataTableOutput("pcombinres")
              ),
              tabPanel(
                "Performance",
                #hr(),
                div(
                  id="pperformid",
                  radioButtons(
                    "pperform",
                    label = h4(""),
                    choices = list("1. Number of detections"=1,"2. Correlation"=2,"3. AUC and F1 score"=3),
                    selected = 1,
                    inline = TRUE
                  )
                ),
                bsTooltip("pperformid",'1. Number of detections means the number of those proteins with (adjusted) p value below the threshold; 2. Correlation means the pearson correlation between p values and Cohen d effect sizes; 3. AUC means the area under the receiver operating characteristic (ROC) curves (AUC) for the different methods.',
                          placement = "right",options = list(container = "body")),
                conditionalPanel(
                  condition = "input.pperform==1",
                  downloadButton("pnumdetecresdl","Download"),
                  dataTableOutput("pnumdetecres")
                ),
                conditionalPanel(
                  condition = "input.pperform==2",
                  downloadButton("prankcorresdl","Download"),
                  dataTableOutput("prankcorcres")
                ),
                conditionalPanel(
                  condition = "input.pperform==3",
                  hr(),
                  div(
                    id="aucidx",
                    radioButtons(
                      "aucradiox",
                      label = h4(""),
                      choices = list("3.1. Standard/spiked protein IDs/names data"=1,"3.2. AUC and F1 score results"=2),
                      selected = 1,
                      inline = TRUE
                    )
                  ),
                  bsTooltip("aucidx",'1. Standard/spiked protein IDs/names data: Please note, if users do not upload anything here, the default is spiked UPS1 (48 human proteins, Sigma-Aldrich) for example data; 2. AUC results show the AUC for the different methods.',
                            placement = "left",options = list(container = "body")),
                  conditionalPanel(
                    condition = "input.aucradiox==1",
                    downloadButton("spikedprodfdl","Download"),
                    dataTableOutput("spikedprodf")
                  ),
                  conditionalPanel(
                    condition = "input.aucradiox==2",
                    downloadButton("paucresdl","Download"),
                    dataTableOutput("paucres")
                  )
                )
              )
            )
          )
        ),
        icon = icon("table")
      )
    )
  )
)
#
server<-shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=1000*1024^2)
  usertimenum<-as.numeric(Sys.time())
  #ui
  output$welcomeui<-renderUI({
    screenwidth<-input$dimension[1]
    if(is.null(screenwidth)){
      return(NULL)
    }else{
      if(screenwidth<=1024){
        imgwidth<-400
      }
      else if(screenwidth>1024 & screenwidth<=1440){
        imgwidth<-500
      }
      else{
        imgwidth<-600
      }
    }

    fluidRow(
      #div(style="text-align:center",h1("~~Welcome~~")),
      div(
        id="mainbody",
        column(3),
        column(
          6,
          div(style="text-align:center;margin-top:20px;font-size:150%;color:#E64B35FF",
              HTML("<em>Welcome to StatsPro</em>")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px",
              HTML("<b><i>StatsPro</i></b> is an open-source software, which integrates 12 common statistical methods and 6 P-value combination strategies, and then provides three evaluation criteria to assess the performance of each method or strategy. This tool is expected to help scientists detect the differentially expressed proteins and realize the ability of different statistical methods in a systematic view. The source codes and detailed manual can be accessed at our <a href='https://github.com/YanglabWCH/StatsPro' target='_blank'>GitHub</a>. (<b>Please Note:</b> If this online version does not work, which means you cannot open the software link, it is probably because our server is down and we will fix it very soon. Or, please try to install this tool and run it locally.)")),
          div(style="text-align:center;margin-top: 50px",
              a(href='#',
                img(src='StatsProhome.png',height=imgwidth))),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;margin-top:20px;font-size:120%",
              HTML("StatsPro is developed by <a href='https://shiny.rstudio.com/' target='_blank'>R shiny (Version 1.3.2)</a>, and is free and open to all users with no login requirement. It can be readily accessed by all popular web browsers including Google Chrome, Mozilla Firefox, Safari and Internet Explorer 10 (or later), and so on. We would highly appreciate that if you could send your feedback about any bug or feature request to Shisheng Wang at <u>wsslearning@omicsolution.com</u>.")),
          div(style="text-align:center;margin-top:20px;font-size:140%;color:darkgreen",
              HTML("<br />"),
              HTML("~~ Enjoy yourself in StatsPro ~~")),
          tags$hr(style="border-color: grey60;"),
          div(style="text-align:center;margin-top: 20px;margin-bottom: 20px;font-size:100%",
              HTML(" &copy; 2021 <a href='http://english.cd120.com/' target='_blank'>Hao Yang's Group</a>. All Rights Reserved."))
        ),
        column(3)
      )
    )
  })
  examplepeakdatas<-reactive({
    library(writexl)
    if(input$datatypex==1){
      dataread<-read.csv("Exampledata1.csv",stringsAsFactors = F,check.names = F)
    }
    else if(input$datatypex==2){
      dataread<-read.csv("Exampledata2.txt",stringsAsFactors = F,check.names = F,sep="\t")
    }
    else if(input$datatypex==3){
      dataread<-read_excel("Exampledata3.xlsx")
    }
    else{
      dataread<-read.csv("Exampledata4.csv",stringsAsFactors = F,check.names = F,na.strings = "Filtered")
    }
    dataread
  })
  examplesampledatas<-reactive({
    if(input$datatypex==1 | input$datatypex==2 | input$datatypex==3){
      dataread<-read.csv("grinfo.csv",header = T,stringsAsFactors = F,check.names = F)
    }else{
      dataread<-read.csv("grinfo2.csv",header = T,stringsAsFactors = F,check.names = F)
    }
    colnames(dataread)<-c("Samples","Groups")
    dataread
  })
  output$loaddatadownload1<-downloadHandler(
    filename = function(){paste("Example_ExpressionData_Normal_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplepeakdatas(),file,row.names = FALSE)
    }
  )
  output$loaddatadownload2<-downloadHandler(
    filename = function(){paste("Example_ExpressionData_MaxQuant_",usertimenum,".txt",sep="")},
    content = function(file){
      write.table(examplepeakdatas(),file,row.names = FALSE,sep="\t")
    }
  )
  output$loaddatadownload3<-downloadHandler(
    filename = function(){paste("Example_ExpressionData_PD_",usertimenum,".xlsx",sep="")},
    content = function(file){
      write_xlsx(examplepeakdatas(),file)
    }
  )
  output$loaddatadownload4<-downloadHandler(
    filename = function(){paste("Example_ExpressionData_Spectronaut_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplepeakdatas(),file,row.names = FALSE)
    }
  )
  #output$loaddatadownload2<-downloadHandler(
  #  filename = function(){paste("Example_SampleData_",usertimenum,".csv",sep="")},
  #  content = function(file){
  #    write.csv(examplesampledatas(),file,row.names = FALSE)
  #  }
  #)
  peaksdataout<-reactive({
    if(input$datatypex==1){
      files1 <- input$file1
      if (is.null(files1)){
        dataread<-data.frame(Description="StatsPro detects that you did not upload your data. Please upload the expression data, or load the example data to check first.")
        list(yuanshidf=dataread)
      }else{
        if(sum(input$firstcol)==1){
          rownametf<-1
        }else{
          rownametf<-NULL
        }
        dataread<-read.csv(files1$datapath,header=input$header,
                           row.names = rownametf,stringsAsFactors = F)
        dataread1<-dataread[,-c(1,2)]
        dataread2<-dataread[,c(1,2)]
        #rowpaste<-apply(dataread2,1,function(x){
        #  paste0(x,collapse = "_")
        #})
        rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
        dataread1x<-dataread1[!duplicated(rowpaste),]
        rownames(dataread1x)<-rowpaste[!duplicated(rowpaste)]
        list(yuanshidf=dataread,yuanshidata=dataread1x,objectinfo=dataread2)
      }
    }
    else if(input$datatypex==2){
      files2 <- input$file2
      if (is.null(files2)){
        dataread<-data.frame(Description="StatsPro detects that you did not upload your data. Please upload the proteinGroups.txt file from MaxQuant, or load the example data to check first.")
        list(yuanshidf=dataread)
      }else{
        dataread<-read.csv(files2$datapath,stringsAsFactors = F,check.names = F,sep="\t")
        datamaxqpro1<-dataread[-which(dataread$Reverse=="+" | dataread$`Potential contaminant`=="+"),]
        proidsstr<-strsplit(datamaxqpro1$`Protein IDs`,";")
        proids<- as.character(lapply(proidsstr,function(x) x[[1]][1]))
        datamaxqpro1$`Protein IDs`<-proids
        prolfdnames1<-grep("LFQ intensity",colnames(datamaxqpro1),value = TRUE)
        datamaxqpro2<-datamaxqpro1[,c("Protein IDs","Razor + unique peptides",prolfdnames1)]
        prolfdnames<-gsub("LFQ intensity ","",prolfdnames1)
        colnames(datamaxqpro2)<-c("Protein IDs","PeptidesNum",prolfdnames)
        datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
        datamaxqpro4<-datamaxqpro3[,-c(1,2)]
        dataread2<-datamaxqpro3[,c(1,2)]
        #rowpaste<-apply(dataread2,1,function(x){
        #  paste0(x,collapse = "_")
        #})
        rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
        rownames(datamaxqpro4)<-rowpaste#datamaxqpro3[,1]
        pro0index<-apply(datamaxqpro4,1,function(x) sum(x)==0)
        datamaxqpro5<-datamaxqpro4[!pro0index,]
        datamaxqpro5[datamaxqpro5==0]<-0
        list(yuanshidf=dataread,yuanshidata=datamaxqpro5,objectinfo=dataread2)
      }
    }
    else if(input$datatypex==3){
      files3 <- input$file3
      if (is.null(files3)){
        dataread<-data.frame(Description="StatsPro detects that you did not upload your data. Please upload that Proteins file (.xlsx) exported from Proteome Discoverer, or load the example data to check first.")
        list(yuanshidf=dataread)
      }else{
        dataread<-read_excel(files3$datapath)
        datamaxqpro1<-dataread
        prolfdnames1<-grep("Abundances \\(Scaled\\): ",colnames(datamaxqpro1),value = TRUE)
        datamaxqpro2<-datamaxqpro1[,c("Accession","# Peptides",prolfdnames1)]
        prolfdnames<-gsub("Abundances \\(Scaled\\): ","",prolfdnames1)
        colnames(datamaxqpro2)<-c("Protein IDs","PeptidesNum",prolfdnames)
        datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
        datamaxqpro4<-as.data.frame(datamaxqpro3[,-c(1,2)])
        dataread2<-datamaxqpro3[,c(1,2)]
        #rowpaste<-apply(dataread2,1,function(x){
        #  paste0(x,collapse = "_")
        #})
        rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
        rownames(datamaxqpro4)<-rowpaste#datamaxqpro3[,1]
        #pro0index<-apply(datamaxqpro4,1,function(x) sum(x)==0)
        datamaxqpro5<-datamaxqpro4#[!pro0index,]
        list(yuanshidf=dataread,yuanshidata=datamaxqpro5,objectinfo=dataread2)
      }
    }
    else{
      files4 <- input$file4
      if (is.null(files4)){
        dataread<-data.frame(Description="StatsPro detects that you did not upload your data. Please upload the Protein Quant file (.csv) exported from Spectronaut, or load the example data to check first.")
        list(yuanshidf=dataread)
      }else{
        dataread<-read.csv(files4$datapath,stringsAsFactors = F,check.names = F,na.strings = "Filtered")
        prolfdnames1<-grep("NrOfPrecursorsUsedForQuantification",colnames(dataread),value = TRUE)
        datamaxqpro1<-dataread[,prolfdnames1]
        PeptidesNum<-apply(datamaxqpro1,1,sum,na.rm=TRUE)
        proidsstr<-strsplit(dataread$PG.ProteinGroups,";")
        proids<- as.character(lapply(proidsstr,function(x) x[[1]][1]))
        prolfdnames1<-grep("PG\\.Quantity",colnames(dataread),value = TRUE)
        datamaxqpro2<-dataread[,c("PG.ProteinGroups",prolfdnames1)]
        prolfdnames<-gsub("\\[\\d+\\] |\\.raw\\.PG\\.Quantity","",prolfdnames1)
        colnames(datamaxqpro2)<-c("Protein IDs",prolfdnames)
        datamaxqpro2$`Protein IDs`<-proids
        datamaxqpro2$PeptidesNum<-PeptidesNum
        datamaxqpro2<-datamaxqpro2[,c(1,ncol(datamaxqpro2),2:(ncol(datamaxqpro2)-1))]
        datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
        datamaxqpro4<-datamaxqpro3[,-c(1,2)]
        dataread2<-datamaxqpro3[,c(1,2)]
        #rowpaste<-apply(dataread2,1,function(x){
        #  paste0(x,collapse = "_")
        #})
        rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
        rownames(datamaxqpro4)<-rowpaste#datamaxqpro3[,1]
        #pro0index<-apply(datamaxqpro4,1,function(x) sum(x)==0)
        datamaxqpro5<-datamaxqpro4#[!pro0index,]
        list(yuanshidf=dataread,yuanshidata=datamaxqpro5,objectinfo=dataread2)
      }
    }
  })
  output$peaksdata<-renderDataTable({
    library(data.table)
    aaxxc<<-peaksdataout()
    if(input$loaddatatype==1){
      datatable(peaksdataout()$yuanshidf, options = list(pageLength = 10))
    }else{
      datatable(examplepeakdatas(), options = list(pageLength = 10))
    }
  })
  ##
  nadataout<-reactive({
    if(input$loaddatatype==1){
      nadatax<-peaksdataout()$yuanshidata
    }else{
      dataread<-examplepeakdatas()
      if(input$datatypex==1){
        dataread1<-dataread[,-c(1,2)]
        dataread2<-dataread[,c(1,2)]
        #rowpaste<-apply(dataread2,1,function(x){
        #  paste0(x,collapse = "_")
        #})
        rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
        dataread1x<-dataread1[!duplicated(rowpaste),]
        rownames(dataread1x)<-rowpaste[!duplicated(rowpaste)]
      }
      else if(input$datatypex==2){
        datamaxqpro1<-dataread[-which(dataread$Reverse=="+" | dataread$`Potential contaminant`=="+"),]
        proidsstr<-strsplit(datamaxqpro1$`Protein IDs`,";")
        proids<- as.character(lapply(proidsstr,function(x) x[[1]][1]))
        datamaxqpro1$`Protein IDs`<-proids
        prolfdnames1<-grep("LFQ intensity",colnames(datamaxqpro1),value = TRUE)
        datamaxqpro2<-datamaxqpro1[,c("Protein IDs","Razor + unique peptides",prolfdnames1)]
        prolfdnames<-gsub("LFQ intensity ","",prolfdnames1)
        colnames(datamaxqpro2)<-c("Protein IDs","PeptidesNum",prolfdnames)
        datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
        datamaxqpro4<-datamaxqpro3[,-c(1,2)]
        dataread2<-datamaxqpro3[,c(1,2)]
        #rowpaste<-apply(dataread2,1,function(x){
        #  paste0(x,collapse = "_")
        #})
        rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
        rownames(datamaxqpro4)<-rowpaste#datamaxqpro3[,1]
        pro0index<-apply(datamaxqpro4,1,function(x) sum(x)==0)
        dataread1x<-datamaxqpro4[!pro0index,]
        dataread1x[dataread1x==0]<-NA
        dataread1x
      }
      else if(input$datatypex==3){
        datamaxqpro1<-dataread
        prolfdnames1<-grep("Abundances \\(Scaled\\): ",colnames(datamaxqpro1),value = TRUE)
        datamaxqpro2<-datamaxqpro1[,c("Accession","# Peptides",prolfdnames1)]
        prolfdnames<-gsub("Abundances \\(Scaled\\): ","",prolfdnames1)
        colnames(datamaxqpro2)<-c("Protein IDs","PeptidesNum",prolfdnames)
        datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
        datamaxqpro4<-as.data.frame(datamaxqpro3[,-c(1,2)])
        dataread2<-datamaxqpro3[,c(1,2)]
        #rowpaste<-apply(dataread2,1,function(x){
        #  paste0(x,collapse = "_")
        #})
        rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
        rownames(datamaxqpro4)<-rowpaste
        dataread1x<-datamaxqpro4
      }
      else{
        prolfdnames1<-grep("NrOfPrecursorsUsedForQuantification",colnames(dataread),value = TRUE)
        datamaxqpro1<-dataread[,prolfdnames1]
        datamaxqpro1[] <- lapply(datamaxqpro1, function(x) as.numeric(as.character(x)))
        PeptidesNum<-apply(datamaxqpro1,1,sum,na.rm=TRUE)
        proidsstr<-strsplit(dataread$PG.ProteinGroups,";")
        proids<- as.character(lapply(proidsstr,function(x) x[[1]][1]))
        prolfdnames1<-grep("PG\\.Quantity",colnames(dataread),value = TRUE)
        datamaxqpro2<-dataread[,c("PG.ProteinGroups",prolfdnames1)]
        prolfdnames<-gsub("\\[\\d+\\] |\\.raw\\.PG\\.Quantity","",prolfdnames1)
        colnames(datamaxqpro2)<-c("Protein IDs",prolfdnames)
        datamaxqpro2$`Protein IDs`<-proids
        datamaxqpro2$PeptidesNum<-PeptidesNum
        datamaxqpro2<-datamaxqpro2[,c(1,ncol(datamaxqpro2),2:(ncol(datamaxqpro2)-1))]
        datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
        datamaxqpro4<-datamaxqpro3[,-c(1,2)]
        dataread2<-datamaxqpro3[,c(1,2)]
        #rowpaste<-apply(dataread2,1,function(x){
        #  paste0(x,collapse = "_")
        #})
        rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
        rownames(datamaxqpro4)<-rowpaste
        dataread1x<-datamaxqpro4
      }
      nadatax<-dataread1x
    }
    nadatax[nadatax==input$natype]<-NA
    nadatax[] <- lapply(nadatax, function(x) as.numeric(as.character(x)))
    nadatax
  })
  plot_missing_xiu<-function (data, title = NULL){
    feature <- num_missing <- pct_missing <- group <- NULL
    is_data_table <- is.data.table(data)
    data_class <- class(data)
    if (!is_data_table) {
      data <- data.table(data)
    }
    missing_value <- data.table(feature = names(data), num_missing = sapply(data,function(x) {sum(is.na(x))}))
    missing_value[, `:=`(feature, factor(feature, levels = feature[order(-rank(num_missing))]))]
    missing_value[, `:=`(pct_missing, num_missing/nrow(data))]
    missing_value[pct_missing < 0.2, `:=`(group, "Good")]
    missing_value[pct_missing >= 0.2 & pct_missing < 0.6, `:=`(group,"OK")]
    missing_value[pct_missing >= 0.6 & pct_missing < 0.8, `:=`(group,"Bad")]
    missing_value[pct_missing >= 0.8, `:=`(group, "Remove")]
    if (!is_data_table) {
      class(missing_value) <- data_class
    }
    output <- ggplot(missing_value, aes_string(x = "feature",y = "num_missing", fill = "group")) +
      geom_bar(stat = "identity",colour = "black", alpha = 0.7) +
      geom_text(aes(label = paste0(round(100 *pct_missing, 2), "%")), hjust = -0.15, size = 4) +
      scale_fill_manual("Group", values = c(Good = "#1FB04C",OK = "#9AD94F", Bad = "#FDA249", Remove = "#D71316"),
                        breaks = c("Good", "OK", "Bad", "Remove")) + coord_flip() +
      xlab("Features") + ylab("Number of missing rows") +
      ylim(c(0,max(missing_value$num_missing)+10))+
      ggtitle(title)
    output
  }
  preheightx<-reactive({
    input$preheight
  })
  observeEvent(
    input$mcsbtn_nafenbu,{
      shinyjs::show(id = "mcsbtn_nafenbu_hid", anim = FALSE)
      output$nadatadf<-renderDataTable({
        datatable(nadataout(), options = list(pageLength = 20))
      })
      output$nadatadfdl<-downloadHandler(
        filename = function(){paste("NAdata_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(nadataout(),file)
        }
      )
      output$naplotbycolumn<-renderPlot({
        dfx<-nadataout()
        plot_missing_xiu(dfx)+theme_bw()+
          theme(legend.position = c("bottom"),axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = preheightx)
      naplotbycolumnout<-reactive({
        dfx<-nadataout()
        plot_missing_xiu(dfx)+theme_bw()+
          theme(legend.position = c("bottom"),axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      })
      output$naplotbycolumndl<-downloadHandler(
        filename = function(){paste("NAplot_bycolumn_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = preheightx()/100+1,height = preheightx()/100+1)
          print(naplotbycolumnout())
          dev.off()
        }
      )
      output$naplotbyrow<-renderPlot({
        library(Amelia)
        natypecolx<-strsplit("grey80;#3C5488FF",";|,")[[1]]#input$natypecol
        dfx<-nadataout()
        tdatamaxqproNA<-as.data.frame(t(dfx))
        colnames(tdatamaxqproNA)<-paste0("Row ",1:ncol(tdatamaxqproNA))
        missmap(tdatamaxqproNA,y.labels=rev(colnames(dfx)),x.cex = 0.5, col = natypecolx)
      },height = preheightx)
      naplotbyrowout<-reactive({
        natypecolx<-strsplit("grey80;#3C5488FF",";|,")[[1]]
        dfx<-nadataout()
        tdatamaxqproNA<-as.data.frame(t(dfx))
        colnames(tdatamaxqproNA)<-paste0("Row ",1:ncol(tdatamaxqproNA))
        missmap(tdatamaxqproNA,y.labels=rev(colnames(dfx)),x.cex = 0.5, col = natypecolx)
      })
      output$naplotbyrowdl<-downloadHandler(
        filename = function(){paste("NAplot_byrow_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = preheightx()/100+1,height = preheightx()/100+1)
          print(naplotbyrowout())
          dev.off()
        }
      )
    }
  )
  #
  filtereddatadfout<-reactive({
    if(input$loaddatatype==1){
      grnames1<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      grnum2<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
      grnames<-rep(grnames1,times=grnum2)
    }else{
      if(input$datatypex==4){
        grnames1<-strsplit(input$examgrnames2,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums2,";")[[1]][1])
        grnum2<-as.numeric(strsplit(strsplit(input$examgrnums2,";")[[1]][2],"-")[[1]])
        grnames<-rep(grnames1,times=grnum2)
      }else{
        grnames1<-strsplit(input$examgrnames,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        grnum2<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
        grnames<-rep(grnames1,times=grnum2)
      }
    }
    datadf<-nadataout()
    naratiox<-input$naratio
    if(input$fenzukaolvif){
      nastatsdf<-NULL
      nastatsdf0<-NULL
      for(i in 1:length(grnames1)){
        dataindex<-datadf[,grnames==grnames1[i]]
        narowsumix<-apply(dataindex,1,function(x){sum(is.na(x))})==ncol(dataindex)
        narowsumi<-apply(dataindex,1,function(x){sum(is.na(x))})/ncol(dataindex)
        dataindex1<-data.frame(nabili=narowsumi<=naratiox)
        nastatsdf<-cbind(nastatsdf,as.matrix(dataindex1))
        nastatsdf0<-cbind(nastatsdf0,as.matrix(dataindex))
      }
      nafenzuindex<-apply(nastatsdf,1,function(x){
        if(all(x)){
          return(TRUE)
        }else{
          return(FALSE)
        }
      })
      datadfchuli<-datadf[nafenzuindex,]
      datadfchuli<-as.data.frame(datadfchuli)
    }else{
      narowsum<-apply(datadf,1,function(x){sum(is.na(x))})/ncol(datadf)
      datadfchuli<-datadf[narowsum<=input$naratio,]
    }
    datadfchulix<-datadfchuli
    if(input$mediannormif){
      medianval<-apply(datadfchuli,2,function(x) {median(x,na.rm = TRUE)})
      datadfchuli<-sweep(datadfchuli,2,medianval,FUN = "/")
    }
    if(input$logif){
      datadfchuli<-log2(datadfchuli)
    }
    list(datadfchuli=datadfchuli,datadfchuli_nonorm=datadfchulix)
  })
  cvfilterdataout<-reactive({
    dfxx<-filtereddatadfout()$datadfchuli
    dfx1<-filtereddatadfout()$datadfchuli_nonorm
    if(input$mediannormif){
      medianval<-apply(dfx1,2,function(x) {median(x,na.rm = TRUE)})
      dfx<-sweep(dfx1,2,medianval,FUN = "/")
    }else{
      dfx<-dfx1
    }
    if(input$loaddatatype==1){
      grnames1<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      grnum2<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
      grnames<-rep(grnames1,times=grnum2)
    }else{
      if(input$datatypex==4){
        grnames1<-strsplit(input$examgrnames2,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums2,";")[[1]][1])
        grnum2<-as.numeric(strsplit(strsplit(input$examgrnums2,";")[[1]][2],"-")[[1]])
        grnames<-rep(grnames1,times=grnum2)
      }else{
        grnames1<-strsplit(input$examgrnames,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        grnum2<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
        grnames<-rep(grnames1,times=grnum2)
      }
    }
    cvdf<-cvifdf<-NULL
    for(i in 1:length(grnames1)){
      datai<-dfx[,grnames==grnames1[i]]
      cvi<-apply(datai,1,function(x){
        if(all(as.numeric(x)==0)|is.na(all(as.numeric(x)==0))){
          0
        }else{
          raster::cv(as.numeric(x),na.rm = TRUE,aszero=TRUE)
        }
      })
      cvdf<-cbind(cvdf,cvi)
      cvifdf<-cbind(cvifdf,cvi>input$cvyuzhi*100)
    }
    cvifdf1<-apply(cvifdf,1,any)
    dfxx1<-dfxx[!cvifdf1,]
    dfxx00<-apply(dfxx1, 1, sum,na.rm = TRUE)
    dfxx2<-dfxx1[dfxx00!=0,]
    round(dfxx2,digits=5)
  })
  knnimputeresout<-reactive({
    library(impute)
    df1<-cvfilterdataout()
    data_zero1<-impute.knn(as.matrix(df1),k = 10, rowmax = 1, colmax = 1)
    df<-data_zero1$data
    df
  })
  observeEvent(
    input$mcsbtn_nafilter,{
      shinyjs::show(id = "mcsbtn_nafilter_hid", anim = FALSE)
      output$filtereddatadf<-renderDataTable({
        datatable(cvfilterdataout(), options = list(pageLength = 20))
      })
      output$filtereddatadfdl<-downloadHandler(
        filename = function(){paste("Filtereddata_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(cvfilterdataout(),file)
        }
      )
      output$knnimputeres<-renderDataTable({
        datatable(knnimputeresout(), options = list(pageLength = 20))
      })
      output$knnimputeresdl<-downloadHandler(
        filename = function(){paste("Data_KNNimputed_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(knnimputeresout(),file)
        }
      )
    }
  )
  #
  namethodsoutxx<-reactive({
    namethods<-vector()
    if(input$ttestif) namethods[1]<-"ttest"
    if(input$aovif) namethods[2]<-"aov"
    if(input$wiltestif) namethods[3]<-"limma"
    if(input$kwtestif) namethods[4]<-"sam"
    if(input$permif) namethods[5]<-"wiltest"
    if(input$limmaif) namethods[6]<-"kwtest"
    if(input$samif) namethods[7]<-"perm"
    if(input$rankproif) namethods[8]<-"rankpro"
    if(input$rotsif) namethods[9]<-"rots"
    if(input$msqrobsumif) namethods[10]<-"msqrobsum"
    if(input$deqmsif) namethods[11]<-"deqms"
    if(input$plgemif) namethods[12]<-"plgem"
    namethods
  })
  namethodsout<-eventReactive(input$mcsbtn_statsxII,{
    namethodsoutxx()
  })
  output$suijidataresultsui<-renderUI({
    namethodsoutx<-tolower(na.omit(namethodsout()))
    selectInput("suijijieguoindex","",
                choices = namethodsoutx)
  })
  statsfunctions<-function(x,classnames,classnamesnum,method="ttest"){
    df<-as.data.frame(x)
    classnames<-classnames
    classnamesnum<-classnamesnum
    grinfo<-rep(classnames,times=classnamesnum)
    method<-tolower(method)
    if(method=="ttest"){
      ttestpval<-apply(df,1,function(x){
        x1<-t.test(as.numeric(x)~grinfo)
        x1$p.value
      })
      df$p.t.value<-ttestpval
      df$p.t.adjust<-p.adjust(ttestpval,method = "BH")
    }else if(method=="aov"){
      testdata2<-df
      p.aov.value<-vector()
      for(i in 1:nrow(testdata2)){
        aovdf<-data.frame(name=as.factor(rep(paste("A",1:length(classnames),sep = ""),times=classnamesnum)),numval=as.numeric(testdata2[i,]))
        aovtest<-summary(aov(numval~name,data=aovdf))
        p.aov.value[i]<-aovtest[[1]][["Pr(>F)"]][1]#round(aovtest[[1]][["Pr(>F)"]][1],7)
      }
      testdata2$p.aov.value<-p.aov.value
      testdata2$p.aov.adjust<-p.adjust(p.aov.value,method = "BH")
      df<-testdata2
    }else if(method=="wiltest"){
      testdata3<-df
      p.wilcox.value<-apply(testdata3,1,function(x){
        x1<-wilcox.test(as.numeric(x)~grinfo)
        x1$p.value
      })
      testdata3$p.wilcox.value<-p.wilcox.value
      testdata3$p.wilcox.adjust<-p.adjust(p.wilcox.value,method = "BH")
      df<-testdata3
    }else if(method=="kwtest"){
      testdata4<-df
      p.kruskal.value<-vector()
      for(i in 1:nrow(testdata4)){
        aovdf<-data.frame(name=as.factor(rep(paste("A",1:length(classnames),sep = ""),times=classnamesnum)),numval=as.numeric(testdata4[i,]))
        kruskaltest<-kruskal.test(numval~name,data=aovdf)
        p.kruskal.value[i]<-kruskaltest$p.value
      }
      testdata4$p.kruskal.value<-p.kruskal.value
      testdata4$p.kruskal.adjust<-p.adjust(p.kruskal.value,method = "BH")
      df<-testdata4
    }else if(method=="perm"){
      library(coin)
      library(exactRankTests)
      testdata13<-df
      p.perm.value<-vector()
      if(length(classnames)==2){
        for(i in 1:nrow(testdata13)){
          permdf<-data.frame(name=as.factor(rep(paste("A",1:length(classnames),sep = ""),times=classnamesnum)),numval=as.numeric(testdata13[i,]))
          x1<-perm.test(numval~name,data=permdf)
          p.perm.value[i]<-x1$p.value
        }
      }else{
        for(i in 1:nrow(testdata13)){
          permdf<-data.frame(name=as.factor(rep(paste("A",1:length(classnames),sep = ""),times=classnamesnum)),numval=as.numeric(testdata13[i,]))
          x1<-oneway_test(numval~name,data=permdf,distribution = approximate(nresample = 499))
          p.perm.value[i]<-as.numeric(coin::pvalue(x1))
        }
      }
      testdata13$p.perm.value<-p.perm.value
      testdata13$p.perm.adjust<-p.adjust(p.perm.value,method = "BH")
      df<-testdata13
    }else if(method=="limma"){
      library(limma)
      testdata6<-df
      designx <- model.matrix(~factor(rep(1:length(classnames),classnamesnum)))
      colnames(designx)<-c("Intercept",paste0("Name",1:(ncol(designx)-1)))
      lmFitdf1 <- lmFit(as.matrix(testdata6), designx)
      lmFitdf <- eBayes(lmFitdf1)
      lmFitdftable<-as.data.frame(topTable(lmFitdf,coef=2,number = Inf,sort.by="none"))
      testdata6$p.limma.value<-lmFitdftable$P.Value
      testdata6$p.limma.adjust<-p.adjust(lmFitdftable$P.Value,method = "BH")
      df<-testdata6
    }else if(method=="sam"){
      library(samr)
      logdataif<-input$logif
      testdata5<-df
      samrdatalist<-list(x=as.matrix(testdata5),y=rep(c(1:length(classnames)),times=classnamesnum),
                         geneid=as.character(1:nrow(testdata5)),
                         genenames=rownames(testdata5), logged2=logdataif)
      samr.obj<-samr(samrdatalist, resp.type=input$samtype, nperms=1000,random.seed=1234567)#"Two class unpaired"
      pv<-samr.pvalues.from.perms(samr.obj$tt, samr.obj$ttstar)
      pv[pv>=1]<-0.999
      testdata5$p.sam.value<-pv
      testdata5$p.sam.fdr<-pv
      df<-testdata5
    }else if(method=="rankpro"){
      library(RankProd)
      logdataif<-input$logif
      testdata7<-df
      RP.outx <- RankProducts(as.matrix(testdata7),rep(0:(length(classnames)-1),classnamesnum),
                              rand=123,logged = logdataif)
      testdata7$p.rp.value<-apply(RP.outx$pval,1,min)
      testdata7$p.rp.fdr<-apply(RP.outx$pfp,1,min)
      testdata7$p.rp.fdr[testdata7$p.rp.fdr>1]<-1
      df<-testdata7
    }else if(method=="rots"){
      library(ROTS)
      logdataif<-input$logif
      testdata8<-df
      rots.out <- ROTS(data = as.matrix(testdata8), groups = rep(0:(length(classnames)-1),classnamesnum),
                       B = 1000, K = NULL, log = logdataif, seed = 1234)
      testdata8$p.rots.value<-rots.out$pvalue
      testdata8$p.rots.fdr<-rots.out$FDR
      df<-testdata8
    }else if(method=="msqrobsum"){
      library(msqrobsum)
      library(MSnbase)
      testdata9<-df
      proteindf<-data.frame(protein=rownames(testdata9),stringsAsFactors=FALSE)
      rownames(proteindf)<-rownames(testdata9)
      conditiondf<-data.frame(condition=grinfo,stringsAsFactors=FALSE)
      rownames(conditiondf)<-colnames(testdata9)
      setx<-MSnSet(exprs=as.matrix(testdata9), fData=AnnotatedDataFrame(proteindf), pData=AnnotatedDataFrame(conditiondf))
      formx<-expression ~ (1|condition)
      msqrobsumres<-msqrobsum(data = setx, formulas = formx, mode = 'msqrobsum', group_vars = 'protein',
                              contrasts = 'condition', parallel_args = list(strategy = 'sequential'))
      testdata9$p.msqrobsum.value<-unlist(lapply(msqrobsumres$contrasts,function(x){
        x$pvalue
      }))
      testdata9$p.msqrobsum.adjust<-unlist(lapply(msqrobsumres$contrasts,function(x){
        x$qvalue
      }))
      df<-testdata9
    }else if(method=="deqms"){
      library(DEqMS)
      testdata10<-df
      classx<-as.factor(grinfo)
      designx<-model.matrix(~0+classx)
      fit1<-lmFit(as.matrix(testdata10),design = designx)
      grinfoxx<<-paste0("classx",classnames,collapse = "-")
      cont<-makeContrasts(grinfoxx, levels = designx)
      fit2<-contrasts.fit(fit1,contrasts = cont)
      fit3<-eBayes(fit2)
      fit3$count<-as.numeric(unlist(lapply(rownames(testdata10),function(x){strsplit(x,"_")[[1]][2]})))
      fit4<-spectraCounteBayes(fit3)
      deqmsres<-outputResult(fit4, coef_col=1)
      testdata10$p.deqms.value<-deqmsres$sca.P.Value
      testdata10$p.deqms.adjust<-deqmsres$sca.adj.pval
      df<-testdata10
    }
    else if(method=="plgem"){
      library(plgem)
      library(MSnbase)
      testdata12<-df
      proteindf<-data.frame(protein=rownames(testdata12),stringsAsFactors=FALSE)
      rownames(proteindf)<-rownames(testdata12)
      conditiondf<-data.frame(condition=grinfo,stringsAsFactors=FALSE)
      rownames(conditiondf)<-colnames(testdata12)
      setxx<-ExpressionSet(assayData=as.matrix(testdata12), featureData=AnnotatedDataFrame(proteindf),
                           phenoData=AnnotatedDataFrame(conditiondf))
      LPSfitx <- plgem.fit(data=setxx, covariate=1, fitCondition=classnames[1], p=10, q=0.5,
                           plot.file=FALSE, fittingEval=TRUE, verbose=TRUE)
      LPSobsStn <- plgem.obsStn(data=setxx, covariate=1, baselineCondition=1,plgemFit=LPSfitx, verbose=TRUE)
      set.seed(123)
      LPSresampledStn <- plgem.resampledStn(data=setxx, plgemFit=LPSfitx,iterations="automatic", verbose=TRUE)
      LPSpValues <- plgem.pValue(observedStn=LPSobsStn,plgemResampledStn=LPSresampledStn, verbose=TRUE)
      testdata12$p.plgem.value<-LPSpValues[,1]
      testdata12$p.plgem.fdr<-LPSpValues[,1]
      df<-testdata12
    }
    else{
      stop("Unspported methods so far~~")
    }
    dfx<-as.data.frame(df)
    set.seed(1234)
    dfx1<-dfx[,c(ncol(dfx)-1,ncol(dfx))]
    dfx1[dfx1==0]<-sample(seq(.0000001,0.001,length.out = 2*length(dfx1[dfx1==0])),length(dfx1[dfx1==0]))
    df<-cbind(dfx[,-c(ncol(dfx)-1,ncol(dfx))],dfx1)
    df
  }
  pvalueresdfout<-reactive({
    library(effsize)
    namethodsoutx<<-tolower(na.omit(namethodsout()))
    namethodsoutx1<-c(namethodsoutx,"Finish")
    statsdatadfx<<-knnimputeresout()
    if(input$loaddatatype==1){
      classnames<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
    }else{
      if(input$datatypex==4){
        classnames<-strsplit(input$examgrnames2,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums2,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums2,";")[[1]][2],"-")[[1]])
      }else{
        classnames<-strsplit(input$examgrnames,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
      }
    }
    classnames<<-classnames
    classnamesnum<<-classnamesnum
    grinfo<<-rep(classnames,times=classnamesnum)
    cohendd<-apply(statsdatadfx,1,function(x){
      x1<-cohen.d(as.numeric(x)~grinfo,pooled=FALSE)
      x1$estimate
    })
    withProgress(message = 'Testing data with ', style = "notification", detail = paste0("1 ",namethodsoutx[1]), value = 0,{
      statstestreslist<-list()
      for(i in 1:length(namethodsoutx)){
        statstestreslisti<-statsfunctions(statsdatadfx,classnames=classnames,classnamesnum=classnamesnum,
                                              method=namethodsoutx[i])
        if(length(classnames)==2){
          if(input$logif){
            statstestreslisti$fold.change<-round(rowMeans(statstestreslisti[,(classnamesnum[1]+1):(sum(classnamesnum))])-
                                                   rowMeans(statstestreslisti[,1:classnamesnum[1]]),7)
          }else{
            statstestreslisti$fold.change<-round(rowMeans(statstestreslisti[,(classnamesnum[1]+1):(sum(classnamesnum))])/
                                                   rowMeans(statstestreslisti[,1:classnamesnum[1]]),7)
          }
        }
        statstestreslisti$cohen.d<-round(cohendd,7)
        statstestreslist[[i]]<-statstestreslisti
        incProgress(1/length(namethodsoutx), detail = paste0(i," ",namethodsoutx[i]," Done ~",
                                                             namethodsoutx1[i+1]," processing..."))
      }
    })
    names(statstestreslist)<-tolower(namethodsoutx)
    statstestreslist
  })
  output$statsIIresui<-renderUI({
    namethodsoutx<-tolower(na.omit(namethodsout()))
    selectInput("topnmethodindex","",choices = namethodsoutx)
  })
  pvalueresdfout2<-reactive({
    pvalueresdfx<-pvalueresdfout()
    namethodsx<-tolower(input$topnmethodindex)
    dataoutx<-pvalueresdfx[[namethodsx]]
    dataoutx
  })
  observeEvent(
    input$mcsbtn_statsxII,{
      namethodsoutx<-tolower(na.omit(namethodsout()))
      showModal(modalDialog(
        title = "Selected Methods",
        div("Dear user, you have chosen several methods as below:",style = "font-size:120%;"),br(),
        div(HTML(paste(paste0("&nbsp;&nbsp;&nbsp&nbspMethod ",1:length(namethodsoutx)),namethodsoutx,sep = ": ",collapse = "</br>"))),br(),
        div("Then click 'OK' and move on...",style = "font-size:120%;"),br(),
        size ="l",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      output$pvalueresdf<-renderDataTable({
        datatable(pvalueresdfout2(), options = list(pageLength = 20))
      })
      output$pvalueresdfdl<-downloadHandler(
        filename = function(){paste("StatsResult_",input$topnmethodindex,"_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(pvalueresdfout2(),file)
        }
      )
    }
  )
  ##plots
  output$pvalueresplot<-renderPlot({
    library(patchwork)
    namethodsx<-tolower(input$topnmethodindex)
    if(input$loaddatatype==1){
      classnames<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
    }else{
      if(input$datatypex==4){
        classnames<-strsplit(input$examgrnames2,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums2,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums2,";")[[1]][2],"-")[[1]])
      }else{
        classnames<-strsplit(input$examgrnames,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
      }
    }
    grinfo<-rep(classnames,times=classnamesnum)
    pvalueresplotdf<<-pvalueresdfout2()
    pvaldf<-data.frame(Values=c(pvalueresplotdf[[sum(classnamesnum)+1]],pvalueresplotdf[[sum(classnamesnum)+2]]),
                       Groups=rep(c("p.value","p.adjust"),each=nrow(pvalueresplotdf)))
    pvaldf$Groups<-factor(pvaldf$Groups,levels = c("p.value","p.adjust"))
    pvaluethresholdx<-input$pvaluethreshold
    xx1<-paste0("p.value (No. < ",pvaluethresholdx,": ",sum(pvalueresplotdf[[sum(classnamesnum)+1]]<pvaluethresholdx),")")
    xx2<-paste0("p.adjust (No. < ",pvaluethresholdx,": ",sum(pvalueresplotdf[[sum(classnamesnum)+2]]<pvaluethresholdx),")")
    pp1<-ggplot(pvaldf, aes(x=Values, color=Groups, fill=Groups)) +
      geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
      geom_density(alpha=0.2,size=1)+
      geom_vline(aes(xintercept=pvaluethresholdx),color="grey60",
                 linetype="dashed",size=1)+
      scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(xx1,xx2))+
      scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(xx1,xx2))+
      labs(x="P values",y="Density",title = paste0("Method: ",namethodsx))+
      theme_bw()+
      theme(legend.position="bottom",axis.text=element_text(size=14),
            axis.title=element_text(size=16),legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            panel.grid.minor = element_blank() ,panel.grid.major = element_blank())
    ##
    if(input$logif){
      volcanofcbig<-log2(input$fcthreshold)
      volcanofcsmall<- log2(1/input$fcthreshold)
    }else{
      volcanofcbig<-input$fcthreshold
      volcanofcsmall<- 1/input$fcthreshold
    }
    volcanopval<-pvaluethresholdx
    volcanorawdata<-pvalueresplotdf
    volcanorawdata$p.value<-volcanorawdata[[sum(classnamesnum)+1]]
    volcanorawdata$p.value[which(volcanorawdata$p.value==0)]<-10^-7
    volcanorawdata$Threshold<-c("NoChange")
    volcanorawdata$Threshold[volcanorawdata$fold.change>=volcanofcbig & volcanorawdata$p.value <= volcanopval]<-c("UP")
    volcanorawdata$Threshold[volcanorawdata$fold.change<=volcanofcsmall & volcanorawdata$p.value <= volcanopval]<-c("DOWN")
    pp2<-ggplot(data=volcanorawdata,aes(x=fold.change,y=-log10(p.value)))+theme_bw()+
      #theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())+
      geom_hline(yintercept =-log10(volcanopval),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =volcanofcsmall,col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =volcanofcbig,col="grey",lty=2,lwd=1)+
      geom_point(aes(colour=Threshold),alpha=0.9,size=2)+theme(legend.position ="bottom")+#,shape=Threshold
      scale_color_manual(values = c("DOWN"="#00A087FF","NoChange"="grey80","UP"="#E64B35FF"))+
      xlab("Fold Change (log2)") + ylab("P Value (-log10)")+
      theme(legend.position="bottom",axis.text=element_text(size=14),
            axis.title=element_text(size=16),legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            panel.grid.minor = element_blank() ,panel.grid.major = element_blank())
    ##
    volcanorawdata2<-pvalueresplotdf
    volcanorawdata2$p.adjust<-volcanorawdata2[[sum(classnamesnum)+2]]
    volcanorawdata2$p.adjust[which(volcanorawdata2$p.adjust==0)]<-10^-7
    volcanorawdata2$Threshold<-c("NoChange")
    volcanorawdata2$Threshold[volcanorawdata2$fold.change>=volcanofcbig & volcanorawdata2$p.adjust <= volcanopval]<-c("UP")
    volcanorawdata2$Threshold[volcanorawdata2$fold.change<=volcanofcsmall & volcanorawdata2$p.adjust <= volcanopval]<-c("DOWN")
    pp3<-ggplot(data=volcanorawdata2,aes(x=fold.change,y=-log10(p.adjust)))+theme_bw()+
      #theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())+
      geom_hline(yintercept =-log10(volcanopval),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =volcanofcsmall,col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =volcanofcbig,col="grey",lty=2,lwd=1)+
      geom_point(aes(colour=Threshold),alpha=0.9,size=2)+theme(legend.position ="bottom")+#,shape=Threshold
      scale_color_manual(values = c("DOWN"="#00A087FF","NoChange"="grey80","UP"="#E64B35FF"))+
      xlab("Fold Change (log2)") + ylab("P adjust (-log10)")+
      theme(legend.position="bottom",axis.text=element_text(size=14),
            axis.title=element_text(size=16),legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            panel.grid.minor = element_blank() ,panel.grid.major = element_blank())
    ##
    pp1/(pp2+pp3)
  })
  pvalueresplotout<-reactive({
    library(patchwork)
    namethodsx<-tolower(input$topnmethodindex)
    if(input$loaddatatype==1){
      classnames<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
    }else{
      if(input$datatypex==4){
        classnames<-strsplit(input$examgrnames2,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums2,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums2,";")[[1]][2],"-")[[1]])
      }else{
        classnames<-strsplit(input$examgrnames,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
      }
    }
    grinfo<-rep(classnames,times=classnamesnum)
    pvalueresplotdf<<-pvalueresdfout2()
    pvaldf<-data.frame(Values=c(pvalueresplotdf[[sum(classnamesnum)+1]],pvalueresplotdf[[sum(classnamesnum)+2]]),
                       Groups=rep(c("p.value","p.adjust"),each=nrow(pvalueresplotdf)))
    pvaldf$Groups<-factor(pvaldf$Groups,levels = c("p.value","p.adjust"))
    pvaluethresholdx<-input$pvaluethreshold
    xx1<-paste0("p.value (No. < ",pvaluethresholdx,": ",sum(pvalueresplotdf[[sum(classnamesnum)+1]]<pvaluethresholdx),")")
    xx2<-paste0("p.adjust (No. < ",pvaluethresholdx,": ",sum(pvalueresplotdf[[sum(classnamesnum)+2]]<pvaluethresholdx),")")
    pp1<-ggplot(pvaldf, aes(x=Values, color=Groups, fill=Groups)) +
      geom_histogram(aes(y=..density..), position="identity", alpha=0.7,bins=60)+
      geom_density(alpha=0.2,size=1)+
      geom_vline(aes(xintercept=pvaluethresholdx),color="grey60",
                 linetype="dashed",size=1)+
      scale_color_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(xx1,xx2))+
      scale_fill_manual(values=c("#7B68EE", "#E64B35FF"),labels = c(xx1,xx2))+
      labs(x="P values",y="Density",title = paste0("Method: ",namethodsx))+
      theme_bw()+
      theme(legend.position="bottom",axis.text=element_text(size=14),
            axis.title=element_text(size=16),legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            panel.grid.minor = element_blank() ,panel.grid.major = element_blank())
    ##
    if(input$logif){
      volcanofcbig<-log2(input$fcthreshold)
      volcanofcsmall<- log2(1/input$fcthreshold)
    }else{
      volcanofcbig<-input$fcthreshold
      volcanofcsmall<- 1/input$fcthreshold
    }
    volcanopval<-pvaluethresholdx
    volcanorawdata<-pvalueresplotdf
    volcanorawdata$p.value<-volcanorawdata[[sum(classnamesnum)+1]]
    volcanorawdata$p.value[which(volcanorawdata$p.value==0)]<-10^-7
    volcanorawdata$Threshold<-c("NoChange")
    volcanorawdata$Threshold[volcanorawdata$fold.change>=volcanofcbig & volcanorawdata$p.value <= volcanopval]<-c("UP")
    volcanorawdata$Threshold[volcanorawdata$fold.change<=volcanofcsmall & volcanorawdata$p.value <= volcanopval]<-c("DOWN")
    pp2<-ggplot(data=volcanorawdata,aes(x=fold.change,y=-log10(p.value)))+theme_bw()+
      #theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())+
      geom_hline(yintercept =-log10(volcanopval),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =volcanofcsmall,col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =volcanofcbig,col="grey",lty=2,lwd=1)+
      geom_point(aes(colour=Threshold),alpha=0.9,size=2)+theme(legend.position ="bottom")+#,shape=Threshold
      scale_color_manual(values = c("DOWN"="#00A087FF","NoChange"="grey80","UP"="#E64B35FF"))+
      xlab("Fold Change (log2)") + ylab("P Value (-log10)")+
      theme(legend.position="bottom",axis.text=element_text(size=14),
            axis.title=element_text(size=16),legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            panel.grid.minor = element_blank() ,panel.grid.major = element_blank())
    ##
    volcanorawdata2<-pvalueresplotdf
    volcanorawdata2$p.adjust<-volcanorawdata2[[sum(classnamesnum)+2]]
    volcanorawdata2$p.adjust[which(volcanorawdata2$p.adjust==0)]<-10^-7
    volcanorawdata2$Threshold<-c("NoChange")
    volcanorawdata2$Threshold[volcanorawdata2$fold.change>=volcanofcbig & volcanorawdata2$p.adjust <= volcanopval]<-c("UP")
    volcanorawdata2$Threshold[volcanorawdata2$fold.change<=volcanofcsmall & volcanorawdata2$p.adjust <= volcanopval]<-c("DOWN")
    pp3<-ggplot(data=volcanorawdata2,aes(x=fold.change,y=-log10(p.adjust)))+theme_bw()+
      #theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())+
      geom_hline(yintercept =-log10(volcanopval),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =volcanofcsmall,col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =volcanofcbig,col="grey",lty=2,lwd=1)+
      geom_point(aes(colour=Threshold),alpha=0.9,size=2)+theme(legend.position ="bottom")+#,shape=Threshold
      scale_color_manual(values = c("DOWN"="#00A087FF","NoChange"="grey80","UP"="#E64B35FF"))+
      xlab("Fold Change (log2)") + ylab("P adjust (-log10)")+
      theme(legend.position="bottom",axis.text=element_text(size=14),
            axis.title=element_text(size=16),legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            panel.grid.minor = element_blank() ,panel.grid.major = element_blank())
    ##
    pp1/(pp2+pp3)
  })
  output$pvalueresplotdl<-downloadHandler(
    filename = function(){paste("Pvalues_figure_",tolower(input$topnmethodindex),usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file, width = 13,height = 14)
      print(pvalueresplotout())
      dev.off()
    }
  )
  #######################
  ##p value combination
  pcombinresout<-reactive({
    library(metaseqR)
    library(survcomp)
    if(input$loaddatatype==1){
      classnames<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
    }else{
      if(input$datatypex==4){
        classnames<-strsplit(input$examgrnames2,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums2,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums2,";")[[1]][2],"-")[[1]])
      }else{
        classnames<-strsplit(input$examgrnames,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
      }
    }
    pvalueresdfx<<-pvalueresdfout()
    pvalueresdfx1<-NULL
    for(i in 1:length(pvalueresdfx)){
      pvalueresdfx1<-cbind(pvalueresdfx1,pvalueresdfx[[i]][[sum(classnamesnum)+1]])
    }
    pvalueresdfx1<-pvalueresdfx2<-as.data.frame(pvalueresdfx1)
    colnames(pvalueresdfx1)<-paste0("p.",names(pvalueresdfx),".value")
    rownames(pvalueresdfx1)<-rownames(pvalueresdfx[[1]])
    p.simes.methodx<-apply(pvalueresdfx2, 1, combine.simes)
    p.fisher.methodx<-fisher.method(pvalueresdfx2, p.corr = "none")$p.value
    p.whitlock.methodx<-apply(pvalueresdfx2, 1, combine.test, method = "z.transform")
    p.maxp.methodx<-apply(pvalueresdfx2, 1, combine.maxp)
    p.minp.methodx<-apply(pvalueresdfx2, 1, combine.minp)
    p.pandora.methodx<-apply(pvalueresdfx2, 1, combine.weight, rep(1/ncol(pvalueresdfx2), ncol(pvalueresdfx2)))
    pvalueresdfx1$p.simes.method<-p.simes.methodx
    pvalueresdfx1$p.fisher.method<-p.fisher.methodx
    pvalueresdfx1$p.whitlock.method<-p.whitlock.methodx
    pvalueresdfx1$p.maxp.method<-p.maxp.methodx
    pvalueresdfx1$p.minp.method<-p.minp.methodx
    pvalueresdfx1$p.pandora.method<-p.pandora.methodx
    pvalueresdfx1$padj.simes.method<-p.adjust(p.simes.methodx,method = "BH")
    pvalueresdfx1$padj.fisher.method<-p.adjust(p.fisher.methodx,method = "BH")
    pvalueresdfx1$padj.whitlock.method<-p.adjust(p.whitlock.methodx,method = "BH")
    pvalueresdfx1$padj.maxp.method<-p.adjust(p.maxp.methodx,method = "BH")
    pvalueresdfx1$padj.minp.method<-p.adjust(p.minp.methodx,method = "BH")
    pvalueresdfx1$padj.pandora.method<-p.adjust(p.pandora.methodx,method = "BH")
    set.seed(1234)
    pvalueresdfx1[pvalueresdfx1==0]<-sample(seq(.0000001,0.001,length.out = 2*length(pvalueresdfx1[pvalueresdfx1==0])),
                                            length(pvalueresdfx1[pvalueresdfx1==0]))
    pvalueresdfx1[,-c(1:length(pvalueresdfx))]
  })
  output$pcombinres<-renderDataTable({
    datatable(pcombinresout())
  })
  output$pcombinresdl<-downloadHandler(
    filename = function(){paste("Pcombination_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(pcombinresout(),file)
    }
  )
  ##performance
  pnumdetecresout<-reactive({
    pvaluethresholdx<-input$pvaluethreshold
    if(input$logif){
      volcanofcbig<-log2(input$fcthreshold)
      volcanofcsmall<- log2(1/input$fcthreshold)
    }else{
      volcanofcbig<-input$fcthreshold
      volcanofcsmall<- 1/input$fcthreshold
    }
    if(input$loaddatatype==1){
      classnames<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
    }else{
      if(input$datatypex==4){
        classnames<-strsplit(input$examgrnames2,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums2,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums2,";")[[1]][2],"-")[[1]])
      }else{
        classnames<-strsplit(input$examgrnames,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
      }
    }
    pvalueresdfx<<-pvalueresdfout()
    pcombinresx<<-pcombinresout()
    pvalueresdfx1<-pvalueresdfx2<-vector()
    for(i in 1:length(pvalueresdfx)){
      pvalueresdfx1[i]<-sum(pvalueresdfx[[i]][[sum(classnamesnum)+1]]<pvaluethresholdx & pvalueresdfx[[i]][[sum(classnamesnum)+3]]>=volcanofcbig)+
        sum(pvalueresdfx[[i]][[sum(classnamesnum)+1]]<pvaluethresholdx & pvalueresdfx[[i]][[sum(classnamesnum)+3]]<=volcanofcsmall)
      pvalueresdfx2[i]<-sum(pvalueresdfx[[i]][[sum(classnamesnum)+2]]<pvaluethresholdx & pvalueresdfx[[i]][[sum(classnamesnum)+3]]>=volcanofcbig)+
        sum(pvalueresdfx[[i]][[sum(classnamesnum)+2]]<pvaluethresholdx & pvalueresdfx[[i]][[sum(classnamesnum)+3]]<=volcanofcsmall)#sum(pvalueresdfx[[i]][[sum(classnamesnum)+2]]<pvaluethresholdx)
    }
    pdfx1<-data.frame(Methods=names(pvalueresdfx),Number.p.value=pvalueresdfx1,
                      Number.p.adjust=pvalueresdfx2,stringsAsFactors = FALSE)
    pdfx2<-data.frame(Methods=c("simes","fisher","whitlock","maxp","minp","pandora"),
                      Number.p.value=apply(pcombinresx[,1:6],2,function(x){sum(x<pvaluethresholdx & pvalueresdfx[[1]][[sum(classnamesnum)+3]]>=volcanofcbig)+
                          sum(x<pvaluethresholdx & pvalueresdfx[[1]][[sum(classnamesnum)+3]]<=volcanofcsmall)}),
                      Number.p.adjust=apply(pcombinresx[,7:12],2,function(x){sum(x<pvaluethresholdx & pvalueresdfx[[1]][[sum(classnamesnum)+3]]>=volcanofcbig)+
                          sum(x<pvaluethresholdx & pvalueresdfx[[1]][[sum(classnamesnum)+3]]<=volcanofcsmall)}),
                      stringsAsFactors = FALSE)
    pdfx12<-rbind(pdfx1,pdfx2)
    rownames(pdfx12)<-NULL
    pdfx12[order(pdfx12$Number.p.value+pdfx12$Number.p.adjust,decreasing = TRUE),]
  })
  output$pnumdetecres<-renderDataTable({
    datatable(pnumdetecresout(), options = list(pageLength = 20))
  })
  output$pnumdetecresdl<-downloadHandler(
    filename = function(){paste("PNo.detection_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(pnumdetecresout(),file)
    }
  )
  ##
  prankcorcresout<-reactive({
    if(input$loaddatatype==1){
      classnames<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
    }else{
      if(input$datatypex==4){
        classnames<-strsplit(input$examgrnames2,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums2,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums2,";")[[1]][2],"-")[[1]])
      }else{
        classnames<-strsplit(input$examgrnames,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
      }
    }
    pvalueresdfx<-pvalueresdfout()
    pcombinresx<-pcombinresout()
    prankcorx1<-prankcorx2<-vector()
    for(i in 1:length(pvalueresdfx)){
      prankcorx1[i]<-cor(-log10(pvalueresdfx[[i]][[sum(classnamesnum)+1]]),abs(pvalueresdfx[[i]]$cohen.d),method = "pearson")#pearson
      prankcorx2[i]<-cor(-log10(pvalueresdfx[[i]][[sum(classnamesnum)+2]]),abs(pvalueresdfx[[i]]$cohen.d),method = "pearson")
    }
    pdfx1<-data.frame(Methods=names(pvalueresdfx),Cor.p.cohen=prankcorx1,
                      Cor.padj.cohen=prankcorx2,stringsAsFactors = FALSE)
    pdfx2<-data.frame(Methods=c("simes","fisher","whitlock","maxp","minp","pandora"),
                      Cor.p.cohen=apply(pcombinresx[,1:6],2,function(x){
                        cor(-log10(x),abs(pvalueresdfx[[1]]$cohen.d),method = "pearson")#pearson
                      }),
                      Cor.padj.cohen=apply(pcombinresx[,7:12],2,function(x){
                        cor(-log10(x),abs(pvalueresdfx[[1]]$cohen.d),method = "pearson")
                      }),
                      stringsAsFactors = FALSE)
    pdfx2$Cor.padj.cohen[is.na(pdfx2$Cor.padj.cohen)]<-0
    pdfx12<-rbind(pdfx1,pdfx2)
    rownames(pdfx12)<-NULL
    pdfx12[order(pdfx12$Cor.p.cohen+pdfx12$Cor.padj.cohen,decreasing = TRUE),]
  })
  output$prankcorcres<-renderDataTable({
    datatable(prankcorcresout(), options = list(pageLength = 20))
  })
  output$prankcorresdl<-downloadHandler(
    filename = function(){paste("Cor.P.Cohen_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(prankcorcresout(),file)
    }
  )
  ##
  spikedprodfout<-reactive({
    aucfile1x<-input$aucfile1
    if(is.null(aucfile1x)){
      spikeddata<-read.csv("UPS1data.csv",stringsAsFactors = F,check.names = F)
    }else{
      spikeddata<-read.csv(aucfile1x$datapath,stringsAsFactors = F,check.names = F)
    }
    spikeddata
  })
  output$spikedprodf<-renderDataTable({
    datatable(spikedprodfout())
  })
  output$spikedprodfdl<-downloadHandler(
    filename = function(){paste("SpikedProteins_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(spikedprodfout(),file)
    }
  )
  paucresout<-reactive({
    library(metaseqR)
    pvaluethresholdx<<-input$pvaluethreshold
    ups1data<-spikeddatax<<-spikedprodfout()
    colnames(ups1data)<-"Accession"
    pvalueresdfx<<-pvalueresdfout()
    pcombinresx<<-pcombinresout()
    pyuzhi<-pvaluethresholdx
    pvalrownames<-unlist(lapply(rownames(pvalueresdfx[[1]]),function(x){
      strsplit(x,"_")[[1]][1]
    }))
    truthdata<-pvalrownames%in%spikeddatax[[1]]*1
    if(input$loaddatatype==1){
      classnames<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
    }else{
      if(input$datatypex==4){
        classnames<-strsplit(input$examgrnames2,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums2,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums2,";")[[1]][2],"-")[[1]])
      }else{
        classnames<-strsplit(input$examgrnames,";")[[1]]
        grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
      }
    }
    pvalueresdfx1<-pvalueresdfx2<-NULL
    zuixiaop1<-zuixiaop2<-F1Score<-F1Scoreadj<-vector()
    for(i in 1:length(pvalueresdfx)){
      #cbind(pvalueresdfx1,pvalueresdfx[[i]][[sum(classnamesnum)+1]])
      datairownames<-unlist(lapply(rownames(pvalueresdfx[[i]]),function(x){strsplit(x,"_")[[1]][1]}))
      pvalueresdfx1<-pvalueresdfx[[i]][,sum(classnamesnum)+1,drop=FALSE]
      pvalueresdfx2<-pvalueresdfx[[i]][,sum(classnamesnum)+2,drop=FALSE]
      if(length(datairownames[pvalueresdfx1[[1]]<pyuzhi])==0){
        F1Score[i]<-0
      }else{
        F1Score[i]<-sum(datairownames[pvalueresdfx1[[1]]<pyuzhi]%in%ups1data$Accession)*2/(sum(datairownames[pvalueresdfx1[[1]]<pyuzhi]%in%ups1data$Accession)*2+
                                                                                     sum(!datairownames[pvalueresdfx1[[1]]<pyuzhi]%in%ups1data$Accession)+
                                                                                     sum(datairownames[pvalueresdfx1[[1]]>=pyuzhi]%in%ups1data$Accession))
      }
      if(length(datairownames[pvalueresdfx2[[1]]<pyuzhi])==0){
        F1Scoreadj[i]<-0
      }else{
        F1Scoreadj[i]<-sum(datairownames[pvalueresdfx2[[1]]<pyuzhi]%in%ups1data$Accession)*2/(sum(datairownames[pvalueresdfx2[[1]]<pyuzhi]%in%ups1data$Accession)*2+
                                                                                        sum(!datairownames[pvalueresdfx2[[1]]<pyuzhi]%in%ups1data$Accession)+
                                                                                        sum(datairownames[pvalueresdfx2[[1]]>=pyuzhi]%in%ups1data$Accession))
      }
      set.seed(1234)
      pvalueresdfx1[pvalueresdfx1==0]<-sample(seq(.0000001,0.001,length.out = 2*length(pvalueresdfx1[pvalueresdfx1==0])),
                                              length(pvalueresdfx1[pvalueresdfx1==0]))
      #min(pvalueresdfx[[i]][[sum(classnamesnum)+1]])>=pvaluethresholdx
      dupx1<-duplicated(pvalueresdfx1[truthdata==1,1][pvalueresdfx1[truthdata==1,1]<pvaluethresholdx])
      if(sum(pvalueresdfx[[i]][[sum(classnamesnum)+1]]<pvaluethresholdx)<3 | sum(pvalueresdfx1[truthdata==1,1]<pvaluethresholdx)==0 |
         ((length(dupx1)-sum(dupx1))==1)){
        zuixiaop1[i]<-0
      }else{
        diagroc1<-diagplot.roc(truthdata, pvalueresdfx1, sig = pvaluethresholdx, x = "fpr", y = "tpr",draw = FALSE)
        zuixiaop1[i]<-diagroc1$ROC[[1]]$AUC
      }
      #pvalueresdfx2<-pvalueresdfx[[i]][,sum(classnamesnum)+2,drop=FALSE]
      pvalueresdfx2[pvalueresdfx2==0]<-sample(seq(.0000001,0.001,length.out = 2*length(pvalueresdfx2[pvalueresdfx2==0])),
                                              length(pvalueresdfx2[pvalueresdfx2==0]))
      #min(pvalueresdfx[[i]][[sum(classnamesnum)+2]])>=pvaluethresholdx
      dupx2<-duplicated(pvalueresdfx2[truthdata==1,1][pvalueresdfx2[truthdata==1,1]<pvaluethresholdx])
      if(sum(pvalueresdfx[[i]][[sum(classnamesnum)+2]]<pvaluethresholdx)<3 | sum(pvalueresdfx2[truthdata==1,1]<pvaluethresholdx)==0 |
         ((length(dupx2)-sum(dupx2))==1)){
        zuixiaop2[i]<-0
      }else{
        diagroc2<-diagplot.roc(truthdata, pvalueresdfx2, sig = pvaluethresholdx, x = "fpr", y = "tpr",draw = FALSE)
        zuixiaop2[i]<-diagroc2$ROC[[1]]$AUC
      }
      #zuixiaop1[i]<-ifelse(min(pvalueresdfx[[i]][[sum(classnamesnum)+1]])>=pvaluethresholdx,FALSE,TRUE)
      #zuixiaop2[i]<-ifelse(min(pvalueresdfx[[i]][[sum(classnamesnum)+2]])>=pvaluethresholdx,FALSE,TRUE)
    }
    #diagroc1<-diagplot.roc(truthdata, pvalueresdfx1[,zuixiaop1,drop=FALSE], sig = 0.05, x = "fpr", y = "tpr",draw = FALSE)
    #diagroc2<-diagplot.roc(truthdata, pvalueresdfx2[,zuixiaop2,drop=FALSE], sig = 0.05, x = "fpr", y = "tpr",draw = FALSE)
    pcombinedata<-pcombinresx1<-pcombinresx
    pcombinedatanames<-unlist(lapply(rownames(pcombinresx1),function(x){strsplit(x,"_")[[1]][1]}))
    aucpcombin1<-aucpcombin2<-F1Score2<-F1Scoreadj2<-vector()
    set.seed(1234)
    pcombinresx1[pcombinresx1==0]<-sample(seq(.0000001,0.001,length.out = 1000),1)
    for(j in 1:6){
      if(length(pcombinedatanames[pcombinedata[[j]]<pyuzhi])==0){
        F1Score2[j]<-0
      }else{
        F1Score2[j]<-sum(pcombinedatanames[pcombinedata[[j]]<pyuzhi]%in%ups1data$Accession)*2/(sum(pcombinedatanames[pcombinedata[[j]]<pyuzhi]%in%ups1data$Accession)*2+
                                                                                                 sum(!pcombinedatanames[pcombinedata[[j]]<pyuzhi]%in%ups1data$Accession)+
                                                                                                 sum(pcombinedatanames[pcombinedata[[j]]>=pyuzhi]%in%ups1data$Accession))
      }
      if(length(pcombinedatanames[pcombinedata[[j+6]]<pyuzhi])==0){
        F1Scoreadj2[j]<-0
      }else{
        F1Scoreadj2[j]<-sum(pcombinedatanames[pcombinedata[[j+6]]<pyuzhi]%in%ups1data$Accession)*2/(sum(pcombinedatanames[pcombinedata[[j+6]]<pyuzhi]%in%ups1data$Accession)*2+
                                                                                                      sum(!pcombinedatanames[pcombinedata[[j+6]]<pyuzhi]%in%ups1data$Accession)+
                                                                                                      sum(pcombinedatanames[pcombinedata[[j+6]]>=pyuzhi]%in%ups1data$Accession))
      }
      aucpcombindata1<-pcombinresx1[,j,drop=FALSE]
      dupx3<-duplicated(aucpcombindata1[truthdata==1,1][aucpcombindata1[truthdata==1,1]<pvaluethresholdx])
      if(min(aucpcombindata1[,1])>=pvaluethresholdx | sum(aucpcombindata1[truthdata==1,1]<pvaluethresholdx)==0 |
         ((length(dupx3)-sum(dupx3))==1)){
        aucpcombin1[j]<-0
      }else{
        diagroc1<-diagplot.roc(truthdata, aucpcombindata1, sig = pvaluethresholdx, x = "fpr", y = "tpr",draw = FALSE)
        aucpcombin1[j]<-diagroc1$ROC[[1]]$AUC
      }
      aucpcombindata2<-pcombinresx1[,j+6,drop=FALSE]
      dupx4<-duplicated(aucpcombindata2[truthdata==1,1][aucpcombindata2[truthdata==1,1]<pvaluethresholdx])
      if(min(aucpcombindata2[,1])>=pvaluethresholdx | sum(aucpcombindata2[truthdata==1,1]<pvaluethresholdx)==0 |
         ((length(dupx4)-sum(dupx4))==1)){
        aucpcombin2[j]<-0
      }else{
        diagroc2<-diagplot.roc(truthdata, aucpcombindata2, sig = pvaluethresholdx, x = "fpr", y = "tpr",draw = FALSE)
        aucpcombin2[j]<-diagroc2$ROC[[1]]$AUC
      }
    }
    pdfx1<-data.frame(Methods=names(pvalueresdfx),AUC.p.value=zuixiaop1,
                      AUC.p.adjust=zuixiaop2,F1score.p.value=F1Score,F1score.p.adjust=F1Scoreadj,
                      stringsAsFactors = FALSE)
    pdfx2<-data.frame(Methods=c("simes","fisher","whitlock","maxp","minp","pandora"),
                      AUC.p.value=aucpcombin1,
                      AUC.p.adjust=aucpcombin2,
                      F1score.p.value=F1Score2,F1score.p.adjust=F1Scoreadj2,
                      stringsAsFactors = FALSE)
    pdfx12<-rbind(pdfx1,pdfx2)
    rownames(pdfx12)<-NULL
    pdfx12[order(pdfx12$AUC.p.value+pdfx12$AUC.p.adjust,decreasing = TRUE),]
  })
  output$paucres<-renderDataTable({
    datatable(paucresout(), options = list(pageLength = 20))
  })
  output$paucresdl<-downloadHandler(
    filename = function(){paste("AUC_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(paucresout(),file)
    }
  )

})

shinyApp(ui = ui, server = server)
