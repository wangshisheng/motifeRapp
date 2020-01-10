library(shiny)
library(shinyjs)
library(shinyBS)
library(openxlsx)
library(gdata)
library(ggsci)
library(DT)
library(data.table)
library(Biostrings)
library(stringi)
library(stringr)
library(rmotifx)
#library(KSEAapp)
colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10),pal_nejm("default")(8),pal_lancet("lanonc")(9),
                      pal_jama("default")(7),pal_jco("default")(10),pal_ucscgb("default")(26),pal_d3("category10")(10),
                      pal_locuszoom("default")(7),pal_igv("default")(51),
                      pal_uchicago("default")(9),pal_startrek("uniform")(7),
                      pal_tron("legacy")(7),pal_futurama("planetexpress")(12),pal_rickandmorty("schwifty")(12),
                      pal_simpsons("springfield")(16),pal_gsea("default")(12)))
#
ui<-renderUI(
  fluidPage(
    title="motifeR",
    shinyjs::useShinyjs(),
    fluidRow(
      column(6,div(
        HTML(
          "<div style='text-align:right;margin-top:20px;margin-right:0px'>
          <a href='#' target=''><img src='motifeRti.png' width='100px'>
          </a>
          </div>"
        )
        )),
      column(6,div(
        HTML(
          "<div style='text-align:left;margin-left:-20px'>
          <a href='#' target=''><img src='motifeRlogo.png' height='80px'>
          </a>
          </div>"
        )
        ))
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
                    ')
      )
    ),

    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(h2(strong("Calculating......")),img(src="rmd_loader.gif"),id="loadmessage")),
    tabsetPanel(
      tabPanel(
        "Welcome",
        uiOutput("welcomeui")
      ),
      tabPanel(
        "Import Data",
        sidebarLayout(
          sidebarPanel(
            width=3,
            h3('Import Sequence Data'),
            radioButtons("metabopath_shujudaoru",label="",choices = list("1. Upload" = 1,"2. Paste"=2),
                         selected = 1,inline = TRUE),
            conditionalPanel(
              condition = "input.metabopath_shujudaoru==1",
              radioButtons(
                "metabopathfileType_Input",
                label = h4("File format："),
                choices = list(".xlsx" = 1,".xls"=2, ".csv/txt" = 3),
                selected = 1,
                inline = TRUE
              ),
              fileInput('metabopathfile1', 'Import your data：',
                        accept=c('text/csv','text/plain','.xlsx','.xls')),
              checkboxInput('metabopathheader', 'Header ?', TRUE),
              checkboxInput('metabopathfirstcol', 'First column ?', FALSE),
              conditionalPanel(condition = "input.metabopathfileType_Input==1",
                               numericInput("metabopathxlsxindex","Sheet index:",value = 1)),
              conditionalPanel(condition = "input.metabopathfileType_Input==2",
                               numericInput("metabopathxlsxindex","Sheet index:",value = 1)),
              conditionalPanel(condition = "input.metabopathfileType_Input==3",
                               radioButtons('metabopathsep', 'Separator：',
                                            c(Comma=',',
                                              Semicolon=';',
                                              Tab='\t',
                                              BlankSpace=' '),
                                            ','))
            ),
            conditionalPanel(
              condition = "input.metabopath_shujudaoru==2",
              textAreaInput("metabopath_zhantie",label = "Paste your data here：",value="",height ="100px")
            ),
            hr(),
            selectInput("origidatatype","Original data type:",choices = c("Normal","MaxQuant","Spectronaut")),
            bsTooltip("origidatatype",'The original post-translational modification (PTM) data obtained from which kind of search software. If you have processed the PTM data with standard format (e.g. NPT#Y#GSWFTEK), you should choose the "Normal", otherwise, if your PTM data are obtained from MaxQuant or Spectronaut, you should choose the relative type.',
                      placement = "right",options = list(container = "body")),
            tags$hr(style="border-color: grey;"),
            textInput("centralres","Central amino acid：",value = "ST"),
            bsTooltip("centralres",'The central residue that users want to analyze, for example, phosphorylation motif analysis, can center on phosphorylated S, T or Y residues. If they want to analyze multi motif sites, here should be "STY".',
                      placement = "right",options = list(container = "body")),
            div(id="centralresfuhao_div",textInput("centralresfuhao","Label of modification：",value = "#")),
            bsTooltip("centralresfuhao_div",'The label represents modification, users can use some label they like, such as "#", "@", where "#" is recommended.',
                      placement = "right",options = list(container = "body")),
            div(id="minseqs_div",numericInput("minseqs","Width：",value = 7)),
            bsTooltip("minseqs_div",'It is the number of left/right side characters of the central residue. The default is "7" but can be changed by the user.',
                      placement = "right",options = list(container = "body")),
            div(id="minseqsnum_div",numericInput("minseqsnum","Minimum number：",value = 20)),
            bsTooltip("minseqsnum_div","This threshold refers to the minimum number of times you wish each of your extracted motifs to occur in the data set.",
                      placement = "right",options = list(container = "body")),
            div(id="pvalcutoff_div",numericInput("pvalcutoff","P-value threshold：",value = 0.000001,min = 0)),
            bsTooltip("pvalcutoff_div","The p-value threshold for the binomial probability. This is used for the selection of significant residue/position in the motif.",
                      placement = "right",options = list(container = "body")),
            tags$hr(style="border-color: grey;"),
            div(id="xuanzebgdatabase_div",radioButtons("xuanzebgdatabase",label="",choices = list("1. Select" = 1,"2. Upload"=2),
                                 selected = 1,inline = TRUE)),
            bsTooltip("xuanzebgdatabase_div","Select or upload the background dataset.",
                      placement = "bottom",options = list(container = "body")),
            conditionalPanel(
              condition = "input.xuanzebgdatabase==1",
              uiOutput("metabopathspecies")
            ),
            conditionalPanel(
              condition = "input.xuanzebgdatabase==2",
              fileInput('fastafileown', 'Please upload your fasta file：',accept=c('.fasta'))
            )
          ),
          mainPanel(
            width = 9,
            hr(),
            actionButton("mcsbtn_seqrawdata","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            radioButtons(
              "seqrawdataxuanze",
              label = h4(""),
              choices = list("Original data" = 1,"Annotated data"=2),
              selected = 1,
              inline = TRUE
            ),
            tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = 'input.seqrawdataxuanze==1',
              dataTableOutput("seqrawdata")
            ),
            conditionalPanel(
              condition = 'input.seqrawdataxuanze==2',
              downloadButton("seqannotatedatadl","Download"),
              dataTableOutput("seqannotatedata")
            )
          )
        )
      ),
      tabPanel(
        "Pre-alignment",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h4("Pre-alignment"),
            div(id="seqalignif_div",checkboxInput('seqalignif', '1. Pre-aligned or not ?', TRUE)),
            bsTooltip("seqalignif_div","Whether to pre-align your sequences. If your sequences are standard (e.g. 15 length amino acids), you can unselect this parameter. Default is true.",
                      placement = "right",options = list(container = "body")),
            div(id="classicmultisiteif_div",checkboxInput('classicmultisiteif', '2. Classical multiple sites analysis or not ?', TRUE)),
            bsTooltip("classicmultisiteif_div",'Whether to process classical analysis. Classical analysis means not replacing the other modified sites with letter "Z" after pre-alignment, for example "TSLWNPT#Y#GSWFTEK" to "TSLWNPTYGSWFTEK", not to "TSLWNPZYGSWFTEK". If true, do not process transformation, otherwise, transformation.',
                      placement = "right",options = list(container = "body")),
            div(id="seqalignhanif_div",checkboxInput('seqalignhanif', '3. Check if containing some regular sequence ?', FALSE)),
            bsTooltip("seqalignhanif_div",'If users want to check whether the aligned peptides contain some specific sequences, for example, you want to find those peptides whose 3th and 5th position are R (arginine), then you can select this parameter and type in a simple regular expression, like "^\\\\w{2}R\\\\w{1}R". Otherwise, you just unselect it.',
                      placement = "right",options = list(container = "body")),
            conditionalPanel(
              condition = "input.seqalignhanif==true",
              textInput("seqalignhan","Regular expression：",value = "^\\w{2}R\\w{1}R")
            ),
            tags$hr(style="border-color: grey;"),
            actionButton("mcsbtn_seqalign","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            radioButtons(
              "prealignxuanze",
              label = h4(""),
              choices = list("Alignment results" = 1,"Sites number distribution plot"=2),
              selected = 1,
              inline = TRUE
            ),
            tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = "input.prealignxuanze==1",
              fluidRow(
                column(
                  2,
                  downloadButton("seqduiqidl","Download")
                ),
                column(
                  2,
                  actionButton("mcsbtn_resjieshi","Result description",icon("file-alt"),
                               style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
                )
              ),
              dataTableOutput("seqduiqi")
            ),
            conditionalPanel(
              condition = "input.prealignxuanze==2",
              h4("Plot:"),
              downloadButton("seqduiqiplotdl","Download"),
              plotOutput("seqduiqiplot",height = "800px"),
              tags$hr(style="border-color: grey;"),
              h4("Multi-Sites Data:"),
              fluidRow(
                column(
                  2,
                  downloadButton("seqduiqiduositedl","Download")
                ),
                column(
                  2,
                  actionButton("mcsbtn_resjieshi2","Result description",icon("file-alt"),
                               style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
                )
              ),
              dataTableOutput("seqduiqiduosite")
            )
          )
        )
      ),
      tabPanel(
        "Own Background",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            div(id="beijingif_div",checkboxInput("beijingif","Upload your own background data?",FALSE)),
            bsTooltip("beijingif_div",'The uploaded database must contain peptide sequences with standard length (e.g. 15 length amino acids). Please note, if you upload you own fasta file as background database in the "Import Data" step, you should unselect this parameter. If you choose this parameter stubbornly, this software will take the data in this step as background database and ignore the fasta file that you upload before.',
                      placement = "bottom",options = list(container = "body")),
            #popify(
            #  checkboxInput("beijingif","Upload your own background data?",FALSE),
            #  "Upload your own background data?",
            #  'The uploaded database must contain peptide sequences with standard length (e.g. 15 length amino acids). Please note, if you upload you own fasta file as background database in the "Import Data" step, you should unselect this parameter. If you choose this parameter stubbornly, this software will take the data in this step as background database and ignore the fasta file that you upload before.',
            #  placement = "bottom"
            #),
            conditionalPanel(
              condition = "input.beijingif==true",
              radioButtons(
                "goortbeijingkufileType_Input",
                label = h4("File format："),
                choices = list(".xlsx" = 1,".xls"=2, ".csv/txt" = 3),
                selected = 1,
                inline = TRUE
              ),
              fileInput('goortbeijingkufile1', 'Import your data：',
                        accept=c('text/csv','text/plain','.xlsx','.xls')),
              checkboxInput('goortbeijingkuheader', 'Header ?', FALSE),
              checkboxInput('goortbeijingkufirstcol', 'First column ?', FALSE),
              conditionalPanel(condition = "input.goortbeijingkufileType_Input==1",
                               numericInput("goortbeijingkuxlsxindex","Sheet:",value = 1)),
              conditionalPanel(condition = "input.goortbeijingkufileType_Input==2",
                               numericInput("goortbeijingkuxlsxindex","Sheet:",value = 1)),
              conditionalPanel(condition = "input.goortbeijingkufileType_Input==3",
                               radioButtons('goortbeijingkusep', 'Separator：',
                                            c(Comma=',',
                                              Semicolon=';',
                                              Tab='\t',
                                              BlankSpace=' '),
                                            ','))
            )
          ),
          mainPanel(
            width = 9,
            dataTableOutput("seqbjdata")
          )
        )
      ),
      tabPanel(
        "Motif Enrichment",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            #div(id="motifquanbuif_div",checkboxInput("motifquanbuif","Species data as background ?",TRUE)),
            #bsTooltip("motifquanbuif_div",'If you upload your own fasta file as background database in the "Import Data" step, you can ignore this parameter (select or unselect is same). Otherwise, if you choose the database in our system (i.e., human) in the ‘Import Data’ step, selecting this parameter means this software will take the database in our system as background database. If you don’t choose, the software will take the foreground data as background database.',
            #          placement = "bottom",options = list(container = "body")),
            div(id="onlymultisiteif_div",checkboxInput("onlymultisiteif","Only use multi-site data ?",FALSE)),
            bsTooltip("onlymultisiteif_div",'If selected, this tool will only take the peptides with multi modification sites as foreground data, that is, it will use the sequences in the Seqwindows_MultiSites column obtain from "Pre-alignment" step as foreground data.',
                      placement = "bottom",options = list(container = "body")),
            tags$hr(style="border-color: grey;"),
            actionButton("mcsbtn_motifquanbu","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            radioButtons(
              "motiffujidfxuanze",
              label = h4(""),
              choices = list("Multiple motifs" = 1,"Regular sequence motif"=2),
              selected = 1,
              inline = TRUE
            ),
            tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = 'input.motiffujidfxuanze==1',
              hidden(
                div(
                  id="motiffujidfxuanze_btn",
                  h4("1. Motif enrichment results:"),
                  fluidRow(
                    column(
                      2,
                      downloadButton("motiffujidl","Download")
                    ),
                    column(
                      2,
                      actionButton("mcsbtn_resjieshi3","Result description",icon("file-alt"),
                                   style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
                    )
                  ),
                  dataTableOutput("motiffuji"),
                  h4("2. Enrichment results mapped to alignment results:"),
                  downloadButton("motiffujidl2","Download"),
                  dataTableOutput("motiffuji2")
                )
              )
            ),
            conditionalPanel(
              condition = 'input.motiffujidfxuanze==2',
              downloadButton("regularmotiffujidl","Download"),
              dataTableOutput("regularmotiffuji")
            )
          )
        )
      ),
      tabPanel(
        "Motif Plot",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            div(id='enrichseqnum_div',textInput("enrichseqnum","Motif index for plot：",value = "1")),
            bsTooltip("enrichseqnum_div",'Which motif would be plotted. If users only type in one number, it will plot the relative motif. If users type in "1-10", it will plot the 1th to 10th motifs.',
                      placement = "bottom",options = list(container = "body")),
            div(id='equalheightif_div',checkboxInput("equalheightif","Equal height or not?",FALSE)),
            bsTooltip("equalheightif_div",'Whether all residues in the figure have equal height. Default is false.',
                      placement = "bottom",options = list(container = "body")),
            tags$hr(style="border-color: grey;"),
            actionButton("mcsbtn_motifplot","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            div(id="motifplot_heightpic_div",checkboxInput("motifplot_heightpic","Change figure size ?",FALSE)),
            conditionalPanel(
              condition = "input.motifplot_heightpic==true",
              sliderInput("motifplot_height","Height：",min = 800,max = 3000,step = 100,value = 800)
            ),
            #radioButtons(
            #  "motifplotxuanze",
            #  label = h4(""),
            #  choices = list("Multiple motifs" = 1,"Regular sequence motif"=2),
            #  selected = 1,
            #  inline = TRUE
            #),
            tags$hr(style="border-color: grey;"),
            downloadButton("motifplotdownload","Download"),
            plotOutput("motifplot")
          )
        )
      ),
      tabPanel(
        "Kinase Analysis",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            div(id='NetworKINcutoff_div',numericInput("NetworKINcutoff","minimum NetworKIN score:",value = 3)),
            bsTooltip("NetworKINcutoff_div",'A numeric value between 1 and infinity setting the minimum NetworKIN score.',
                      placement = "bottom",options = list(container = "body")),
            div(id='genenamesif_div',checkboxInput("genenamesif","Show gene names or not?",TRUE)),
            bsTooltip("genenamesif_div",'If true, the gene names will be appeared in the network plot, otherwise, the uniprot ids will be shown.',
                      placement = "bottom",options = list(container = "body")),
            uiOutput("kinasemotifui"),
            tags$hr(style="border-color: grey;"),
            actionButton("mcsbtn_kniase","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              id="subnav",
              tabPanel(
                "Results",
                fluidRow(
                  column(
                    2,
                    downloadButton("kinasedatadl","Download")
                  ),
                  column(
                    2,
                    actionButton("mcsbtn_resjieshi4","Result description",icon("file-alt"),
                                 style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
                  )
                ),
                dataTableOutput("kinasedata")
              ),
              tabPanel(
                "Network Plot",
                div(id="cmheatmap_div",checkboxInput("cmheatmap","Change figure size？",FALSE)),
                conditionalPanel(
                  condition = "input.cmheatmap==true",
                  sliderInput("cmheatmap_height","figure height：",min = 500,max = 5000,step = 100,value = 800)
                ),
                downloadButton("cmheatmappicdl","Download"),
                plotOutput("cmheatmappic")
              )
            )
          )
        )
      ),
      tabPanel(
        "Building Species Database",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            div(id='refastafileif_div',checkboxInput("refastafileif","Re-upload?",FALSE)),
            bsTooltip("refastafileif_div",'This step can build the standard database based on the fata file that users upload, herein there is no species limit. And this results can also be used in "Own background" step. If users want to build their own database, they can select this parameter and then upload a fasta file.',
                      placement = "bottom",options = list(container = "body")),
            conditionalPanel(
              condition = "input.refastafileif==true",
              fileInput('fastafile', 'Please upload your fasta file：',accept=c('.fasta'))
            ),
            tags$hr(style="border-color: grey;"),
            actionButton("mcsbtn_fastaalign","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            fluidRow(
              column(
                2,
                downloadButton("allfastadl","Download")
              ),
              column(
                2,
                actionButton("mcsbtn_resjieshi5","Result description",icon("file-alt"),
                             style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
              )
            ),
            dataTableOutput("allfasta")
          )
        )
      )
    )
  )
)
#
server<-shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=30*1024^2)
  usertimenum<-as.numeric(Sys.time())
  #ui
  output$welcomeui<-renderUI({
    screenwidth<-input$dimension[1]
    #screenheight<-input$dimension[2]
    #tryCatch({},error=function(e) NULL)
    if(is.null(screenwidth)){
      return(NULL)
    }else{
      if(screenwidth<=1024){
        imgwidth<-350
      }
      else if(screenwidth>1024 & screenwidth<=1440){
        imgwidth<-450
      }
      else{
        imgwidth<-600
      }
    }

    fluidRow(
      div(style="text-align:center",h1("~~Welcome~~")),
      column(
        7,
        div(style="text-align:center",h3("Workflow")),
        div(style="text-align:center",
            a(href='#',
              img(src='Figure1app.png',height=imgwidth)))
      ),
      column(
        5,
        div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;",
            h3("I. The detailed manual can be found here, please visit our github to get it:"),
            a(href="https://github.com/wangshisheng/motifeR",h4("https://github.com/wangshisheng/motifeR"),target="_black"),
            h3("II. The source codes are here:"),
            a(href="https://github.com/wangshisheng/motifeRapp",h4("https://github.com/wangshisheng/motifeRapp"),target="_black"),
            h3("III. The example data used in this software can be download from here:"),
            h4("a. Normal data with standard format: "),
            a(href="https://github.com/wangshisheng/motifeR/blob/master/Normal_Exampledata.csv",h4("https://github.com/wangshisheng/motifeR/blob/master/Normal_Exampledata.csv"),target="_black"),
            h4("b. PTM data obtained from MaxQuant: "),
            a(href="https://github.com/wangshisheng/motifeR/blob/master/MaxQuant_Exampledata.csv",h4("https://github.com/wangshisheng/motifeR/blob/master/MaxQuant_Exampledata.csv"),target="_black"),
            h4("c. PTM data obtained from Spectronaut: "),
            a(href="https://github.com/wangshisheng/motifeR/blob/master/Spectronaut_Exampledata.csv",h4("https://github.com/wangshisheng/motifeR/blob/master/Spectronaut_Exampledata.csv"),target="_black"),
            h3("IV. Citation:"),
            h4("Shisheng Wang, Yue Cai, Jingqiu Cheng, Wenxue Li, Yansheng Liu and Hao Yang. motifeR: An Integrated Web Software for Identification and Visualization of Protein Post‐Translational Modification Motifs. Proteomics: 201900254."),
            a(href="https://doi.org/10.1002/pmic.201900245",h4("DOI: 10.1002/pmic.201900245"),target="_black"))
      )
    )
  })
  #show data
  output$metabopathspecies<-renderUI({
    metabopath_spedf<-read.csv("metabopath-species.csv",header = T,stringsAsFactors = F)
    metabopath_spedf_paste<-paste(metabopath_spedf$Organism.ID,metabopath_spedf$Organism,sep = "-")
    selectizeInput('metabopathspeciesselect', 'Species for background：', choices =metabopath_spedf_paste,options = list(maxOptions = 6000))
  })
  #######
  seqrawdataoutxx<-reactive({
    if(input$metabopath_shujudaoru==1){
      files <- input$metabopathfile1
      if(is.null(files)){
        if(input$origidatatype=="MaxQuant"){
          dataread<-read.csv("MaxQuant_Exampledata.csv",stringsAsFactors = F)
        }
        else if(input$origidatatype=="Spectronaut"){
          dataread<-read.csv("Spectronaut_Exampledata.csv",stringsAsFactors = F)
        }
        else{
          dataread<-read.csv("Normal_Exampledata.csv",stringsAsFactors = F)
        }
      }else{
        if (input$metabopathfileType_Input == "1"){
          dataread<-read.xlsx(files$datapath,rowNames=input$metabopathfirstcol,
                              colNames = input$metabopathheader,sheet = input$metabopathxlsxindex)
        }
        else if(input$metabopathfileType_Input == "2"){
          if(sum(input$metabopathfirstcol)==1){
            rownametfmetabopath<-1
          }else{
            rownametfmetabopath<-NULL
          }
          dataread<-read.xls(files$datapath,sheet = input$metabopathxlsxindex,header=input$metabopathheader,
                             row.names = rownametfmetabopath, sep=input$metabopathsep,stringsAsFactors = F)
        }
        else{
          if(sum(input$metabopathfirstcol)==1){
            rownametfmetabopath<-1
          }else{
            rownametfmetabopath<-NULL
          }
          dataread<-read.csv(files$datapath,header=input$metabopathheader,
                             row.names = rownametfmetabopath, sep=input$metabopathsep,stringsAsFactors = F)
        }
      }
    }else{
      zhantieidstr<-strsplit(input$metabopath_zhantie,"\n")
      dataread<-data.frame(Input_ID=zhantieidstr[[1]])
    }
    dataread
  })
  output$seqrawdata<-renderDataTable({
    dataread<-seqrawdataoutxx()
    datatable(dataread, options = list(pageLength = 10))
  })
  
  seqrawdataout<-reactive({
    datareadx<-seqrawdataoutxx()
    origidatatypex<<-isolate(input$origidatatype)
    Peptides<-vector()
    if(origidatatypex=="MaxQuant"){
      withProgress(message = 'Generating data', style = "notification", detail = "index 1", value = 0,{
        for(i in 1:nrow(datareadx)){
          pep1<-datareadx[[1]][i]
          Peptidesi1<-strsplit(gsub("[^0-9.]", ";", pep1),";")[[1]]
          Peptidesi2<-Peptidesi1[Peptidesi1!=""]
          for(ii in 1:length(Peptidesi2)){
            pep1<-gsub(paste0("\\(",Peptidesi2[ii],"\\)"),"#",pep1)
          }
          Peptides[i]<-pep1
          
          incProgress(1/nrow(datareadx), detail = paste("index", i))
        }
      })
      dataconvert<-data.frame(AnnotatedPeps=Peptides,stringsAsFactors = FALSE)
    }
    else if(origidatatypex=="Spectronaut"){
      withProgress(message = 'Annotated data:',min = 0, max = 2, style = "notification", detail = "Generating data", value = 1,{
        phosphoindex<-grep("\\[Phospho \\(STY\\)\\]",datareadx[[1]], perl = TRUE)
        uploaddata1<-datareadx[phosphoindex,]
        Peptidesx<-gsub("_","",uploaddata1, perl = TRUE)
        Peptidesx<-gsub("\\[Phospho \\(STY\\)\\]","#",Peptidesx, perl = TRUE)
        Peptidesx2<-str_replace_all(Peptidesx,"\\[.*?\\]","")
        
        shiny::incProgress(1, detail = "Generating data")
      })
      dataconvert<-data.frame(AnnotatedPeps=Peptidesx2,stringsAsFactors = FALSE)
    }
    else{
      dataconvert<-datareadx
    }
    dataconvert
  })
  observeEvent(
    input$mcsbtn_seqrawdata,{
      output$seqannotatedata<-renderDataTable({
        dataread<-seqrawdataout()
        datatable(dataread, options = list(pageLength = 10))
      })
      output$seqannotatedatadl<-downloadHandler(
        filename = function(){paste("Annotated_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(seqrawdataout(),file,row.names=FALSE)
        }
      )
    }
  )
  
  #
  seqbjdataout<-reactive({
    if(input$beijingif){
      files <- input$goortbeijingkufile1
      if(is.null(files)){
        dataread<-NULL
      }else{
        if (input$goortbeijingkufileType_Input == "1"){
          dataread<-read.xlsx(files$datapath,rowNames=input$goortbeijingkufirstcol,
                              colNames = input$goortbeijingkuheader,sheet = input$goortbeijingkuxlsxindex)
        }
        else if(input$goortbeijingkufileType_Input == "2"){
          if(sum(input$goortbeijingkufirstcol)==1){
            rownametfgoortbeijingku<-1
          }else{
            rownametfgoortbeijingku<-NULL
          }
          dataread<-read.xls(files$datapath,sheet = input$goortbeijingkuxlsxindex,header=input$goortbeijingkuheader,
                             row.names = rownametfgoortbeijingku, sep=input$goortbeijingkusep,stringsAsFactors = F)
        }
        else{
          if(sum(input$goortbeijingkufirstcol)==1){
            rownametfgoortbeijingku<-1
          }else{
            rownametfgoortbeijingku<-NULL
          }
          dataread<-read.csv(files$datapath,header=input$goortbeijingkuheader,
                             row.names = rownametfgoortbeijingku, sep=input$goortbeijingkusep,stringsAsFactors = F)
        }
      }
    }else{
      dataread<-NULL
    }
    dataread
  })
  output$seqbjdata<-renderDataTable({
    datareadbj<-seqbjdataout()
    datatable(datareadbj, options = list(pageLength = 10))
  })
  
  fastaseqownout<-reactive({
    files <- input$fastafileown
    if(is.null(files)){
      datareadfasta<-NULL
    }else{
      datafasta<-readAAStringSet(files$datapath)
      pro_seqdf<-pro_seqdf1<-as.data.frame(datafasta)
      pro_seqdf_rown<-unlist(lapply(rownames(pro_seqdf1),function(x) strsplit(x,"\\|")[[1]][2]))
      rownames(pro_seqdf1)<-pro_seqdf_rown
      pro_seqdfncar<-unlist(lapply(pro_seqdf1$x,nchar))
      danlength<-input$minseqs
      pro_seqdf<-pro_seqdf1[pro_seqdfncar>(2*danlength+1),,drop=FALSE]
      n_data_fasta<-nrow(pro_seqdf)
      wincenter<-strsplit(input$centralres,"")[[1]]
      seqwindowsall_S<-vector()
      seqnamesall_S<-vector()
      wincenteri<-vector()
      k<-1
      for(ii in wincenter){
        withProgress(message = paste('Generating data',ii), style = "notification", detail = "index 1", value = 0,{
          for(i in 1:n_data_fasta){
            seqindex1<-stri_locate_all(pattern = ii, pro_seqdf$x[i], fixed = TRUE)[[1]][,1]
            if(length(seqindex1)>0){
              seqnchar<-nchar(pro_seqdf$x[i])
              seqseq<-vector()
              for(j in 1:length(seqindex1)){
                indexjian1<-seqindex1[j]-danlength
                indexjian2<-seqindex1[j]+danlength
                if(indexjian1<=0){
                  xhx1<-paste(rep("_",abs(indexjian1)+1),collapse ="")
                  xhx2<-stri_sub(pro_seqdf$x[i],from = 0,to=indexjian2)
                  xhx3<-paste0(xhx1,xhx2)
                }
                else if(indexjian2>seqnchar){
                  xhx1<-paste(rep("_",(indexjian2-seqnchar)),collapse="")
                  xhx2<-stri_sub(pro_seqdf$x[i],from = indexjian1,to=seqnchar)
                  xhx3<-paste0(xhx2,xhx1)
                }
                else{
                  xhx3<-stri_sub(pro_seqdf$x[i],from = indexjian1,to=indexjian2)
                }
                seqwindowsall_S[k]<-xhx3
                seqnamesall_S[k]<-rownames(pro_seqdf)[i]
                wincenteri[k]<-ii
                k<-k+1
              }
            }
            incProgress(1/n_data_fasta, detail = paste("index", i))
          }
        })
      }
      datareadfasta<-data.frame(ID=seqnamesall_S,Windows=seqwindowsall_S,
                                Center=wincenteri,stringsAsFactors = F)
    }
    datareadfasta
  })
  #
  observeEvent(input$mcsbtn_resjieshi, {
    showModal(modalDialog(
      title = "Pre-alignment result description:",
      paste0("1. Pep.upload: this column contains those peptides users upload."),br(),
      paste0("2. Stripped.pep: the peptide skeleton."),br(),
      paste0("3. Pep.main.index: the position of the main modified amino acid in the peptide, for example, if users upload their peptides containing Class I phosphorylation sites with high confidence, such as ‘TSLWNPT#Y@GSWFTEK’, then this software will recognize ‘#’ as Class I phosphorylation site and ‘@’ as non-Class I phosphorylation site by default, so the Pep.main.index will be 7."),br(),
      paste0("4. Pep.all.index: the position of all modified amino acid in the peptide. As the example in Pep.main.index, the Pep.all.index will be 7;8."),br(),
      paste0("5. Center.amino.acid: the central amino acid in the aligned peptide."),br(),
      paste0("6. Seqwindows: the aligned standard peptides. Note for multiple modification sites or types, the column provides peptides with all the sites respectively centered."),br(),
      paste0("7. PRO.from.Database: provide the protein name containing this peptide from the fasta file the user uploaded."),br(),
      paste0("8. PROindex.from.Database: the position of modified amino acid in the protein sequence."),br(),
      paste0("9. Contain.if: whether containing the sequences that match the regular expression (see above), if true, marked with “Yes”, otherwise, “No”. This column only appears when users choose the parameter--- Check if containing some regular sequence."),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_resjieshi2, {
    showModal(modalDialog(
      title = "Pre-alignment result description:",
      paste0("1. Pep.upload: this column contains those peptides users upload."),br(),
      paste0("2. Stripped.pep: the peptide skeleton."),br(),
      paste0("3. Pep.main.index: the position of the main modified amino acid in the peptide, for example, if users upload their peptides containing Class I phosphorylation sites with high confidence, such as ‘TSLWNPT#Y@GSWFTEK’, then this software will recognize ‘#’ as Class I phosphorylation site and ‘@’ as non-Class I phosphorylation site by default, so the Pep.main.index will be 7."),br(),
      paste0("4. Pep.all.index: the position of all modified amino acid in the peptide. As the example in Pep.main.index, the Pep.all.index will be 7;8."),br(),
      paste0("5. Center.amino.acid: the central amino acid in the aligned peptide."),br(),
      paste0("6. Seqwindows: the aligned standard peptides. Note for multiple modification sites or types, the column provides peptides with all the sites respectively centered."),br(),
      paste0("7. PRO.from.Database: provide the protein name containing this peptide from the fasta file the user uploaded."),br(),
      paste0("8. PROindex.from.Database: the position of modified amino acid in the protein sequence."),br(),
      paste0("9. Contain.if: whether containing the sequences that match the regular expression (see above), if true, marked with “Yes”, otherwise, “No”. This column only appears when users choose the parameter--- Check if containing some regular sequence."),br(),
      paste0("10. Seqwindows_MultiSites: there are two situations here: First, the modified amino acid will be replaced with “X” if it is not the central residue, for example, ‘NKPTSLWNPT(0.832)Y(0.168)GSWFTEK’ has two phosphosites, one is the 10th amino acid with 0.832 location probability, the other is the 11th amino acid with 0.168 location probability, thus if we transform it like ‘NKPTSLWNPT#Y@GSWFTEK’ (high probability is replaced with ‘#’, while low probability is replaced with ‘@’). Then in motifeR, the 10th amino acid will be considered as central residue, the 11th amino acid will be replaced with “X”, thus the standard sequence is ‘PTSLWNPTYGSWFTE’, correspondingly, the Seqwindows_MultiSites should be ‘PTSLWNPTXGSWFTE’. Second, if we transform this peptide like ‘NKPTSLWNPT#Y#GSWFTEK’, the two amino acids will be both considered as central residue, thus the standard sequence is ‘PTSLWNPTYGSWFTE;TSLWNPTYGSWFTEK’, correspondingly, the Seqwindows_MultiSites is still ‘PTSLWNPTYGSWFTE;TSLWNPTYGSWFTEK’."),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_resjieshi3, {
    showModal(modalDialog(
      title = "Motif Enrichment result description:",
      paste0("1. motif: the overrepresented motif."),br(),
      paste0("2. score: the motif score, which is calculated by taking the sum of the negative log probabilities used to fix each position of the motif. Higher motif scores typically correspond to motifs that are more statistically significant as well as more specific."),br(),
      paste0("3. fg.matches: frequency of sequences matching this motif in the foreground set."),br(),
      paste0("4. fg.size: total number of foreground sequences."),br(),
      paste0("5. bg.matches: frequency of sequences matching this motif in the background set."),br(),
      paste0("6. bg.size: total number of background sequences."),br(),
      paste0("7. fold.increase: An indicator of the enrichment level of the extracted motifs. Specifically, it is calculated as (foreground matches/foreground size)/(background matches/background size)."),br(),
      paste0("8. Enrich.seq: those peptides are overrepresented in this motif."),br(),
      paste0("9. Enrich.pro: those proteins in which the peptides exist from Enrich.seq."),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_resjieshi4, {
    showModal(modalDialog(
      title = "Kinase-substrate result description:",
      paste0("1. KIN_ACC_ID: kinase uniprot id."),br(),
      paste0("2. KINASE: kinase id."),br(),
      paste0("3. GENE: kinase gene name."),br(),
      paste0("4. SUBSTRATE: substrate id."),br(),
      paste0("5. SUB_ACC_ID: substrate uniprot id."),br(),
      paste0("6. SUB_GENE: substrate gene name."),br(),
      paste0("7. networkin_score: the prediction score from networKIN database (https://networkin.info/)."),br(),
      paste0("8. Enrich.seq: the peptide that is overrepresented in the relevant motif."),br(),
      paste0("9. Motif: the overrepresented motif."),br(),
      paste0("10. Pep.upload: the original peptide."),br(),
      paste0("11. Center.amino.acid: the central amino acid in the aligned peptide."),br(),
      paste0("12. PROindex.from.Database: the position of modified amino acid in the protein sequence."),br(),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_resjieshi5, {
    showModal(modalDialog(
      title = "Building species database result description:",
      paste0("1. ID: uniprot ids."),br(),
      paste0("2. Windows: the standard peptides."),br(),
      paste0("3. Center: Central residue."),br(),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  seqduiqioutx<-reactive({
    uploaddata1<-datareaddq<<-seqrawdataout()
    if(input$seqalignif){
      centralres1<<-strsplit(input$centralresfuhao,";|")[[1]]
      centralres<<-centralres1[centralres1!=""]
      centralres2<<-paste(centralres,collapse = "|")
      uploaddata1$Stripped.pep<-gsub(paste0("_|",centralres2),"",datareaddq[[1]], perl = TRUE)
      EGindex<-lapply(datareaddq[[1]],function(x){
        xx4<-gregexpr(centralres[1],x)[[1]]
        xx3<-gregexpr(centralres2,x)[[1]]
        xx5<-unlist(lapply(xx4,function(x) which(x==xx3)))
        xx1<-1:length(xx3)
        xx6<-as.numeric(xx3)-xx1
        xx2<-paste(xx6[xx5],collapse = ";")
        xx2
      })
      EGindex1<-lapply(datareaddq[[1]],function(x){
        xx3<-gregexpr(centralres2,x)[[1]]
        xx1<-1:length(xx3)
        xx6<-as.numeric(xx3)-xx1
        xx2<-paste(xx6,collapse = ";")
        xx2
      })
      centeranjisuan<-lapply(datareaddq[[1]],function(x){
        pepi<-strsplit(gsub(centralres2,"",x),"")[[1]]
        xx4<-gregexpr(centralres[1],x)[[1]]
        xx3<-gregexpr(centralres2,x)[[1]]
        xx5<-unlist(lapply(xx4,function(x) which(x==xx3)))
        xx1<-1:length(xx3)
        xx6<-as.numeric(xx3)-xx1
        xx2<-paste(pepi[xx6[xx5]],collapse = ";")
        xx2
      })
      uploaddata1$Pep.main.index<-unlist(EGindex)
      uploaddata1$Pep.all.index<-unlist(EGindex1)
      uploaddata1$Center.amino.acid<-unlist(centeranjisuan)
      colnames(uploaddata1)<-c("Pep.upload","Stripped.pep","Pep.main.index","Pep.all.index","Center.amino.acid")

      if(input$xuanzebgdatabase==1){
        wuzhong<-strsplit(input$metabopathspeciesselect,"-")[[1]][1]
        if(is.na(wuzhong)){
          stop("Please select one background dataset or upload a fasta file as background dataset in the 'Import Data' step!")
        }else{
          datafasta<-readAAStringSet(paste0("fasta/",wuzhong,".fasta"))
        }
      }else{
        files <- isolate(input$fastafileown)
        if(is.null(files)){
          stop("Please select one background dataset or upload a fasta file as background dataset in the 'Import Data' step!")
        }else{
          datafasta<-readAAStringSet(files$datapath)
        }
      }

      n_data_fasta<-length(datafasta@ranges@NAMES)
      pro_seqdf<-as.data.frame(datafasta)
      pro_seqdfnames<-unlist(lapply(rownames(pro_seqdf),function(x) strsplit(x,"\\|")[[1]][2]))
      danlength<-input$minseqs
      seqseqall<-vector()
      proidall<-vector()
      proidindexall<-vector()
      withProgress(message = 'Generating data', style = "notification", detail = "index 1", value = 0,{
        for(i in 1:nrow(uploaddata1)){
          seqindex1<-grep(uploaddata1$Stripped.pep[i],pro_seqdf$x, perl = TRUE)
          seqindex3<-as.numeric(strsplit(uploaddata1$Pep.main.index[i],";")[[1]])
          seqseqall1<-vector()
          proidindexall1<-vector()
          if(length(seqindex1)>0 & length(seqindex3)>0){
            for(k in 1:length(seqindex1)){
              seqindex2<-stri_locate_all(pattern = uploaddata1$Stripped.pep[i], pro_seqdf$x[seqindex1[k]], fixed = TRUE)[[1]][,1]
              seqnchar<-nchar(pro_seqdf$x[seqindex1[k]])
              indexjian<-unlist(lapply(seqindex2, function(x){x+seqindex3-1}))
              seqseq<-vector()
              for(j in 1:length(indexjian)){
                indexjian1<-indexjian[j]-danlength
                indexjian2<-indexjian[j]+danlength
                if(indexjian1<=0){
                  xhx1<-paste(rep("_",abs(indexjian1)+1),collapse ="")
                  xhx2<-stri_sub(pro_seqdf$x[seqindex1[k]],from = 0,to=indexjian2)
                  xhx3<-paste0(xhx1,xhx2)
                }
                else if(indexjian2>seqnchar){
                  xhx1<-paste(rep("_",(indexjian2-seqnchar)),collapse="")
                  xhx2<-stri_sub(pro_seqdf$x[seqindex1[k]],from = indexjian1,to=seqnchar)
                  xhx3<-paste0(xhx2,xhx1)
                }
                else{
                  xhx3<-stri_sub(pro_seqdf$x[seqindex1[k]],from = indexjian1,to=indexjian2)
                }
                seqseq[j]<-xhx3
              }
              seqseqall1[k]<-paste(seqseq,collapse = ";")
              proidindexall1[k]<-paste(indexjian,collapse = ";")
            }
            seqseqall[i]<-paste(seqseqall1,collapse = "::")#"_",";"
            proidall[i]<-paste(pro_seqdfnames[seqindex1],collapse = "::")
            proidindexall[i]<-paste(proidindexall1,collapse = "::")
          }else{
            seqseqall[i]<-"No Match"
            proidall[i]<-"No Match"
            proidindexall[i]<-"No Match"
          }

          incProgress(1/nrow(uploaddata1), detail = paste("index", i))
        }
      })

      uploaddata1$Seqwindows<-seqseqall
      uploaddata1$PRO.from.Database<-proidall
      uploaddata1$PROindex.from.Database<-proidindexall
      datareaddq<-uploaddata1
    }

    if(input$seqalignhanif){
      containif<-rep("No",nrow(datareaddq))
      if(ncol(datareaddq)==1){
        containif[grep(input$seqalignhan,datareaddq[[1]])]<-"Yes"
      }else{
        containif[grep(input$seqalignhan,datareaddq$Seqwindows)]<-"Yes"
      }
      datareaddq$Contain.if<-containif
    }
    datareaddqxx<-datareaddq[datareaddq$Seqwindows!="No Match",]
    datareaddqxx
  })
  seqduiqiduositeout<-reactive({
    datareaddq<-seqduiqioutx()#isolate(seqduiqioutx())
    sitesnum<-unlist(lapply(datareaddq$Pep.all.index,function(x){
      length(strsplit(x,";")[[1]])
    }))
    datareaddqx<-datareaddqx1<-datareaddq[sitesnum>1,]
    if(nrow(datareaddqx)>0){
      Seqwindows_MultiSites<-vector()
      withProgress(message = 'Generating data', style = "notification", detail = "index 1", value = 0,{
        for(i in 1:nrow(datareaddqx)){
          pepindexi1<-as.numeric(strsplit(datareaddqx$Pep.all.index[i],";")[[1]])
          pepindexi2<-as.numeric(strsplit(datareaddqx$Pep.main.index[i],";")[[1]])
          pepindexi<-setdiff(pepindexi1,pepindexi2)
          seqwindowi<-strsplit(datareaddqx$Seqwindows[i],";")[[1]]
          
          Seqwindows_multix<-vector()
          for(ii in 1:length(pepindexi2)){
            seqwindowix<-strsplit(seqwindowi[ii],"")[[1]]
            if(length(pepindexi)>0){
              posi<-input$minseqs+1+(pepindexi-pepindexi2[ii])
              posi_low<-which(posi>length(seqwindowix) | posi<1)
              if(length(posi_low)>0){
                seqwindowix[posi[-posi_low]]<-"X"
              }else{
                seqwindowix[posi]<-"X"
              }
              Seqwindows_multix[ii]<-paste(seqwindowix,collapse ="")
            }else{
              Seqwindows_multix[ii]<-seqwindowi[ii]
            }
          }
          Seqwindows_MultiSites[i]<-paste(Seqwindows_multix,collapse =";")
          incProgress(1/nrow(datareaddqx), detail = paste("index", i))
        }
        datareaddqx$Seqwindows_MultiSites<-Seqwindows_MultiSites
      })
      if(!input$classicmultisiteif){
        sitesnum_main<-unlist(lapply(datareaddqx$Pep.main.index,function(x){
          length(strsplit(x,";")[[1]])
        }))
        datareaddqx2<-datareaddqx[sitesnum_main>1,]
        Seqwindows_MultiSites_main<-vector()
        withProgress(message = 'Generating data', style = "notification", detail = "index 1", value = 0,{
          for(i in 1:nrow(datareaddqx2)){
            pepindexi2<-as.numeric(strsplit(datareaddqx2$Pep.main.index[i],";")[[1]])
            seqwindowi<-strsplit(datareaddqx2$Seqwindows[i],";")[[1]]
            
            Seqwindows_multix_main<-vector()
            for(ii in 1:length(pepindexi2)){
              seqwindowix<-strsplit(seqwindowi[ii],"")[[1]]
              posi<-input$minseqs+1+(pepindexi2[-ii]-pepindexi2[ii])
              posi_low<-which(posi>length(seqwindowix) | posi<1)
              if(length(posi_low)>0){
                seqwindowix[posi[-posi_low]]<-"Z"
              }else{
                seqwindowix[posi]<-"Z"
              }
              Seqwindows_multix_main[ii]<-paste(seqwindowix,collapse ="")
              
            }
            Seqwindows_MultiSites_main[i]<-paste(Seqwindows_multix_main,collapse =";")
            incProgress(1/nrow(datareaddqx2), detail = paste("index", i))
          }
          datareaddqx$Seqwindows_MultiSites[which(sitesnum_main>1)]<-Seqwindows_MultiSites_main
        })
        #datareaddq_all1<-datareaddq[sitesnum<=1,]
        #datareaddq_all<-rbind(datareaddq_all1,datareaddqx)
      }
    }else{
      datareaddqx<-NULL
    }
    datareaddqx
    #list(datareaddq_all=datareaddq_all,datareaddq_multi=datareaddqx)
  })
  seqduiqiout<-reactive({
    duiqidfall1<<-seqduiqioutx()#isolate(seqduiqioutx())
    datareaddq_multi1<<-isolate(seqduiqiduositeout())
    if(is.null(datareaddq_multi1)){
      duiqidfall<-duiqidfall1
    }else{
      datareaddq_multi1$Seqwindows<-datareaddq_multi1$Seqwindows_MultiSites
      sitesnum<-unlist(lapply(duiqidfall1$Pep.main.index,function(x){
        length(strsplit(x,";")[[1]])
      }))
      if(!input$classicmultisiteif){
        datareaddq_all1<-duiqidfall1[sitesnum<=1,]
        duiqidfall<-rbind(datareaddq_all1,datareaddq_multi1[,-ncol(datareaddq_multi1)])
      }else{
        duiqidfall<-duiqidfall1
      }
    }
    duiqidfall
  })
  seqduiqievent<-eventReactive(input$mcsbtn_seqalign,{
    datareaddq<-seqduiqiout()
  })


  output$seqduiqi<-renderDataTable({
    datareaddq<-seqduiqievent()#isolate(seqduiqiout())
    datatable(datareaddq, options = list(pageLength = 10))
  })
  output$seqduiqidl<-downloadHandler(
    filename = function(){paste("Prealign_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(seqduiqievent(),file,row.names=FALSE)
    }
  )

  output$seqduiqiplot<-renderPlot({
    library(ggplot2)
    library(ggrepel)

    datareaddq<-seqduiqievent()#isolate(seqduiqiout())
    sitesnum<-unlist(lapply(datareaddq$Pep.all.index,function(x){
      length(strsplit(x,";")[[1]])
    }))
    datareaddq1<-as.data.frame(table(sitesnum))
    ggplot(datareaddq1,aes(x=sitesnum,y=Freq, group=1))+
      geom_bar(stat = "identity",col=colpalettes[1:nrow(datareaddq1)],fill=colpalettes[1:nrow(datareaddq1)],alpha=0.8)+
      geom_line(size=1.5,col=colpalettes[17]) +
      geom_point(size=6, col=colpalettes[16],shape=18)+
      geom_text_repel(aes(label=Freq),size=6)+
      labs(x="Sites Number",y="Counts",title = "Distribution of Modification Sites")+
      theme_bw()
  })
  seqduiqiplotout<-reactive({
    datareaddq<-seqduiqievent()#isolate(seqduiqiout())
    sitesnum<-unlist(lapply(datareaddq$Pep.all.index,function(x){
      length(strsplit(x,";")[[1]])
    }))
    datareaddq1<-as.data.frame(table(sitesnum))
    ggplot(datareaddq1,aes(x=sitesnum,y=Freq, group=1))+
      geom_bar(stat = "identity",col=colpalettes[1:nrow(datareaddq1)],fill=colpalettes[1:nrow(datareaddq1)],alpha=0.8)+
      geom_line(size=1.5,col=colpalettes[17]) +
      geom_point(size=6, col=colpalettes[16],shape=18)+
      geom_text_repel(aes(label=Freq),size=6)+
      labs(x="Sites Number",y="Counts",title = "Distribution of Modification Sites")+
      theme_bw()
  })
  output$seqduiqiplotdl<-downloadHandler(
    filename = function(){paste("SiteNumplot",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file, width = 7,height = 7)
      print(seqduiqiplotout())
      dev.off()
    }
  )

  output$seqduiqiduosite<-renderDataTable({
    datatable(seqduiqiduositeout())
  })
  output$seqduiqiduositedl<-downloadHandler(
    filename = function(){paste("Prealign_MultiSites_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(seqduiqiduositeout(),file,row.names=FALSE)
    }
  )

  #
  motiffujiout<-reactive({
    datareaddq<<-seqduiqiout()
    datareadbj<<-seqbjdataout()
    fastaseqownoutdf<<-fastaseqownout()
    wuzhong<<-strsplit(input$metabopathspeciesselect,"-")[[1]][1]
    if(input$onlymultisiteif){
      seqduiqiduositedf<-seqduiqiduositeout()
      fgseqs<-unique(unlist(lapply(seqduiqiduositedf$Seqwindows_MultiSites,function(x) strsplit(x,";|::")[[1]])))
    }else{
      fgseqs<-unique(unlist(lapply(datareaddq$Seqwindows,function(x) strsplit(x,";|::")[[1]])))
    }

    withProgress(message = 'Motif Enrichment:',min = 0, max = 2, style = "notification", detail = "Generating data", value = 1,{
      if(is.null(datareadbj)){
        if(input$xuanzebgdatabase==1){
          #if(input$motifquanbuif){
          #  load(file = paste0("winsSTY_",wuzhong,".RData"))
          #  motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(seqseqalldf_STY$Windows), central.res = input$centralres,
          #                   min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
          #}else{
          #  #warning("Please note: No background dataset is chosen or uploaded! The foreground dataset is treated as the background database by default, but this is not recommended!")
          #  motseq <- motifx(fg.seqs=fgseqs, bg.seqs=fgseqs, central.res = input$centralres,
          #                   min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
          #}
          load(file = paste0("winsSTY_",wuzhong,".RData"))
          motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(seqseqalldf_STY$Windows), central.res = input$centralres,
                           min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
        }else{
          motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(fastaseqownoutdf$Windows), central.res = input$centralres,
                           min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
        }
      }else{
        motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(datareadbj[[1]]),central.res = input$centralres,
                         min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
      }
      
      shiny::incProgress(1, detail = "Generating data")
    })
    
    #motseq<<-motseq
    if(is.null(motseq)){
      stop("No enrichment results, maybe you need to adjust the 'Minimum number' and/or 'P-value threshold' parameters~~")
    }
    motseqdf<-motseq$df
    motseqdf$Enrich.seq<-sapply(motseq$motiflist,function(x) paste(x$pos,collapse = ";"))
    matchpro<-sapply(motseq$motiflist,function(x){
      xx<-unlist(lapply(x$pos,function(x) grep(x,datareaddq$Seqwindows,perl=TRUE)))
      paste(unique(datareaddq$PRO.from.Database[xx]),collapse = ";")
    })
    motseqdf$Enrich.pro<-matchpro
    motseqdf
  })
  motiffujiout2<-reactive({
    datareaddq<-seqduiqiout()
    motiffujioutx<-motiffujiout()[,-9]
    tabdata1<-tidyr::separate_rows(motiffujioutx, Enrich.seq, sep =";")
    tabdata1x<-unique(tabdata1)
    tabdata2<-tidyr::separate_rows(datareaddq[,-c(2:4)], Seqwindows,PROindex.from.Database, sep =";")
    tabdata3<-unique(tabdata2)
    tabdata4<-base::merge(tabdata1x,tabdata3,by.x="Enrich.seq",by.y="Seqwindows",sort=FALSE)
    tabdata4
  })
  regularmotiffujiout<-reactive({
    datareaddq<<-seqduiqiout()
    datareadbj<<-seqbjdataout()
    fastaseqownoutdf<<-fastaseqownout()
    wuzhong<<-strsplit(input$metabopathspeciesselect,"-")[[1]][1]
    if(input$onlymultisiteif){
      seqduiqiduositedf<-seqduiqiduositeout()
      datareaddq1<-seqduiqiduositedf$Seqwindows_MultiSites[seqduiqiduositedf$Contain.if=="Yes"]
      fgseqs<-unique(unlist(lapply(datareaddq1,function(x) strsplit(x,";|::")[[1]])))
    }else{
      datareaddq1<-datareaddq$Seqwindows[datareaddq$Contain.if=="Yes"]
      fgseqs<-unique(unlist(lapply(datareaddq1,function(x) strsplit(x,";|::")[[1]])))
    }

    withProgress(message = 'Motif Enrichment:',min = 0, max = 2, style = "notification", detail = "Generating data", value = 1,{
      if(is.null(datareadbj)){
        if(input$xuanzebgdatabase==1){
          #if(input$motifquanbuif){
          #  load(file = paste0("winsSTY_",wuzhong,".RData"))
          #  motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(seqseqalldf_STY$Windows), central.res = input$centralres,
          #                   min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
          #}else{
          #  #warning("Please note: No background dataset is chosen or uploaded! The foreground dataset is treated as the background database by default, but this is not recommended!")
          #  motseq <- motifx(fg.seqs=fgseqs, bg.seqs=fgseqs, central.res = input$centralres,
          #                   min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
          #}
          load(file = paste0("winsSTY_",wuzhong,".RData"))
          motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(seqseqalldf_STY$Windows), central.res = input$centralres,
                           min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
        }else{
          motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(fastaseqownoutdf$Windows), central.res = input$centralres,
                           min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
        }
      }else{
        motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(datareadbj[[1]]),central.res = input$centralres,
                         min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
      }
      
      shiny::incProgress(1, detail = "Generating data")
    })
    
    #motseq<<-motseq
    if(is.null(motseq)){
      stop("No enrichment results, maybe you need to adjust the 'Minimum number' and/or 'P-value threshold' parameters~~")
    }
    motseqdf<-motseq$df
    motseqdf$Enrich.seq<-sapply(motseq$motiflist,function(x) paste(x$pos,collapse = ";"))
    matchpro<-sapply(motseq$motiflist,function(x){
      xx<-unlist(lapply(x$pos,function(x) grep(x,datareaddq$Seqwindows,perl=TRUE)))
      paste(unique(datareaddq$PRO.from.Database[xx]),collapse = ";")
    })
    motseqdf$Enrich.pro<-matchpro
    motseqdf
  })

  observeEvent(
    input$mcsbtn_motifquanbu,{
      shinyjs::show(id = "motiffujidfxuanze_btn", anim = FALSE)
      output$motiffuji<-renderDataTable({
        motiffujidf<-isolate(motiffujiout())
        datatable(motiffujidf, options = list(pageLength = 10))
      })
      output$motiffujidl<-downloadHandler(
        filename = function(){paste("Motif.Enrich_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(motiffujiout(),file,row.names=FALSE)
        }
      )
      #
      output$motiffuji2<-renderDataTable({
        motiffujidf<-isolate(motiffujiout2())
        datatable(motiffujidf, options = list(pageLength = 10))
      })
      output$motiffujidl2<-downloadHandler(
        filename = function(){paste("Motif.Enrich.mapped_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(motiffujiout2(),file,row.names=FALSE)
        }
      )
      #
      output$regularmotiffuji<-renderDataTable({
        motiffujidf<-isolate(regularmotiffujiout())
        datatable(motiffujidf, options = list(pageLength = 10))
      })
      output$regularmotiffujidl<-downloadHandler(
        filename = function(){paste("RegularMotif.Enrich_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(regularmotiffujiout(),file,row.names=FALSE)
        }
      )
    }
  )

  motifplot_height <- reactive({
    heightx<-input$motifplot_height
    heightx
  })

  observeEvent(
    input$mcsbtn_motifplot,{
      output$motifplot<-renderPlot({
        library(ggseqlogo)
        library(igraph)
        library(ggraph)

        motiffujidf<-isolate(motiffujiout())
        enrichseqnumstr<-isolate(as.numeric(strsplit(input$enrichseqnum,"-|;")[[1]]))
        if(input$equalheightif){
          equalh<-"probability"
        }else{
          equalh<-"bits"
        }
        if(length(enrichseqnumstr)==1){
          enrichseq<-strsplit(motiffujidf$Enrich.seq[enrichseqnumstr],";")[[1]]
          ggseqlogo(enrichseq, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }else{
          motiffujidf1<-motiffujidf[enrichseqnumstr[1]:enrichseqnumstr[2],]
          enrichseq<-lapply(motiffujidf1$Enrich.seq,function(x){
            xx<-strsplit(x,";")[[1]]
          })
          names(enrichseq)<-motiffujidf1$motif
          ggseqlogo(enrichseq, ncol = 2, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }
      },height = motifplot_height)

      motifplotout<-reactive({
        motiffujidf<-isolate(motiffujiout())
        enrichseqnumstr<-isolate(as.numeric(strsplit(input$enrichseqnum,"-|;")[[1]]))
        if(input$equalheightif){
          equalh<-"probability"
        }else{
          equalh<-"bits"
        }
        if(length(enrichseqnumstr)==1){
          enrichseq<-strsplit(motiffujidf$Enrich.seq[enrichseqnumstr],";")[[1]]
          ggseqlogo(enrichseq, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }else{
          motiffujidf1<-motiffujidf[enrichseqnumstr[1]:enrichseqnumstr[2],]
          enrichseq<-lapply(motiffujidf1$Enrich.seq,function(x){
            xx<-strsplit(x,";")[[1]]
          })
          names(enrichseq)<-motiffujidf1$motif
          ggseqlogo(enrichseq, ncol = 2, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }
      })

      output$motifplotdownload<-downloadHandler(
        filename = function(){paste("Motifplot",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = motifplot_height()/100,height = motifplot_height()/100+3)
          print(motifplotout())
          dev.off()
        }
      )
    }
  )

  #
  fastaseqout<-reactive({
    files <- input$fastafile
    if(is.null(files)){
      datareadfasta<-NULL
    }else{
      datafasta<-readAAStringSet(files$datapath)
      pro_seqdf<-pro_seqdf1<-as.data.frame(datafasta)
      pro_seqdf_rown<-unlist(lapply(rownames(pro_seqdf1),function(x) strsplit(x,"\\|")[[1]][2]))
      rownames(pro_seqdf1)<-pro_seqdf_rown
      pro_seqdfncar<-unlist(lapply(pro_seqdf1$x,nchar))
      pro_seqdf<-pro_seqdf1[pro_seqdfncar>20,,drop=FALSE]
      n_data_fasta<-nrow(pro_seqdf)
      danlength<-input$minseqs
      wincenter<-strsplit(input$centralres,"")[[1]]
      seqwindowsall_S<-vector()
      seqnamesall_S<-vector()
      wincenteri<-vector()
      k<-1
      for(ii in wincenter){
        withProgress(message = paste('Generating data',ii), style = "notification", detail = "index 1", value = 0,{
          for(i in 1:n_data_fasta){
            seqindex1<-stri_locate_all(pattern = ii, pro_seqdf$x[i], fixed = TRUE)[[1]][,1]
            if(length(seqindex1)>0){
              seqnchar<-nchar(pro_seqdf$x[i])
              seqseq<-vector()
              for(j in 1:length(seqindex1)){
                indexjian1<-seqindex1[j]-danlength
                indexjian2<-seqindex1[j]+danlength
                if(indexjian1<=0){
                  xhx1<-paste(rep("_",abs(indexjian1)+1),collapse ="")
                  xhx2<-stri_sub(pro_seqdf$x[i],from = 0,to=indexjian2)
                  xhx3<-paste0(xhx1,xhx2)
                }
                else if(indexjian2>seqnchar){
                  xhx1<-paste(rep("_",(indexjian2-seqnchar)),collapse="")
                  xhx2<-stri_sub(pro_seqdf$x[i],from = indexjian1,to=seqnchar)
                  xhx3<-paste0(xhx2,xhx1)
                }
                else{
                  xhx3<-stri_sub(pro_seqdf$x[i],from = indexjian1,to=indexjian2)
                }
                seqwindowsall_S[k]<-xhx3
                seqnamesall_S[k]<-rownames(pro_seqdf)[i]
                wincenteri[k]<-ii
                k<-k+1
              }
            }
            incProgress(1/n_data_fasta, detail = paste("index", i))
          }
        })
      }
      datareadfasta<-data.frame(ID=seqnamesall_S,Windows=seqwindowsall_S,
                                Center=wincenteri,stringsAsFactors = F)
    }
    datareadfasta
  })
  observeEvent(
    input$mcsbtn_fastaalign,{
      output$allfasta<-renderDataTable({
        if(input$refastafileif){
          datareaddq<-isolate(fastaseqout())
        }else{
          datareaddq<-isolate(fastaseqownout())
        }
        datatable(datareaddq, options = list(pageLength = 10))
      })
      output$allfastadl<-downloadHandler(
        filename = function(){paste("Fasta.align_data",usertimenum,".csv",sep="")},
        content = function(file){
          fwrite(fastaseqout(),file)
        }
      )
    }
  )
  #
  kinasedataout<-reactive({
    load(file = "PSP_NetworKIN_Kinase_Substrate_Dataset_July2016.rdata")
    #KSData<-read.csv("PSP_NetworKIN_Kinase_Substrate_Dataset_July2016.csv",stringsAsFactors = F)
    KSData<-Kinase_Substrate_Dataset
    KSData.filtered <- KSData[grep("[a-z]", KSData$Source),]
    KSData.filtered <- KSData.filtered[(KSData.filtered$networkin_score >= input$NetworKINcutoff),]
    KSData.filtered$networkin_score[is.infinite(KSData.filtered$networkin_score)]<-300
    motiffujidf<-motiffujiout2()
    #motiffujidf_motifdf<-NULL
    #for(i in 1:nrow(motiffujidf)){
    #  motiffujidf_pro<-strsplit(motiffujidf$Enrich.pro[i],";|_")[[1]]
    #  motiffujidf_motif<-rep(motiffujidf$motif[i],length(motiffujidf_pro))
    #  xxdf<-data.frame(Motif=motiffujidf_motif,KIN_ACC_ID=motiffujidf_pro,stringsAsFactors = FALSE)
    #  motiffujidf_motifdf<-rbind(motiffujidf_motifdf,xxdf)
    #}
    #dfmerge1<-base::merge(motiffujidf_motifdf,KSData.filtered,by="KIN_ACC_ID",sort=FALSE)
    #dfmerge<-unique(dfmerge1[,c(1,8,2:3,6,14)])
    KSData.filtered1<-KSData.filtered[,c(1:3,5,7,8,13)]
    motiffujidf1<-motiffujidf[,-c(3:8)]
    dfmerge<-base::merge(KSData.filtered1,motiffujidf1,by.y="PRO.from.Database",by.x="KIN_ACC_ID",sort=FALSE)
    colnames(dfmerge)[9]<-"Motif"
    dfmerge
  })

  output$kinasemotifui<-renderUI({
    kkdf<-kinasedataout()
    selectInput("kinasemotif","Select Motif class:",choices = unique(kkdf$Motif),selected = unique(kkdf$Motif)[1],multiple = TRUE)
  })

  observeEvent(
    input$mcsbtn_kniase,{
      output$kinasedata<-renderDataTable({
        kinasedatadf<-isolate(kinasedataout())
        kinasedatadf1<-paste0("https://www.uniprot.org/uniprot/",kinasedatadf[[1]])
        kinasedatadf[[1]]<-paste0("<a href='",kinasedatadf1,"' target='_blank'>",kinasedatadf[[1]],"</a>")
        kinasedatadf2<-paste0("https://www.uniprot.org/uniprot/",kinasedatadf[[5]])
        kinasedatadf[[5]]<-paste0("<a href='",kinasedatadf2,"' target='_blank'>",kinasedatadf[[5]],"</a>")
        datatable(kinasedatadf,escape = FALSE,selection="single",class = "cell-border hover",
                  options = list(pageLength = 10,columnDefs = list(list(className = 'dt-center', targets = 0:2))))
      })
      output$kinasedatadl<-downloadHandler(
        filename = function(){paste("Kinase_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(kinasedataout(),file,row.names=FALSE)
        }
      )
      #
      cmheatmap_height<-reactive({
        input$cmheatmap_height
      })
      output$cmheatmappic<-renderPlot({
        library(graphlayouts)
        library(scales)

        dfmergex<-kinasedataout()
        #aa<<-input$kinasemotif
        dfmerge<-isolate(dfmergex[dfmergex$Motif%in%c(input$kinasemotif),])
        #data.frame(name=base::unique(nodesdf2$namex),stringsAsFactors = FALSE)
        #motifgr<-lapply(nodesdf$name,function(x) paste())
        if(input$genenamesif){
          edgesdf<-data.frame(from=dfmerge$KINASE,to=dfmerge$SUBSTRATE,edgecol=dfmerge$Motif,
                              Scores=scales::rescale(dfmerge$networkin_score,to=c(0.1,5)),stringsAsFactors = FALSE)
          nodesdf1<-data.frame(name=c(dfmerge$KINASE,dfmerge$SUBSTRATE),
                               motifgr=c(rep("Kinase",length(dfmerge$KINASE)),rep("Substrate",length(dfmerge$SUBSTRATE))),
                               stringsAsFactors = FALSE)
          nodesdf3<-nodesdf2<-unique(nodesdf1)
          jiaohudouyou<-intersect(dfmerge$KINASE,dfmerge$SUBSTRATE)
          if(length(jiaohudouyou)>0) nodesdf3$motifgr[nodesdf2$name %in% jiaohudouyou]<-"Combine"
          nodesdf<-unique(nodesdf3)
        }else{
          edgesdf<-data.frame(from=dfmerge$KIN_ACC_ID,to=dfmerge$SUB_ACC_ID,edgecol=dfmerge$Motif,
                              Scores=scales::rescale(dfmerge$networkin_score,to=c(0.1,5)),stringsAsFactors = FALSE)
          nodesdf1<-data.frame(name=c(dfmerge$KIN_ACC_ID,dfmerge$SUB_ACC_ID),
                               motifgr=c(rep("Kinase",length(dfmerge$KIN_ACC_ID)),rep("Substrate",length(dfmerge$SUB_ACC_ID))),
                               stringsAsFactors = FALSE)
          nodesdf3<-nodesdf2<-unique(nodesdf1)
          jiaohudouyou<-intersect(dfmerge$KIN_ACC_ID,dfmerge$SUB_ACC_ID)
          if(length(jiaohudouyou)>0) nodesdf3$motifgr[nodesdf2$name %in% jiaohudouyou]<-"Combine"
          nodesdf<-unique(nodesdf3)
        }
        gp<-graph_from_data_frame(edgesdf, directed=TRUE, vertices=nodesdf)
        V(gp)$grp <- nodesdf$motifgr
        #,layout="stress"
        ggraph(gp)+
          geom_edge_link(aes(col=edgecol),width=1,arrow = arrow(length = unit(4, 'mm')))+
          geom_node_point(aes(col=grp),size=5)+geom_node_text(aes(label = name), nudge_x = 0.1, nudge_y = 0.2)+
          scale_color_brewer(palette = "Set1")+
          theme_graph(base_family="sans")
      },height = cmheatmap_height)
      cmheatmappicout<-reactive({
        dfmergex<-kinasedataout()
        #aa<<-input$kinasemotif
        dfmerge<-isolate(dfmergex[dfmergex$Motif%in%c(input$kinasemotif),])
        nodesdf1<-data.frame(name=c(dfmerge$KIN_ACC_ID,dfmerge$SUB_ACC_ID),
                             motifgr=c(rep("Kinase",length(dfmerge$KIN_ACC_ID)),rep("Substrate",length(dfmerge$SUB_ACC_ID))),
                             stringsAsFactors = FALSE)
        nodesdf3<-nodesdf2<-unique(nodesdf1)
        jiaohudouyou<-intersect(dfmerge$KIN_ACC_ID,dfmerge$SUB_ACC_ID)
        if(length(jiaohudouyou)>0) nodesdf3$motifgr[nodesdf2$name %in% jiaohudouyou]<-"Combine"
        nodesdf<-unique(nodesdf3)#data.frame(name=base::unique(nodesdf2$namex),stringsAsFactors = FALSE)
        #motifgr<-lapply(nodesdf$name,function(x) paste())
        if(input$genenamesif){
          edgesdf<-data.frame(from=dfmerge$KINASE,to=dfmerge$SUBSTRATE,edgecol=dfmerge$Motif,
                              Scores=scales::rescale(dfmerge$networkin_score,to=c(0.1,5)),stringsAsFactors = FALSE)
        }else{
          edgesdf<-data.frame(from=dfmerge$KIN_ACC_ID,to=dfmerge$SUB_ACC_ID,edgecol=dfmerge$Motif,
                              Scores=scales::rescale(dfmerge$networkin_score,to=c(0.1,5)),stringsAsFactors = FALSE)
        }
        gp<-graph_from_data_frame(edgesdf, directed=TRUE, vertices=nodesdf)
        V(gp)$grp <- nodesdf$motifgr
        #,layout="stress"
        ggraph(gp)+
          geom_edge_link(aes(col=edgecol),width=1,arrow = arrow(length = unit(4, 'mm')))+
          geom_node_point(aes(col=grp),size=5)+geom_node_text(aes(label = name), nudge_x = 0.1, nudge_y = 0.2)+
          scale_color_brewer(palette = "Set1")+
          theme_graph(base_family="sans")
      })
      output$cmheatmappicdl<-downloadHandler(
        filename = function(){paste("Kinase_network",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = cmheatmap_height()/80+3,height = cmheatmap_height()/80+2)
          print(cmheatmappicout())
          dev.off()
        }
      )
    }
  )

})

shinyApp(ui = ui, server = server)
