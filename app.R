## TO DO:
## remove 3b results if same as 2b results
## in CI report, caffeiene exposure type is continuous - what is it in the triangulation summary script? - it's either 
## check data and coding for asthma and eczema - negative assoc with smoking - it's just in moba I think
## tidy cohort summary data for MoBa (regenerate it as a tableone and then follow make table one script)
## re-run table ones for ALSPAC and BiB (and MoBa?) after multiple pregnancies removed
# coef plot issues:
    ## coef put all subplots on same axis range
   ## consider adding another tab under coef plot for coef plots by outcome
# forest tab
    ## put on drop downs for everything, same as for coeff
    ## plot takes ages to load
# triangulation tab
## need to add an error message when the plot can't be drawn due to insufficient data (e.g. partner smoking fmi grs and other exposures)
## consider adding another drop down for timing of outcome as plots look too busy and patterns are hard to extract otherwise
## crashing for hdl cholesterol and insulin as outc, partner/mother smoking as exp (maybe something to do with letter case) - check this is resolved?

## DONE:
## verbal summary of odds ratio - rounded a bit too far, e.g. an OR or 0.85 is summarised as a 10% lower odds
## Depressive symptoms should be under outcome subclass 1 behaviour and affect? Currently have their own subclass 1
## runs locally but not in published version - something to do with triang summaries (perhaps not read/stored correctly?)
## add linking text to explain how each graph reveals something about causal inference
## why do we have results for model b for GRS that include alspac and BiB? Should just be MoBa with paternal grs data?
## Volcano plot ranked version not right - rank pvalues before generating volcanoes, else all paternal seem smaller than maternal
## doesn't look like model 3b is different from 2b if exposure in first trimester - check that first trimester should be/is being adjusted for preconception - It doesn't appear to be. So two options: 1) go back to cohorts and rerun with adjustment, 2) remove model 3 if exposure in first trimester and have model 3 just for adjusting for previous TPs IN PREG (and any time in preg if exp=postnatal). Have gone with opt 2 for now. If update to opt 1 in future, will have to edit combine_clean_meta_results
## make dose of SEP make sense - maybe change for all, so dose is called level or something and it's heavy vs none, light vs none, any vs none, etc and for SEP, lowest vs other
## remove age and sex adjusted BMI as it's confusing and the results are similar to non-adjusted (and we adjust for age and sex in the model anyway)
##change names of models, remove model c etc
##change SEP terminology to make it clear we're talking about lowest vs other SEP
## MCS any drinking in pregnancy ordinal should be binary /////
## model 3b doesn't exist for alcohol postnatal mother binary, is that right? /////
## Why are there a bunch of psychosocial continuous outcomes not available for cohorts other than MCS for mother ever alcohol in preg model 1a? Or moba for model 2b partner alcohol ever in preg. check ordinal and binary alcohol exposures the same in each cohort \\\
## download tab = select only some columns
## add interpretation tab fa-solid fa-quotes - remember that GRS are continuous so I think two exposures (GRS and caffeine) are continuous therefore the text needs updating for continuous exposures
## DONE: coef a bit broken - name of exposure sometimes covers data (if missing exp/outcome combo). Possible work around would be to just title Comparison 1, 2, etc and have a table above summarising what each is.
## DONE: confidence intervals sometimes join up over different lines.... example in screenshots
## DONE some psychosocial outcomes are called "behaviour and affect" for subclass 2, but should be more specific than that - these are depressive traits
## DONE change sdq (etc?) binary cut-off traits to have a different name from continuous, e.g. emotional problems (cont) vs emotional problems >CT etc, change 'autism' to autistic traits >CT if not based on diagnosis. CT=clinical threshold. Can do this in the original data/key or in the preparation/qc script
## DONE why is heavy maternal alcohol in second and third trim not available (bw as outcome)? DONE - it's because there aren't enough exposed
## manhattan exposures on x axis remove na from continuous or binary
## manhattan sort out tool tips (e.g. estimate to SMD and OR, NA where applicable)
## manhattan not all outcomes listed on x for eg SEP-psychosocial
## Add error message to volcanoes and manhattans to appear if eg model 3 for SEP doesn't exist, OR have model options dependent on previous selections
## run BiB stratified by ethnicity too for cross context comparison
## in original data, change sep binary comparison to be lowest vs any other, so that exposed == exposed to low SEP. Think I have done this (but check) need to rerun phewas and meta-analysis
## check sex stratified models because they look like they maybe haven't run for all exp/out combinations - they haven't run for continuous outcomes - have fixed. need to re-run for MoBa
## remove model c? - partially done - have done it in the combine_clean_meta_results file, but it could also happen in the phewas running file to cut down on time it takes to run that file


# Load required packages -------------------------------
library(shiny)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(bslib)
library(shinyalert)
library(shinydisconnect)
library(ggh4x)
library(ggnewscale)

source("data.R")
source("plot.R")


ui <- function(request) {
  fluidPage(
    tags$head(tags$style(HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
           "
        )
      )
    ),
    tags$head(tags$style(".modal { text-align: center}")),
    theme = bs_theme(version = 4, bootswatch = "minty"),#, heading_font = font_google("Segoe UI"))
    tags$head(tags$script('
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
                            ')),
    
    # Start with a loading page
    useShinyjs(),
    div(
      id = "loading_page",
      br(),br(),
      img(src='logo.png',
            style='width: 60%'),
      br(),
      p(HTML("<p>A tool to explore associations between prenatal exposures and child health. 
      <br>For more information about where the data came from, check out the <a href='https://gcsharp.github.io/EPoCH_website/'>EPoCH study website</a>. </p>"),
        style = "font-size: 85%"),
      br(),
      actionButton("load_results","Click to get started"),
    style="text-align: center;"),
    
    # Then move to the main app
  hidden(
    div( id="epoch",
  navbarPage(
    
  title = img(src='logo.png',
                  style="margin-top: 0px;
                               padding-right:10px;
                               padding-bottom:10px",
                  height = 60),
  windowTitle = "EPoCH Explorer",
  
              tabPanel("Information",icon=icon("circle-info"),
                       tabsetPanel(
                         tabPanel(title = "About EPoCH Explorer",id="about",
                       fluidRow(
                         column(1),
                         column(10, align="center",
                                uiOutput(outputId = "information_tab",style="text-align: left;")
                         ),
                         column(1)
                         )), # closing ABOUT
                       tabPanel(title="Cohort details",id="cohortdetails",
                                br(),
                                HTML("We used data from four cohorts: the Avon Longitudinal Study of Parents and Children (ALSPAC), Born in Bradford (BiB), Millennium Cohort Study (MCS), and the Mother, Father, and Child Cohort Study (MoBa).<br><br>Use the tabs below to explore the cohort summaries.<br><br>"),
                       tabsetPanel(
                       tabPanel(title="ALSPAC",id="alspac",
                                br(),
                                img(src='alspac-logo.jpeg', style='width: 20%; display: block; margin-left: auto; margin-right: auto;'),
                                br(),
                                p(HTML("<p>The Avon Longitudinal Study of Parents and Children (ALSPAC) is a longitudinal birth cohort study based in Bristol (UK) and the surrounding areas. Pregnant women resident in the area with expected dates of delivery between 1st April 1991 and 31st December 1992 were invited to take part in the study.  Click to see the <a href='https://pubmed.ncbi.nlm.nih.gov/22507742/'>cohort profile for the mothers</a> and the <a href='https://pubmed.ncbi.nlm.nih.gov/22507742/'>children.</a></p>")),
                       DTOutput("alspactable")
                       ),
                       tabPanel(title="BIB",id="bib",
                                br(),
                                img(src='bib-logo.png', style='width: 80%; display: block; margin-left: auto; margin-right: auto;'),
                                br(),
                                p(HTML("<p>Born in Bradford (BIB) was established to examine how genetic, nutritional, environmental, behavioural and social factors impact on health and development during childhood, and subsequently adult life in a deprived multi-ethnic population. Between 2007 and 2011, detailed information has been collected from 12453 women with 13776 pregnancies (recruited at ∼28 weeks) and 3448 of their partners. Click to see the <a href='https://pubmed.ncbi.nlm.nih.gov/23064411/'>cohort profile</a>.</p>")),
                                DTOutput("bibtable")
                       ),
                       tabPanel(title="MCS",id="mcs",
                                br(),
                                img(src='mcs-logo.png', style='width: 80%; display: block; margin-left: auto; margin-right: auto;'),
                                br(),
                                p(HTML("<p>The Millenium Cohort Study (MCS) is following the lives of 19,517 children born across the UK in 2000-01. Click to see the <a href='https://pubmed.ncbi.nlm.nih.gov/24550246/'>cohort profile</a>. </p>")),
                                DTOutput("mcstable")
                       ),
                       tabPanel(title="MOBA",id="moba",
                                br(),
                                img(src='moba-logo.png', style='width: 40%; display: block; margin-left: auto; margin-right: auto;'),
                                br(),
                                p(HTML("<p>The Norwegian Mother, Father and Child Cohort Study (MOBA) is a Norwegian national longitudinal study where over 90,000 pregnant women were recruited from 1998 to 2008. More than 70,000 fathers have participated. Click to see the <a href='https://pubmed.ncbi.nlm.nih.gov/16926217/'>cohort profile</a>.</p>")),
                                DTOutput("mobatable")
                       )
                         )#closing tabsetpanel containing cohort details
                       ),# closing COHORT DETAILS
                         tabPanel(title="Statistical models", id="models",
                                  fluidRow(
                                    column(1),
                                    column(10, align="center",
                                           uiOutput(outputId = "models_information_tab",style="text-align: left;")
                                    ),
                                    column(1)
                                  ))
                       )# closing information tabset panel
              ),#closing instructions tab
              
              tabPanel("Manhattan",icon=icon("fa-solid fa-city"),
                       h4("Manhattan plots"),
                       HTML("<br>Manhattan plots allow you to visualise trends by summarising the results (P values or effect estimates) across all exposures and outcomes.<br><br>Select the data to plot using the boxes below, and then click to generate the plot.<br><br>You can filter to display results for mother, partners, or both parents.<br><br>"),
                       h5("Select data to plot:"),
                       inputPanel(
                         selectizeInput(inputId = "exposure_choice_manhattans",
                                        label = NULL,
                                        choices = NULL,
                                        selected = NULL,
                                        multiple = F,
                                        options = list(placeholder = 'Exposure class', maxItems = 1)),
                         selectizeInput(inputId = "outcome_choice_manhattans",
                                        label = NULL,
                                        choices = NULL,
                                        selected = NULL,
                                        multiple = F,
                                        options = list(placeholder = 'Outcome class', maxItems = 1)),
                         selectizeInput(inputId = "model_choice_manhattans",
                                        label = NULL,
                                        choices = NULL,
                                        selected = NULL,
                                        multiple = F,
                                        options = list(placeholder = 'Model', maxItems = 1))
                       ),
   actionButton("plot_data_manhattans","Plot results",icon=icon("chart-simple")),
   br(), br(),
                       
                       fluidRow(
                         column(width=12, align="center",
                       tabsetPanel(
                         tabPanel(title="-log10(P) on Y-axis",id="manhattans_p",
                                  hr(),
                         tabsetPanel(type="pills",
                           tabPanel("Exposures on X-axis",
                                    withSpinner(plotlyOutput("exposureManhattanPlot_p", height="100%"),image = "spinner.gif")
                                    ),
                           tabPanel("Outcomes on X-axis",
                                    withSpinner(plotlyOutput("outcomeManhattanPlot_p", height="100%"),image = "spinner.gif")
                                    )
                         )
                         ),
                         
                       tabPanel(title="Coefficients on Y-axis",id="manhattans_c",
                                hr(),
                         tabsetPanel(type="pills",
                            tabPanel("Exposures on X-axis",
                                     withSpinner(plotlyOutput("exposureManhattanPlot_c", height="100%"),image = "spinner.gif")),
                            tabPanel("Outcomes on X-axis",
                                     withSpinner(plotlyOutput("outcomeManhattanPlot_c", height="100%"),image = "spinner.gif"))
                            )
                       )
                       )
                       )) #closing mainPanel for manhattans
              ),#closing manhattans tab
              
              tabPanel("Volcano",icon=icon("fa-solid fa-volcano"),
                       h4("Volcano plots"),
                       HTML("<br>Volcano plots allow you to visualise results by both precision (P value) and magnitude of association (effect estimate), while comparing results for each parent side by side. <br><br>Select the data to plot using the boxes below, and then click to generate the plot.<br><br>"),
                       h5("Select data to plot:"),
                                  inputPanel(
                                    selectizeInput(inputId = "exposure_choice_volcanoes",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = F,
                                                   options = list(placeholder = 'Exposure class', maxItems = 1)),
                                    selectizeInput(inputId = "outcome_choice_volcanoes",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = F,
                                                   options = list(placeholder = 'Outcome class', maxItems = 1)),
                                    selectizeInput(inputId = "model_choice_volcanoes",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = F,
                                                   options = list(placeholder = 'Model', maxItems = 1))
                                  ),# close input panel for volcanoes
                       actionButton("plot_data_volcanoes","Plot results",icon=icon("chart-simple")),
                       br(),br(),
                       fluidRow(
                         column(12, align="center",
                                tabsetPanel(type="pills",
                                            tabPanel("Ranked -log10(P-values)",
                                                     withSpinner(plotlyOutput("exposureVolcanoPlot_ranked"),image = "spinner.gif")
                                            ),
                                            tabPanel("Raw -log10(P-values)",
                                                     withSpinner(plotlyOutput("exposureVolcanoPlot_raw"),image = "spinner.gif")
                                            )
                                )
                       
              ))#closing main panel for volcano
              ),#closing volcano tab
              
              tabPanel("Coefficient plots", icon = icon("fa-solid fa-chart-gantt"), #fa-solid fa-starfighter-twin-ion-engine
                       h4("Coefficient plots"),
                       HTML("<br>Coefficient plots compare effect estimates and 95% confidence intervals across up to four different exposure/model combinations, for different sets of outcomes.<br><br>Select the exposures/models to plot using the boxes below, then select the outcome class of interest and click to generate the plot.<br><br>If you want to change your selection (e.g. from binary to continuous outcomes, or vice versa), you will have to reselect the outcome class first.<br><br>"),
                       h5("Select exposure and model:"),
                       inputPanel(
                                    selectizeInput(inputId = "exposure_choice_coeff",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = T,
                                                   options = list(placeholder = 'Class', maxItems = 1)),
                                    selectizeInput(inputId = "coeff_person",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   options = list(placeholder = 'Parent', maxItems = 1)),
                                    selectizeInput(inputId = "coeff_subclass",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   options = list(placeholder = 'Subclass', maxItems = 1)),
                                    selectizeInput(inputId = "coeff_exptime",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   options = list(placeholder = 'Time/type', maxItems = 1)),
                                    selectizeInput(inputId = "coeff_exptypedose",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   options = list(placeholder = 'Level/type', maxItems = 1)),
                                    selectizeInput(inputId = "model_choice_coeff",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = T,
                                                   options = list(placeholder = 'Model', maxItems = 1))
                                    ),
                       #  selectizeInput(width = '100%', inputId = "coeff_explink",
                       #                 label = NULL,
                       #                 choices = NULL,
                       #                 selected = NULL,
                       #                 multiple = F,
                       #                 options = list(placeholder = 'Select an option', maxItems = 1)),
                      actionButton("add_comp","Add to plot"),
                       uiOutput("showActiveLinkers"),
                       br(),
                       actionButton("clear_comps","Delete all from plot",
                                    style="color: #fff; background-color: #909fc7; border-color: #909fc7"),
                       h5("Select outcome:"),
                       inputPanel(
                                    selectizeInput(inputId = "outcome_choice_coeff",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = T,
                                                   width = "1200px",
                                                   options = list(placeholder = 'Class', maxItems = 1)),
                       selectizeInput(inputId = "outcome_type_coeff",
                                      label = NULL,
                                      choices = NULL,
                                      selected = NULL,
                                      multiple = T,
                                      width = "1200px",
                                      options = list(placeholder = 'Data type', maxItems = 1))
                       ),
                       actionButton("plot_data_coeff","Plot results",icon=icon("chart-simple")),
                       fluidRow(
                         column(12, align="center",
                                withSpinner(plotlyOutput("exposureCoeffPlot"),image = "spinner.gif")
                         ))
              ),
   
              tabPanel("Deep dive", icon = icon("fa-solid fa-otter"),
                       h4("Deep dive"),
                       HTML("<br>This tab allows you to narrow in on a specific exposure/outcome/model combination to find out more about the data used to derive the result, its interpretation, and to see how the result from each cohort compares to the meta-analysed summary in a forest plot.<br><br>Select the exposure/model/outcome combination to plot using the boxes below, and then click to generate the text and plot.<br><br>If you want to change your selection after generating this information, you may have to reselect some options before clicking the button again.<br><br>"),
                       h5("Select exposure:"),
                                  inputPanel(
                                    selectizeInput(inputId = "exposure_choice_forest",
                                                   label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = T,
                                                   options = list(placeholder = 'Class', maxItems = 1)),
                                      selectizeInput(inputId = "forest_person",
                                                            label = NULL,
                                                            choices = NULL,
                                                            selected = NULL,
                                                            options = list(placeholder = 'Parent', maxItems = 1)),
                                        selectizeInput(inputId = "forest_subclass",
                                                            label = NULL,
                                                            choices = NULL,
                                                            selected = NULL,
                                                            options = list(placeholder = 'Subclass', maxItems = 1)),
                                         selectizeInput(inputId = "forest_exptime",
                                                            label = NULL,
                                                            choices = NULL,
                                                            selected = NULL,
                                                            options = list(placeholder = 'Timing', maxItems = 1))
                                    ),
                       selectizeInput(width = '100%', inputId = "forest_explink",
                                              label = NULL,
                                               choices = NULL,
                                               selected = NULL,
                                               multiple = F,
                                      options = list(placeholder = 'Select an option', maxItems = 1)),
                       h5("Select outcome:"),
                       inputPanel(
                                    selectizeInput(inputId = "outcome_choice_forest",
                                                    label = NULL,
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = T,
                                                   options = list(placeholder = 'Class', maxItems = 1)),
                                             selectizeInput(inputId = "forest_outcometype",
                                                             label = NULL,
                                                            choices = NULL,
                                                            selected = NULL,
                                                            options = list(placeholder = 'Subclass', maxItems = 1)),
                                    ),
                                               selectizeInput(width = '100%', inputId = "forest_outlink",
                                                           label = NULL,
                                                           choices = NULL,
                                                           selected = NULL,
                                                           multiple = F,
                                                           options = list(placeholder = 'Select an option', maxItems = 1)),
h5("Select model:"),
inputPanel(
selectizeInput(inputId = "model_choice_forest",
               label = NULL,
               choices = NULL,
               selected = NULL,
               multiple = T,
               options = list(placeholder = '----------', maxItems = 1))
),
                                    actionButton("plot_data_forest","Plot results",icon=icon("chart-simple")),
                         fluidRow(
                           column(6, align="left",
                                  br(),
                                  h5("Verbal interpretation of results:"),
                                  htmlOutput("inWords")
                                  ),
                           column(6, align="center",
                                  br(),
                                  h5("Forest plot:"),
                                  withSpinner(plotlyOutput("forestPlot"),image = "spinner.gif")
                           ))
                       ),
tabPanel("Causal inference report", icon = icon("fa-solid fa-file"),
         h4("Causal inference and interpretation report"),
         HTML("<br>This tab allows you to see a report to help you evaluate and interpret the evidence around whether a parental health behaviour in pregnancy has a causal effect on your chosen outcome.<br><br>Select the exposure class, parent, and outcome combination you want to explore using the boxes below, and then click to generate the report.<br><br>"),
         fluidPage(
           column(12,
                  h5("Select exposure:"),
                  inputPanel(
                    selectizeInput(inputId = "exposure_class_tri",
                                   label = NULL,
                                   choices = NULL,
                                   selected = NULL,
                                   multiple = T,
                                   options = list(placeholder = 'Class', maxItems = 1)),
                    selectizeInput(inputId = "exposure_person_tri",
                                   label = NULL,
                                   choices = NULL,
                                   selected = NULL,
                                   options = list(placeholder = 'Parent', maxItems = 1))
                  ),
                  h5("Select outcome:"),
                  inputPanel(
                    selectizeInput(inputId = "outcome_class_tri",
                                   label = NULL,
                                   choices = NULL,
                                   selected = NULL,
                                   multiple = T,
                                   options = list(placeholder = 'Class', maxItems = 1)),
                    selectizeInput(inputId = "outcome_subclass1_tri",
                                   label = NULL,
                                   choices = NULL,
                                   selected = NULL,
                                   options = list(placeholder = 'Subclass 1', maxItems = 1)),
                    selectizeInput(inputId = "outcome_subclass2_tri",
                                   label = NULL,
                                   choices = NULL,
                                   selected = NULL,
                                   options = list(placeholder = 'Subclass 2', maxItems = 1))
                  ),
                  actionButton("gen_tri_report","Triangulate evidence",icon=icon("chart-simple"))),
           fluidRow(
             column(12,align="left",
                    withSpinner(uiOutput(outputId = "tri_report_summary",style="text-align: left;"),image="spinner.gif")
             )
           ),
           withSpinner(plotOutput("triangSummary",height="100px"),image = "spinner.gif"),
           br(),
           h4("Examine triangulated evidence"),
           HTML("You can use the buttons below to examine the results for each approach.<br>"),
           tabsetPanel(type="pills",
             tabPanel(title = "Multivariable regression (M)",id="mvr",
                      fluidRow(
                        column(12,align="left",
                               withSpinner(uiOutput(outputId = "tri_report_mvr",style="text-align: left;"),image="spinner.gif"),
                               withSpinner(plotlyOutput("triangMVRplot"),image = "spinner.gif")
                        )
                      )),
             tabPanel(title = "Dose (D)",id="dose",
                      fluidRow(
                        column(12,align="left",
                               withSpinner(uiOutput(outputId = "tri_report_dose",style="text-align: left;"),image="spinner.gif"),
                               withSpinner(plotlyOutput("triangDOSEplot"),image = "spinner.gif")
                        )
                      )),
             tabPanel(title = "Negative control exposure (N)",id="negcon",
                      fluidRow(
                        column(12,align="left",
                               withSpinner(uiOutput(outputId = "tri_report_negcon",style="text-align: left;"),image="spinner.gif"),
                               withSpinner(plotlyOutput("triangNCplot"),image = "spinner.gif")
                        )
                      )),
             tabPanel(title = "Timing (P)",id="timing",
                      fluidRow(
                        column(12,align="left",
                               withSpinner(uiOutput(outputId = "tri_report_timing",style="text-align: left;"),image="spinner.gif"),
                               withSpinner(plotlyOutput("triangTIMEplot"),image = "spinner.gif")
                        )
                      )),
             tabPanel(title = "Genetic risk score (G)",id="grs",
                      fluidRow(
                        column(12,align="left",
                               withSpinner(uiOutput(outputId = "tri_report_grs",style="text-align: left;"),image="spinner.gif"),
                               withSpinner(plotlyOutput("triangGRSplot"),image = "spinner.gif")
                        )
                      )),
             tabPanel(title = "Other exposures",id="other_exp",
                      fluidRow(
                        column(12,align="left",
                               withSpinner(uiOutput(outputId = "tri_report_other_exp",style="text-align: left;"),image="spinner.gif"),
                               withSpinner(plotlyOutput("triangOEplot"),image = "spinner.gif")
                        )
                      )),
             tabPanel(title = "Comparison to SEP",id="sep",
                      fluidRow(
                        column(12,align="left",
                               withSpinner(uiOutput(outputId = "tri_report_sep",style="text-align: left;"),image="spinner.gif"),
                               withSpinner(plotlyOutput("triangSEPplot"),image = "spinner.gif")
                        )
                      ))
           )
           )),


tabPanel("Download data", icon = icon("fa-solid fa-download"),
         fluidPage(
         column(12,
           img(src='logo.png',
               style='width: 20%; display: block; margin-left: auto; margin-right: auto;'),
           br(),
           p(HTML("<p>In the spirit of open science and friendly collaboration, all the results from the EPoCH study are available to download.
      <br>We just ask that you agree to do a few things, especially if you're planning to publish or present any research that uses these results. </p>"),
             style = "font-size: 85%"),
           br(),
           h6("Before downloading the data, I agree that..."),
           br(),
           checkboxGroupInput("checklist", label=NULL,width='100%',
                             choiceNames= list(
                                           HTML("<p>I understand the limitations of the EPoCH study (as outlined in the EPoCH paper, linked from the <a href='https://gcsharp.github.io/EPoCH_website/'>EPoCH study website</a>) and will consider these when drawing inferences and interpreting results </p>"),
                                                HTML("<p>I will contact Dr Gemma Sharp, the principal investigator for EPoCH, if I am unsure of any of the details of the study (up-to-date contact information on the <a href='https://gcsharp.github.io/EPoCH_website/'>EPoCH study website</a>) </p>"),
                                                     HTML("<p>I will cite the EPoCH study paper if I use results from EPoCH, and/or if I use EPoCH explorer or downloaded data in my own analyses and/or to generate hypotheses that form the basis of my own research. </p>")
                                           ),
                             choiceValues=list("p1","p2","p3"),
                             selected=NULL),
           textOutput("message"),
           downloadButton("download_results","Download EPoCH study results", 
                        style = "display: none;"),
           br(),br()
           )
         ))

              
  )#closing navbarPage
  )#closing div
  ),#closing hidden
  disconnectMessage2()
  )#closing fluidPage
  }#closing ui

server <- function(input, output, session) {
  
  global_data <- reactiveValues(data = NULL, data_is_loaded = FALSE,
                                coeff_linkers = NULL)

  source("data_server.R",local=T)$value
  source("plot_server.R",local=T)$value
}

# Load UI and server controls
shinyApp(ui = ui, server = server)