library(shiny)
library(dplyr)
library(leaflet)
library(shinyWidgets)
library(RColorBrewer)
library(lattice)
library(scales)
library(sp)
library(rgdal)
library(shinycssloaders)
library(ggplot2)
library(cvms)
library(car)
library(Rcpp)
library(Metrics)
library(merTools)
library(leaps)
library(sf)
library(tidyr)
library(groupdata2)
library(pROC)
library(DT)
library(shinyBS)
library(ggpubr)

#SAEapp=function(...){
#library(leaflet)
#source("global.R", local=TRUE)
source("loaddata_fn.R",local=TRUE)

ui <- fluidPage(theme='bootstrap.css',

      # Define UI for application that draws a histogram
    navbarPage(
      id="tabs",
      windowTitle = "SAE Tool",
      title="", 
      header=headerPanel(div(style = "width: 100%", img(src="mini-SAE-Logo.png", height="10%", width="10%"), "SAE Tool")), 
    
## Landing Page ## 
      tabPanel("",
           title="Home",
           h3("Getting Started with SAE"),
           p("Small area estimation techniques span a large array of statistical methods. The UNFPA method used in 
             this tool is aimed proviing a robust approach that can be implemented in any setting where both census and 
             survey data are available."),
          
           h4("Approach"),
           p("Population demographic data found in international household surveys often use binary 
             responses to estimate prevalence rates in populations. Because of this, the application 
             uses a logistic regression modeling approach. In addition, the distribution of effort in 
             population health surveys is rarely equal within the regions, meaning that over-representation 
             of sampling effort in some areas may bias results in others. For this reason, the 
             application allows for use of a random effects model."),
           br(),
           
           fluidRow(align="center",
             column(4),
             column(4, actionButton(inputId = "gotoIntro", label="Click here to start",   style="height:50px; width:170px; font-size:120%"), 
                    ),
             column(4)),
           
           hr(),
           fluidRow(align="center",
              column(4, 
                     h4("SAE for Family planning "),
                     p("For detailed information on the UNFPA SAE theory, method and the Nepal use case.")),
              column(4, 
                     h4("SAE training and videos"),
                     p("For training materials and videos showing how to use the app.")),
              column(4, 
                     h4("Offline Tool"),
                     p("If you have limited bandwidth, it is possible to download the tool from github and use it on your own computer.")),
           ),
           
           
           ### Pictures
           fluidRow(align="center",
                    column(4,
                           img(src="tab1_FPguidance.png", width="90%")),
                    column(4,
                           img(src="tab1_coursecover.png", width="90%")),
                    column(4,
                           img(src="tab1_github.png", width="90%")),
           ),
           
           
           ### link buttons
           fluidRow(align="center",
              column(4,
                     actionButton("theory", "SAE Theory", icon("chart-line", style='padding:4px; font-size:80%'), 
                                  onclick ="window.open('https://drive.google.com/file/d/1SKodX4STs6Rc1DLq3aKuifw4eR48xsTn/view', '_blank')", 
                                  style="height:50px; width:170px; font-size:120%")),
              column(4,
                     actionButton("manual", "SAE Training", icon("graduation-cap", style='padding:4px; font-size:80%'), 
                                  onclick ="window.open('https://unfpasae.github.io/SAE_Manual/', '_blank')", 
                                  style="height:50px; width:170px;font-size:120%")),
              column(4,
                     actionButton("download", "Offline Tool", icon("github", style='padding:4px; font-size:80%'), 
                                  onclick ="window.open('https://github.com/unfpasae/SAEpackage/', '_blank')", 
                                  style="height:50px; width:170px; font-size:120%")),
           ),
          
           br(),
           br(),
           hr()
           ), # End Landing Page


## Instructions ##
tabPanel("Instructions",
         navlistPanel("Instructions", widths=c(2,10),
                      tabPanel("Start here",
                               h4("Using the Tool"),
                               p("The application consists of different tabs for each stage of the analysis. These include selecting 
                       and comparing data, Setup of the models, model assessment and finally prediction. Under each tab, 
                       simple directions allow the user to control outputs and download results."),
                               hr(),
                               h4("Tab Navigation"),
                               p("The different stages of analysis are organized into tabs at the top of the page."),
                               img(src="tab1_toptabs.png", width="60%", border="6px"),
                               hr(),        
                               fluidRow(
                               column(8,
                                      h4("Sub-Tabs"),
                                      p("Certain stages of the app are divided into sub-tabs. Use these to explore different parts of the analysis.")),
                                column(4,
                                      br(),
                                      img(src="tab1_subtab.png", width="90%")
                                      )),
                               hr(),
                               fluidRow(
                                 column(8,
                                        h4("Downloading results"),
                                        p("At certain stages in the analysis, you can choose to download results tables, figures or maps.")),
                                 column(4,
                                        br(),
                                        img(src="tab_1_download.png", width="90%")
                                 )),
                               
                               hr(),
                               fluidRow(
                                 column(8,
                                        h4("Error Messages"),
                                        p("Note that you may encounter error messages, especially if you do not load data in Step 1.  Messages in grey 
                                          usually relate to something about data loading. Messages in red usually indicate unexpected behavior in the data. In this case 
                                          it is usually best to go back a step and see if the data are loaded and selected correctly. ")),
                                 column(4, 
                                        br(),
                                        img(src="tab1_error1.png", width="90%"),
                                        br(),
                                        img(src="tab1_error2.png", width="90%")
                                 )),
                               hr(),
                              
                      ),
                      tabPanel("Step 1: Load data",
                               h4("Data requirements"),
                               p("In order to be used in this tool, survey and census data must be harmonized appropriately. 
                        At minimum, this means that:",
                                 tags$li("Survey and census data must share exactly the same variable names"),
                                 tags$li("Categorical variables must share identical classes"),
                                 tags$li("Census and survey variables must have roughly similar distributions.")),
                               p("For a complete description on how to harmonize data, please see the ", 
                                 a(href="https://unfpasae.github.io/SAE_Manual/01-Harmonization.html", "Harmonization Guide", target="_blank"), 
                                 "in the SAE manual. You can also download a sample of formatted data from the ", 
                                 a(href="https://github.com/tomasbird/shinyapps_SAE/tree/main/R/data/Nepal", "Github site.", target="_blank"), 
                                 ),
                               hr(),
                               h4("Using pre-loaded data"),
                               fluidRow(
                                 column(8, 
                                        p("To get an understanding of how the tool works, pre-loaded numerical and spatialdata from Nepal are available. 
                                      Simply check the box and these data will be loaded into the tool. These data are helpful for understanding the analysis
                                          flow and the stucture of data needed.")
                                 ),
                                 column(4, 
                                        img(src="Use_Nepal.png", width="80%")
                                 )),
                               hr(),
                               
                               h4("Loading your own data"),
                               fluidRow(
                                 column(8, 
                                        p("If you have your own data formatted as described above, you can load them using the 
                                 load data dialogue. Note that you must do this for both census and survey datasets. Examples of harmonized 
                                          tabular data are avalable on the ", 
                                          a(href="https://github.com/tomasbird/shinyapps_SAE/tree/main/R/data/Nepal", "Github site.", target="_blank"))
                                 ),
                                 column(4, 
                                        img(src="tab1_upload.png", width="80%")
                                 )),
                               hr(),
                               
                               h4("Spatial data"),
                               fluidRow(
                                 column(8, 
                                        p("If you wish to view maps for the raw data and results, you will need to upload shapefiles.  These should be in
                                        .shp format and include all necessary metadata files. Sample shapefiles are available at the ",  
                                        a(href="https://github.com/tomasbird/shinyapps_SAE/tree/main/R/data/Nepal/Survey/shp", "Github site.", target="_blank"))
                                 ),
                                 column(4, 
                                        img(src="tab1_spatial_up.png", width="80%")
                                 )),
                               hr(),
                               
                               h4("Survey Data Indicator"),
                               fluidRow(
                                 column(8, 
                                        p("Once the survey data are loaded, you must identify the  indicator in the dataset.  In this case we have chosen CPR. 
                                          You must also select the column name that indicates the survey region in the dataset. In this case, it is REGNAME.")
                                 ),
                                 column(4, 
                                        img(src="tab1_indicators.png", width="80%")
                                 )),
                               hr(),
                               
                               h4("Choosing census data predictors"),
                               fluidRow(
                                 column(8, 
                                        p("Once the census data are loaded, you must identify the census spatial data field (in the sample data it is DIST).  
                                         you can also select or de-select any predictors you wish. Here we have excluded VDC_ID as it is not needed.")
                                 ),
                                 column(4, 
                                        img(src="tab1_census_pred.png", width="60%")
                                 )),
                               hr(),
                               br()
                      ),
                      tabPanel("Step 2: Inspect Predictors",
                               h4("Inspecting and choosing predictors"),
                               p("The SAE model technique here relies on using variables that occur in both DHS and census to predict the distribution of 
                                 the indicator."),
                               hr(),
                               fluidRow(
                                 column(8,
                                        h4("Choosing correlated covariates"),
                                        p("To be effective, the predictor variables need to have some level of linear 
                                 relatioship with the indicator. To choose these variables, we compare the
                                          indicator  predictor aggregated at the survey region level.  
                                          Variables that show some level of linear relationship can be kept in the model.")
                                 ),
                                 column(4,img(src="tab_2_Correlations.png", width="80%")
                                 )),
                               hr(),
                               fluidRow(
                                 column(8,
                                        h4("Comparing census and survey distributions"),
                                        p("It is also important that the census and survey data came from similar populations.
                                            For example, if respondant Age was an important factor, then it would be important 
                                            to confirm that census and survey datasets surveyed similar numbers in each age category.
                                            In the distributions tab, we look at the histograms of survey and census data in each 
                                            of the chosen covariates.")
                                 ),
                                 column(4,img(src="tab_2_Distributions.png", width="90%")
                                 )),
                               hr(),
                               br(),
                               fluidRow(
                                 column(8,
                                        h4("Spatial distribution of effort"),
                                        p("Finally it is important that each area was given a sampling effort proportional to the 
                                            population size. The survey maps provide a measure of how much sampling effort was allocated
                                          in each region.")
                                 ),
                                 column(4,img(src="tab_2_Spatial.png", width="90%")
                                 )),
                               br(),
                               hr()
                      ),
                      tabPanel("Step 3: Model Setup",
                               h3("Model Setup"),
                               p("This section allows you to do a final check on the validity of variables in one tab before 
                                 testing different model configurations in the other."),
                               br(),
                               fluidRow(
                               column(4,
                                      h4("Parameter Test Tab")),
                               column(4, 
                                      img(src="tab1_parametertest.png", width="90%"))),
                              hr(),
                               fluidRow(
                               column(8,
                                  h4("Aliasing test"),
                                  p("First, the Aliasing button allows you to test whether any of the variables are identical to 
                                 one another. Note that you must check for aliasing before running the model, 
                                 and remove any aliased variables from the model and you must reconfirm by running the aliasing
                                    test again.")),
                               column(4,
                                      img(src="tab3_Aliasing.png", width="80%")
                                      )),
                              hr(),
                               br(),
                               fluidRow(
                                 column(8,
                                        h4("Collinearity test"),
                                        p("The collinearity test checks to see whether any variables are highly correlated 
                                          with one another.")),
                                 column(4,
                                        img(src="tab3_collinearity.png", width="80%")
                                 )),
                               hr(),
                               br(),
                              fluidRow(
                                column(4,
                                       h4("Build the Model Tab")),
                                column(4, 
                                       img(src="tab1_buildmodel.png", width="90%"))),
                              hr(),
                              h4("Model Setup Tab"),
                              p("The parameter testing tab allows you to check the suitability of parameters."),
                              hr(),
                               fluidRow(
                                 column(8,
                                        h4("Included variables"),
                                        p("The included variables list allows you to check or uncheck variables that are 
                                          included in the model.")),
                                 column(4,
                                        img(src="tab3_included_vars.png", width="80%")
                                 )),
                              hr(),
                              
                              fluidRow(
                                column(8,
                                       h4("Variable Selection"),
                                       p("Stepwise variable selection allows you to automatically choose the model 
                                         that best predicts the indicator with the fewest variables included.")),
                                column(4,
                                       img(src="tab4_stepwise.png", width="80%")
                                )),
                              hr(),
                              fluidRow(
                                column(8,
                                       h4("Random or fixed effects"),
                                       p("The random or fixed effects checkboxes allow you to include survey areas in
                                         the model structure.")),
                                column(4,
                                       img(src="tab4_fixfx.png", width="80%")
                                )),
                              hr(),
                              br()
                               ),
                      tabPanel("Step 4: Model Assessment",
                               h3("Model Assessment"),
                               p("In the model assessment panel, there are three panels with five different ways of
                                 Assessing the model fit."),
                               hr(),
                               fluidRow(
                                 column(8,
                                        h4("Visual Assessment"),
                                        p("The Visual Assessment tab gives you two ways to inspect the results. 
                                          The residuals plot should show relatively even distribution above and below 0.")),
                                 column(4,
                                        img(src="tab4_residuals.png", width="80%")
                                 )),
                               hr(),
                               fluidRow(
                                 column(8,
                                        h4("Scatterplot"),
                                        p("The scatterplot shows the model results plotted against the indicator data, 
                                          aggregated at survey region area. A good model fit will show points clustered close
                                          to the line. ")),
                                 column(4,
                                        img(src="tab4_scatterplot.png", width="80%")
                                 )),
                               hr(),
                               fluidRow(
                                 column(8,
                                        h4("Confusion Matrix"),
                                        p("The confusion matrix shows how frequently the model correctly predicts the 
                                          right answers. The dark blue squares should have values higher than 35% for a\
                                          reasonable model fit.")),
                                 column(4,
                                        img(src="tab4_confusion.png", width="80%")
                                 )),
                               hr(),
                               fluidRow(
                                 column(8,
                                        h4("ROC curve"),
                                        p("The ROC curve gives another measure of predictive power.  The higher the AUC value, 
                                          the better the model predictive power.")),
                                 column(4,
                                        img(src="tab4_ROC.png", width="80%")
                                 )),
                               hr(),
                               fluidRow(
                                 column(8,
                                        h4("Cross Validation"),
                                        p("Cross validation allows you to check the true predictive power of the model. The 
                                          Choosing the number of folds determines the size of test and training sets. For models 
                                          that include random effects this process can take a long time.")),
                                 column(4,
                                        img(src="tab4_xval.png", width="80%")
                                 )),
                               hr(),
                               ),
                      tabPanel("Step 5: Prediction",
                               h3("Prediction"),
                               p("The prediction tab allows you to view either mapped output or numerical data. 
                                 These two data types are displayed on separate tabs and can be downloaded. The results 
                                 are further divided into census-level predictions, survey-level predictions and survey-level 
                                 direct estimates."),
                               hr(),
                               fluidRow(
                                 column(6,
                                        h4("Mapped outputs"),
                                        p("Mapped predictions are produced for the census level areas and survey areas.
                                          Direct estimates are also mapped at Survey areas for comparison.")),
                                 column(6,
                                        img(src="tab5_predictcensus.png", width="80%"))
                                 ),
                               fluidRow(
                                 column(6,
                                        h4("Table outputs"),
                                        p("Predictions are also produced as tabular data. These can be downloaded as .csv files.")),
                                 column(6,
                                        img(src="tab5_predtable.png", width="80%"))
                               ),
                               )
         )), # end Instructions
      
## Data load ##
      tabPanel("Load Data",
          h3("Load Data"),
          p("In this tab, prepare data for analysis  using a dataset from Nepal, 
          or load your own formatted dataset."),  
             
                                 
          sidebarLayout(
            sidebarPanel(
              p("Upload your own tabular (.csv) and shapefile data for both census and survey."),
              surveyloadPanel,
              censusloadPanel,
              
              
              conditionalPanel('input.datatoload === "Survey"',
                  p("Template survey data from Nepal are available, and can be downloaded from the Github site, including tabular data",
                a(href="https://github.com/tomasbird/shinyapps_SAE/blob/main/R/data/Nepal/Survey/NPL_DHS_harmonized.csv", 
                  "(NPL_DHS_harmonized.csv)", target="_blank"), 
                " and shapefiles", 
                a(href="https://github.com/tomasbird/shinyapps_SAE/blob/main/R/data/Nepal/Survey/shp", "(NPL_DHS_Regions.shp).", 
                  target="_blank"))), 
              conditionalPanel('input.datatoload === "Census"',
                               p("Template census data from Nepal are available, and can be downloaded from the Github site, including tabular data",
                                 a(href="https://github.com/tomasbird/shinyapps_SAE/blob/main/R/data/Nepal/Census/NPL_census_harmonized.csv", 
                                   "(NPL_DHS_harmonized.csv)", target="_blank"), 
                                 " and shapefiles", 
                                 a(href="https://github.com/tomasbird/shinyapps_SAE/blob/main/R/data/Nepal/Census/shp", "(NPL_census_districts.shp).", 
                                   target="_blank"))), 
               
              p("Example data from Nepal are pre-loaded for learning how to use the app."),
              checkboxInput("usedemo", label = "Check to use Nepal data", value = FALSE),
              
             
              #conditionalPanel('input.usedemo == 0',
              #  ),
                
           # Choose indicator and spatial data
              conditionalPanel('input.datatoload === "Survey"',
                p("Choose the response variable below."),
                uiOutput("choose_survey_indicator"),
                           
                p("Choose the DHS spatial area names"),
                uiOutput("choose_survey_spatial")
                ),
               
              conditionalPanel('input.datatoload === "Census"',
                p("Choose the column in the census data representing census spatial areas."),
               
                uiOutput("choose_census_spatial") %>% withSpinner(color="#0dc5c1"),
                p("Choose variables to be used as predictors. Please exclude any variables (such as spatial data) 
                    that won't be used in the model"),
               
                uiOutput("choose_census_vars") %>% withSpinner(color="#0dc5c1"))
                ),
           
            # Display Data from census and survey ----
          mainPanel(
            tabsetPanel(id='datatoload',
                tabPanel("Survey", 
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                      tags$div("Loading survey data...",id="loadmessage")
                      ),
                         
                    fluidRow(plotOutput("surveyMap")),
                    fluidRow(DT::dataTableOutput("survey_preview")),
                    fluidRow(verbatimTextOutput("pathprint"))
                    ),
                
                tabPanel("Census", 
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                      tags$div("Loading census data...",id="loadmessage")
                      ),
                         
                    fluidRow(plotOutput("censusMap") %>% withSpinner(color="#0dc5c1")),
                    fluidRow(DT::dataTableOutput("census_preview") %>% withSpinner(color="#0dc5c1"))
                        )
                 )))), # end Select Data

## Comparing Data ##        
    tabPanel('Inspect Predictors', 
         h3("Inspect Predictors"),
         p("Because the model predictions are based on linear regression, it is important that the variables 
           are correlated with the indicator of interest, that the predictor variables are similarly distributed in 
           census and survey data and that the census and survey data have similar distribution of effort in space."),
         conditionalPanel('input.comparison=="Correlations"',
            p("Observe which predictors vary strongly correlated with your chosen indicator. Generally variables that have
            correlation values above 0.5 are should be included. Others may be included if desired.")),
         
         conditionalPanel('input.comparison=="Distributions"',
            p("Check which variables have similar distributions between census and survey data 
            across their factor levels. It is best to only include variables that
              have similar coverage of factor levels across survey and census data.")),
         
         conditionalPanel('input.comparison=="Spatial"',
            p("Use this tab to see whether there was similar spatial distribution of effort between survey and census data. 
              On each map, the colour each region represents the percentage of all surveys done in that area.  It is best if the 
              census and survey distribution of effort are similar.  If they are not, it is best to run a random effects model.")),
         
         tabsetPanel(id = 'comparison',
            tabPanel("Correlations", 
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                  tags$div("Loading correlations...",id="loadmessage")),
                fluidRow(plotOutput("VarsR2plot") %>% withSpinner(color="#0dc5c1")),
                  downloadButton("VarsR2plot_down", "Download comparisons")
                     ),
                     
                     tabPanel("Distributions", 
                              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                               tags$div("Loading plots",id="loadmessage")),
                              uiOutput("show_survey_vars"),
                              fluidRow(
                                column(6, plotOutput("compare_vars_barplot")),
                                column(6, plotOutput("compare_vars_scatterplot"))),
                              fluidRow(
                                column(6, p(paste0("Figure 2: Histogram showing how frequently categories 
                                                   in each variable occur in Survey and census data."))),
                                column(6, p(paste0("Figure 3: Scatterplots comparing the relative frequencies 
                                                    of individual categories for each variable in census versus survey data.
                                                    The R2 value in each plot shows the strength of the corelation between the two 
                                                    datasets.")))),
                              fluidRow(
                                column(6,downloadButton("compare_vars_barplot_down", "Download Barplot")),
                                column(6,downloadButton("compare_vars_scatterplot_down", "Download Scatterplot"))
                              )),
                     
                     tabPanel("Spatial",
                              fluidRow(
                                h3("Frequency of observations by survey area")),
                              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                               tags$div("Loading maps...",id="loadmessage")),
                              fluidRow(plotOutput("survey_freq_plot")), 
                              fluidRow(plotOutput("census_freq_plot"))
                     ))), # end Compare data
              

## Model Setup ##
    tabPanel('Model Setup', 
         sidebarLayout(
           sidebarPanel(
             h3("Model Setup"),
             conditionalPanel('input.modelbuild=="Parameter Tests"',
                              h4("Aliasing"),
                              p("Aliasing refers to when two variables are perfectly correlated. "),
                              actionButton("checkalias", "Check for Aliasing"),             
                              hr(),
                              br(),
                              h4("Collinearity"),
                              p("Collinearity occurs when two variables are closely correlated."),
                              actionButton("checkvif", "Variance Inflation Factors"),
                              br()
             ),
             conditionalPanel('input.modelbuild=="Build the Model"',
                              h4("Stepwise regression"),
                              p("Stepwise regression selects a best subset of the available variables. Please do this 
                      before including a random effect."),
                              br(),
                              actionButton("runstepwise", "Stepwise variable selection", icon=icon("shoe-prints")),             
                              
                                #uiOutput("include_rfx"),
                              
                                #uiOutput("include_ffx"),
                              
                                uiOutput("regionfx")
             ),
             
             h4("Included Variables"),
             p("Variables to include in the final model. Uncheck those that you would like to exclude"),
             uiOutput("choose_model_params")
           ),
           
           mainPanel(
             tabsetPanel(id = 'modelbuild',
                         tabPanel("Parameter Tests", 
                                  p("You must test for Aliasing and Multicollinearity to determine which variables should be excluded."),
                                  hr(),
                                  h3("Alias Report"),
                                  #p("Aliasing exists when two variables are perfectly correlated."),
                                  fluidRow(
                                    textOutput("Aliasreport")),
                                  br(),
                                  hr(),
                                  h3("Variance Inflation Table"),
                                  p("The VIF table shows  Generalized Variance Inflation Factor (GVIF) and degrees of freedom (df) 
                                  for each variable. Variables with GVIF score of greater than 2 should be excluded."),
                                  fluidRow(DT::dataTableOutput("VIFtable")),
                                  br(),
                                  fluidRow(downloadButton("vif_down", "Download VIF table")),
                                  hr()
                         ),
                         
                         tabPanel("Build the Model", 
                                  h4("Model Formula"),
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   tags$div("Loading formula",id="loadmessage")
                                  ),
                                  
                                  fluidRow(textOutput("print_formula")),
                                  hr(),
                                  br(),
                                  h4("Model Summary"),
                                  p("The model summary shows the estimated model coefficients, their standard deviations and the 
                                    degree of significance for each coefficient. Use this table to determine which variables 
                                    might be important to include in the model. The Deviance residuals help describe how much of 
                                    the variation in the data can be described by the model."),
                                  fluidRow(verbatimTextOutput("model_summary") %>% withSpinner(color="#0dc5c1")),
                                  hr()
                         ))))), # End Model Setup

##  Assessing up Model ##
    tabPanel('Model Assessment', 
         tabsetPanel(id = 'validation',
                     tabPanel("Assess", h3("Model Assessment"),
                              fluidRow(
                                column(6, h3("Residual Plot")), 
                                column(6, h3("Observed versus Predicted"))),
                              fluidRow(
                                column(6, p("A residual plot shows the difference between observed values and those 
                                    expected by the model, or how much the model result varies from reality.")), 
                                column(6, p("This plot shows the relationship between observed and expected values when 
                                    aggregated at regional level. A closer correlation indicates a better model fit.
                                    However it is also important to run independent assessments through cross-validation."))),
                              fluidRow(
                                column(6,plotOutput("resid_plot")%>% withSpinner(color="#0dc5c1")),
                                column(6,plotOutput("r2plot")%>% withSpinner(color="#0dc5c1"))),
                              fluidRow(
                                column(6, downloadButton("residplot_down", "Download Residuals plot")),
                                column(6, downloadButton("r2plot_down", "Download correlation plot")))
                     ),
                     
                     tabPanel("Validation", 
                              fluidRow(
                                column(6,h3("Confusion matrix")), 
                                column(6,h3("ROC Curve"))),
                              fluidRow(
                                column(6,
                                       p("A confusion matrix is another way to assess the difference between observed and expected values. 
                           The top-left panel indicates the percentage of true positive observations the model predicted 
                           correctly, while the bottom-right shows true negatives.  The top-right and bottom left indicate 
                           false positive and false negatives.")), 
                                column(6,p("The Receiver-Operating Curve shows the rate at which the model distinguishes true 
                                    positives relative to the false positive rate. "))
                              ),
                              fluidRow(
                                column(6,plotOutput("conf.mat") %>% withSpinner(color="#0dc5c1")),
                                column(6,plotOutput("roc.plot")%>% withSpinner(color="#0dc5c1"))
                              ),
                              fluidRow(
                                column(6, downloadButton("conf.mat_down", "Download Confusion Matrix")),
                                column(6, downloadButton("roc.plot_down", "Download ROC curve"))
                              )
                     ),
                     
                     tabPanel("Cross Validation", 
                              titlePanel("Cross-validation"),
                              p("Cross-validation takes a sample of the data to build the model, then uses that model to predict into the 
                   remainder of the data. The true values are calculated compared to the predicted values to get an assessment of the model's predictive power. 
                   This process is repeated multiple times to determine how repeatable this predictive power is."),
                              p("Cross validation can take longer for larger datasets,  increased numbers of folds and for models that
                                include a random effect."),
                              
                              actionButton("runXval", "Start Cross Validation"),
                              
                              fluidRow(column(6, numericInput("numberfolds", "Number of Folds", 5))
                              ),
                              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                               tags$div("Running Cross Validation...",id="loadmessage")),
                              DT::dataTableOutput("xval_table") %>% withSpinner(color="#0dc5c1")     
                     ))), # end model Assessment

## Tab for prediction ##
tabPanel('Prediction', 
         tabsetPanel(id = 'pred',
                     tabPanel("Map output",
                              h3("Mapped predictions"),
                              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                               tags$div("Loading prediction maps...",id="loadmessage")
                                               ),
                              fluidRow(
                                column(3, 
                                       fluidRow(h4("Census-level predictions")),
                                       fluidRow(br()),
                                       fluidRow(downloadButton("predicted_census_down", "Download Census Predictions"))
                                ),
                                column(9, 
                                       plotOutput("predicted_census_map")%>% withSpinner(color="#0dc5c1")
                                      )),
                              fluidRow(
                                column(3, 
                                       fluidRow(h4("Survey-level predictions")),
                                       fluidRow(br()),
                                       fluidRow(downloadButton("predicted_survey_down", "Download Survey Predictions"))),
                                column(9, 
                                       plotOutput("predicted_survey_map")%>% withSpinner(color="#0dc5c1")
                                  )),
                              fluidRow(
                                column(3, 
                                       fluidRow(h4("Direct Estimates")),
                                       fluidRow(br()),
                                       fluidRow(downloadButton("direct_estimate_map_down", "Estimates"))
                                       ),
                                column(9, plotOutput("direct_plot")%>% withSpinner(color="#0dc5c1")
                                       ))
                              ),
         
         tabPanel("Table output", 
                  h3("Tabular predictions"),
                  p("Predicted outputs are presented here as tabular outputs, with mean, lower and upper 
                               95% confidence intervals."),
                  br(),
                  h4("Results within survey regions"),
                  fluidRow(DT::dataTableOutput("predicted_survey_table")%>% withSpinner(color="#0dc5c1")),
                  fluidRow(downloadButton("pred_survey_table_down", "Download survey level predictions")),
                  br(),
                  h4("Results within census regions"),
                  fluidRow(DT::dataTableOutput("predicted_census_table")%>% withSpinner(color="#0dc5c1")),
                  downloadButton("pred_census_table_down", "Download census level predictions")
         ))) # end Prediction


    ) # end navbarPage
) # end ui


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ## Objects for Landing Page
    observeEvent(input$gotoIntro, {
      updateTabsetPanel(session = session, inputId = "tabs", selected = "Instructions")
    })
  
    ## Objects for Data load Tab
    source("server/load_server_objects.R", local=TRUE)
    
    # Data Select Tab
    source("server/load_server_maps.R", local=TRUE)
    
    # Data Compare Tab
    source("server/load_compare_objects.R", local=TRUE)
    
    # Model Build Tab
    source("server/load_model_objects.R", local=TRUE)
    
    #  Assessments Tab
    source("server/load_assessment_objects.R", local=TRUE)
  
    # Cross Validation Tab
    #source("server/load_Xval_objects.R", local=TRUE)

    # Prediction Tab
    source("server/load_prediction_objects.R", local=TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

#}
