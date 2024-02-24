



## Model Setup ##
tabPanel('Model Setup', 
         sidebarLayout(
           sidebarPanel(
             titlePanel("Build the Model"),
             conditionalPanel('input.modelbuild=="Parameter Tests"',
                              h3("Aliasing"),
                              p("Aliasing refers to when two variables are perfectly correlated. "),
                              actionButton("checkalias", "Check for Aliasing"),             
                              br(),
                              h3("Collinearity"),
                              p("Collinearity occurs when two variables are closely correlated.  We can check for collinearity by 
                      calculating the variance inflation factor (vif) for all variables in a model."),
                              actionButton("checkvif", "Variance Inflation Factors"),
                              br()
             ),
             conditionalPanel('input.modelbuild=="Build the Model"',
                              h3("Stepwise regression"),
                              p("Stepwise regression selects a best subset of the available variables. Please do this 
                      before including a random effect."),
                              br(),
                              actionButton("runstepwise", "Stepwise variable selection", icon=icon("shoe-prints")),             
                              uiOutput("include_rfx"),
                              uiOutput("include_ffx")
             ),
             
             h4("Included Variables"),
             p("Variables to include in the final model. Uncheck those that you would like to exclude"),
             uiOutput("choose_model_params")
           ),
           
           mainPanel(
             tabsetPanel(id = 'modelbuild',
                         tabPanel("Parameter Tests", 
                                  p("While we may have data available for many parameters, not all should be used at the same time in 
                      the model. In this panel, we test for Aliasing and Multicollinearity to determine which variables 
                      should be excluded."),
                                  h3("Alias Report"),
                                  p("Aliasing exists when two variables are perfectly correlated."),
                                  fluidRow(
                                    textOutput("Aliasreport")),
                                  br(),
                                  h3("Variance Inflation Table"),
                                  p("Collinearity occurs when multiple variables are highly correlated with one another.  
                        Such correlations can make it difficult to estimate the coefficients for those variables.  Variance
                        inflation factors help estimate the severity of multicollinearity in individual variables. Variables 
                        with a VIF score of greater than 2 should be excluded."),
                                  fluidRow(DT::dataTableOutput("VIFtable")),
                                  br(),
                                  fluidRow(downloadButton("vif_down", "Download VIF table"))
                         ),
                         
                         tabPanel("Build the Model", 
                                  h4("Model Formula"),
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   tags$div("Loading formula",id="loadmessage")
                                  ),
                                  fluidRow(textOutput("print_formula")),
                                  br(),
                                  h4("Model Summary"),
                                  fluidRow(verbatimTextOutput("model_summary") %>% withSpinner(color="#0dc5c1"))
                         ))))),

##  Assessing up Model ##
tabPanel('Model Assessment', 
         tabsetPanel(id = 'validation',
                     
                     tabPanel("Assess", titlePanel("Assess model fit"),
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
                              fluidRow(column(6, numericInput("numberfolds", "Number of Folds", 5))
                              ),
                              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                               tags$div("Running Cross Validation...",id="loadmessage")),
                              DT::dataTableOutput("xval_table") %>% withSpinner(color="#0dc5c1")     
                     ))),

## Tab for prediction ##
tabPanel('Prediction', 
         tabsetPanel(id = 'pred',
                     tabPanel("Map output", 
                              titlePanel("Spatial predictions"),
                              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                               tags$div("Loading prediction maps...",id="loadmessage")),
                              fluidRow(
                                column(4, 
                                       fluidRow(h3("Census-level predictions")),
                                       fluidRow(br()),
                                       fluidRow(downloadButton("predicted_census_down", "Download Census Predictions"))),
                                column(8, plotOutput("predicted_census_map")%>% withSpinner(color="#0dc5c1"))),
                              
                              fluidRow(
                                column(4, 
                                       fluidRow(h3("Survey-level predictions")),
                                       fluidRow(br()),
                                       fluidRow(downloadButton("predicted_survey_down", "Download Survey Predictions"))),
                                column(8, 
                                       plotOutput("predicted_survey_map")%>% withSpinner(color="#0dc5c1"))),
                              fluidRow(
                                column(4, 
                                       fluidRow(h3("Direct Estimates")),
                                       fluidRow(br()),
                                       fluidRow(downloadButton("direct_estimate_map_down", "Estimates"))),
                                column(8, plotOutput("direct_plot")%>% withSpinner(color="#0dc5c1"))),
                              column(2, downloadButton("direct_estimate_map_down", "Direct Estimates at survey area")))
         ),
         
         tabPanel("Table output", 
                  titlePanel("Tabular predictions"),
                  p("Predicted outputs are presented here as tabular outputs, with mean, lower and upper 
                               95% confidence intervals."),
                  br(),
                  h3("Results within survey regions"),
                  fluidRow(DT::dataTableOutput("predicted_survey_table")%>% withSpinner(color="#0dc5c1")),
                  fluidRow(downloadButton("pred_survey_table_down", "Download survey level predictions")),
                  br(),
                  h3("Results within census regions"),
                  fluidRow(DT::dataTableOutput("predicted_census_table")%>% withSpinner(color="#0dc5c1")),
                  downloadButton("pred_census_table_down", "Download census level predictions")
         )),

## Instructions ##
tabPanel("Instructions",
         navlistPanel("Step-by-Step guide",
                      tabPanel("Start here",
                               h3("Using the Tool"),
                               p("The application consists of different tabs for each stage of the analysis. These include selecting 
                       and comparing data, Setup of the models, model assessment and finally prediction. Under each tab, 
                       simple directions allow the user to control outputs and download results."),
                               fluidRow(
                                 column(6,
                                        h3("Downloading results"),
                                        p("At certain stages in the analysis, you can choose to download results tables, figures 
                                or maps.")),
                                 column(6, 
                                        img(src="Use_Nepal.png", width="70%")
                                 )),
                               h3("Error Messages"),
                               p("Note that the contents of each tab depend on the previous tab results, so errors in data setup 
                               will  result in errors elsewhere in the tool.")
                      ),
                      tabPanel("Step 1: Load data",
                               h3("Data requirements"),
                               p("In order to be used in this tool, survey and census data must be harmonized appropriately. 
                        At minimum, this means that:",
                                 tags$li("Survey and census data must share exactly the same variable names"),
                                 tags$li("Categorical variables must share identical classes"),
                                 tags$li("Census and survey variables must have roughly similar distributions.")),
                               p("For a complete description on how to harmonize data, please see the ", 
                                 a(href="https://unfpasae.github.io/SAE_Manual/01-Harmonization.html", "Harmonization Guide", target="_blank"), 
                                 "in the SAE manual."),
                               h3("Using pre-loaded data"),
                               fluidRow(
                                 column(6, 
                                        p("To get an understanding of how the tool works, pre-loaded data from Nepal are available. 
                                      Simply check the box and these data will be loaded into the tool. ")
                                 ),
                                 column(6, 
                                        img(src="Use_Nepal.png", width="70%")
                                 )),
                               h3("Loading your own data"),
                               p("If you have your own data available, ")
                      ),
                      tabPanel("Step 2: Inspecting predictors"),
                      tabPanel("Step 3: Setup the model"),
                      tabPanel("Step 4: Assess the model"),
                      tabPanel("Step 5: Generate Predictions")
                      