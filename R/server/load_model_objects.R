### Objects for building and checking models
library(lme4)
library(leaps)
library(ggplot2)
set.seed(1234)
#setwd("~/Google Drive/Business/Customers/UNFPA/SAE contract/Working_SAE_App/R/data")

#### Check Aliasing
# Button to run AliasTest
#Aliastest <- eventReactive(input$checkalias, {
  #shiny::validate(
  #  need(sum(1-(input$Predictors %in% names(surveyDF())))==0, "Predictor missing in survey data.  
  #       Check whether unnecessary census spatial variables have been excluded." )
  #)
Aliastest <- reactive({
  alias(formula(paste(input$indicator, "~" , 
                      paste0(input$model_params, collapse=" + "))), data=surveyDF())
})

# Alias report output
Aliasout=reactive({
  
  aliased=Aliastest()
  al_all=data.frame(aliased$Complete)
  al=names(al_all)[which(al_all<0)]
  if(length(al)>0) {print(paste("The variable(s) [", paste(al, collapse=", "), "] are perfectly correlated with other variables and should be removed."))
  } else {print("No aliasing detected")}
})

output$Aliasreport <- renderPrint({
  shiny::validate(
    need(Aliasout(), "Please run Alias test")
     )
  Aliasout()
  })


### Check Variance Inflation Factors
## VIF test on all variables
VIFtest <- eventReactive(input$checkvif, {

  #shiny::validate(
  #  need(Aliasout()=="No aliasing detected", "Cannot run Variance Inflation Test with Aliased variables")
  #)
  fullform=formula(paste(input$indicator, "~" , 
                         paste0(input$model_params, collapse=" + ")))
 mod=glm(fullform, data=surveyDF(), family="binomial")
 data.frame(vif(mod))[,1:2]
})

## download VIF test
output$vif_down<-downloadHandler(
  filename = function() {
    paste0("Variance inflation test", ".csv")
  },
  content = function(file) {
    ggsave(file, VIFtest())
  })


# VIF tabular output
output$VIFtable=DT::renderDataTable({
  
  shiny::validate(
    need(VIFtest(), "VIF table not yet generated, please run VIF test.")
  )
  #req()
  DT::datatable(VIFtest())  %>%
    formatSignif(columns= which(sapply(VIFtest(), class) %in% c("numeric")), digits=2)
})



### stepwise selection
# Stepwise AIC

# run multi-model inference
swfun=function(mod, scope=list(~.), dir="both") {
  stepAIC(object=mod, scope=scope, direction=dir)
}

## reactive values for stepwise regression
r=reactiveValues()
r$runstepwise=0
r$stepwise=NULL

observeEvent(input$runstepwise, {
  r$runstepwise=1
  swmod=glm(form(), data=surveyDF(), family="binomial")
  r$stepwise=swfun(mod=swmod, scope=list(~.), dir="both")
})

## switch for resetting variables
vars=reactive({
  if(r$runstepwise==0)
    input$Predictors
  else if(r$runstepwise==1)
    labels(r$stepwise$terms)
})


#### Build model
# Model Terms
output$choose_model_params <- renderUI({
  checkboxGroupInput("model_params", "Choose parameters to include",
                     input$Predictors, vars())
})



output$regionfx=renderUI({
  awesomeRadio(
    inputId = "regionfx",
    label = "What kind of effect should survey regions be treated as?", 
    choices = c("None", "Fixed", "Random"),
    selected = "Fixed",
    status = "warning"
    )
})

## build formula


form=reactive({
  shiny::validate(
    need(sum(1-(input$model_params %in% names(surveyDF())))==0, "Predictor missing in survey data.  
         Check whether unneeded  variables have been excluded." ),
    need(Aliasout()=="No aliasing detected", "Cannot run model with Aliased variables"),
    need(Aliasout(), "Complete Alias test first")
    
  )
  
    paste(input$indicator, "~" , paste0(input$model_params, collapse=" + "))
  })

form_rfx<-reactive({
  shiny::validate(
    need(sum(1-(input$model_params %in% names(surveyDF())))==0, "Predictor missing in survey data.  
         Check whether unneeded  variables have been excluded." ),
    need(Aliasout()=="No aliasing detected", "Cannot run model with Aliased variables"),
    need(Aliasout(), "Complete Alias test first")  )
  paste(input$indicator, "~" ,paste0(input$model_params, collapse=" + "), "+ (1|", input$survey_spatial, ")") 
})

form_ffx<-reactive({
  shiny::validate(
    need(sum(1-(input$model_params %in% names(surveyDF())))==0, "Predictor missing in survey data.  
         Check whether unneeded  variables have been excluded." ),
    need(Aliasout()=="No aliasing detected", "Cannot run model with Aliased variables"),
    need(Aliasout(), "Complete Alias test first")  )
  paste(input$indicator, "~" ,paste0(input$model_params, collapse=" + "),  "+", input$survey_spatial)
})


## print formula as text
output$print_formula <- renderText({
  shiny::validate(
    need(form(), "Formula not yet generated"),
    need(Aliasout(), "Complete Alias test first")
  )
  if(input$regionfx=="Random")
    form_rfx()
  else if(input$regionfx=="Fixed")
    form_ffx()
  else
    form()
  })


## create reactive model object
mod=reactive({ 
  shiny::validate(
  need(sum(1-(input$Predictors %in% names(surveyDF())))==0, "Census predictor missing in survey data.  
         Check whether unnecessary census spatial variables have been excluded." ),
  need(Aliasout()=="No aliasing detected", "Cannot run model with Aliased variables"),
  need(Aliasout(), "Complete Alias test first")
  
  )
  
    if(input$regionfx=="Random")
      glmer(form_rfx(), data=surveyDF(), family="binomial")
    else if (input$regionfx=="Fixed")
      glm(form_ffx(), data=surveyDF(), family="binomial")
    else
      glm(form(), data=surveyDF(), family="binomial")
})

## Model Summary
output$model_summary <- renderPrint({
  shiny::validate(
    need(mod(), "Model not yet formulated, check to see whether you have completed the modeling step.")
  )  
  summary(mod())
})



