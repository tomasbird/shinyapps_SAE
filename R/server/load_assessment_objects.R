#### Assessment of model

## resid_plot function
resid_plot_fn=function(model){
  plot(model, which=1)
}

## residual plot
output$resid_plot <- renderPlot({
  shiny::validate(
    #need(input$usedemo | !is.null(input$survey.file), "Survey dataset not yet loaded"),
    #need(input$usedemo | !is.null(input$census.file), "Census dataset not yet loaded"),
    need(mod(), "Model not yet generated.")
  )
  resid_plot_fn(mod())
 
})

# download residual plot
output$residplot_down<-downloadHandler(
  filename = function() {
    paste0("residual_plot", ".jpg")
  },
  content = function(file) {
    jpeg(file) 
      resid_plot_fn(mod())
    dev.off()
  }
  )

## model prediction
predicted <- reactive({
  shiny::validate(
    #need(rawsurveyDF(), "Model not generated, check whether survey data are loaded."),
    #need(rawcensusDF(), "Model not generated, check whether census data are loaded."),
    need(mod(), "Model not yet generated.")
    )
  predict(mod(), type="response")
})

# predicted data
pred_df <- reactive({
  shiny::validate(
    need(surveyDF(), "Survey Data not yet loaded"),
    need(predicted(), "Predictions not yet generated.")
  )
  surveyDF() %>%
    mutate(predicted=predicted(), #predict(mod(), type="response"),
           observed=surveyDF()[,input$indicator])
})

# Observed Versus Predicted Data
r2data <- reactive({
  shiny::validate(
    need(pred_df(), "Model not yet generated.")
    )
  
  pred_df() %>%
    group_by_at(input$survey_spatial) %>%
    summarize(predicted=mean(predicted, na.rm=T),
              observed=mean(observed, na.rm=T)) 
})

r2plotfun=function(data) {
  ggplot(data, aes(x=observed, 
           y=predicted)) + 
  geom_point() + 
  geom_abline()
}

output$r2plot=renderPlot({
  shiny::validate(
    need(r2data(), "Model not yet generated.")
  )
  
  r2plotfun(r2data())
})

# download R2plot
output$r2plot_down<-downloadHandler(
  filename = function() {
    paste0("r2plot", ".jpg")
  },
  content = function(file) {
    ggsave(file, r2plotfun(r2data()))
  })


### Confusion matrix
conf.mat_fun=function(data){
  evaluate(pred_df(), target_col="observed", prediction_cols="predicted", type="binomial" )$`Confusion Matrix`[[1]]
}

# make the plot
output$conf.mat=renderPlot({
  shiny::validate(
    need(pred_df(), "Model not yet generated.")
  )
  plot_confusion_matrix(conf.mat_fun(data=pred_df()))
})

# confusion matrix downloader
output$conf.mat_down<- downloadHandler(
  filename = function() {
    paste("Confusion_Matrix", ".jpg", sep = "")
  },
  content = function(file) {
   ggsave(file, plot_confusion_matrix(conf.mat_fun(data=pred_df())))  }
)


### ROC curve          
surveyroc=reactive({
  shiny::validate(
    need(pred_df(), "Model not yet generated.")
  )
  
  roc(data=pred_df(), response=observed, predictor=predicted)
  })

# plot roc
output$roc.plot=renderPlot({ 
  shiny::validate(
    need(surveyroc(), "ROC curve not yet generated.")
  )
  
  plot.roc(surveyroc())
  text(0.2,0.2, paste0("AUC = ", round(surveyroc()$auc, 3)))
  })

# download ROC curve
output$roc.plot_down<- downloadHandler(
  filename = function() {
    paste("ROC_Curve", ".jpg", sep = "")
  },
  content = function(file) {
    jpeg(file) 
       plot.roc(surveyroc())
       text(0.2,0.2, paste0("AUC = ", round(surveyroc()$auc, 3)))
    dev.off()
    }
)


### Cross Validation
folded_surv=reactive({
  shiny::validate(
    need(pred_df(), "Predictions not yet generated.")
  )
  
  newdat=pred_df()
  newdat$sub=sample(1:input$numberfolds, nrow(newdat), replace=TRUE)
  fold(newdat, k = input$numberfolds, cat_col=input$survey_spatial)  
  })

### put cross-validation inside a function
xval_fn=function(dat, fold, rfx){
  shiny::validate(
    need(input$numberfolds>1, "Set number of folds to greater than 1"),
    need(input$numberfolds, "Select a number of folds"),
    need(input$numberfolds<25, "Select a number of folds less than 25")
  )
  
  ## need to call the indicator 'indicator' for the roc function
  dat$indicator=dat[[input$indicator]]
  
  moddat=subset(dat, .folds!=fold)
  testdat=subset(dat, .folds==fold)
  
  if(rfx=="Random")
    mod=glmer(form_rfx(), data=moddat, family="binomial")
  else
    mod=glm(form(), data=moddat, family="binomial")
 
  moddat$prediction=predict(mod, type="response", newdata=moddat)
  testdat$prediction=predict(mod, type="response", newdata=testdat)
  
  mod.roc=roc(moddat, response="indicator", predictor="prediction", ci=TRUE)
  test.roc=roc(testdat, response="indicator", predictor="prediction", ci=TRUE)
  
  #mod.roc.ci=as.numeric(mod.roc$ci)
  #xtest.roc.ci=as.numeric(test.roc$ci)
  preds=data.frame(model.AUC=as.numeric(mod.roc$auc),
                   test.AUC=as.numeric(test.roc$auc))
  
  return(preds)
}

xval=eventReactive(input$runXval,{
  shiny::validate(
    need(mod(), "Model not yet generated."),
    need(input$regionfx, "Model not yet prepared.")
  )
  
  xvalout=data.frame()
  
  withProgress(message="calculating folds", value=0, min=0, max=1, {
  for(i in 1:input$numberfolds){
   incProgress(1/input$numberfolds)
     xv=xval_fn(dat=folded_surv(), fold=i, rfx=input$regionfx)
     xvalout=rbind(xvalout, xv)
  }
  
  })
  xvalout

})


output$xval_table=DT::renderDataTable({
  shiny::validate(
    need(xval(), "Cross validation not yet run")
  )
  DT::datatable(xval(), rownames=FALSE) %>%
    formatSignif(columns= which(sapply(xval(), class) %in% c("numeric")), digits=2)
})
