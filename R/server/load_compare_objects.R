
#####Spatial tab
# plot frequency of observations at survey scale ## objects used for merging shape and df data sources
mergeshpfn=function(shp,df, select.spatial){ #select.vars,
  
  shp=shp %>% st_as_sf
  
  freqdat=subset(df, select=c( select.spatial)) %>% #select.vars,
    as.data.frame() %>%
    group_by_at(select.spatial) %>%
    summarize(n=n()) %>%
    mutate(freq=n/sum(n)) 
  
  srvshpmerge=merge(shp, freqdat, by=select.spatial)
}

srvshpmerge=reactive({
  shiny::validate(
    need(surveyDF(), "Survey data not found. Please check that it has been loaded in the Load tab"),
    need(surveyShp(), "Survey shapefile not found. Please check that it has been loaded in the Load tab")
    #need(input$select.vars %in% names(df),"Predictor variable not found in dataset. Please check your list of predictors."),
    #need(sum(input$select_spatial %in% names(censusDF()))>0,"Spatial variable not found in dataset. Please check that you have 
    #     chosen the correct name for the spatial region")
  )
  
  mergeshpfn(shp=surveyShp(), df=surveyDF(),  select.spatial=input$survey_spatial) #select.vars=input$Predictors,
  })

## function to calculate ranges of census and survey
rangefun=function(cen, surv){
  cenrng=range(cen)
  survrng=range(surv)
  minlims=min(c(cenrng, survrng))
  maxlims=max(c(cenrng, survrng))
  ranges=c(minlims,maxlims)
  return(ranges)
}

## spatial plot of census and survey
output$survey_freq_plot <- renderPlot({
  shiny::validate(
    need(censhpmerge(), "Error merging census data with shapefile"),
    need(srvshpmerge(), "Error merging survey data with shapefile")
  )
  
  ranges=rangefun(censhpmerge()$freq, srvshpmerge()$freq)
  ggplot(data=srvshpmerge(), aes(fill=freq)) + 
    geom_sf() +
    theme_void() +
    scale_fill_gradientn(colors=c("black", "yellow", "blue"), limits=ranges) +
    labs(title="Survey Data", fill="Frequency")
})

# merge census summaries with spatial data
censhpmerge=reactive({
  shiny::validate(
    need(censusDF(), "Please check to see whether census data have been loaded."),
    need(surveyShp(), "Please check to see whether survey data have been loaded.")
  )
  mergeshpfn(shp=surveyShp(), df=censusDF(), 
             select.spatial=input$survey_spatial) # select.vars=input$show_survey_vars,
})

## plot census frequencies on same scale as surveys
output$census_freq_plot <- renderPlot({
  shiny::validate(
    need(censhpmerge(), "Error merging census data with shapefile"),
    need(srvshpmerge(), "Error merging survey data with shapefile")
  )
  
  cenrng=range(censhpmerge()$freq)
  survrng=range(srvshpmerge()$freq)
  minlims=min(c(cenrng, survrng))
  maxlims=max(c(cenrng, survrng))
  ggplot(censhpmerge(), aes(fill=freq)) +
    geom_sf() +
    theme_void() +
    scale_fill_gradientn(colors=c("black", "yellow", "blue")) + #, limits=c(minlims, maxlims)) +
    labs(title="Census Data", fill="Frequency")
})




######## compare distributions of individual variables
### Reactive UI to choose what survey variable to compare
output$show_survey_vars <- renderUI({
  selectInput("show_survey_vars", "Choose variable to compare", input$Predictors)
})



## Reactive function aggregating  data into survey areas for 'Distributions' comparison tab
Agg_fn=function(df, datatype){
  shiny::validate(
      #need(surveyDF(), "Please check to see whether survey data have been loaded."),
      #need(censusDF(), "Please check to see whether census data have been loaded."),
      need(input$survey_spatial %in% names(df), "Spatial variable name not found in dataset. 
           Please check you have chosen the correct spatial variable.")
    )
  
    df %>%
    group_by_all(.groups=c(input$show_survey_vars, input$survey_spatial)) %>%
    summarise(n=n()) %>%
    group_by(input$survey_spatial) %>%
    mutate(nreg=sum(n),
           freqreg=n/nreg,
           data=datatype) %>%
    ungroup()
}

# reactive for survey and census aggregated data 
Surv_Agg=reactive( {
  shiny::validate(
    need(input$show_survey_vars %in% names(surveyDF()), paste0(input$show_survey_vars," not found in survey data. 
         Please check that names are consistent")),
    need(surveyDF(), "Please check to see whether survey data have been loaded.")
   )
  Agg_fn(df=subset(surveyDF(), select=c(input$show_survey_vars, input$survey_spatial)),datatype="Survey")
  })

Cen_Agg=reactive( {
  shiny::validate(
    need(censusDF(), "Please check to see whether survey data have been loaded.")
   )
  Agg_fn(df=subset(censusDF(), select=c(input$show_survey_vars, input$survey_spatial)), datatype="Census")
  })



######## Correlations Tab
## Tab that shows individual variables correlated against each other

compare_vars_scatterplot_fn <- function(cen, surv){
  
  SURV= surv %>% 
    mutate(freqdatsurv=freqreg) %>%
    select_all(.vars=c(input$select_survey_spatial, input$show_survey_vars, "freqdatsurv"))
  
  CEN= cen %>%
    mutate(freqdatcen=freqreg) %>%
    select_all(.vars=c(input$survey_spatial, input$show_survey_vars, "freqdatcen"))
  
    ## make plot
    merge(SURV,CEN, by=c(input$survey_spatial, input$show_survey_vars)) %>%
      ggplot(aes(x=freqdatsurv, y=freqdatcen)) + 
      geom_point(position="dodge") + 
      facet_wrap(as.formula(paste("~", input$show_survey_vars )), ncol=2) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
      labs(x="Frequency observed in Surveys", y="Frequency observed in Census", fill="Categories")+
      stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")
}

# render all correlations scatterplot
output$compare_vars_scatterplot=renderPlot({
  shiny::validate(
    need(censusDF(), "Census data not found, please check it has loaded properly"),
    need(surveyDF(), "Survey data not found, please check it has loaded properly")
  )
  compare_vars_scatterplot_fn(cen=Cen_Agg(), surv=Surv_Agg())
})

# download scatterplot
output$compare_vars_scatterplot_down<-downloadHandler(
  filename = function() {
    paste0("scatterplot", ".jpg")
  },
  content = function(file) {
    ggsave(file, compare_vars_scatterplot_fn(surv=Surv_Agg(), cen=Cen_Agg()))
})


## Barplot for comparing variables
# barplot function
compare_vars_barplot_fn<-function(SURV, CEN){
  shiny::validate(
    need(censusDF(), "Please check to see whether census data have been loaded."),
    need(surveyDF(), "Please check to see whether survey data have been loaded."),
    need(sum(1-(input$Predictors %in% names(surveyDF())))==0 , "Predictor missing in survey data.  
         Check whether unnecessary census spatial variables have been excluded.")
  )
  
  rbind(SURV,CEN) %>%
    ggplot(aes_string(x=input$survey_spatial, y="freqreg",  fill="data")) + 
       geom_col(position="dodge") + 
        facet_wrap(as.formula(paste("~", input$show_survey_vars )), ncol=2) +
       theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
    labs(y="Frequency per Region")
}


# render barplot
output$compare_vars_barplot <- renderPlot({
  compare_vars_barplot_fn(SURV=Surv_Agg(), CEN=Cen_Agg())
})


# download barplot
output$compare_vars_barplot_down <-downloadHandler(
  
   filename = function() {
    paste0("barplot", ".jpg")
   },
  content = function(file) {
    ggsave(file, compare_vars_barplot_fn(SURV=Surv_Agg(), CEN=Cen_Agg()))
  })


##### Multi-panel correlation plot between census and survey data for all variables
### 
cen_agg_long <- reactive({
  shiny::validate(
    need(censusDF(), "Please check to see whether census data have been loaded."),
    need(surveyDF(), "Please check to see whether survey data have been loaded."),
    need(sum(1-(input$Predictors %in% names(surveyDF())))==0 , "Predictor missing in survey data.  
         Check whether unnecessary census spatial variables have been excluded."),
    need(input$survey_spatial %in% names(censusDF()), "Please check to see whether survey spatial variable names are 
         present in the census data."),
    need(input$Predictors, "No predictors found. please check that census data have been loaded.")
  )
  
  subset(censusDF(), 
              select=c(input$Predictors,  input$survey_spatial)) %>%
              pivot_longer(cols=input$Predictors, 
                            names_to="Variable", values_to="outcome") %>%
                  group_by_all(.groups=c(input$survey_spatial, "Variable", "outcome")) %>%
                  summarize(n.cen=n())
  })


surv_agg_long <- reactive({
  shiny::validate(
    need(sum(1-(input$Predictors %in% names(surveyDF())))==0 , "Predictor missing in survey data.  
         Check whether unnecessary census spatial variables have been excluded."),
    need(input$survey_spatial %in% names(censusDF()), "Please check to see whether survey spatial variable names are 
         present in the census data.")
  )
  subset(surveyDF(), 
                select=c(input$Predictors,  input$survey_spatial)) %>%
    pivot_longer(cols=input$Predictors, 
                 names_to="Variable", values_to="outcome") %>%
    group_by_all(.groups=c(input$survey_spatial, "Variable", "outcome")) %>%
    summarize(n.surv=n())
})



### Things for the R2 plots

# R2 function 
VarsR2Fun=function(cen,surv){
  shiny::validate(
    need(sum(1-(input$Predictors %in% names(surveyDF())))==0 , "Predictor missing in survey data.  
         Check whether unnecessary census spatial variables have been excluded.")
  )
  
  left_join(cen,surv) %>%
    ggplot(aes(x=n.surv, y=n.cen, colour=outcome)) + 
    geom_point() +
    facet_wrap(~Variable)+
    labs(y=input$indicator, x="Predictors", fill="Categories")+
    stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")
}


# render R2 plot
output$VarsR2plot <-renderPlot({
 
  VarsR2Fun(cen=cen_agg_long(), surv=surv_agg_long())
    })


# Download R2 plot
output$VarsR2plot_down<-downloadHandler(
  filename = function() {
    paste0("R2_plot", ".jpg")
  },
  content = function(file) {
    ggsave(file,   VarsR2Fun(cen=cen_agg_long(), surv=surv_agg_long()))
  })
  
  
