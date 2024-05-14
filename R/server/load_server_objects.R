


### Conditional panel if loading data
surveyloadPanel=conditionalPanel(
  condition='input.datatoload == "Survey"', 
  fileInput("survey.file", "Upload .csv for survey data",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  #Input: Select a survey shapefile ----
fileInput("survey.shp.file", "Upload survey shapefile",
            multiple = TRUE, 
            accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.xml', ".DBF"))
  ) 

### Conditional panel if loading data
censusloadPanel=conditionalPanel(
  condition='input.datatoload == "Census"', 
  fileInput("census.file", "Upload .csv for census data",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),

  
  #Input: Select a census shapefile ----
  fileInput("census.shp.file", "Upload census shapefile",
            multiple = TRUE,
            accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.xml', ".DBF"))
) 


# load survey DF
rawsurveyDF <- reactive({
  shiny::validate(
    need(input$usedemo | !is.null(input$survey.file), "Survey dataset not yet loaded")
    )
  
  path=ifelse(input$usedemo==TRUE, "data/Nepal/Survey/NPL_DHS_harmonized.csv", input$survey.file$datapath )
  read.csv(path)
})

## load census DF 
rawcensusDF <- reactive({
  shiny::validate(
    need(input$usedemo | !is.null(input$census.file), "Census dataset not yet loaded")
  )

  cenpath=ifelse(input$usedemo==TRUE, "data/Nepal/Census/NPL_census_harmonized.csv", input$census.file$datapath )
  read.csv(cenpath)
})


## Reactive UI for choosing survey variables to use
# indicator
output$choose_survey_indicator <- renderUI({
  selectInput("indicator", "Indicator to model", names(rawsurveyDF()))
})

# survey spatial identifier
output$choose_survey_spatial <- renderUI({
  selectInput("survey_spatial", "Survey areas", names(rawsurveyDF()))
})

# choose response variables
output$choose_census_vars <- renderUI({
  shiny::validate(
    need(input$survey_spatial %in% names(rawcensusDF()) , "Survey spatial area names missing in census data.  
         Please check whether the right area has been selected.")
   )
  rem= which(names(rawcensusDF()) %in% c(input$survey_spatial, input$census_spatial))
  checkboxGroupInput("Predictors", "Predictor variables", 
                     choices=names(rawcensusDF()[-c(rem)]), 
                     selected=names(rawcensusDF())[-c(rem)])
})

# census spatial identifier
output$choose_census_spatial <- renderUI({
  shiny::validate(
    need(input$survey_spatial %in% names(rawcensusDF()) , "Survey spatial area names missing in census data.  
         Please check whether the right area has been selected.")
  )
  req(rawcensusDF(), input$survey_spatial)
  remspat= which(names(rawcensusDF()) %in% c(input$survey_spatial))
  selectInput("census_spatial", "Census area names", names(rawcensusDF())[-remspat])
})


### resulting survey table
# 
censusDF=reactive({
  shiny::validate(
    need(rawcensusDF(), "Please load census dataset", label="censusdfmissing"),
    need(input$survey_spatial, "Survey spatial variable not yet defined"),
    need(input$survey_spatial %in% names(rawcensusDF()), "Survey spatial variable not found in census data. Please check your selection")
    )
 
  subset(rawcensusDF(), select=c(input$Predictors, input$census_spatial, input$survey_spatial))
})

surveyDF=reactive({
  shiny::validate(
    need(rawsurveyDF(), "Please load survey dataset")
   )
  rawsurveyDF()})
  
## Tabular output for viewing data
# Survey
# preview 
output$survey_preview <- DT::renderDataTable({

    shiny::validate(
    need(!(input$survey_spatial == input$indicator), "Spatial and indicator variables should be different"),
    need(surveyDF(), "Survey data not yet loaded")
    )
  
  dat=head(surveyDF(), n=100)
  DT::datatable(surveyDF(), rownames=FALSE) %>%
    formatSignif(columns =  which(sapply(dat, class) %in% c("numeric")), digits=2)
})


output$census_preview <- DT::renderDataTable({
  shiny::validate(
   need(input$survey_spatial %in% names(censusDF()), "Survey spatial variable not found in census data. Please check your selection"),
   need(censusDF(), "Please load census dataset", label="censusdfmissing")
   )
  dat=head(censusDF(), n=100)
  DT::datatable(dat, rownames=FALSE) #%>%
    #formatSignif(columns=  which(sapply(dat, class) %in% c("numeric")), digits=2)
})



##### create reactive survey shapefile
surveyShp <- reactive({
 ## Validation
  shiny::validate(
    need(input$usedemo | !is.null(input$survey.shp.file), "Please load survey shapefile")
  )
  
  localdsn="data/Nepal/Survey/shp"
  locallay="NPL_DHS_Regions"
  if(input$usedemo==FALSE) {
    if (!is.null(input$survey.shp.file)){
      shpDF <- input$survey.shp.file
      prevWD <- getwd()
      uploadDirectory <- dirname(shpDF$datapath[1])
      setwd(uploadDirectory)
      for (i in 1:nrow(shpDF)){
        file.rename(shpDF$datapath[i], shpDF$name[i])
      }
      shpName <- tools::file_path_sans_ext(shpDF$name)
      #if(length(shpName)>1) shpName=shpName[-(grep(".xml", shpName))]
      #shpPath <- paste(uploadDirectory, shpName, sep="/")
      setwd(prevWD)
      survey_shp <- try(read_sf(dsn=uploadDirectory, lay=shpName), silent=T)
      return(survey_shp)
    } else {
      return()
    }
  } else {
    survey_shp = try(st_read(dsn=localdsn, lay=locallay), silent=T)
    return(survey_shp)}  
})

#surveyShp <- reactive({
#  ## Validation
#  shiny::validate(
#    need(input$usedemo | !is.null(input$survey.shp.file), "Please load survey shapefile")
#  )
  
#  localfile="data/Nepal/Survey/shp/NPL_DHS_Regions.shp"
#  if(input$usedemo==FALSE) {
#    if (!is.null(input$survey.shp.file)){
#      shpDF <- input$survey.shp.file
#      prevWD <- getwd()
#      uploadDirectory <- dirname(shpDF$datapath[1])
#      setwd(uploadDirectory)
#      for (i in 1:nrow(shpDF)){
#        file.rename(shpDF$datapath[i], shpDF$name[i])
#      }
#      shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
#      if(length(shpName)>1) shpName=shpName[-(grep(".xml", shpName))]
#      shpPath <- paste(uploadDirectory, shpName, sep="/")
#      setwd(prevWD)
#      survey_shp <- try(read_sf(shpPath), silent=T)
#      return(survey_shp)
#    } else {
#      return()
#    }
#  } else {
#    survey_shp = try(read_sf(localfile), silent=T)
#    return(survey_shp)}  
#})


# map of survey areas
output$surveyMap <- renderPlot({
  shiny::validate(
    need(class(surveyShp())=="sf", "No survey shapefile detected?")
  )
  
  #req(surveyShp())
  surveyShp() %>% 
    st_as_sf()  %>%
    ggplot() +
    geom_sf() +
    theme_void() 
})

# census
censusShp <- reactive({
  shiny::validate(
    need(input$usedemo | !is.null(input$census.shp.file), "Please load census shapefile")
  )
  
  localfile="data/Nepal/Census/shp/NPL_census_districts.shp"
  if(input$usedemo==FALSE) {
    if (!is.null(input$census.shp.file)){
      shpDF <- input$census.shp.file
      prevWD <- getwd()
      uploadDirectory <- dirname(shpDF$datapath[1])
      setwd(uploadDirectory)
      for (i in 1:nrow(shpDF)){
        file.rename(shpDF$datapath[i], shpDF$name[i])
      }
      shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
      if(length(shpName)>1) shpName=shpName[-(grep(".xml", shpName))]
      shpPath <- paste(uploadDirectory, shpName, sep="/")
      setwd(prevWD)
      census_shp <- try(readOGR(shpPath), silent=T)
      return(census_shp)
    } else {
      return()
    }
  } else {
    census_shp = try(readOGR(localfile), silent=T)
    return(census_shp)}
  
})


## map of census units
output$censusMap <- renderPlot({
  shiny::validate(
    need(class(censusShp())=="SpatialPolygonsDataFrame", "No census shapefile detected. Did you load all associated files?")
  )
  
  req(censusShp())
  censusShp() %>% 
    st_as_sf() %>%
    ggplot() +
    geom_sf() +
    theme_void() 
})

