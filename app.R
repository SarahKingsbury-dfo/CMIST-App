# setwd("C:/Users/KingsburyS/Desktop/RCMIST")

library(CMISTR)
library(htmltools)
library(shiny)
library(dplyr)
library(ggplot2)
library(reactable)
library(devtools)
#devtools::install_github("jrowen/rhandsontable")
library(rhandsontable)
library(shinyMatrix)
library(DT)


fillable_table<-read.csv(file="data/AssessmentInfo.csv", header=TRUE)

AssessmentFunction <- function (input, output, session, data, reset) {
  v=reactiveValues(data=data)
  
  observeEvent (input$mod_table_cell_edit, {
    print(names(v$data))
    info=input$mod_table_cell_edit
    str(info)
    i=info$row
    j=info$col
    k=info$value
    str(info)
  
    isolate(
      if (j %in% match ("fillable", names(v$data))) {
        print (match ("fillable", names(v$data)))
        v$data[i,j] <<- DT::coerceValue(k, v$data[i,j])
        print(v$data)
        
      }
      else{
        stop ("You cannot change this column.")
      }
    )
    replaceData(proxy, v$data, resetPaging=FALSE)
  })
  
  observeEvent( reset(), {
    v$data<- data
  })
  
  #print (isolate(colnames (v$data)))
  output$Info_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE)
  })
  
}

AssessmentUI<- function (id) {
  ns<-NS(id)
  DT::dataTableOutput(ns("Info_table"))
}

input_data<-read.csv(file="data/dataGoals.csv", header=TRUE)

CMISTFunction<-function(input, output, session, data, reset){
  v=reactiveValues(data=data)
  
  observeEvent (input$mod_table_cell_edit, {
    print(names(v$data))
    info=input$mod_table_cell_edit
    str(info)
    i=info$row
    j=info$col
    k=info$value
    str(info)
    
    isolate(
      if (j %in% match (c("Risk", "Uncertainty", "Rationale"), names(v$data))) {
        print (match (c("Risk", "Uncertainty", "Rationale"), names(v$data)))
        v$data[i,j] <<- DT::coerceValue(k, v$data[i,j])
        print(v$data)
        
      }
      else{
        stop ("You cannot change this column.")
      }
    )
    replaceData(proxy, v$data, resetPaging=FALSE)
  })
  
  observeEvent( reset(), {
    v$data<- data
  })
  
  #print (isolate(colnames (v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE)
  })
}

CMISTUI<- function (id) {
  ns<-NS(id)
  DT::dataTableOutput(ns("mod_table"))
}

Ref<-read.csv(file="data/Ref.csv", header=TRUE)

RefFunction <- function (input, output, session, data, reset) {
  v=reactiveValues(data=data)
  
  observeEvent (input$mod_table_cell_edit, {
    print(names(v$data))
    info=input$mod_table_cell_edit
    str(info)
    i=info$row
    j=info$col
    k=info$value
    str(info)
    
    isolate(
      if (j %in% match (c("Annotated_Reference", "Full_Reference"), names(v$data))) {
        print (match (c("Annotated_Reference", "Full_Reference"), names(v$data)))
        v$data[i,j] <<- DT::coerceValue(k, v$data[i,j])
        print(v$data)
        
      }
      else{
        stop ("You cannot change this column.")
      }
    )
    replaceData(proxy, v$data, resetPaging=FALSE)
  })
  
  observeEvent( reset(), {
    v$data<- data
  })
  
  #print (isolate(colnames (v$data)))
  output$Ref_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE)
  })
  
}

RefUI<- function (id) {
  ns<-NS(id)
  DT::dataTableOutput(ns("Ref_table"))
}

    
ui<-navbarPage( title="CMIST",
  
  #tabsetPanel(
    
  tabPanel ("Home Page",
            h1( "Canadian Marine Invasive Screening Tool Manual"),
            h3("Introduction"),
            h5("CMIST (Canadian Marine Invasive Screening Tool) assessments provide information on the likelihood and impact of invasions of different species in different assessment areas with scores adjusted for assessor uncertainty. Together, these assessments act as a new resource of collated information on invasive species that can assist researchers and managers dealing with aquatic invasive species (AIS) and increase the efficiency of decision-making. The growing database of CMIST assessments is as an important repository for information on invasive species for students, researchers, and citizens around the world."),
            h5("The Canadian Marine Invasive Screening Tool (CMIST) is a peer-reviewed screening-level risk assessment tool for aquatic invasive species (Drolet et al. 2016). It is a short questionnaire that follows the invasion process from arrival to impact and is designed so an informed assessor can evaluate one species in an assessment area in approximately one day using easily accessible information from internet databases, primary literature, and grey literature. Species can be those with an invasion history in an area or those that are candidates for future invasions. "),
            h5("CMIST is score-based and incorporates both likelihood and impact of invasion as well as uncertainty. Questions are general to make CMIST broadly applicable to different taxa, different assessment areas, and different project goals. To date, CMIST has been tested with molluscs, tunicates, crustaceans, and polychaetes introduced or at risk of introduction to three Canadian marine ecoregions (DFO 2009). CMIST has also been successfully applied to non-indigenous freshwater fish in British Columbia with adapted guidelines (T. Therriault, pers. comm.). Upon completion, CMIST produces a risk score adjusted for the assessor's uncertainty which, combined with information collected during the assessment, can be used to assist in management decisions. For example, in 2015, CMIST assessments were used to identify high risk invaders in three Canadian marine ecoregions (DFO 2016). Completed assessments submitted from assessors are reviewed for accuracy and completeness then housed online in the searchable CMIST database. "),
            h5("The user manual contains best practices for using CMIST and should be read in full before proceeding with an assessment to achieve consistent results. For more in-depth background information, refer to Drolet et al. (2016)."),
            h3("Highlights"),
            h5(".	CMIST was designed for and tested with marine invertebrates, but its theoretical basis and general questions make it suitable for other organisms (i.e. will work for freshwater or brackish water species too)."),
            h5(".	Screening-level risk assessments in CMIST are designed to be completed in 1-2 days including collection of information and answering questions. "),
            h5(".	Assessors should have good general knowledge on invasive species and, ideally, the assessment area and species being assessed."),
            h5(".	Prior to assessment, background information on the selected species that pertains to CMIST questions should be collated from available resources and quickly reviewed. "),
            h5(".	Assessors should always use their best judgement when interpreting questions, answers, and guidance for the selected species, which will likely not have species or taxon-specific examples."),
            h5(".	Assessors should read the manual before completing any assessments to ensure consistency with guidance.")
             ),

  tabPanel(title="CMIST Assessment Information",
              helpText("CMIST Assessment Information to be completed. Some field must be filled in for the assessment to be peer-reviewed."),
           helpText("Please note that by using this CMIST tool, you are providing your informed consent that some of the information such as the assessment area, species information, date of assessment, and general CMIST score may be made available publicly through this site. All personal information (e.g. Assessor Name, Email, etc.)
           will be anonymized."),
           tags$hr(),
           AssessmentUI("InfoTable"),
           actionButton("submit","SUBMIT")
      ),
  
  
  tabPanel(title="CMIST Assessment Scoring", 
             helpText("Uncertainty Score Quidance:",
                      "Low Certainty=1: Little to no reliable information is available AND assessor has no experience with species.",
                      "Moderate Certainty=2: Some reliable information is available. Incomplete information is supplemented with information on (or experience with) similar species in a similar environment.",
                      "High Certainty=3: Considerable reliable information is available OR assessor has experience with species and assessment area."),
             helpText("Rationale Guidance:",
                      "Include key information used to determine risk and uncertainty scores.",
                      "Include direct quotations from literature, paraphrased summaries, and statements of expert opinion.",
                      "In cases where the answer is one of two options (e.g., high impact in few areas OR moderate impact in many areas), indicate which option was favoured and why.",
                      "Include annotated references in the text (e.g., Drolet et al. 2016; EOL; personal observation). Please include full citations in the References sheet in this workbook."),
             tags$hr(),
             CMISTUI("editable"),
             actionButton ("submit", "SUBMIT")
           ),
  
  tabPanel(title="Reference List",
           helpText("Please fill out the table below with:",
                    "(1)The annotated reference that was used for in-text referencing in the CMIST Tab; and",
                    "(2) The full refernence that cooresponds to the annotated version."),
           tags$hr(),
           RefUI("RefTable"),
           actionButton("submit", "SUBMIT")
  ))


server<- function(input,output) {
  
  callModule(AssessmentFunction, "InfoTable", fillable_table,
             reset=reactive(input$reset))
  
  output$InfoTable<- renderTable({input$reset})
  
  callModule(CMISTFunction, "editable", input_data,
             reset=reactive(input$reset))
  
  output$editable<- renderTable ({input$reset})
  
  callModule(RefFunction, "RefTable", Ref,
             reset=reactive(input$reset)) 
  
  output$RefTable<- renderTable({input$reset})
}
  
  # FinalScore<- callModule(CMISTFunction, "CMISTScore",editable,
  #                                       risks="Risk",
  #                         uncertainties="Uncertainty")
  # 
  # output$FinalScore<- renderText ({CMISTScore(risks, uncertainties)})
 
  

shinyApp(ui=ui, server=server)
  
  