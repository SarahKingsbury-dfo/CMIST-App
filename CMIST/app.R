#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinydashboard)
library(CMISTR)
library(DT)

CMISTFunction<- function(input, output, session, data){
    
    
header <- dashboardHeader(title ="CMIST Assessment")

body <- dashboardBody(
        
        h2("CMIST Scoring Assessment"),
        fluidPage(
            # data<-
          fluidRow(
                 column(5, selectInput("Q1",
                             "Is the species established in the assessment area?",
                             #helpText("This question is meant to differentiate species that are not present in the assessment area (1) from species that are established in the assessment area (3). Species that are present in the assessment area but not established would score 2."),
                             c(1,2,3)))),
                 column(5,selectInput("U1",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q2",
                             "How frequently and in what numbers is the species expected to arrive into the assessment area?",
                             #helpText("Consider initial arrival into the assessment area by primary vectors only. Do not consider secondary spread (anthropogenic or natural) within the assessment area by species that are already established. Consider all primary anthropogenic and natural vectors for transport into the assessment area (e.g., ballast water, hull fouling, aquaculture, rafting, and natural dispersal from outside the assessment area). "),
                             c(1,2,3))),
           column(5,selectInput("U2",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q3",
                             "How much of the assessment area offers suitable habitat for the species?",
                             c(1,2,3))),
            column(5,selectInput("U3",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q4",
                             "How much of the assessment area offers suitable environmental conditions for the species to survive?",
                             c(1,2,3))),
            column(5,selectInput("U4",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q5",
                             "Are the species' reproductive requirements available in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U5",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q6",
                             "To what extent could natural control agents slow the species' population growth in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U6",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q7",
                             "What is the range of the species' potential natural dispersal in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U7",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q8",
                             "What is the range of the species' potential dispersal in the assessment area from anthropogenic mechanisms?",
                             c(1,2,3))),
            column(5,selectInput("U8",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3))),
        fluidRow(
            column(5,selectInput("Q9",
                             "What level of impact could the species have on population growth of other species in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U9",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q10",
                             "What level of impact could the species have on communities in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U10",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q11",
                             "What level of impact could the species have on habitat in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U11",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q12",
                             "What level of impact could the species have on ecosystem function in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U12",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q13",
                             "What level of impact could the species' associated diseases, parasites, or travellers have on other species in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U13",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q14",
                             "What level of genetic impact could the species have on other species in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U14",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q15",
                             "What level of impact could the species have on at-risk or depleted species in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U15",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q16",
                             "What level of impact could the species have on aquaculture and fished species in the assessment area?",
                             c(1,2,3))),
            column(5,selectInput("U16",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        fluidRow(
            column(5,selectInput("Q17",
                             "Is the species known or generally considered to be invasive anywhere in the world?",
                             c(1,2,3))),
            column(5,selectInput("U17",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)))),
        actionButton("go", "Submit")))

# v=reactiveValues(data=data)
                 
output$score <- eventReactive (input$go, { 
    
    #input$score <-DT::renderDataTable (DT::datatable (
    
    risks<- tibble(Q1= input$Q1,
                   Q2= input$Q2,
                   Q3=input$Q3,
                   Q4=input$Q4,
                   Q5=input$Q5,
                   Q6=input$Q6,
                   Q7=input$Q7,
                   Q8=input$Q8,
                   Q9=input$Q9,
                   Q10=input$Q10,
                   Q11=input$Q11,
                   Q12=input$Q12,
                   Q13=input$Q13,
                   Q14=input$Q14,
                   Q15=input$Q15,
                   Q16=input$Q16,
                   Q17=input$Q17
    )%>%
        mutate_all(as.numeric)
    
    uncertainties<- tibble(U1= input$U1,
                           U2= input$U2,
                           U3=input$U3,
                           U4=input$U4,
                           U5=input$U5,
                           U6=input$U6,
                           U7=input$U7,
                           U8=input$U8,
                           U9=input$U9,
                           U10=input$U10,
                           U11=input$U11,
                           U12=input$U12,
                           U13=input$U13,
                           U14=input$U14,
                           U15=input$U15,
                           U16=input$U16,
                           U17=input$U17)%>%
        mutate_all(as.numeric)})
    
      # output$score<- DT:: dataTableOutput({
      #     DT::datatable(v$data, editable=TRUE)
      # })
        
}

        

sidebar <- dashboardSidebar(
    h2("The Canadian Marine Invasive Species Screen Tool"),
    h3(verbatimTextOutput("score", placehold=T)),
    tags$head(tags$style("#score{color:black;
        font-size: 20px;
        font-family: Source Sans Pro}")),
    menuItem("help", tabName="Help", icon=icon("bar-chart-o"))
)


CMISTui <- function(id){
    ns<-NS(id)
    DT::dataTableOutput(ns("score"))
    dashboardPage(header, 
                    sidebar, 
                    body,
                    #useShinyjs()
                    )
}
            
            

server <- function(input, output, session) {
    callModule(CMISTFunction, "CMIST_Score", score, 
                risks=score$risks,
               uncertainties=score$uncertainies,
               Final=CMISTScore(risks, uncertainties))
    
    
    output$CMIST_Score<-renderText(Final())
     
    
    # observeEvent(input$score, {renderTable(score())})
    
   
    
        #))
    
        
      #print(risks)
      #print(uncertainties)
      
      
    #   score<- CMISTScore(risks, uncertainties)
    #   paste0(score)
    #     
    # })
    # 
    
    # output$score<-renderText(score())
        }

shinyApp(ui, server)

