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
require("leaflet.minicharts")
library(sf)
library(leaflet)
library(tidyverse)
library(data.table)

ui<-navbarPage(
  title = "Canadian Marine Invasive Screening Tool (CMIST) App",
  
  tabPanel("Pre-Assessment Information",
            # sidebarLayout(
            #  
            #   sidebarPanel(
               
               h3("Assessor Information"),
               textInput("A1", "*Title: Project Title (e.g. Cipangopaludina chinensis Assessment-Maritime Region"),
               textInput("A2", "*Description: Please enter a short project or species description"),
               textInput("A3", "*Assessor Name:Your Name (first and last)"),
               textInput("A4", "*Affilitation: Please enter your affiliation (may enter more than one)"),
               textInput("A5", "Address: Please enter your address"),
               textInput("A6", "Phone: Enter Phone Number"),
               textInput("A7","*Email Adress"),
               
               h3("Species Information"),
               
               textInput("A8", "*Species AphiaID (WoRMS)"),
               textInput("A9", "*Species TSN (ITIS)"),
               textInput("A10", "Taxon: General taxon or group"),
               textInput("A11", "Species Kingdom"),
               textInput("A12", "Species Phylum"),
               textInput("A13", "Species Subphylum"),
               textInput("A14", "Species Class"),
               textInput("A15", "Species Order"),
               textInput("A16", "Species Family"),
               textInput("A17", "*Species Genius"),
               textInput("A18", "*Species"),
               selectInput("A19", "*Please select the appropriate ecosystem (either Freshwater, Brackish, or Marine)", c("Freshwater", "Marine", "Brackish")),
               textInput("A20", "*Please list all common names for the species (e.g. Chinese mystery snail, Mud snail, etc)"),
               textInput("A21", "Additional Notes: Additional taxonomic notes here"),
               
               h3("Study Area Information"),
               
               textInput("A22", "*Area:Study Area Name"),
               textInput("A23", "*Region:Study Region (e.g. Province, State)"),
               textInput("A24", "*Country: Country of Study Area"),
               textInput("A25", "*Latitude 1: Latitude (northern boundary) in decimal degrees"),
               textInput("A26", "*Latitude 2: Latitude (southern boundary) in decimal degrees"),
               textInput("A27", "*Longitude 1: Longitude (western boundary) in decimal degrees"),
               textInput("A28", "*Longitude 2: Longitude (eastern boundary) in decimal degrees")
               
             #   actionButton("sub", "SUBMIT")
             # ),
             # mainPanel(tableOutput("prepMaterials"))
 # )
  ),
  
  tabPanel("CMIST Assessment",
           # sidebarLayout(
           #   
           #   sidebarPanel(
               selectInput("Q1",
                           "1.(a) Is the species established in the assessment area?",
                           c(1,2,3)),
               selectInput("U1",
                           "1.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R1", "1.(c) Rationale:"),
               selectInput("Q2",
                           "2.(a) How frequently and in what numbers is the species expected to arrive into the assessment area?",
                           c(1,2,3)),
               selectInput("U2",
                           "2.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R2", "2.(c) Rationale:"),
               selectInput("Q3",
                           "3.(a) How much of the assessment area offers suitable habitat for the species?",
                           c(1,2,3)),
               selectInput("U3",
                           "3.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R3", "3.(c) Rationale:"),
               selectInput("Q4",
                           "4.(a) How much of the assessment area offers suitable environmental conditions for the species to survive?",
                           c(1,2,3)),
               selectInput("U4",
                           "4.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R4", "4.(c) Rationale:"),
               selectInput("Q5",
                           "5.(a) Are the species' reproductive requirements available in the assessment area?",
                           c(1,2,3)),
               selectInput("U5",
                           "5.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R5", "5.(c) Rationale:"),
               selectInput("Q6",
                           "6.(a) To what extent could natural control agents slow the species' population growth in the assessment area?",
                           c(1,2,3)),
               selectInput("U6",
                           "6.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R6", "6.(c) Rationale:"),
               selectInput("Q7",
                           "7.(a) What is the range of the species' potential natural dispersal in the assessment area?",
                           c(1,2,3)),
               selectInput("U7",
                           "7.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R7", "7.(c) Rationale:"),
               selectInput("Q8",
                           "8.(a) What is the range of the species' potential dispersal in the assessment area from anthropogenic mechanisms?",
                           c(1,2,3)),
               selectInput("U8",
                           "8.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R8", "8.(c) Rationale:"),
               selectInput("Q9",
                           "9.(a) What level of impact could the species have on population growth of other species in the assessment area?",
                           c(1,2,3)),
               selectInput("U9",
                           "9.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R9", "9.(c) Rationale:"),
               selectInput("Q10",
                           "10.(a) What level of impact could the species have on communities in the assessment area?",
                           c(1,2,3)),
               selectInput("U10",
                           "10.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R10", "10.(c) Rationale:"),
               selectInput("Q11",
                           "11.(a) What level of impact could the species have on habitat in the assessment area?",
                           c(1,2,3)),
               selectInput("U11",
                           "11.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R11", "11.(c) Rationale:"),
               selectInput("Q12",
                           "12.(a) What level of impact could the species have on ecosystem function in the assessment area?",
                           c(1,2,3)),
               selectInput("U12",
                           "12.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R12", "12.(c) Rationale:"),
               selectInput("Q13",
                           "13.(a) What level of impact could the species' associated diseases, parasites, or travellers have on other species in the assessment area?",
                           c(1,2,3)),
               selectInput("U13",
                           "13.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R13", "13.(c) Rationale:"),
               selectInput("Q14",
                           "14.(a) What level of genetic impact could the species have on other species in the assessment area?",
                           c(1,2,3)),
               selectInput("U14",
                           "14.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R14", "14.(c) Rationale:"),
               selectInput("Q15",
                           "15.(a) What level of impact could the species have on at-risk or depleted species in the assessment area?",
                           c(1,2,3)),
               selectInput("U15",
                           "15.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R15", "15.(c) Rationale:"),
               selectInput("Q16",
                           "16.(a) What level of impact could the species have on aquaculture and fished species in the assessment area?",
                           c(1,2,3)),
               selectInput("U16",
                           "16.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R16", "16.(c) Rationale:"),
               selectInput("Q17",
                           "17.(a) Is the species known or generally considered to be invasive anywhere in the world?",
                           c(1,2,3)),
               selectInput("U17",
                           "17.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                           c(1,2,3)),
               textInput("R17", "17.(c) Rationale:")
              # actionButton("go", "SUBMIT")
           #   ),
           #   mainPanel(tableOutput("cmist.table"),
           #             tableOutput("cmist.score"))
           # )
  ),
  
  tabPanel("References",
           # sidebarLayout(
           #   
           #   sidebarPanel(
               textInput("C1", "Reference:"),
               textInput("C2", "Reference:"),
               textInput("C3", "Reference:"),
               textInput("C4", "Reference:"),
               textInput("C5", "Reference:"),
               textInput("C6", "Reference:"),
               textInput("C7", "Reference:"),
               textInput("C8", "Reference:"),
               textInput("C9", "Reference:"),
               textInput("C10", "Reference:"),
               textInput("C11", "Reference:"),
               textInput("C12", "Reference:"),
               textInput("C13", "Reference:"),
               textInput("C14", "Reference:"),
               textInput("C15", "Reference:"),
               textInput("C16", "Reference:"),
               textInput("C17", "Reference:"),
               textInput("C18", "Reference:"),
               textInput("C19", "Reference:"),
               textInput("C20", "Reference:"),
               textInput("C21", "Reference:"),
               textInput("C22", "Reference:"),
               textInput("C23", "Reference:"),
               textInput("C24", "Reference:"),
               textInput("C25", "Reference:"),
               textInput("C26", "Reference:"),
               textInput("C27", "Reference:"),
               textInput("C28", "Reference:"),
               textInput("C29", "Reference:"),
               textInput("C30", "Reference:")
               
           #     actionButton("ref", "SUBMIT")
           #   ),
           #   mainPanel(tableOutput("references")) 
           # )
  ),
  
  tabPanel("Summary",
           sidebarLayout("CMIST Score", tableOutput("table1")),
           mainPanel(
           tabsetPanel(
             tabPanel("Pre-Assessment Info", tableOutput("table2")),
             tabPanel("CMIST Assessment", tableOutput("table3")),
             tabPanel("Reference List", tableOutput("table4"))
           ))
           ),
  
  tabPanel("Interactive Map",
           leafletOutput("leafletmap",height=800)
  )
)


server <- function(input, output, session) {
  
  
  
    prep<-reactive({c(A1= input$A1,
                                     A2= input$A2,
                                     A3=input$A3,
                                     A4=input$A4,
                                     A5=input$A5,
                                     A6=input$A6,
                                     A7=input$A7,
                                     A8=input$A8,
                                     A9=input$A9,
                                     A10=input$A10,
                                     A11=input$A11,
                                     A12=input$A12,
                                     A13=input$A13,
                                     A14=input$A14,
                                     A15=input$A15,
                                     A16=input$A16,
                                     A17=input$A17,
                                     A18=input$A18,
                                     A19=input$A19,
                                     A20=input$A20,
                                     A21=input$A21,
                                     A22=input$A22,
                                     A23=input$A23,
                                     A24=input$A24,
                                     A25=input$A25,
                                     A26=input$A26,
                                     A27=input$A27,
                                     A28=input$A28
    )})
    
    
    risks<- reactive({c(Q1= input$Q1,
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
                                      Q17=input$Q17)%>%
        as.numeric()
    })
    
    uncertainties<-reactive({c(U1= input$U1,
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
        as.numeric()
    })
    
    rationale<-reactive({c(R1= input$R1,
                                         R2= input$R2,
                                         R3=input$R3,
                                         R4=input$R4,
                                         R5=input$R5,
                                         R6=input$R6,
                                         R7=input$R7,
                                         R8=input$R8,
                                         R9=input$R9,
                                         R10=input$R10,
                                         R11=input$R11,
                                         R12=input$R12,
                                         R13=input$R13,
                                         R14=input$R14,
                                         R15=input$R15,
                                         R16=input$R16,
                                         R17=input$R17)
    })
    
    REFList<-reactive({c(C1= input$C1,
                                        C2= input$C2,
                                        C3=input$C3,
                                        C4=input$C4,
                                        C5=input$C5,
                                        C6=input$C6,
                                        C7=input$C7,
                                        C8=input$C8,
                                        C9=input$C9,
                                        C10=input$C10,
                                        C11=input$C11,
                                        C12=input$C12,
                                        C13=input$C13,
                                        C14=input$C14,
                                        C15=input$C15,
                                        C16=input$C16,
                                        C17=input$C17,
                                        C18=input$C18,
                                        C19=input$C19,
                                        C20=input$C20,
                                        C21=input$C21,
                                        C22=input$C22,
                                        C23=input$C23,
                                        C24=input$C24,
                                        C25=input$C25,
                                        C26=input$C26,
                                        C27=input$C27,
                                        C28=input$C28,
                                        C29=input$C29,
                                        C30=input$C30)
    })
    
summaryValue<-reactive(rbind(risk=req(risks()), uncertainties=req(uncertainties()),rational=req(rationale())))
summaryPrep<-reactive(prep())
summaryRef<- reactive(REFList())
    
 #data<- renderTable(rbind(risk=req(risks()), uncertainties=req(uncertainties()),rational=req(rationale())))  
    
# output$prepMaterials<- renderTable({prep()})
# 
# output$cmist.score<- renderTable({CMISTScore(req(risks()), req(uncertainties()))})
#   
# output$cmist.table<- renderTable(rbind(risk=req(risks()), uncertainties=req(uncertainties()),rational=req(rationale())))
# #output$cmist.table<-renderTable(data)
# 
# output$references<- renderTable({REFList()})
# 
# #Not working...need to possible add tabs to table and download function
output$table1<- renderTable({CMISTScore(req(risks()), req(uncertainties()))})

 output$table2<- renderTable({summaryPrep()})
    

 output$table3<-renderTable({summaryValue()})

output$table4<-renderTable({summaryRef()})
 }


shinyApp(ui, server)

