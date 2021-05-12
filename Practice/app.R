library(tidyverse)
library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "Demo Predictor")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Predict!", tabName = "model", icon = icon("bar-chart-o"),
                 selectInput("Q1",
                             "Is the species established in the assessment area?",
                             c(1,2,3)),
                 selectInput("U1",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q2",
                             "How frequently and in what numbers is the species expected to arrive into the assessment area?",
                             c(1,2,3)),
                 selectInput("U2",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q3",
                             "How much of the assessment area offers suitable habitat for the species?",
                             c(1,2,3)),
                 selectInput("U3",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q4",
                             "How much of the assessment area offers suitable environmental conditions for the species to survive?",
                             c(1,2,3)),
                 selectInput("U4",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q5",
                             "Are the species' reproductive requirements available in the assessment area?",
                             c(1,2,3)),
                 selectInput("U5",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q6",
                             "To what extent could natural control agents slow the species' population growth in the assessment area?",
                             c(1,2,3)),
                 selectInput("U6",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q7",
                             "What is the range of the species' potential natural dispersal in the assessment area?",
                             c(1,2,3)),
                 selectInput("U7",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q8",
                             "What is the range of the species' potential dispersal in the assessment area from anthropogenic mechanisms?",
                             c(1,2,3)),
                 selectInput("U8",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q9",
                             "What level of impact could the species have on population growth of other species in the assessment area?",
                             c(1,2,3)),
                 selectInput("U9",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q10",
                             "What level of impact could the species have on communities in the assessment area?",
                             c(1,2,3)),
                 selectInput("U10",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q11",
                             "What level of impact could the species have on habitat in the assessment area?",
                             c(1,2,3)),
                 selectInput("U11",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q12",
                             "What level of impact could the species have on ecosystem function in the assessment area?",
                             c(1,2,3)),
                 selectInput("U12",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q13",
                             "What level of impact could the species' associated diseases, parasites, or travellers have on other species in the assessment area?",
                             c(1,2,3)),
                 selectInput("U13",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q14",
                             "What level of genetic impact could the species have on other species in the assessment area?",
                             c(1,2,3)),
                 selectInput("U14",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q15",
                             "What level of impact could the species have on at-risk or depleted species in the assessment area?",
                             c(1,2,3)),
                 selectInput("U15",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q16",
                             "What level of impact could the species have on aquaculture and fished species in the assessment area?",
                             c(1,2,3)),
                 selectInput("U16",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 selectInput("Q17",
                             "Is the species known or generally considered to be invasive anywhere in the world?",
                             c(1,2,3)),
                 selectInput("U17",
                             "What is the certainty (1=low certainty, 2=some certainty, 3=high certainty",
                             c(1,2,3)),
                 
                     actionButton("go", "Predict!"),
                 
                     actionButton("reset", "Clear")
        )
    )
)


body <- dashboardBody(
    h2("The predicted miles per gallon is: "),
    h3(verbatimTextOutput("pred", placeholder = T))
)

ui <- dashboardPage(header, 
                    sidebar, 
                    body)

server <- function(input, output, session) { 
    
    #reset
    observeEvent(input$reset, {
        updateSelectInput(session, 'Q1')
        updateNumericInput(session, 'Q2')
        updateNumericInput(session, 'Q3')
        updateNumericInput(session, 'Q4')
        updateNumericInput(session, 'Q5')
        updateNumericInput(session, 'Q6')
        updateSelectInput(session, 'Q7')
        updateSelectInput(session, 'Q8')
        updateSelectInput(session, 'Q9')
        updateSelectInput(session, 'Q10')
        updateSelectInput(session, 'Q11')
        updateSelectInput(session, 'Q12')
        updateSelectInput(session, 'Q13')
        updateSelectInput(session, 'Q14')
        updateSelectInput(session, 'Q15')
        updateSelectInput(session, 'Q16')
        updateSelectInput(session, 'Q17')
    })
    
    fit <- lm(mpg ~ ., data = mtcars)
    
    pred <- eventReactive(input$go, {
        
        newdata<- tibble(Q1= input$Q1,
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
        
        pred <- predict(fit, newdata = newdata)
        pred <- round(pred, 2)
        paste0(pred)
    })
    
    output$pred <- renderText(pred())
}



shinyApp(ui, server)
