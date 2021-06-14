#
# 
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
library(openxlsx)
library(ggplot2)
library(dplyr)

# library(readr)
# cmist_data_5_ <- read_csv("data/cmist_data (5).csv")

cmist_database<-read.csv("data/cmist_data.csv")

cmist_database<-cmist_database%>%
   dplyr::mutate(SPC_GENUS.SPC_SPECIES=paste(cmist_database$SPC_GENUS, cmist_database$SPC_SPECIES, sep = "_"))

scale<-list(1,2,3,4,5,6,7,8)

ui<-navbarPage(
  title = "Canadian Marine Invasive Screening Tool (CMIST) App",
  
#   tabPanel("CMIST Home Page",
#            tabsetPanel(
#              tabPanel("User Manual",
#                       h3("Highlights"),
#                       h5(".	CMIST was designed for and tested with marine invertebrates, but its theoretical basis and general questions make it suitable for other organisms."),
#                       h5("
# .	Screening-level risk assessments in CMIST are designed to be completed in 1-2 days including collection of information and answering questions. "),
#                       h5("
# .	Assessors should have good general knowledge on invasive species and, ideally, the assessment area and species being assessed."),
#                       h5("
# .	Prior to assessment, background information on the selected species that pertains to CMIST questions should be collated from available resources and quickly reviewed. "),
#                       h5("
# .	Assessors should always use their best judgement when interpreting questions, answers, and guidance for the selected species, which will likely not have species or taxon-specific examples."),
#                       h5("
# .	Assessors should read the manual before completing any assessments to ensure consistency with guidance.
# "),
#                       
#                       h3("Introduction"),
#                       h5("CMIST (Canadian Marine Invasive Screening Tool) assessments provide information on the likelihood and impact of invasions of different species in different assessment areas with scores adjusted for assessor uncertainty. Together, these assessments act as a new resource of collated information on invasive species that can assist researchers and managers dealing with aquatic invasive species (AIS) and increase the efficiency of decision-making. The growing database of CMIST assessments is as an important repository for information on invasive species for students, researchers, and citizens around the world."),
#                       h5("
# The Canadian Marine Invasive Screening Tool (CMIST) is a peer-reviewed screening-level risk assessment tool for marine invasive species (Drolet et al. 2016). It is a short questionnaire that follows the invasion process from arrival to impact and is designed so an informed assessor can evaluate one species in an assessment area in approximately one day using easily accessible information from internet databases, primary literature, and grey literature. Species can be those with an invasion history in an area or those that are candidates for future invasions. "),
#                       h5("
# CMIST is score-based and incorporates both likelihood and impact of invasion as well as uncertainty. Questions are general to make CMIST broadly applicable to different taxa, different assessment areas, and different project goals. To date, CMIST has been tested with molluscs, tunicates, crustaceans, and polychaetes introduced or at risk of introduction to three Canadian marine ecoregions (DFO 2009). CMIST has also been successfully applied to non-indigenous freshwater fish in British Columbia with adapted guidelines (T. Therriault, pers. comm.). Upon completion, CMIST produces a risk score adjusted for the assessor's uncertainty which, combined with information collected during the assessment, can be used to assist in management decisions. For example, in 2015, CMIST assessments were used to identify high risk invaders in three Canadian marine ecoregions (DFO 2016). Completed assessments submitted from assessors are reviewed for accuracy and completeness then housed online in the searchable CMIST database. "),
#                       h5("
# This manual contains best practices for using CMIST and should be read in full before proceeding with an assessment to achieve consistent results. For more in-depth background information, refer to Drolet et al. (2016).
# "),
#                       
#                       h3("Pre-assessment Preparation"),
#                       h4("Species"),
#                       h5("Although CMIST was designed for and tested with marine invertebrates, its theoretical basis and general questions makes it suitable for other organisms. Assessors should always use their best judgement when interpreting questions, answers, and guidance for the selected species, which will likely not have species or taxon-specific examples."),
#                       h5("
# Prior to assessment, background information on the selected species that pertains to CMIST questions should be collated from available resources and quickly reviewed. If there is little known about the species, information on other species in the genus (or higher taxonomic level) should be obtained and used as complementary resources. A lack of species-specific information would be expected to increase uncertainty.
# "),
#                       h4("Assessment area"),
#                       h5("The assessment area can be any size or scope but must be defined since most CMIST questions use this assessment area as a context for answers (see Appendix A for questions, answers, and guidance).  It is up to the assessor to determine (and document) the scale used for their assessment."),
#                       h5("
# Prior to assessment, background information should be collated on the assessment area from available resources and quickly reviewed. Pertinent information includes both physical characteristics (e.g., bottom types, habitats, temperature range, and salinity range) and biological components (e.g., species at risk, aquaculture species, commercially fished species, and species of special interest).
# "),
#                       h4("Assessor"),
#                       h5("All CMIST questions are semi-quantitative and require interpretation and judgement to answer based on available information as well as the assessor's expert opinion. An assessor should therefore have good general knowledge on invasive species and, ideally, the assessment area and species being assessed. A less-knowledgeable assessor would be expected to have higher uncertainty, especially for species with little available information.
# 
# Prior to assessment, assessors should review background information on the species and assessment area and familiarize themselves with CMIST questions, answers, guidance, and glossary. 
# "),
#                       
#                       h3("Using CMIST"),
#                       h4("1) Assessment Information"),
#                       h5("This section contains general information about the species, the assessment area, and the assessor. If you submit your assessment for inclusion in the CMIST database, you must complete all information labelled with an asterisk. Contact information including address, phone, and email is for communication purposes only and will not be made publicly available. "),
#                       h4("2) CMIST Assessment"),
#                       h5("This section contains the questionnaire and final risk scores for CMIST. The assessor should answer questions by entering risk scores, uncertainty scores, and rationales in the designated cells. To obtain the risk score adjusted for uncertainty, press the "Calculate adjusted risk scores" button. Adjusted risk scores and lower and upper confidence limits will then be displayed. See below for guidance on specific sections."),
#                       h5("Questions"),
#                       h6("CMIST has 17 questions that follow the invasion process: arrival (Q1 and Q2), survival (Q3 and Q4), establishment (Q5 and Q6), spread (Q7 and Q8), and impact (Q9-Q17). The first 8 questions relate to the likelihood of invasion and the next 9 questions relate to the impact of invasion."),
#                       h6("
# Many terms used in CMIST questions (e.g., population growth, invasive, community) have different definitions depending on perspective and context. Such terms and their intent in the CMIST context are defined in the glossary. 
# "),
#                       h5("Answers"),
#                       h6("Each question has three possible answers (1-3). Answers differ between questions and should be considered carefully in conjunction with the risk score guidance before entering a score."),
#                       h5("Risk score"),
#                       h6("Risk scores of low (1), moderate (2), and high (3) are entered based on the interpretation of the question, answers, and risk score guidance in the context of available information for the species. When information is not readily available or is of poor quality or when there is conflicting information, assessors should use their best judgement to decide on a score and adjust the uncertainty score accordingly. In the absence of any information on a species, information from similar species in the same genus (or higher taxonomic level) should be used."),
#                       h5("Uncertainty score"),
#                       h6("Uncertainty scores are determined based on the availability and quality of information and the level of expertise of the assessor as follows: "),
#                       h6("
# 1 - Low certainty - Little to no reliable information is available AND the assessor has no experience with the species."),
#                       h6("
# 2 - Moderate certainty - Some reliable information is available. If information is incomplete, it is supplemented with information on or experience with similar species in a similar environment."),
#                       h6("
# 3 - High certainty - A considerable amount of reliable information is available OR the assessor has first-hand experience with the species and the assessment area.
# "),
#                       h5("Rationale"),
#                       h6("The rationale is a short summary of the reasoning behind both the risk score and uncertainty score for each question, including relevant information and sources. Rationales can include direct quotes from databases or literature or a paraphrased summary or an expert reasoning. Short yes or no statements are less valuable and should be avoided. All sources should be listed in annotated form, with full references listed in the References sheet. "),
#                       h5("Raw scores"),
#                       h6("Raw scores for likelihood of invasion, impact of invasion, and mean risk score are calculated using individual question risk scores only (not uncertainty scores) as follows: "),
#                       h6("
# Likelihood of invasion: mean Q1 to Q8, range 1-3"),
#                       h6("
# Impact of invasion: mean Q9 to Q17, range 1-3"),
#                       h6("
# Mean risk score: Likelihood of invasion x Impact of invasion, range 1-9"),
#                       h6("
# Raw scores are not displayed but can be calculated using the above formulas if required. 
# "),
#                       h5("Adjusted scores"),
#                       h6("Adjusted risk scores (i.e., Likelihood of invasion, Impact of invasion, and CMIST score) are calculated by incorporating uncertainty scores with risk scores in a Monte Carlo simulation (see Drolet et al. (2016) for detailed explanation). To obtain adjusted risk scores in CMIST, press the "Calculate adjusted risk scores" button after all risk scores and uncertainty scores have been entered. Once the button has been pressed, any changes to risk or uncertainty scores for questions will automatically be reflected. Because this method uses random numbers, if the button is pressed again, adjusted risk scores will be recalculated with new random numbers and may generate slightly different scores and confidence limits."),
#                       h6("
# We recommend using adjusted risk scores because they allow uncertainty to be quantified and incorporated into the risk score. However, raw risk scores may also be used in conjunction with question-specific uncertainty and rationales depending on the needs and objectives of the assessor or managers. Adjusted risk scores should always be reported with their confidence limits.
# "),
#                       
#                       h4("3) References"),
#                       h5("This section is a list of the resources used in the rationales on the CMIST Assessment sheet. Websites and publications should be listed separately in this sheet in the indicated columns in a consistent format. If additional resources were used during the assessment but not used specifically in the rationales (e.g., models, environmental data, and ancillary information), include them in the Additional Resources column. There is no prescribed format for references, but the information should be complete enough to guide others to the resource. References listed here may be included as suggested resources for future assessments in a given assessment area. "),
#                       h5("Literature Cited"),
#                       h6("DFO (2009) Development of a framework and principles for the biogeographic classification of Canadian marine areas. DFO Canadian Science Advisory Secretariat Science Advisory Report 2009/056, p 17"),
#                       h6("
# DFO (2016) Proceedings of the national science advisory process to peer review the marine screening-level risk assessment tool for aquatic non-indigenous species; February 4-6, 2015. DFO Can. Sci. Advis. Sec. Proceed. Ser. 2016/033"),
#                       h6("
# Drolet D, DiBacco C, Locke A, McKenzie CH, McKindsey CW, Moore AM, Webb JL, Therriault TW (2016) Evaluation of a new screening-level risk assessment tool applied to non-indigenous marine invertebrates in Canadian coastal waters. Biological Invasions. 18(1): 279-294 DOI:10.1007/s10530-015-1008-y
# "),
#                       ),
#              tabPanel(),
#              tabPanel()
#            )
#   ),
#   
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
               textInput("A17", "*Species Genus"),
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
           sidebarLayout(
             
             sidebarPanel(
               
              # "CMIST Score", tableOutput("table1"),
               selectInput(inputId = "filterID", label="Filter by Species",
                           choices=unique(cmist_database$SPC_GENUS.SPC_SPECIES)),
               selectInput(inputId = "filterRegion", label = "Filter by Region",
                           choices = unique (cmist_database$ASA_STUDY_AREA)),
           radioButtons("err_bar", "Error Bar", c("With error bar"="yes",
                                                  "No error bar"="no")),
           radioButtons("comp", "Filter On/OFF",
                        c("Turn on Filters"="yes",
                          "Turn off Filters"="no"))
           ),
           
           mainPanel(
             plotOutput("cmist.plot", click =  "plot.click"),
             verbatimTextOutput("text1"),
             plotOutput("filterPlot", click="filtered.click"),
             verbatimTextOutput("text2"),
           tabsetPanel(
             tabPanel("Pre-Assessment Info", tableOutput("table2")),
             tabPanel("CMIST Assessment", tableOutput("table3")),
             tabPanel("Reference List", tableOutput("table4")),
             box(textInput("downloadName", "Specify desired name of results file"),
                 downloadButton("downloadData", "Download Results")
           ))
           ))),
  
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
summaryScore<- reactive(CMISTScore(req(risks()), req(uncertainties())))

#output$table1<- renderTable(summaryScore())

output$cmist.plot<- renderPlot({
  cm<-summaryScore()
  cm_plot<-ggplot(cm, aes(x=Impact_Score, y=Likelihood_Score))+
    geom_point(colour="red", size=5)
  if(input$err_bar=="yes") {cm_plot<- cm_plot +
    geom_errorbar(aes(ymin=Likelihood_Lower, ymax=Likelihood_Upper))+
    geom_errorbarh(aes(xmin=Impact_Lower, xmax=Impact_Upper))}
  if(input$comp=="yes"){cm_plot<-
    cm_plot+geom_point(data=cmist_database, aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="blue")}
  cm_plot
})
  
  select_species<-reactive({
    cmist_database%>%
      filter(SPC_GENUS.SPC_SPECIES==input$filterID)%>%
      filter(ASA_STUDY_AREA==input$filterRegion)
  })
  

output$filterPlot<- renderPlot({
  cm<-summaryScore()
  ss<-select_species()
  p<-ggplot(cm, aes(x=Impact_Score, y=Likelihood_Score))+
    geom_point(colour="red", size=5)
  if(input$comp=="yes"){p<-p +geom_point(data=ss,
                                         aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="blue")}
  p
})

output$cmist_score<-renderPrint({
  req(input$plot.click)
  text.cm<-summaryScore()
  test.cm.score<-text.cm$CMIST_Score
})

output$table2<- renderTable({summaryPrep()})
    

output$table3<-renderTable({summaryValue()})

output$table4<-renderTable({summaryRef()})

output$downloadData<- downloadHandler(
  filename=function(){
    paste(input$downloadName, ".xlsx", sep="")
  },
  content=function(file){
    
     wb<- write.xlsx(summaryPrep(), file, sheetName="Pre_Assessment_Info")
     addWorksheet(wb, "CMIST_Data")
     addWorksheet(wb, "References")
     writeData(wb, "CMIST_Data", summaryValue())
     writeData(wb, "References", summaryRef())
     saveWorkbook(wb, file, overwrite = TRUE)
   
  }
)

 }


shinyApp(ui, server)

