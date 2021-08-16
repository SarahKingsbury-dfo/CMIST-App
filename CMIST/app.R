if(!require("tidyverse")) install.packages("tidyverse")
if(!require("shiny")) install.packages("shiny")
if(!require("shinydashboard")) install.packages("shinydashboard")
if(!require("devtools")) install.packages("devtools")
if(!require("CMISTR")) devtools::install_github("https://github.com/remi-daigle/CMISTR")
if(!require("DT")) install.packages("DT")
if(!require("leaflet.minicharts")) install.packages("leaflet.minicharts")
if(!require("sf")) install.packages("sf")
if(!require("raster")) install.packages("raster")
if(!require("leaflet")) install.packages("leaflet")
if(!require("data.table")) install.packages("data.table")
if(!require("openxlsx")) install.packages("openxlsx")
if(!require("rio")) install.packages("rio")
if(!require("rJava")) install.packages("rJava")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("dplyr")) install.packages("dplyr")
if(!require("RColourBrewer")) install.packages("RColourBrewer")
if(!require("leaflet.opacity")) install.packages("leaflet.opacity")
if(!require("mailR")) install.packages("mailR")
if(!require("spocc")) install.packages("spocc")
if(!require("robis")) install.packages("robis")
if(!require("rgbif")) install.packages("rgbif")
if(!require("rnaturalearth")) install.packages("rnaturalearth")
if(!require("scrubr")) install.packages("scrubr")
if(!require("mapr")) install.packages("mapr")
if(!require("viridis")) install.packages("viridis")


proj <- "+proj=longlat +datum=WGS84"

cmist_database<-read.csv("data/cmist_data.csv")

CMISTdata <- get_CMIST_database()

CMISTdata<-CMISTdata%>%
  dplyr::mutate(SPC_GENUS.SPC_SPECIES=paste(CMISTdata$SPC_GENUS, CMISTdata$SPC_SPECIES, sep = " "),
                g=(row.names(.)))%>%
  dplyr::mutate(g=as.numeric(g),
                ASU_ADJ_RISK_SCORE=as.numeric(ASU_ADJ_RISK_SCORE),
                ASU_RAW_IMPACT_INVASION=as.numeric(ASU_RAW_IMPACT_INVASION),
                ASU_RAW_LIKELIHOOD_INVASION=as.numeric(ASU_RAW_LIKELIHOOD_INVASION))

#generating a geometry for polygons of the cmist database


mist_sf<-CMISTdata%>%
  mutate(ASA_EASTERN_LONGITUDE=as.numeric(ASA_EASTERN_LONGITUDE),
         ASA_NORTHERN_LATITUDE=as.numeric(ASA_NORTHERN_LATITUDE),
         ASA_SOUTHERN_LATITUDE=as.numeric(ASA_SOUTHERN_LATITUDE),
         ASA_WESTERN_LONGITUDE=as.numeric(ASA_WESTERN_LONGITUDE))%>%
  rowwise() %>%
  dplyr::mutate(geometry= (list(
           matrix(c(ASA_EASTERN_LONGITUDE, ASA_NORTHERN_LATITUDE,
                     ASA_EASTERN_LONGITUDE, ASA_SOUTHERN_LATITUDE,
                     ASA_WESTERN_LONGITUDE, ASA_SOUTHERN_LATITUDE,
                     ASA_WESTERN_LONGITUDE, ASA_NORTHERN_LATITUDE,
                     ASA_EASTERN_LONGITUDE, ASA_NORTHERN_LATITUDE),
                  ncol=2,byrow = TRUE))) %>%
           #mutate(geometry=as.numeric(geometry))%>%
            #list() %>%
            st_polygon() %>%
           #st_sfc(proj)
             st_sfc(crs="+proj=longlat +datum=WGS84")
         )


#setting the base map to Canada
Canada_map<- leaflet()%>%
  setView(lng=-106.9299, lat=51.9788, zoom=3)%>%
  addTiles()

#creating a colour pallete for the cmist database
mist_sf_map<-mist_sf%>%
  mutate(ASA_STUDY_AREA=as.factor(ASA_STUDY_AREA))%>%
  st_as_sf()%>%
  st_transform(crs=4326)

binpal<- colorFactor("Blues", mist_sf_map$ASA_STUDY_AREA, 9)


ui<-navbarPage(
  title = "Canadian Marine Invasive Screening Tool (CMIST) App",
  
  tabPanel("CMIST Home Page",
           tabsetPanel(
             tabPanel("The CMIST App",
                      h3("Highlights"),
                      h5(".	CMIST was designed for and tested with marine invertebrates, but its theoretical basis and general questions make it suitable for other organisms."),
                      h5(".	Screening-level risk assessments in CMIST are designed to be completed in 1-2 days including collection of information and answering questions. "),
                      h5(".	Assessors should have good general knowledge on invasive species and, ideally, the assessment area and species being assessed."),
                      h5(".	Prior to assessment, background information on the selected species that pertains to CMIST questions should be collated from available resources and quickly reviewed. "),
                      h5(".	Assessors should always use their best judgement when interpreting questions, answers, and guidance for the selected species, which will likely not have species or taxon-specific examples."),
                      h5(".	Assessors should read the manual before completing any assessments to ensure consistency with guidance."),
                      
                      h3("Introduction"),
                      h5("CMIST (Canadian Marine Invasive Screening Tool) assessments provide information on the likelihood and impact of invasions of different species in different assessment areas with scores adjusted for assessor uncertainty. Together, these assessments act as a new resource of collated information on invasive species that can assist researchers and managers dealing with aquatic invasive species (AIS) and increase the efficiency of decision-making. The growing database of CMIST assessments is as an important repository for information on invasive species for students, researchers, and citizens around the world."),
                      h5("The Canadian Marine Invasive Screening Tool (CMIST) is a peer-reviewed screening-level risk assessment tool for marine invasive species (Drolet et al. 2016). It is a short questionnaire that follows the invasion process from arrival to impact and is designed so an informed assessor can evaluate one species in an assessment area in approximately one day using easily accessible information from internet databases, primary literature, and grey literature. Species can be those with an invasion history in an area or those that are candidates for future invasions."),
                      h5("CMIST is score-based and incorporates both likelihood and impact of invasion as well as uncertainty. Questions are general to make CMIST broadly applicable to different taxa, different assessment areas, and different project goals. To date, CMIST has been tested with molluscs, tunicates, crustaceans, and polychaetes introduced or at risk of introduction to three Canadian marine ecoregions (DFO 2009). CMIST has also been successfully applied to non-indigenous freshwater fish in British Columbia with adapted guidelines (T. Therriault, pers. comm.). Upon completion, CMIST produces a risk score adjusted for the assessor's uncertainty which, combined with information collected during the assessment, can be used to assist in management decisions. For example, in 2015, CMIST assessments were used to identify high risk invaders in three Canadian marine ecoregions (DFO 2016). Completed assessments submitted from assessors are reviewed for accuracy and completeness then housed online in the searchable CMIST database."),
                      h5("This manual contains best practices for using CMIST and should be read in full before proceeding with an assessment to achieve consistent results. For more in-depth background information, refer to Drolet et al. (2016)."),
                      
                      h3("Pre-assessment Preparation"),
                      h4("Species"),
                      h5("Although CMIST was designed for and tested with marine invertebrates, its theoretical basis and general questions makes it suitable for other organisms. Assessors should always use their best judgement when interpreting questions, answers, and guidance for the selected species, which will likely not have species or taxon-specific examples."),
                      h5("Prior to assessment, background information on the selected species that pertains to CMIST questions should be collated from available resources and quickly reviewed. If there is little known about the species, information on other species in the genus (or higher taxonomic level) should be obtained and used as complementary resources. A lack of species-specific information would be expected to increase uncertainty."),
                      h4("Assessment area"),
                      h5("The assessment area can be any size or scope but must be defined since most CMIST questions use this assessment area as a context for answers (see Appendix A for questions, answers, and guidance).  It is up to the assessor to determine (and document) the scale used for their assessment."),
                      h5("Prior to assessment, background information should be collated on the assessment area from available resources and quickly reviewed. Pertinent information includes both physical characteristics (e.g., bottom types, habitats, temperature range, and salinity range) and biological components (e.g., species at risk, aquaculture species, commercially fished species, and species of special interest)."),
                      h4("Assessor"),
                      h5("All CMIST questions are semi-quantitative and require interpretation and judgement to answer based on available information as well as the assessor's expert opinion. An assessor should therefore have good general knowledge on invasive species and, ideally, the assessment area and species being assessed. A less-knowledgeable assessor would be expected to have higher uncertainty, especially for species with little available information.
                      Prior to assessment, assessors should review background information on the species and assessment area and familiarize themselves with CMIST questions, answers, guidance, and glossary."),
                      
                      ),
             tabPanel("Using CMIST",
                      #h3("Using CMIST"),
                      h3("1) Assessment Information"),
                      h4("This section contains general information about the species, the assessment area, and the assessor. If you submit your assessment for inclusion in the CMIST database, you must complete all information labelled with an asterisk. Contact information including address, phone, and email is for communication purposes only and will not be made publicly available."),
                      h3("2) CMIST Assessment"),
                      h4("This section contains the questionnaire and final risk scores for CMIST."),
                      #h5("The assessor should answer questions by entering risk scores, uncertainty scores, and rationales in the designated cells. To obtain the risk score adjusted for uncertainty, press the "Calculate adjusted risk scores" button. Adjusted risk scores and lower and upper confidence limits will then be displayed. See below for guidance on specific sections."),
                      h4("Questions"),
                      h5("CMIST has 17 questions that follow the invasion process: arrival (Q1 and Q2), survival (Q3 and Q4), establishment (Q5 and Q6), spread (Q7 and Q8), and impact (Q9-Q17). The first 8 questions relate to the likelihood of invasion and the next 9 questions relate to the impact of invasion."),
                      h5("Many terms used in CMIST questions (e.g., population growth, invasive, community) have different definitions depending on perspective and context. Such terms and their intent in the CMIST context are defined in the glossary."),
                      h4("Answers"),
                      h5("Each question has three possible answers (1-3). Answers differ between questions and should be considered carefully in conjunction with the risk score guidance before entering a score."),
                      h4("Risk score"),
                      h5("Risk scores of low (1), moderate (2), and high (3) are entered based on the interpretation of the question, answers, and risk score guidance in the context of available information for the species. When information is not readily available or is of poor quality or when there is conflicting information, assessors should use their best judgement to decide on a score and adjust the uncertainty score accordingly. In the absence of any information on a species, information from similar species in the same genus (or higher taxonomic level) should be used."),
                      h4("Uncertainty score"),
                      h5("Uncertainty scores are determined based on the availability and quality of information and the level of expertise of the assessor as follows: "),
                      h5("1 - Low certainty - Little to no reliable information is available AND the assessor has no experience with the species."),
                      h5("2 - Moderate certainty - Some reliable information is available. If information is incomplete, it is supplemented with information on or experience with similar species in a similar environment."),
                      h5("3 - High certainty - A considerable amount of reliable information is available OR the assessor has first-hand experience with the species and the assessment area."),
                      h4("Rationale"),
                      h5("The rationale is a short summary of the reasoning behind both the risk score and uncertainty score for each question, including relevant information and sources. Rationales can include direct quotes from databases or literature or a paraphrased summary or an expert reasoning. Short yes or no statements are less valuable and should be avoided. All sources should be listed in annotated form, with full references listed in the References sheet."),
                      h4("Raw scores"),
                      h5("Raw scores for likelihood of invasion, impact of invasion, and mean risk score are calculated using individual question risk scores only (not uncertainty scores) as follows: "),
                      h5("Likelihood of invasion: mean Q1 to Q8, range 1-3"),
                      h5("Impact of invasion: mean Q9 to Q17, range 1-3"),
                      h5("Mean risk score: Likelihood of invasion x Impact of invasion, range 1-9"),
                      h5("Raw scores are not displayed but can be calculated using the above formulas if required."),
                      h4("Adjusted scores"),
                      h5("Adjusted risk scores (i.e. Likelihood of invasion, Impact of invasion, and CMIST score) are calculated by incorporating uncertainty scores with risk scores in a Monte Carlo simulation (see Drolet et al. (2016) for detailed explanation)."),
                      #h6("To obtain adjusted risk scores in CMIST, press the "Calculate adjusted risk scores" button after all risk scores and uncertainty scores have been entered. Once the button has been pressed, any changes to risk or uncertainty scores for questions will automatically be reflected. Because this method uses random numbers, if the button is pressed again, adjusted risk scores will be recalculated with new random numbers and may generate slightly different scores and confidence limits."),
                      h5("We recommend using adjusted risk scores because they allow uncertainty to be quantified and incorporated into the risk score. However, raw risk scores may also be used in conjunction with question-specific uncertainty and rationales depending on the needs and objectives of the assessor or managers. Adjusted risk scores should always be reported with their confidence limits."),
                      
                      h3("3) References"),
                      h4("This section is a list of the resources used in the rationales on the CMIST Assessment sheet. Websites and publications should be listed separately in this sheet in the indicated columns in a consistent format. If additional resources were used during the assessment but not used specifically in the rationales (e.g., models, environmental data, and ancillary information), include them in the Additional Resources column. There is no prescribed format for references, but the information should be complete enough to guide others to the resource. References listed here may be included as suggested resources for future assessments in a given assessment area. "),
                      h4("Literature Cited"),
                      h5("DFO (2009) Development of a framework and principles for the biogeographic classification of Canadian marine areas. DFO Canadian Science Advisory Secretariat Science Advisory Report 2009/056, p 17"),
                      h5("DFO (2016) Proceedings of the national science advisory process to peer review the marine screening-level risk assessment tool for aquatic non-indigenous species; February 4-6, 2015. DFO Can. Sci. Advis. Sec. Proceed. Ser. 2016/033"),
                      h5("Drolet D, DiBacco C, Locke A, McKenzie CH, McKindsey CW, Moore AM, Webb JL, Therriault TW (2016) Evaluation of a new screening-level risk assessment tool applied to non-indigenous marine invertebrates in Canadian coastal waters. Biological Invasions. 18(1): 279-294 DOI:10.1007/s10530-015-1008-y"))
           ),
              tabPanel("Glossary",
                       h3("assessment area"),
                       h5("an area of any size as pre-defined by the user for the purposes of the assessment"),
                       
                       h3("community*"),
                       h5("any group of organisms belonging to a number of different species that co-occur in the same habitat or area and interact through trophic and spatial relationships; typically characterized by reference to one or more dominant species"),
                       
                       h3("ecosystem*"),
                       h5("a community of organisms and their physical environment interacting as an ecological unit"),
                       
                       h3("effective"),
                       h5("producing the intended result (e.g., slowed population growth by a predator)"),
                       
                       h3("established*"),
                       h5("growing and reproducing successfully in a given area"),
                       
                       h3("habitat"),
                       h5("the locality, site, and particular type of local environment occupied by an organism"),
                       
                       h3("invasive???"),
                       h5("a nonindigenous species that spreads rapidly, causing environmental or economic damage (definition often used by managers)"),
                       
                       h3("other species"),
                       h5("any species that is not the subject of the assessment"),
                       
                       h3("population growth*"),
                       h5("the change in population size with time as a net result of natality, mortality, immigration, and emigration"),
                       
                       h3("species"),
                       h5("the subject of the assessment"),
                       
                       h3("suitable habitat"),
                       h5("the portion of the habitat zone within the assessment area in which the species could live"))
              
  ),

  tabPanel("Pre-Assessment Information",
            # sidebarLayout(
            #  
            #   sidebarPanel(
           
           h4("Please fill in the pre-assessment information. This information will be combined with the CMIST Assessment Score information and the Reference List into a PDF, a copy of which will be sent to DFO for peer-review and the other copy sent to the assessors email address."),
           h4("Note: The CMIST Score, the assessment area, region, country, and location (i.e the latitude and longitude), and the species's information from this assessment report will become part of the CMIST database. Through collecting these value paces of information from multiple CMIST assessments, we are able to provide CMIST app users with the ability to make comparisons between regions or between species within one articular region. By using the CMIST App, you are providing your informed consent that this information will be made public. Please note that the assessor's information will not become part of any public database. Only the DFO peer-reviewers and the assessor will have access to the assessor's personal information. The DFO peer-review staff need access to the assessor's information in case further information or clarification on assessment information is needed. "),
               
               h3("Assessor Information"),
           fluidRow(
               column(6, textInput("A1", "*Title: Project Title (e.g. Cipangopaludina chinensis Assessment-Maritime Region")),
               column(6, textInput("A2", "*Description: Please enter a short project or species description")),
               column(6, textInput("A3", "*Assessor Name:Your Name (first and last)")),
               column(6, textInput("A4", "*Affilitation: Please enter your affiliation (may enter more than one)")),
               column(6, textInput("A5", "Address: Please enter your address")),
               column(6, textInput("A6", "Phone: Enter Phone Number")),
               column(6, textInput("A7","*Email Adress"))
           ),
               
               h3("Species Information"),
            fluidRow(
              column(4, textInput("A8", "*Species AphiaID (WoRMS)")),
              column(4,textInput("A9", "*Species TSN (ITIS)")),
              column(4,textInput("A10", "Taxon: General taxon or group")),
              column(4,textInput("A11", "Species Kingdom")),
              column(4,textInput("A12", "Species Phylum")),
              column(4,textInput("A13", "Species Subphylum")),
              column(4,textInput("A14", "Species Class")),
              column(4,textInput("A15", "Species Order")),
              column(4,textInput("A16", "Species Family")),
              column(4,textInput("A17", "*Species Genus")),
              column(4,textInput("A18", "*Species")),
              column(5,selectInput("A19", "*Please select the appropriate ecosystem (either Freshwater, Brackish, or Marine)", c("Freshwater", "Marine", "Brackish"))),
              column(5,textInput("A20", "*Please list all common names for the species (e.g. Chinese mystery snail, Mud snail, etc)")),
              column(5,textInput("A21", "Additional Notes: Additional taxonomic notes here"))
               ),
               
               h3("Study Area Information"),
            fluidRow(   
              column(4,textInput("A22", "*Area:Study Area Name")),
              column(4,textInput("A23", "*Region:Study Region (e.g. Province, State)")),
              column(4,textInput("A24", "*Country: Country of Study Area")),
              column(6,textInput("A25", "*Latitude 1: Latitude (northern boundary) in decimal degrees")),
              column(6,textInput("A26", "*Latitude 2: Latitude (southern boundary) in decimal degrees")),
              column(6,textInput("A27", "*Longitude 1: Longitude (western boundary) in decimal degrees")),
              column(6,textInput("A28", "*Longitude 2: Longitude (eastern boundary) in decimal degrees"))
            )
             #   actionButton("sub", "SUBMIT")
             # ),
             # mainPanel(tableOutput("prepMaterials"))
 # )
  ),
  
  tabPanel("CMIST Assessment",
            sidebarLayout(
              
              sidebarPanel(
               selectInput("Q1",
                           "1.(a) Is the species established in the assessment area?",
                           c(1,2,3)),
               selectInput("U1",
                           "1.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R1", "1.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species distribution data indicate species is widely dispersed in assessment area (ref).", resize = "both"),
               selectInput("Q2",
                           "2.(a) How frequently and in what numbers is the species expected to arrive into the assessment area?",
                           c(1,2,3)),
               selectInput("U2",
                           "2.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R2", "2.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species distribution data and habitat suitability models indicate low arrivals (ref).", resize = "both"),
               selectInput("Q3",
                           "3.(a) How much of the assessment area offers suitable habitat for the species?",
                           c(1,2,3)),
               selectInput("U3",
                           "3.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R3", "3.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Habitat suitability models indicate abundant suitable habitat (ref).", resize = "both"),
               selectInput("Q4",
                           "4.(a) How much of the assessment area offers suitable environmental conditions for the species to survive?",
                           c(1,2,3)),
               selectInput("U4",
                           "4.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R4", "4.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Ecological threshold testing shows that species is highly adaptable (ref).", resize = "both"),
               selectInput("Q5",
                           "5.(a) Are the species' reproductive requirements available in the assessment area?",
                           c(1,2,3)),
               selectInput("U5",
                           "5.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R5", "5.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species reproductive needs are/could be met within assessment area (ref).", resize = "both"),
               selectInput("Q6",
                           "6.(a) To what extent could natural control agents slow the species' population growth in the assessment area?",
                           c(1,2,3)),
               selectInput("U6",
                           "6.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R6", "6.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. There are no known control methods that are affective for this species (ref).", resize = "both"),
               selectInput("Q7",
                           "7.(a) What is the range of the species' potential natural dispersal in the assessment area?",
                           c(1,2,3)),
               selectInput("U7",
                           "7.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R7", "7.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species is like to disperse naturally within assessment area (ref).", resize = "both"),
               selectInput("Q8",
                           "8.(a) What is the range of the species' potential dispersal in the assessment area from anthropogenic mechanisms?",
                           c(1,2,3)),
               selectInput("U8",
                           "8.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R8", "8.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Anthropogenic recreational water use will likely lead to furtehr species dispersal (ref).", resize = "both"),
               selectInput("Q9",
                           "9.(a) What level of impact could the species have on population growth of other species in the assessment area?",
                           c(1,2,3)),
               selectInput("U9",
                           "9.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R9", "9.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species will likely outcompete native species for available resources (ref).", resize = "both"),
               selectInput("Q10",
                           "10.(a) What level of impact could the species have on communities in the assessment area?",
                           c(1,2,3)),
               selectInput("U10",
                           "10.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R10", "10.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species will likely alter community structure in a top-down approach  (ref).", resize = "both"),
               selectInput("Q11",
                           "11.(a) What level of impact could the species have on habitat in the assessment area?",
                           c(1,2,3)),
               selectInput("U11",
                           "11.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R11", "11.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species is documented as an ecosystem engineer and will likely alter invaded habitat (ref).", resize = "both"),
               selectInput("Q12",
                           "12.(a) What level of impact could the species have on ecosystem function in the assessment area?",
                           c(1,2,3)),
               selectInput("U12",
                           "12.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R12", "12.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species presence will likely decrease aquaculture yields (ref).", resize = "both"),
               selectInput("Q13",
                           "13.(a) What level of impact could the species' associated diseases, parasites, or travellers have on other species in the assessment area?",
                           c(1,2,3)),
               selectInput("U13",
                           "13.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R13", "13.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species is a known host of multiple parasites (ref).", resize = "both"),
               selectInput("Q14",
                           "14.(a) What level of genetic impact could the species have on other species in the assessment area?",
                           c(1,2,3)),
               selectInput("U14",
                           "14.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R14", "14.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species may hybridize with other indigenous species (ref).", resize = "both"),
               selectInput("Q15",
                           "15.(a) What level of impact could the species have on at-risk or depleted species in the assessment area?",
                           c(1,2,3)),
               selectInput("U15",
                           "15.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R15", "15.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species will impact species-at-risk life cycles, especially juvenile stages (ref).", resize = "both"),
               selectInput("Q16",
                           "16.(a) What level of impact could the species have on aquaculture and fished species in the assessment area?",
                           c(1,2,3)),
               selectInput("U16",
                           "16.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R16", "16.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species will prey on mussel spat (ref).", resize = "both"),
               selectInput("Q17",
                           "17.(a) Is the species known or generally considered to be invasive anywhere in the world?",
                           c(1,2,3)),
               selectInput("U17",
                           "17.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(1,2,3)),
               textAreaInput("R17", "17.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species is considered invasive in USA, Mexico, and Canada (ref).", resize = "both")
              # actionButton("go", "SUBMIT")
              ),
              mainPanel(
                h2("Question Considerations"),
                        h4("1. This question is meant to differentiate species that are not present in the assessment area (1) from species that are established in the assessment area (3). Species that are present in the assessment area but not established would score 2."),
                        h5("1 ??? No
2 ??? Observed but not reported as established 
3 ??? Yes"
                        ),
                        
                        h4("2. Consider initial arrival into the assessment area by primary vectors only. Do not consider secondary spread (anthropogenic or natural) within the assessment area by species that are already established. Consider all primary anthropogenic and natural vectors for transport into the assessment area (e.g., ballast water, hull fouling, aquaculture, rafting, and natural dispersal from outside the assessment area). "),
                        h5("1 ???  Infrequently in low numbers 
2 ??? Frequently in low numbers OR infrequently in high numbers
3 ??? Frequently in high numbers"
                        ),
                        
                        h4("3. This question is meant to differentiate habitat specialists (1) from habitat generalists (3). Species that fall between these extremes would score 2. First consider the species' broad habitat zone (e.g., intertidal, subtidal, benthic, and pelagic). Then consider the proportion of that zone within the assessment area that offers suitable habitat for the species. Consider suitable anthropogenic habitat (e.g., docks and aquaculture sites) as well as natural habitat."),
                        h5("1 ??? Negligible proportion of the assessment area
2 ??? Moderate proportion of the assessment area
3 ??? Most of the assessment area"
                        ),
                        
                        h4("4. This question is meant to differentiate species with very poor environmental match for survival (1) from those with a very good environmental match (3). Species that fall between these extremes would score 2. Consider environmental conditions (e.g., temperature, salinity, and turbidity) in its suitable habitat (see Question 3).  Consider the most tolerant life stage at any time of year.  Consider survival only, not reproduction."),
                        h5("1 ??? Negligible proportion of the assessment area
2 ??? Moderate proportion of the assessment area
3 ??? Most of the assessment area"
                        ),
                        
                        h4("5. This question is meant to differentiate species that face severe constraints in reproduction in the assessment area and are very unlikely to reproduce in a typical year (1) from those that face few constraints in reproduction in the assessment area and are very likely to reproduce every year (3). Species that fall between these extremes would score 2.Consider any constraint (e.g., temperature, salinity, and stage-specific habitat) in the species' ontogenetic development (e.g., spawning, fertilization, and propagule dispersal) that may affect its ability to reproduce successfully in otherwise suitable habitat (see Question 3)."),
                        h5("1 - Almost never
2 - Sometimes
3 - Almost always"
                        ),
                        
                        h4("6. This question is meant to differentiate species with known, effective natural control agents in the assessment area (1) from those with no known, effective natural control agents in the assessment area (3). Species with known-but not necessarily effective-natural control agents in the assessment area would score 2. Consider presence and incidence of known natural control agents (e.g., predators, competitors, disease, and disturbance) in the species' suitable habitat (see Question 3) and to what extent they could slow the species' population growth."),
                        h5("1 - Likely to severely restrict population growth
2 - Could slow population growth 
3 - Unlikely to slow population growth"
                        ),
                        
                        h4("7. This question is meant to differentiate species that face severe constraints in natural dispersal (e.g., short larval planktonic stage and sessile adults) (1) from those that face few constraints (e.g., long larval planktonic stage, motile adults) (3). Species that fall between these extremes would score 2. Consider the natural dispersal vectors (e.g., currents, rafting, and migration) for all life stages. Consider any constraints on natural dispersal vectors in the assessment area.
"),
                        h5("1 - Very limited range
2 - Moderate range
3 - Wide range
"),
                        
                        h4("8. This question is meant to differentiate species likely to have little to no contact with anthropogenic mechanisms of dispersal in the assessment area (1) from those that are likely to have contact with anthropogenic mechanisms that could disperse them over large distances (e.g., among embayments) (3). Species that have contact with anthropogenic mechanisms that could disperse them over short distances (e.g., among sites in an embayment) would score a 2. Consider anthropogenic dispersal vectors (e.g., ballast, hull fouling, and aquaculture) for all life stages.
"),
                        h5("1 - Very limited range
2 - Moderate range
3 - Wide range
"),
                        
                        h4("9. Only consider impacts in the species' suitable habitat (see Question 3). Consider positive and negative impacts (i.e. population increase or decrease). Consider impacts to indigenous and non-indigenous populations. Consider ecological impacts on aquaculture and commercially fished species, not economic impacts on the industry itself.
"),
                        h5("1 - Low or no impact
2 - High impact in few areas OR moderate impact in many areas
3 - High impact in many areas"),
                        
                        h4("10. Only consider impacts in the species' suitable habitat (see Question 3). Consider positive and negative impacts (i.e. population increase or decrease). Consider impacts to indigenous and non-indigenous populations. 
"),
                        h5("1 - Low or no impact
2 - High impact in few areas OR moderate impact in many areas
3 - High impact in many areas"),
                        
                        h4("11. Only consider impacts in the species' suitable habitat (see Question 3) and not on associated communities. Consider habitat engineering (e.g., reef-building organisms) and habitat destruction (e.g., bioturbating organisms).
"),
                        h5("1 - Low or no impact
2 - High impact in few areas OR moderate impact in many areas
3 - High impact in many areas"),
                        
                        h4("12. Only consider impacts in the species' suitable habitat (see Question 3). Consider changes (positive or negative) to the physical, chemical, and biological processes that would normally maintain the ecosystem.
"),
                        h5("1 - Low or no impact
2 - High impact in few areas OR moderate impact in many areas
3 - High impact in many areas"),
                        
                        h4("13. Only consider impacts in the species' suitable habitat (see Question 3)."),
                        h5("1 - Low or no impact
2 - High impact in few areas OR moderate impact in many areas
3 - High impact in many areas"),
                        
                        h4("14. Only consider impacts in the species' suitable habitat (see Question 3). Consider indigenous and non-indigenous species in the assessment area. Consider hybridization (among species hybridization and supplementation of genetic material between strains or varieties of a species) as well as other genetic impacts.
"),
                        h5("1 - Low or no impact
2 - High impact in few areas OR moderate impact in many areas
3 - High impact in many areas"),
                        
                        h4("15. Consider all possible impacts on species in the assessment area that are depleted, of extra value, or recognized as being at risk.
"),
                        h6("1 - Low or no impact
2 - High impact in few areas OR moderate impact in many areas
3 - High impact in many areas"),
                        
                        h4("16. Consider ecological impacts on aquaculture and fished species (e.g., from commercial, recreational, and indigenous fisheries) in aquaculture operations and the wild, but not economic impacts on the industry itself.
"),
                        h5("1 - Low or no impact
2 - High impact in few areas OR moderate impact in many areas
3 - High impact in many areas"),
                        
                        h4("17. This question is meant to differentiate species that are not invasive and not likely to be invasive based on their life history traits (1) from those that are known or generally considered to be invasive (3). An introduced species that is not generally considered to be invasive but that has traits related to invasiveness would score a 2. An introduced species can be non-invasive.
"),
                        h5("1 - No
2 - No, but has traits related to invasiveness
3 - Yes"),
                        h2("Rationale Guidance"),
                h4("Include key information used to determine risk and uncertainty scores."),
                h4("Include direct quotations from literature, paraphrased summaries, and statements of expert opinion."),
                h4("In cases where the answer is one of two options (e.g., high impact in few areas OR moderate impact in many areas), indicate which option was favoured and why."),
                h4("Include annotated references in the text (e.g., Drolet et al. 2016; EOL; personal observation). Please include full citations in the this section.")
                # tableOutput("cmist.table"),
                # tableOutput("cmist.score")
                )
            )
  ),
  
  tabPanel("References",
           # sidebarLayout(
           #   
           #   sidebarPanel(
           h4("Please list all references used in the rationales."),
           h4("Also include other resources used during the assessment (e.g., models, environmental data, and ancillary information) in the Other Resources column."),
           h4("Links will be assumed to have been accessed on the date of the CMIST assessment but will not be maintained. References can include websites (please include the URL), publications, reports, theses, etc. Please include the URL or doi link for each reference and provide full bibliography style references."),
             fluidRow( 
               column(4,textInput("C1", "Reference:")),
               column(4,textInput("C2", "Reference:")),
               column(4,textInput("C3", "Reference:")),
               column(4,textInput("C4", "Reference:")),
               column(4,textInput("C5", "Reference:")),
               column(4,textInput("C6", "Reference:")),
               column(4,textInput("C7", "Reference:")),
               column(4,textInput("C8", "Reference:")),
               column(4,textInput("C9", "Reference:")),
               column(4,textInput("C10", "Reference:")),
               column(4,textInput("C11", "Reference:")),
               column(4,textInput("C12", "Reference:")),
               column(4,textInput("C13", "Reference:")),
               column(4,textInput("C14", "Reference:")),
               column(4,textInput("C15", "Reference:")),
               column(4,textInput("C16", "Reference:")),
               column(4,textInput("C17", "Reference:")),
               column(4,textInput("C18", "Reference:")),
               column(4,textInput("C19", "Reference:")),
               column(4,textInput("C20", "Reference:")),
               column(4,textInput("C21", "Reference:")),
               column(4,textInput("C22", "Reference:")),
               column(4,textInput("C23", "Reference:")),
               column(4,textInput("C24", "Reference:")),
               column(4,textInput("C25", "Reference:")),
               column(4,textInput("C26", "Reference:")),
               column(4,textInput("C27", "Reference:")),
               column(4,textInput("C28", "Reference:")),
               column(4,textInput("C29", "Reference:")),
               column(4,textInput("C30", "Reference:"))
             )
           #     actionButton("ref", "SUBMIT")
           #   ),
           #   mainPanel(tableOutput("references")) 
           # )
  ),
  
  tabPanel("Summary",
           sidebarLayout(
             
             sidebarPanel(
               
               h4("You can costumize the plots seen in the main panel by selecting various options below:"),
               h5("Filter plots by species:"),
               radioButtons("filters", "Species and Region Filters", c("No Filters"="no",
                                                                       "Turn on Region Filters"="region", 
                                                                       "Turn on Species Filters"="species", 
                                                                       "Turn on both Region and Species Filters"="both")),
               selectInput(inputId = "filterSpecies", label="Filter by Species",
                           choices=unique(CMISTdata$SPC_GENUS.SPC_SPECIES)),
               
               h5("Filter CMIST Scores by Geographic Region:"),
               selectInput(inputId = "filterRegion", label = "Filter by Region",
                           choices = unique (CMISTdata$ASA_STUDY_AREA)),
               
           
           h5("Turn comparisons with other CMIST assessments on or off:"),
           radioButtons("comp", "Comparisons On/OFF",
                        c("Turn on comparisons"="yes",
                          "Turn off comparisons"="no")),
           
           h5("Download the results of your CMIST assessment including the pre-assessment information, risk assessment answers, references, and CMIST score:"),
           textInput("downloadName", "Specify desired name of results file"),
               downloadButton("downloadData", "Download Results"),
           
           h4("Please submit your results to the CMIST Team to have the assessment included in the CMIST Database. Your results can help others!"),
           h5("Once you have downloaded your results, please attache the xlsx file to an email (click link below) and send to the CMIST Assessors to have your assessment peer-reviewed. "),
           
           a(
             actionButton("email", label="Send to CMIST Database",
                          icon = icon("envelope", lib="font-awesome")),
             href='mailto:sarah.kingsbury@dfo-mpo.gc.ca?
subject=CMIST%20Assessment&
body=Please%20make%20sure%20you%20have%20attached%20the%20xlsx%20file%20before%20sending'
           ),
           
           h5("You can also download a PDF with all the information seen on this page."),
           downloadButton("downloadPDF", "Download PDF")
            
           ),
           
           
           mainPanel(
             h4("Hover over the plot points below to see the species, study area, and CMIST score. Each taxonomic group is a different colour and the point data (i.e. the individual data from each assessment) is a different shape depending on the taxonomic group it belongs to."),
             plotOutput("cmist.plot", hover = hoverOpts(id="plotH")),
             verbatimTextOutput("plotH_info"),
             h4("Hover over the plot points below to see the species, study area, and CMIST score. The black dots are the individual assessments from the CMIST database, the yellow dot is the specific assessment for your current CMIST assessment."),
             h5("Note: You can filter the scatter plot by geographic region, species, or both by selecting options in the side bar."),
             plotOutput("filterPlot", hover = hoverOpts(id="plot_hover")),
             verbatimTextOutput("plot_hoverinfo"),
             h4("Your current CMIST Assessment Score and table outputs below:"),
             "CMIST Score", tableOutput("table1"),
             tabsetPanel(
               tabPanel("Pre-Assessment Info", tableOutput("table2")),
               tabPanel("CMIST Assessment", tableOutput("table3")),
               tabPanel("Reference List", tableOutput("table4"))
               
               )
               )
             )
           ),
                
  
 tabPanel("Explore the CMIST Database",
          h4("Use the text input box below to search public databases such as iNaturalist, GBiF, OBIS, and VertNet for species occurrence records."),
          h4("The interactive map has multiple layers that you can choose to turn on and off."),
          h4("The 'CMIST Database' tab contains a data table that shows all the data currently available in the CMIST Database."),
          textInput("m1", "Search public databases by writing the name of the species you want tosee distribution data for!"),
          radioButtons("m2", "Check whether you want to see species distribution data or not", choices = c("Don't see species data"="no",
                                                                                                           "See species data"="yes")),
          tabsetPanel(
            tabPanel("Interactive Map",leafletOutput("leafletmap",height=800)),
            tabPanel("CMIST Database", DT::dataTableOutput("table5"))
          )
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
  
  rationale<-reactive({
   # browser()
    r=c(R1= input$R1,
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
  if(all(r=="")) {
    r<-c(R1= "NA",
         R2= "NA",
         R3="NA",
         R4="NA",
         R5="NA",
         R6="NA",
         R7="NA",
         R8="NA",
         R9="NA",
         R10="NA",
         R11="NA",
         R12="NA",
         R13="NA",
         R14="NA",
         R15="NA",
         R16="NA",
         R17="NA")
  }
    return(r)
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
  
  question<-c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17")
  
  assessor_info<-c("Project Title",
                   "Description",
                   "Assessor Name",
                   "Affiliations",
                   "Address",
                   "Phone Number",
                   "Email",
                   "AphiaID (WoRMS)",
                   "TSN (ITIS)",
                   "General taxon/group",
                   "Kingdom",
                   "phylum",
                   "subphylum",
                   "class",
                   "order",
                   "family",
                   "genus",
                   "species",
                   "common name(s)",
                   "taxonomic notes",
                   "Study Area",
                   "Study Region",
                   "Ecosystem Type",
                   "Country",
                   "Northern Latitude",
                   "Southern Latitude",
                   "Western Longitude",
                   "Eastern Longitude"
                   )
  
  summaryValue<-reactive({
    cbind(question, risk=req(risks()), uncertainties=req(uncertainties()),rationale=req(rationale()))
    })
  summaryPrep<-reactive({
    cbind(assessor_info, prep= req(prep()))
          })
  summaryRef<- reactive(REFList())
  summaryScore<- reactive(CMISTScore(req(risks()), req(uncertainties())))
  
  
  output$cmist.plot<- renderPlot({
    cm<-summaryScore()
    cm_plot<-
      ggplot(data=CMISTdata, aes(x=Taxonomic_Group, y=ASU_ADJ_RISK_SCORE, fill=Taxonomic_Group))+
    geom_boxplot(show.legend = FALSE)+
      scale_fill_viridis(discrete=TRUE, alpha=0.6)+
      geom_jitter(aes(shape=Taxonomic_Group),size=2, alpha=0.9, show.legend = FALSE)+
      ggtitle("CMIST Database Risk Scores by Taxonomic Group")+
      xlab("Taxonomic Group")+
      ylab("Adjusted CMIST Risk Score")+
      #theme(legend.position = "none")+
      theme_bw()
    cm_plot
  })
  
  plotH_sel<-eventReactive(input$plotH,{
    res2<-nearPoints(CMISTdata, input$plotH, xvar="Taxonomic_Group", yvar = "ASU_ADJ_RISK_SCORE", allRows = FALSE)
    list(paste("CMIST Score",res2$ASU_ADJ_RISK_SCORE, sep=":"), 
         paste("Species",res2$SPC_GENUS.SPC_SPECIES, sep=":"), 
         paste("Study Area",res2$ASA_STUDY_AREA, sep=":"))
  })
  
  output$plotH_info<-renderPrint({plotH_sel()})

  output$filterPlot<- renderPlot({
    cm<-summaryScore()
    p<-ggplot (data=cm, aes(x=Impact_Score, y=Likelihood_Score))+
      geom_point(color="gold", size=5)+
      geom_hline(yintercept=c(1.66,2.33), linetype="dashed", color="grey")+
      geom_vline(xintercept=c(1.66,2.33), linetype="dashed", color="grey")+
      ggtitle("Plot of Likelihood of Species Invasion by Risk of Impact Score")+
      xlab("Impact Score")+
      ylab("Likelihood Score")+
      theme_bw()
    if(input$comp=="yes"){p<-p +
      geom_point(data=CMISTdata,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)}
    if(input$filters=="region"){
      filterData<-CMISTdata%>%
        filter(input$filterRegion)
      p<-p +
        geom_point(data=filterData,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)
    }
    if(input$filters=="species"){
      filterData<-CMISTdata%>%
        filter(input$filterSpecies)
      p<-p +
        geom_point(data=filterData,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)
    }
    if(input$filters=="both"){
      filterData<-CMISTdata%>%
        filter(input$filterRegion$filterSpecies)
      p<-p +
        geom_point(data=filterData,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)
    }

     p
  })

  selCmist<-eventReactive(input$plot_hover,{
    res<-nearPoints(CMISTdata, input$plot_hover, xvar ="ASU_RAW_IMPACT_INVASION", yvar ="ASU_RAW_LIKELIHOOD_INVASION", allRows = FALSE)
    list(paste("CMIST Score",res$ASU_ADJ_RISK_SCORE, sep=":"), 
         paste("Species",res$SPC_GENUS.SPC_SPECIES, sep=":"), 
         paste("Study Area",res$ASA_STUDY_AREA, sep=":"))
  })
  
  output$plot_hoverinfo<- renderPrint({selCmist()})
  
  output$cmist_score<-renderPrint({
    req(input$plot.click)
    text.cm<-summaryScore()
    test.cm.score<-text.cm$CMIST_Score
  })
  
  output$table1<- renderTable({summaryScore()})
  
  output$table2<- renderTable({summaryPrep()})
  
  
  output$table3<-renderTable({summaryValue()})
  
  output$table4<-renderTable({summaryRef()})
  
  
  output$downloadData<- downloadHandler(
    
    filename=function(){
      
      paste(input$downloadName, ".xlsx", sep="")
    },
   
    content=
      function(file){
        
        wb<- write.xlsx(summaryPrep(), file, sheetName="Pre_Assessment_Info")
          addWorksheet(wb, "CMIST_Data")
          addWorksheet(wb, "References")
          addWorksheet(wb, "CMIST_Score")

          writeData(wb, "Pre_Assessment_Info", summaryPrep())
          writeData(wb, "CMIST_Data", summaryValue())
          writeData(wb, "References", summaryRef())
          writeData(wb, "CMIST_Score", summaryScore())


          saveWorkbook(wb, file, overwrite = TRUE)

        }
  
  )
  
  output$downloadPDF<- downloadHandler(
    filename= function(){paste(input$downloadName, '.pdf', sep='')},

    content=
      function(file){
        
        cairo_pdf(filename=file,
                  width = 18, height = 10, pointsize = 12, family = "sans", bg = "transparent",
                  antialias = "subpixel",fallback_resolution = 300)
        #plot(cm_plot)
        #plot(p)
        
        #plot(summaryPrep)
        
# 
#         prep<-summaryPrep()
#         value<-summaryValue()
#         ref<-summaryRef()
#         score<-summaryScore()
# 
        cm<-summaryScore()
        cm_plot<-
          ggplot(data=CMISTdata, aes(x=Taxonomic_Group, y=ASU_ADJ_RISK_SCORE, fill=Taxonomic_Group))+
          geom_boxplot(show.legend = FALSE)+
          scale_fill_viridis(discrete=TRUE, alpha=0.6)+
          geom_jitter(aes(shape=Taxonomic_Group),size=2, alpha=0.9, show.legend = FALSE)+
          ggtitle("CMIST Database Risk Scores by Taxonomic Group")+
          xlab("Taxonomic Group")+
          ylab("Adjusted CMIST Risk Score")+
          #theme(legend.position = "none")+
          theme_bw()
        plot(cm_plot)

        p<-ggplot (data=cm, aes(x=Impact_Score, y=Likelihood_Score))+
          geom_point(color="gold", size=5)+
          geom_hline(yintercept=c(1.66,2.33), linetype="dashed", color="grey")+
          geom_vline(xintercept=c(1.66,2.33), linetype="dashed", color="grey")+
          ggtitle("Plot of Likelihood of Species Invasion by Risk of Impact Score")+
          xlab("Impact Score")+
          ylab("Likelihood Score")+
          theme_bw()
        if(input$comp=="yes"){p<-p +
          geom_point(data=CMISTdata,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)}
        if(input$filters=="region"){
          filterData<-CMISTdata%>%
            filter(input$filterRegion)
          p<-p +
            geom_point(data=filterData,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)
        }
        if(input$filters=="species"){
          filterData<-CMISTdata%>%
            filter(input$filterSpecies)
          p<-p +
            geom_point(data=filterData,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)
        }
        if(input$filters=="both"){
          filterData<-CMISTdata%>%
            filter(input$filterRegion$filterSpecies)
          p<-p +
            geom_point(data=filterData,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)
        }

        plot(p)

        dev.off()
      },
contentType = "application/pdf"
  )
  
  
  
  output$leafletmap<- renderLeaflet({
     
   lf<- Canada_map%>%
      addTiles()%>%
      addPolygons(data=mist_sf_map,
                  weight=2,
                  col='blue',
                  fillColor = mist_sf_map$ASA_STUDY_AREA,
                  # stroke = FALSE,
                  fillOpacity = 0.005,
                  group="CMIST Regions"
                  )%>%
      addLayersControl(overlayGroups = "CMIST Regions")
   
   if(input$m2=="yes"){
     
     cmist.occ<- occ(query= input$m1,
                     from=c('gbif', 'inat', 'obis', 'vertnet'),
                     geometry = mist_sf$geometry,
                     has_coords = TRUE)
     
     cmist.oc2<- occ2df(cmist.occ)%>%
       mutate(longitude=as.numeric(longitude),
              latitude=as.numeric(latitude))%>%
       dedup()
     
     lf<-lf%>%
     addCircleMarkers(data=cmist.oc2, lng = ~longitude, lat= ~latitude, popup = ~as.character(prov), label=~as.character(prov))
     }
  lf
  
  })
  
  output$table5<- DT::renderDataTable({
    DT::datatable(CMISTdata)
  })
  
}


shinyApp(ui, server)


