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
#if(!require("RColourBrewer")) install.packages("RColourBrewer")
if(!require("leaflet.opacity")) install.packages("leaflet.opacity")
if(!require("mailR")) install.packages("mailR")
if(!require("spocc")) install.packages("spocc")
if(!require("robis")) install.packages("robis")
if(!require("rgbif")) install.packages("rgbif")
if(!require("rnaturalearth")) install.packages("rnaturalearth")
if(!require("scrubr")) install.packages("scrubr")
if(!require("mapr")) install.packages("mapr")
if(!require("viridis")) install.packages("viridis")
if(!require("rgdal")) install.packages("rgdal")
if(!require("maptools")) install.packages("maptools")
if(!require("rgeos")) install.packages("rgeos")
if(!require("meowR")) devtools::install_github("https://github.com/jebyrnes/meowR")


proj <- "+proj=longlat +datum=WGS84"

#cmist_database<-read.csv("data/cmist_data.csv")

CMISTdata <- get_CMIST_database()

#accesses marine ecoregions of the world (MEOW) files through the meowR package

data("regions")
meow_regions<- regions %>% st_as_sf()


#setting up the regions for selection in the pre-assessment info

prov<-c("British Columbia",
        "Alberta",
        "Saskatchewan",
        "Ontario",
        "Quebec",
        "New Brunswick",
        "Nova Scotia",
        "Prince Edward Island",
        "Newfoundland & Labrador",
        "Nunavut",
        "Northwest Territories",
        "Yukon")
meowR<- c("Aleutian Islands",
          "Baffin Bay-Davis Strait",
          "Beaufort-Amundsen-Viscount Melville-Queen Maud",
          "Beaufort Sea-continental coast and shelf",
          "Chukchi Sea",
          "Eastern Bering Sea",
          "Gulf of Alaska",
          "Gulf of Maine/Bay of Fundy",
          "Gulf of St. Lawerence-Eastern Scotian Shelf",
          "High Arctic Archipelago",
          "Hudson Complex",
          "Lancaster Sound",
          "North American Pacific Fijordland",
          "Northern Grand Banks-Southern Labrador",
          "North Greenland",
          "Northern Labrador",
          "Oregon, Washington, Vancouver Coast and Shelf",
          "Puget Trough/Georgia Basin",
          "Scotian Shelf",
          "Southern Grand Banks-South Newfoundland",
          "West Greenland Shelf")

#setting up CMIST database polygons

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
            st_polygon() %>%
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


ui<-
  #shinyUI(
  function(request){
  navbarPage(
  title = "Canadian Marine Invasive Screening Tool (CMIST) App",
  
  
  tabPanel("CMIST Home Page",
          
           
           tabsetPanel(
             tabPanel("About CMIST",
                      h4("Note: Before you start using this app, please read the Manual, Using the App, and Saving Your Work tabs."),
                      br(),
                      h3("Highlights"),
                      h5(".	CMIST was designed for and tested with marine invertebrates, but its theoretical basis and general questions make it suitable for other organisms."),
                      h5(".	Screening-level risk assessments in CMIST are designed to be completed in 1-2 days including collection of information and answering questions. "),
                      h5(".	Assessors should have good general knowledge on invasive species and, ideally, the assessment area and species being assessed."),
                      h5(".	Prior to assessment, background information on the selected species that pertains to CMIST questions should be collated from available resources and quickly reviewed. "),
                      h5(".	Assessors should always use their best judgement when interpreting questions, answers, and guidance for the selected species, which will likely not have species or taxon-specific examples."),
                      h5(".	Assessors should read the 'CMIST Manual tab' before completing any assessments to ensure consistency with guidance."),
                      
                      br(),
                      
                      h3("Introduction"),
                      h5("CMIST (Canadian Marine Invasive Screening Tool) assessments provide information on the likelihood and impact of invasions of different species in different assessment areas with scores adjusted for assessor uncertainty. Together, these assessments act as a new resource of collated information on invasive species that can assist researchers and managers dealing with aquatic invasive species (AIS) and increase the efficiency of decision-making. The growing database of CMIST assessments is as an important repository for information on invasive species for students, researchers, and citizens around the world."),
                      h5("The Canadian Marine Invasive Screening Tool (CMIST) is a peer-reviewed screening-level risk assessment tool for marine invasive species (Drolet et al. 2016). It is a short questionnaire that follows the invasion process from arrival to impact and is designed so an informed assessor can evaluate one species in an assessment area in approximately one day using easily accessible information from internet databases, primary literature, and grey literature. Species can be those with an invasion history in an area or those that are candidates for future invasions."),
                      h5("CMIST is score-based and incorporates both likelihood and impact of invasion as well as uncertainty. Questions are general to make CMIST broadly applicable to different taxa, different assessment areas, and different project goals. To date, CMIST has been tested with molluscs, tunicates, crustaceans, and polychaetes introduced or at risk of introduction to three Canadian marine ecoregions (DFO 2009). CMIST has also been successfully applied to non-indigenous freshwater fish in British Columbia with adapted guidelines (T. Therriault, pers. comm.). Upon completion, CMIST produces a risk score adjusted for the assessor's uncertainty which, combined with information collected during the assessment, can be used to assist in management decisions. For example, in 2015, CMIST assessments were used to identify high risk invaders in three Canadian marine ecoregions (DFO 2016). Completed assessments submitted from assessors are reviewed for accuracy and completeness then housed online in the searchable CMIST database."),
                      h5("This manual contains best practices for using CMIST and should be read in full before proceeding with an assessment to achieve consistent results. For more in-depth background information, refer to Drolet et al. (2016)."),
                      
                      br(),
                      
                      h3("Pre-assessment Preparation"),
                      h4("Species"),
                      h5("Although CMIST was designed for and tested with marine invertebrates, its theoretical basis and general questions makes it suitable for other organisms. Assessors should always use their best judgement when interpreting questions, answers, and guidance for the selected species, which will likely not have species or taxon-specific examples."),
                      h5("Prior to assessment, background information on the selected species that pertains to CMIST questions should be collated from available resources and quickly reviewed. If there is little known about the species, information on other species in the genus (or higher taxonomic level) should be obtained and used as complementary resources. A lack of species-specific information would be expected to increase uncertainty."),
                      h4("Assessment area"),
                      h5("The assessment area can be any size or scope but must be defined since most CMIST questions use this assessment area as a context for answers (see 'Question Consideration' section under the 'CMIST Assessment' tab for questions, answers, and guidance).  It is up to the assessor to determine (and document) the scale used for their assessment."),
                      h5("Prior to assessment, background information should be collated on the assessment area from available resources and quickly reviewed. Pertinent information includes both physical characteristics (e.g., bottom types, habitats, temperature range, and salinity range) and biological components (e.g., species at risk, aquaculture species, commercially fished species, and species of special interest)."),
                      h4("Assessor"),
                      h5("All CMIST questions are semi-quantitative and require interpretation and judgement to answer based on available information as well as the assessor's expert opinion. An assessor should therefore have good general knowledge on invasive species and, ideally, the assessment area and species being assessed. A less-knowledgeable assessor would be expected to have higher uncertainty, especially for species with little available information.
                      Prior to assessment, assessors should review background information on the species and assessment area and familiarize themselves with CMIST questions, answers, guidance, and glossary."),
                      
                      h4("Literature Cited"),
                      h5("DFO (2009) Development of a framework and principles for the biogeographic classification of Canadian marine areas. DFO Canadian Science Advisory Secretariat Science Advisory Report 2009/056, p 17"),
                      h5("DFO (2016) Proceedings of the national science advisory process to peer review the marine screening-level risk assessment tool for aquatic non-indigenous species; February 4-6, 2015. DFO Can. Sci. Advis. Sec. Proceed. Ser. 2016/033"),
                      h5("Drolet D, DiBacco C, Locke A, McKenzie CH, McKindsey CW, Moore AM, Webb JL, Therriault TW (2016) Evaluation of a new screening-level risk assessment tool applied to non-indigenous marine invertebrates in Canadian coastal waters. Biological Invasions. 18(1): 279-294 DOI:10.1007/s10530-015-1008-y")
                      ),
             
             tabPanel("CMIST Manual",
                      #h3("Using CMIST"),
                      h3("1) Pre-Assessment Information"),
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
                      h5("Drolet D, DiBacco C, Locke A, McKenzie CH, McKindsey CW, Moore AM, Webb JL, Therriault TW (2016) Evaluation of a new screening-level risk assessment tool applied to non-indigenous marine invertebrates in Canadian coastal waters. Biological Invasions. 18(1): 279-294 DOI:10.1007/s10530-015-1008-y")
                      ),
             
             tabPanel("Using the App",
                      h3("The Information Tabs"),
                      h4("The Information tabs are the Pre-Assessment, CMIST Assessment, and Reference tabs. Fill-in the appropriate information for each tab."),
                      h4("Note: Some boxes have the * symbol throughout the Pre-Assessment tab because these are boxes that require information for the assessment to be considered complete. Also, the box for the study area region can be specified as either provincial boundaries or Marine Ecosystem Regions of the World (MEOW). There is a map in the Pre-Asessment tab to help decide which MEOW region best describes your marine study area."),
                      h3("The Summary Tab"),
                      h4("The Summary Tab has options to download the content from the Information tabs into a csv file. This csv file can be attached to an email (generated when you click the email to CMIST database box)."),
                      h4("IMPORTANT: It is critical that we all contribute to the CMIST database. The more CMIST assessments submited to the database, the more data points will area in the plots on the Summary Tab. These plots can be used to compare CMIST scores of various species within a region or one species across multiple regions or both!"),
                      h3("The Explore Tab"),
                      h4("This tab has a map that has a datalayer that can be clicked on-and-off that shows CMIST assessment regions from previous assessments. Also, it is possible to access species occurrence records and plot those in the map."),
                      h4("This tab also has a sub-tab that shows all the CMIST database taken from the CMIST website (https://www.bio.gc.ca/science/monitoring-monitorage/cmist/index-en.php).")
                      ),
             
             tabPanel("Saving Your Work",
                      h3("Saving Your Work:"), 
                      h4("If your CMIST assessment is incomplete or you feel that you need to leave the app inactive for more than 5 mins at a time, we recommend that you save your work."),
                      h4("There are two options for saving your work:"),
                      h4("Option 1: Bookmark your work using the button below.
                  This is the best option for short-term saving and for individuals that do not wish to have a file downloaded to their computer.
                  The information entered into the app will be encoded into a URL.
                  You will need to have this URL on-hand to reaccess the app in the state which you have saved it.
                  You will need a new URL each time you provide new input data."),
                      # h4("Option 2: Save a copy to your desktop. If you feel that you will be away/inactive in your assessment a little longer (e.g. more than 1-day), we recommend that you save a copy to you computer.
                      # The app has a file path pre-loaded (~/user_inputs.csv), which will automatically save a csv file to the DOcument folder on your computer. Once you have completed this step, you can close the app or leave it as long as you like inactive. By clicking the 'Load inputs' button the app will automatically access the 'user_inputs.csv' file from your computer and willreload your saved work.
                      #    By keeping the file path (~/user_inputs.csv) the same, you can continuously resave to the same file/overwrite the old file without creating additional csv files."),
                      h4("Option 2: On the 'Summary tab' you can download a csv copy of your work completed to date.
                This is the only accepted file to be submitted to the CMIST database because it will contain extra tabs and will seperate out the information into easy to understand tables.
                The benefit of using the 'Bookmark' function or the 'Load or Save input' function is that the user will not need to re-enter information.
                Using the 'Download Results' function from the 'Summary tab' will require that the user either merge the multiple draft copies of csv files saved to the user's computer or copying and pasting the data from the csv back into the app."),
                      
                      br(),
                      bookmarkButton(),
                      
               # h4("If you expect to leave this page inactive for longer than 5 mins or close the app at any point, please save your data"),
               # h4("To save your data using the 'Load or Save input' functions, please enter the file path where you would like the data to save in the 'Inputs Location' box below. 
               #    The 'Save inputs' button will save the information as a csv on your computer. 
               #    To reaccess the saved information, enter the file path and hit 'Load input.'"),
               # textInput(inputId = 'inputsLocation', label = 'Inputs Location', value = "~/user_inputs.csv"),
               # actionButton('load_inputs', 'Load inputs'),
               # actionButton('save_inputs', 'Save inputs'),
               # br(),
               # br()
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
                       
                       h3("invasive"),
                       h5("a nonindigenous species that spreads rapidly, causing environmental or economic damage (definition often used by managers)"),
                       
                       h3("other species"),
                       h5("any species that is not the subject of the assessment"),
                       
                       h3("population growth*"),
                       h5("the change in population size with time as a net result of natality, mortality, immigration, and emigration"),
                       
                       h3("species"),
                       h5("the subject of the assessment"),
                       
                       h3("suitable habitat"),
                       h5("the portion of the habitat zone within the assessment area in which the species could live"))
              
  )),
  
  #Pre-assessment Questions for identifying contact person, assessment area, and species information

  tabPanel("Pre-Assessment Information",
           
           h4("Please fill in the pre-assessment information. This information will be combined with the CMIST Assessment Score information and the Reference List into a PDF, a copy of which will be sent to DFO for peer-review and the other copy sent to the assessors email address."),
           h4("Note: The CMIST Score, the assessment area, region, country, and location (i.e the latitude and longitude), and the species's information from this assessment report will become part of the CMIST database. Through collecting these valuable pieces of information from multiple CMIST assessments, we are able to provide CMIST app users with the ability to make comparisons between regions or between species within one particular region, within a taxon, or throughout Canada. By using the CMIST App, you are providing your informed consent that this information will be made public. Please note that the assessor's information will not become part of any public database. Only the DFO peer-reviewers and the assessor will have access to the assessor's personal information. The DFO peer-review staff need access to the assessor's information in case further information or clarification on assessment information is needed. "),
          
           
               h3("Assessor Information"),
           fluidRow(
               column(6, textInput("A1", "*Title: Project Title (e.g. Cipangopaludina chinensis Assessment-Maritime Region)")),
               column(6, textInput("A2", "*Description: Please enter a short project or species description")),
               column(6, textInput("A3", "*Assessor Name: Your Name (first and last)")),
               column(6, textInput("A4", "*Affilitation: Please enter your affiliation (may enter more than one)")),
               column(6, textInput("A5", "Address: Please enter your address")),
               column(6, textInput("A6", "Phone: Please enter a contact phone number")),
               column(6, textInput("A7","*Email Adress: Please enter a contact email"))
           ),
               
               h3("Species Information"),
            fluidRow(
              h4("Note: EIther Species AphiaID (WoRMS) or Species TSN (ITIS) is required, not both. However, both may be included if available."),
              column(4, textInput("A8", "*Species AphiaID (WoRMS)")),
              column(4,textInput("A9", "*Species TSN (ITIS)")),
              column(4,selectInput("A10", "Taxon: General taxon or group", c("Ascidians", "Bryozoans", "Cnidarians", "Crustaceans", "Echinoderms", "Fish", "Molluscs", "Plants and Algae", "Sponges", "Worms", "Other"))),
              column(4,textInput("A11", "Species Kingdom")),
              column(4,textInput("A12", "Species Phylum")),
              column(4,textInput("A13", "Species Subphylum")),
              column(4,textInput("A14", "Species Class")),
              column(4,textInput("A15", "Species Order")),
              column(4,textInput("A16", "Species Family")),
              column(4,textInput("A17", "*Species Genus")),
              column(4,textInput("A18", "*Species")),
              column(4,selectInput("A19", "*Please select the appropriate ecosystem (either Freshwater, Brackish, or Marine)", c("Freshwater", "Marine", "Brackish"))),
              column(4, textInput("A29", "Body of Water (e.g. Atlantic, Pacific, Arctic)")),
              column(4,textInput("A20", "*Common names for the species (e.g. Chinese mystery snail)")),
              column(4, textInput("A30", "Additional common name #2")),
              column(4, textInput("A31", "Additional common name #3")),
              column(4,textInput("A21", "Additional Notes: Additional taxonomic notes here"))
               ),
               
               h3("Study Area Information"),
            fluidRow(  
              h4("Click on the blue regions on the map to see the name of the Marine Ecoregion of the WOrld (MEOW)"),
              leafletOutput("meow.region.map"),
              column(4,radioButtons("regionSelect", "*Region: Study Region or Province (Marine Ecoregions of the World (MEOW) for marine species and Provincial boundaries for freshwater species)",
                                    choices=c("Province", "MEOW"))),
              column(4,textInput("A22", "*Area:Study Area Name")),
              column(4,selectInput("A23", "*Select Region", choices=prov )),
              column(4,textInput("A24", "*Country: Country of Study Area")),
              column(6,textInput("A25", "*Latitude 1: Latitude (northern boundary) in decimal degrees")),
              column(6,textInput("A26", "*Latitude 2: Latitude (southern boundary) in decimal degrees")),
              column(6,textInput("A27", "*Longitude 1: Longitude (western boundary) in decimal degrees")),
              column(6,textInput("A28", "*Longitude 2: Longitude (eastern boundary) in decimal degrees"))
            )
  ),
  
  #CMIST assessment Questions w/ uncertainty and rational. Also page has instructions for the considerations for each question. 
  
  tabPanel("CMIST Assessment",
           h4("Note: There is no 'submit' button on this page. Any information entered into this tab will automatically be entered into a table in the 'Summary tab.' The CMIST Score will be calculted automatically as well once all 17 questions and uncertainties are answered in this tab, which can be seen in the 'Summary tab.'"),
            sidebarLayout(
              
              sidebarPanel(
               selectInput("Q1",
                           "1.(a) Is the species established in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U1",
                           "1.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R1", "1.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species distribution data indicate species is widely dispersed in assessment area (ref).", resize = "both"),
               selectInput("Q2",
                           "2.(a) How frequently and in what numbers is the species expected to arrive into the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U2",
                           "2.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R2", "2.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species distribution data and habitat suitability models indicate low arrivals (ref).", resize = "both"),
               selectInput("Q3",
                           "3.(a) How much of the assessment area offers suitable habitat for the species?",
                           c(NA,1,2,3)),
               selectInput("U3",
                           "3.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R3", "3.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Habitat suitability models indicate abundant suitable habitat (ref).", resize = "both"),
               selectInput("Q4",
                           "4.(a) How much of the assessment area offers suitable environmental conditions for the species to survive?",
                           c(NA,1,2,3)),
               selectInput("U4",
                           "4.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R4", "4.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Ecological threshold testing shows that species is highly adaptable (ref).", resize = "both"),
               selectInput("Q5",
                           "5.(a) Are the species' reproductive requirements available in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U5",
                           "5.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R5", "5.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species reproductive needs are/could be met within assessment area (ref).", resize = "both"),
               selectInput("Q6",
                           "6.(a) To what extent could natural control agents slow the species' population growth in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U6",
                           "6.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R6", "6.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. There are no known control methods that are affective for this species (ref).", resize = "both"),
               selectInput("Q7",
                           "7.(a) What is the range of the species' potential natural dispersal in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U7",
                           "7.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R7", "7.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species is like to disperse naturally within assessment area (ref).", resize = "both"),
               selectInput("Q8",
                           "8.(a) What is the range of the species' potential dispersal in the assessment area from anthropogenic mechanisms?",
                           c(NA,1,2,3)),
               selectInput("U8",
                           "8.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R8", "8.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Anthropogenic recreational water use will likely lead to furtehr species dispersal (ref).", resize = "both"),
               selectInput("Q9",
                           "9.(a) What level of impact could the species have on population growth of other species in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U9",
                           "9.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R9", "9.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species will likely outcompete native species for available resources (ref).", resize = "both"),
               selectInput("Q10",
                           "10.(a) What level of impact could the species have on communities in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U10",
                           "10.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R10", "10.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species will likely alter community structure in a top-down approach  (ref).", resize = "both"),
               selectInput("Q11",
                           "11.(a) What level of impact could the species have on habitat in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U11",
                           "11.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R11", "11.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species is documented as an ecosystem engineer and will likely alter invaded habitat (ref).", resize = "both"),
               selectInput("Q12",
                           "12.(a) What level of impact could the species have on ecosystem function in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U12",
                           "12.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R12", "12.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species presence will likely decrease aquaculture yields (ref).", resize = "both"),
               selectInput("Q13",
                           "13.(a) What level of impact could the species' associated diseases, parasites, or travellers have on other species in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U13",
                           "13.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R13", "13.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species is a known host of multiple parasites (ref).", resize = "both"),
               selectInput("Q14",
                           "14.(a) What level of genetic impact could the species have on other species in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U14",
                           "14.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R14", "14.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species may hybridize with other indigenous species (ref).", resize = "both"),
               selectInput("Q15",
                           "15.(a) What level of impact could the species have on at-risk or depleted species in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U15",
                           "15.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R15", "15.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species will impact species-at-risk life cycles, especially juvenile stages (ref).", resize = "both"),
               selectInput("Q16",
                           "16.(a) What level of impact could the species have on aquaculture and fished species in the assessment area?",
                           c(NA,1,2,3)),
               selectInput("U16",
                           "16.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R16", "16.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species will prey on mussel spat (ref).", resize = "both"),
               selectInput("Q17",
                           "17.(a) Is the species known or generally considered to be invasive anywhere in the world?",
                           c(NA,1,2,3)),
               selectInput("U17",
                           "17.(b) What is the certainty (1=low certainty, 2=some certainty, 3=high certainty)",
                           c(NA,1,2,3)),
               textAreaInput("R17", "17.(c) Rationale:", width = '100%', height = '100%', placeholder = "E.g. Species is considered invasive in USA, Mexico, and Canada (ref).", resize = "both")
             
              ),
              mainPanel(
                h2("Question Considerations"),
                
                h3("---"),
                h4("1. This question is meant to differentiate species that are not present in the assessment area (1) from species that are established in the assessment area (3)."),
                h4("Species that are present in the assessment area but not established would score 2."),
                h5("1 - No"),
                h5("2 - Observed but not reported as established"),
                h5("3 - Yes"),
                
                
                
                h3("---"),
                h4("2. Consider initial arrival into the assessment area by primary vectors only."),
                h4("Do not consider secondary spread (anthropogenic or natural) within the assessment area by species that are already established."),
                h4("Consider all primary anthropogenic and natural vectors for transport into the assessment area (e.g., ballast water, hull fouling, aquaculture, rafting, and natural dispersal from outside the assessment area). "),
                h5("1 - Infrequently in low numbers"),
                h5("2 - Frequently in low numbers OR infrequently in high numbers"),
                h5("3 - Frequently in high numbers"),
                
                
                
                h3("---"),
                h4("3. This question is meant to differentiate habitat specialists (1) from habitat generalists (3)."),
                h4("Species that fall between these extremes would score 2."),
                h4("First consider the species' broad habitat zone (e.g., intertidal, subtidal, benthic, and pelagic)."),
                h4("Then consider the proportion of that zone within the assessment area that offers suitable habitat for the species. Consider suitable anthropogenic habitat (e.g., docks and aquaculture sites) as well as natural habitat."),
                h5("1 - Negligible proportion of the assessment area"),
                h5("2 - Moderate proportion of the assessment area"),
                h5("3 - Most of the assessment area"),
                
                
                
                h3("---"),
                h4("4. This question is meant to differentiate species with very poor environmental match for survival (1) from those with a very good environmental match (3)."),
                h4("Species that fall between these extremes would score 2."),
                h4("Consider environmental conditions (e.g., temperature, salinity, and turbidity) in its suitable habitat (see Question 3)."),
                h4("Consider the most tolerant life stage at any time of year.  Consider survival only, not reproduction."),
                h5("1 - Negligible proportion of the assessment area"),
                h5("2 - Moderate proportion of the assessment area"),
                h5("3 - Most of the assessment area"),
                
                
                
                h3("---"),
                h4("5. This question is meant to differentiate species that face severe constraints in reproduction in the assessment area and are very unlikely to reproduce in a typical year (1) from those that face few constraints in reproduction in the assessment area and are very likely to reproduce every year (3)."),
                h4("Species that fall between these extremes would score 2."),
                h4("Consider any constraint (e.g., temperature, salinity, and stage-specific habitat) in the species' ontogenetic development (e.g., spawning, fertilization, and propagule dispersal) that may affect its ability to reproduce successfully in otherwise suitable habitat (see Question 3)."),
                h5("1 - Almost never"),
                h5("2 - Sometimes"),
                h5("3 - Almost always"),
                
                
                
                h3("---"),
                h4("6. This question is meant to differentiate species with known, effective natural control agents in the assessment area (1) from those with no known, effective natural control agents in the assessment area (3)."),
                h4("Species with known-but not necessarily effective-natural control agents in the assessment area would score 2."),
                h4("Consider presence and incidence of known natural control agents (e.g., predators, competitors, disease, and disturbance) in the species' suitable habitat (see Question 3) and to what extent they could slow the species' population growth."),
                h5("1 - Likely to severely restrict population growth"),
                h5("2 - Could slow population growth "),
                h5("3 - Unlikely to slow population growth"),
                
                
                h3("---"),
                h4("7. This question is meant to differentiate species that face severe constraints in natural dispersal (e.g., short larval planktonic stage and sessile adults) (1) from those that face few constraints (e.g., long larval planktonic stage, motile adults) (3)."),
                h4("Species that fall between these extremes would score 2."),
                h4("Consider the natural dispersal vectors (e.g., currents, rafting, and migration) for all life stages. Consider any constraints on natural dispersal vectors in the assessment area.
"),
                h5("1 - Very limited range"),
                h5("2 - Moderate range"),
                h5("3 - Wide range"),
                
                
                
                h3("---"),
                h4("8. This question is meant to differentiate species likely to have little to no contact with anthropogenic mechanisms of dispersal in the assessment area (1) from those that are likely to have contact with anthropogenic mechanisms that could disperse them over large distances (e.g., among embayments) (3)."),
                h4("Species that have contact with anthropogenic mechanisms that could disperse them over short distances (e.g., among sites in an embayment) would score a 2."),
                h4("Consider anthropogenic dispersal vectors (e.g., ballast, hull fouling, and aquaculture) for all life stages.
"),
                h5("1 - Very limited range"),
                h5("2 - Moderate range"),
                h5("3 - Wide range"),
                
                
                
                h3("---"),
                h4("9. Only consider impacts in the species' suitable habitat (see Question 3)."),
                h4("Consider positive and negative impacts (i.e. population increase or decrease)."),
                h4("Consider impacts to indigenous and non-indigenous populations. Consider ecological impacts on aquaculture and commercially fished species, not economic impacts on the industry itself.
"),
                h5("1 - Low or no impact"),
                h5("2 - High impact in few areas OR moderate impact in many areas"),
                h5("3 - High impact in many areas"),
                
                
                h3("---"),
                h4("10. Only consider impacts in the species' suitable habitat (see Question 3)."),
                h4("Consider positive and negative impacts (i.e. population increase or decrease)."),
                h4("Consider impacts to indigenous and non-indigenous populations. 
"),
                h5("1 - Low or no impact"),
                h5("2 - High impact in few areas OR moderate impact in many areas"),
                h5("3 - High impact in many areas"),
                
                
                h3("---"),
                h4("11. Only consider impacts in the species' suitable habitat (see Question 3) and not on associated communities."),
                h4("Consider habitat engineering (e.g., reef-building organisms) and habitat destruction (e.g., bioturbating organisms).
"),
                h5("1 - Low or no impact"),
                h5("2 - High impact in few areas OR moderate impact in many areas"),
                h5("3 - High impact in many areas"),
                
                
                h3("---"),
                h4("12. Only consider impacts in the species' suitable habitat (see Question 3)."),
                h4("Consider changes (positive or negative) to the physical, chemical, and biological processes that would normally maintain the ecosystem.
"),
                h5("1 - Low or no impact"),
                h5("2 - High impact in few areas OR moderate impact in many areas"),
                h5("3 - High impact in many areas"),
                
                
                h3("---"),
                h4("13. Only consider impacts in the species' suitable habitat (see Question 3)."),
                h5("1 - Low or no impact"),
                h5("2 - High impact in few areas OR moderate impact in many areas"),
                h5("3 - High impact in many areas"),
                
                
                h3("---"),
                h4("14. Only consider impacts in the species' suitable habitat (see Question 3)."),
                h4("Consider indigenous and non-indigenous species in the assessment area."),
                h4("Consider hybridization (among species hybridization and supplementation of genetic material between strains or varieties of a species) as well as other genetic impacts.
"),
                h5("1 - Low or no impact"),
                h5("2 - High impact in few areas OR moderate impact in many areas"),
                h5("3 - High impact in many areas"),
                
                
                h3("---"),
                h4("15. Consider all possible impacts on species in the assessment area that are depleted, of extra value, or recognized as being at risk.
"),
                h6("1 - Low or no impact"),
                h5("2 - High impact in few areas OR moderate impact in many areas"),
                h5("3 - High impact in many areas"),
                
                
                h3("---"),
                h4("16. Consider ecological impacts on aquaculture and fished species (e.g., from commercial, recreational, and indigenous fisheries) in aquaculture operations and the wild, but not economic impacts on the industry itself.
"),
                h5("1 - Low or no impact"),
                h5("2 - High impact in few areas OR moderate impact in many areas"),
                h5("3 - High impact in many areas"),
                
                
                h3("---"),
                h4("17. This question is meant to differentiate species that are not invasive and not likely to be invasive based on their life history traits (1) from those that are known or generally considered to be invasive (3)."),
                h4("An introduced species that is not generally considered to be invasive but that has traits related to invasiveness would score a 2."),
                h4("An introduced species can be non-invasive.
"),
                h5("1 - No"),
                h5("2 - No, but has traits related to invasiveness"),
                h5("3 - Yes"),
                
                
                h2("Rationale Guidance"),
                h4("Include key information used to determine risk and uncertainty scores."),
                h4("Include direct quotations from literature, paraphrased summaries, and statements of expert opinion."),
                h4("In cases where the answer is one of two options (e.g., high impact in few areas OR moderate impact in many areas), indicate which option was favoured and why."),
                h4("Include annotated references in the text (e.g., Drolet et al. 2016; EOL; personal observation). Please include full citations in the Reference section.")
              )
            )
  ),
  
  #Reference tab. Anything can be added into the box. 
  
  tabPanel("References",
           h4("Please list all references used in the rationales."),
           h4("Links will be assumed to have been accessed on the date of the CMIST assessment but will not be maintained. References can include websites (please include the URL), publications, reports, theses, etc. Please include the URL or doi link for each reference and provide full bibliography style references."),
           h4("Note: The boxes provide below are for ALL types of references. App users can provide full bibliography-style references or links to applicable websites or a few lines of text explaining a personal experience."),
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
               column(4,textInput("C30", "Reference:")),
               column(4,textInput("C31", "Reference:")),
               column(4,textInput("C32", "Reference:")),
               column(4,textInput("C33", "Reference:")),
               column(4,textInput("C34", "Reference:")),
               column(4,textInput("C35", "Reference:")),
               column(4,textInput("C36", "Reference:")),
               column(4,textInput("C37", "Reference:")),
               column(4,textInput("C38", "Reference:")),
               column(4,textInput("C39", "Reference:")),
               column(4,textInput("C40", "Reference:")),
               column(4,textInput("C41", "Reference:")),
               column(4,textInput("C42", "Reference:")),
               column(4,textInput("C43", "Reference:")),
               column(4,textInput("C44", "Reference:")),
               column(4,textInput("C45", "Reference:")),
               column(4,textInput("C46", "Reference:")),
               column(4,textInput("C47", "Reference:")),
               column(4,textInput("C48", "Reference:")),
               column(4,textInput("C49", "Reference:")),
               column(4,textInput("C50", "Reference:")),
               column(4,textInput("C51", "Reference:")),
               column(4,textInput("C52", "Reference:")),
               column(4,textInput("C53", "Reference:")),
               column(4,textInput("C54", "Reference:")),
               column(4,textInput("C55", "Reference:")),
               column(4,textInput("C56", "Reference:")),
               column(4,textInput("C57", "Reference:")),
               column(4,textInput("C58", "Reference:")),
               column(4,textInput("C59", "Reference:")),
               column(4,textInput("C60", "Reference:")),
               column(4,textInput("C61", "Reference:")),
               column(4,textInput("C62", "Reference:")),
               column(4,textInput("C63", "Reference:")),
               column(4,textInput("C64", "Reference:")),
               column(4,textInput("C65", "Reference:")),
               column(4,textInput("C66", "Reference:")),
               column(4,textInput("C67", "Reference:")),
               column(4,textInput("C68", "Reference:")),
               column(4,textInput("C69", "Reference:")),
               column(4,textInput("C70", "Reference:")),
               column(4,textInput("C71", "Reference:")),
               column(4,textInput("C72", "Reference:")),
               column(4,textInput("C73", "Reference:")),
               column(4,textInput("C74", "Reference:")),
               column(4,textInput("C75", "Reference:")),
               column(4,textInput("C76", "Reference:")),
               column(4,textInput("C77", "Reference:")),
               column(4,textInput("C78", "Reference:")),
               column(4,textInput("C79", "Reference:")),
               column(4,textInput("C80", "Reference:")),
               column(4,textInput("C81", "Reference:")),
               column(4,textInput("C82", "Reference:")),
               column(4,textInput("C83", "Reference:")),
               column(4,textInput("C84", "Reference:")),
               column(4,textInput("C85", "Reference:")),
               column(4,textInput("C86", "Reference:")),
               column(4,textInput("C87", "Reference:")),
               column(4,textInput("C88", "Reference:")),
               column(4,textInput("C89", "Reference:")),
               column(4,textInput("C90", "Reference:")),
               column(4,textInput("C91", "Reference:")),
               column(4,textInput("C92", "Reference:")),
               column(4,textInput("C93", "Reference:")),
               column(4,textInput("C94", "Reference:")),
               column(4,textInput("C95", "Reference:")),
               column(4,textInput("C96", "Reference:")),
               column(4,textInput("C97", "Reference:")),
               column(4,textInput("C98", "Reference:")),
               column(4,textInput("C99", "Reference:")),
               column(4,textInput("C100", "Reference:"))
             )
  ),
  
  #Summary Tab w/ filters, plots, and comparisons to the CMIST database, download report buttons, and summary tables. 
  
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
                           choices = unique (CMISTdata$Region)),
           
           h5("Download the results of your CMIST assessment including the pre-assessment information, risk assessment answers, references, and CMIST score:"),
           h6("The csv file will also have a 'For_database' tab that is meant for easy copying of information into the CMIST database by CMIST assessors."),
           textInput("downloadName", "Specify desired name of results file"),
               downloadButton("downloadData", "Download Results"),
           
           h4("Please submit your results to the CMIST Team to have the assessment included in the CMIST Database. Your results can help others!"),
           h5("Once you have downloaded your results, please attache the csv file to an email (click link below) and send to the CMIST Assessors to have your assessment peer-reviewed. "),
           
           a(
             actionButton("email", label="Send to CMIST Database",
                          icon = icon("envelope", lib="font-awesome")),
             href='mailto:DFO.CESDDataRequest-DSECDemandededonnes.MPO@dfo-mpo.gc.ca?
subject=CMIST%20Assessment&
body=Please%20make%20sure%20you%20have%20attached%20the%20xlsx%20file%20before%20sending'
           )
           # 
           # h5("You can also download a PDF with all the information seen on this page."),
           # downloadButton("downloadPDF", "Download PDF")
            
           ),
           
           
           mainPanel(
             h3("The plots seen below are based on data extracted from the CMIST database. Therefore, it is extremely important that everyone submit their CMIST assessments to the CMIST database, so that we can continue to grow these comparisons and offer more complex plots."),
             br(),
             h4("Click a point on the plot points below to see the species, study area, and CMIST score. Each taxonomic group is a different colour and the point data (i.e. the individual data from each assessment) is a different shape depending on the taxonomic group it belongs to."),
             plotOutput("cmist.plot", click =  clickOpts(id="plot_click1")),
             verbatimTextOutput("plotH_info"),
             br(),
             h4("Click a point on the the plot points below to see the species, study area, and CMIST score. The black dots are the individual assessments from the CMIST database, the yellow dot is the specific assessment for your current CMIST assessment."),
             h5("Note: You can filter the scatter plot by geographic region, species, or both by selecting options in the side bar."),
             plotOutput("filterPlot", hover = hoverOpts(id="plot_hover")),
             verbatimTextOutput("plot_hoverinfo"),
             br(),
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
                
 #Map and CMIST database tab. Use this tab to see the species distribution
 
 tabPanel("Explore the CMIST Database",
          h4("Use the text input box below to search public databases such as iNaturalist, GBiF, OBIS, and VertNet for species occurrence records."),
          h4("The interactive map has multiple layers that you can choose to turn on and off."),
          h4("The 'CMIST Database' tab contains a data table that shows all the data currently available in the CMIST Database."),
          textInput("m1", "Search public databases by writing the name of the species you want to see distribution data for!"),
          radioButtons("m2", "Check whether you want to see species distribution data or not", choices = c("Don't see species data"="no",
                                                                                                           "See species data"="yes")),
          tabsetPanel(
            tabPanel("Interactive Map",leafletOutput("leafletmap",height=800)),
            tabPanel("CMIST Database", DT::dataTableOutput("table5"))
          )
)
)
}
#)


server <-
  shinyServer(
  function(input, output, session) {
  
  
  
  #this generates a vector string of the pre-assessment info
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
                    A29=input$A29,
                    A20=input$A20,
                    A30=input$A30,
                    A31=input$A31,
                    A21=input$A21,
                    A22=input$A22,
                    A23=input$A23,
                    A24=input$A24,
                    A25=input$A25,
                    A26=input$A26,
                    A27=input$A27,
                    A28=input$A28
  )})
  
  #this generates a vector string of the risks from the CMIST tab
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
                      Q17=input$Q17)
    # %>%
    #   as.numeric()
    
  })
  
  #this generates a vector string of the uncertainty from the CMIST tab
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
                             U17=input$U17)
    # %>%
    #   as.numeric()
  })
  
  #this generates a vector string of the rationale from the CMIST tab
  rationale<-reactive({
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
  
  
  #this generates a vector string of the references from the Reference tab
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
                       C30=input$C30,
                       C31=input$C31,
                       C32=input$C32,
                       C33=input$C33,
                       C34=input$C34,
                       C35=input$C35,
                       C36=input$C36,
                       C37=input$C37,
                       C38=input$C38,
                       C39=input$C39,
                       C40=input$C40,
                       C41=input$C41,
                       C42=input$C42,
                       C43=input$C43,
                       C44=input$C44,
                       C45=input$C45,
                       C46=input$C46,
                       C47=input$C47,
                       C48=input$C48,
                       C49=input$C49,
                       C50=input$C50,
                       C51=input$C51,
                       C52=input$C52,
                       C53=input$C53,
                       C54=input$C54,
                       C55=input$C55,
                       C56=input$C56,
                       C57=input$C57,
                       C58=input$C58,
                       C59=input$C59,
                       C60=input$C60,
                       C61=input$C61,
                       C62=input$C62,
                       C63=input$C63,
                       C64=input$C64,
                       C65=input$C65,
                       C66=input$C66,
                       C67=input$C67,
                       C68=input$C68,
                       C69=input$C69,
                       C70=input$C70,
                       C71=input$C71,
                       C72=input$C72,
                       C73=input$C73,
                       C74=input$C74,
                       C75=input$C75,
                       C76=input$C76,
                       C77=input$C77,
                       C78=input$C78,
                       C79=input$C79,
                       C80=input$C80,
                       C81=input$C81,
                       C82=input$C82,
                       C83=input$C83,
                       C84=input$C84,
                       C85=input$C85,
                       C86=input$C86,
                       C87=input$C87,
                       C88=input$C88,
                       C89=input$C89,
                       C90=input$C90,
                       C91=input$C91,
                       C92=input$C92,
                       C93=input$C93,
                       C94=input$C94,
                       C95=input$C95,
                       C96=input$C96,
                       C97=input$C97,
                       C98=input$C98,
                       C99=input$C99,
                       C100=input$C100)
  })
  
  #this generates a vector string of the question numbers for labelling the table
  question<-c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17")
  
  #this generates a vector string of the questions asked during the pre-assessment step
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
                   "Ecosystem Type",
                   "Body of Water",
                   "common name 1",
                   "common name 2",
                   "common name 3",
                   "taxonomic notes",
                   "Study Area",
                   "Study Region or Province",
                   "Country",
                   "Northern Latitude",
                   "Southern Latitude",
                   "Western Longitude",
                   "Eastern Longitude"
                   )
  
  #Generating reactive tables from the information entered on the pre-assessment tab, cmist tab, and reference tab
  summaryValue<-reactive({
    cbind(question, risk=req(risks()), uncertainties=req(uncertainties()),rationale=req(rationale()))
    })
  
  summaryPrep<-reactive({
    cbind(assessor_info, prep= req(prep()))
          })
  
  summaryRef<- reactive(REFList())
  
  summaryScore<- reactive({
    #browser()
    if (any(risks()=="NA")&any(uncertainties()=="NA")){
      data.frame(CMIST_Score="NA",
                 CMIST_Upper="NA",
                 CMIST_Lower="NA",
                 Likelihood_Score="NA",
                 Likelihood_Upper="NA",
                 Likelihood_Lower="NA",
                 Impact_Score="NA",
                 Impact_Upper="NA",
                 Impact_Lower="NA")
    }else{
      
      CMISTScore(req(risks()), req(uncertainties()))
    }
  })
  
  
  # create function to summarize all inputs
  get_data <- reactive({
    # browser()
    #cm<-summaryScore()
    data.frame(
    "ASM_MONTH" = month(Sys.Date()),
    "ASM_YEAR" = year(Sys.Date()),
    "PRJ_ID" = input$A1,
    "COU_ID_1" = input$A24,
    "RG_ID" = input$A23,
    "ASA_STUDY_AREA" = input$A22,
    "MWT_ID" = input$A19,
    "BOW_ID" = input$A29,
    "ASA_NORTHERN_LATITUDE" = input$A25,
    "ASA_SOUTHERN_LATITUDE" = input$A26,
    "ASA_WESTERN_LONGITUDE" = input$A27,
    "ASA_EASTERN_LONGITUDE" = input$A28,
    "SPC_WORMS_APHIAID" = input$A8,
    "SPC_ITIS_TSN" = input$A9,
    "TAX_ID" = input$A10,
    "SPC_KINGDOM" = input$A11,
    "SPC_PHYLUM" = input$A12,
    "SPC_SUBPHYLUM" = input$A13,
    "SPC_CLASS" = input$A14,
    "SPC_ORDER" = input$A15,
    "SPC_FAMILY" = input$A16,
    "SPC_GENUS" = input$A17,
    "SPC_SPECIES" = input$A18,
    "SPC_GENUS-SPC_SPECIES" = paste(input$A17,input$A18),
    "SPC_COMMON_1" = input$A20,
    "SPC_COMMON_2" = input$A30,
    "SPC_COMMON_3" = input$A31,
    "SPC_TAXONOMIC_NOTES" = input$A21,
    "ASU_RAW_LIKELIHOOD_INVASION" = "",
    "ASU_RAW_IMPACT_INVASION" = "",
    "ASU_RAW_MEAN_RISK_SCORE" = "",
    "ASU_ADJ_RISK_SCORE" = "",
    "ASU_ADJ_LOW_CONFIDENCE_LIMIT" = "",
    "ASU_ADJ_UPPER_CONFIDENCE_LIMIT" = "",
    "ASU_RAW_LIKELIHOOD_INVASION" = "",
    "ASU_RAW_IMPACT_INVASION" = "",
    "ASU_RAW_MEAN_RISK_SCORE" = "",
    "ASU_ADJ_RISK_SCORE" = "",
    "ASU_ADJ_LOW_CONFIDENCE_LIMIT" = "",
    "ASU_ADJ_UPPER_CONFIDENCE_LIMIT" = "",
    "QST_QUESTION.1" = "Is the species established in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.1" = input$U1,
    "UNC_UNCERTAINTY.1" = case_when(
      input$U1==1~"Low certainty",
      input$U1==2~"Medium certainty",
      input$U1==3~"High certainty"
    ),
    "QRS_RISK_SCORE.1" = input$Q1,
    "QRS_DESCRIPTION.1" = case_when(
      input$Q1==1~"No",
      input$Q1==2~"Observed",
      input$Q1==3~"Yes"
    ),
    "QRP_RATIONALE.1" = input$R1,
    "QST_QUESTION.2" = "How frequently and in what numbers is the species expected to arrive into the assessment area?",
    "UNC_UNCERTAINTY_SCORE.2" = input$U2,
    "UNC_UNCERTAINTY.2" = case_when(
      input$U2==1~"Low certainty",
      input$U2==2~"Medium certainty",
      input$U2==3~"High certainty"
    ),
    "QRS_RISK_SCORE.2" = input$Q2,
    "QRS_DESCRIPTION.2" = case_when(
      input$Q2==1~"Infrequently in low numbers",
      input$Q2==2~"Frequently in low numbers OR infrequently in high numbers",
      input$Q2==3~"Frequently in high numbers"
    ),
    "QRP_RATIONALE.2"=input$R2,
    "QST_QUESTION.3" = "How much of the assessment area offers suitable habitat for the species?",
    "UNC_UNCERTAINTY_SCORE.3" = input$U3,
    "UNC_UNCERTAINTY.3" = case_when(
      input$U3==1~"Low certainty",
      input$U3==2~"Medium certainty",
      input$U3==3~"High certainty"
    ),
    "QRS_RISK_SCORE.3" = input$Q3,
    "QRS_DESCRIPTION.3" = case_when(
      input$Q3==1~"Negligible proportion of the assessment area",
      input$Q3==2~"Moderate proportion of the assessment area",
      input$Q3==3~"Most of the assessment area"
    ),
    "QRP_RATIONALE.3" = input$R3,
    "QST_QUESTION.4" = "How much of the assessment area offers suitable environmental conditions for the species to survive?",
    "UNC_UNCERTAINTY_SCORE.4" = input$U4,
    "UNC_UNCERTAINTY.4" = case_when(
      input$U4==1~"Low certainty",
      input$U4==2~"Medium certainty",
      input$U4==3~"High certainty"
    ),
    "QRS_RISK_SCORE.4" = input$Q4,
    "QRS_DESCRIPTION.4" = case_when(
      input$Q4==1~"Negligible proportion of the assessment area",
      input$Q4==2~"Moderate proportion of the assessment area",
      input$Q4==3~"Most of the assessment area"
    ),
    "QRP_RATIONALE.4" = input$R4,
    "QST_QUESTION.5" = "Are the species' reproductive requirements available in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.5" = input$U5,
    "UNC_UNCERTAINTY.5" = case_when(
      input$U5==1~"Low certainty",
      input$U5==2~"Medium certainty",
      input$U5==3~"High certainty"
    ),
    "QRS_RISK_SCORE.5" = input$Q5,
    "QRS_DESCRIPTION.5" = case_when(
      input$Q5==1~"Almost never",
      input$Q5==2~"Sometimes",
      input$Q5==3~"Almost always"
    ),
    "QRP_RATIONALE.5" = input$R5,
    "QST_QUESTION.6" = "To what extent could natural control agents slow the species' population growth in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.6" = input$U6,
    "UNC_UNCERTAINTY.6" = case_when(
      input$U6==1~"Low certainty",
      input$U6==2~"Medium certainty",
      input$U6==3~"High certainty"
    ),
    "QRS_RISK_SCORE.6" = input$Q6,
    "QRS_DESCRIPTION.6" = case_when(
      input$Q6==1~"Likely to severely restrict population growth",
      input$Q6==2~"Could slow population growth",
      input$Q6==3~"Unlikely to slow population growth"
    ),
    "QRP_RATIONALE.6" = input$R6,
    "QST_QUESTION.7" = "What is the range of the species' potential natural dispersal in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.7" = input$U7,
    "UNC_UNCERTAINTY.7" = case_when(
      input$U7==1~"Low certainty",
      input$U7==2~"Medium certainty",
      input$U7==3~"High certainty"
    ),
    "QRS_RISK_SCORE.7" = input$Q7,
    "QRS_DESCRIPTION.7" = case_when(
      input$Q7==1~"Very limited range",
      input$Q7==2~"Moderate range",
      input$Q7==3~"Wide range"
    ),
    "QRP_RATIONALE.7" = input$R7,
    "QST_QUESTION.8" = "What is the range of the species' potential dispersal in the assessment area from anthropogenic mechanisms?",
    "UNC_UNCERTAINTY_SCORE.8" = input$U8,
    "UNC_UNCERTAINTY.8" = case_when(
      input$U8==1~"Low certainty",
      input$U8==2~"Medium certainty",
      input$U8==3~"High certainty"
    ),
    "QRS_RISK_SCORE.8" = input$Q8,
    "QRS_DESCRIPTION.8" = case_when(
      input$Q8==1~"Very limited range",
      input$Q8==2~"Moderate range",
      input$Q8==3~"Wide range"
    ),
    "QRP_RATIONALE.8" = input$R8,
    "QST_QUESTION.9" = "What level of impact could the species have on population growth of other species in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.9" = input$U9,
    "UNC_UNCERTAINTY.9" = case_when(
      input$U9==1~"Low certainty",
      input$U9==2~"Medium certainty",
      input$U9==3~"High certainty"
    ),
    "QRS_RISK_SCORE.9" = input$Q9,
    "QRS_DESCRIPTION.9" = case_when(
      input$Q9==1~"Low or no impact",
      input$Q9==2~"High impact in few areas OR moderate impact in many areas",
      input$Q9==3~"High impact in many areas"
    ),
    "QRP_RATIONALE.9" = input$R9,
    "QST_QUESTION.10" = "What level of impact could the species have on communities in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.10" = input$U10,
    "UNC_UNCERTAINTY.10" = case_when(
      input$U10==1~"Low certainty",
      input$U10==2~"Medium certainty",
      input$U10==3~"High certainty"
    ),
    "QRS_RISK_SCORE.10" = input$Q10,
    "QRS_DESCRIPTION.10" = case_when(
      input$Q10==1~"Low or no impact",
      input$Q10==2~"High impact in few areas OR moderate impact in many areas",
      input$Q10==3~"High impact in many areas"
    ),
    "QRP_RATIONALE.10" = input$R10,
    "QST_QUESTION.11" = "What level of impact could the species have on habitat in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.11" = input$U11,
    "UNC_UNCERTAINTY.11" = case_when(
      input$U11==1~"Low certainty",
      input$U11==2~"Medium certainty",
      input$U11==3~"High certainty"
    ),
    "QRS_RISK_SCORE.11" = input$Q11,
    "QRS_DESCRIPTION.11" = case_when(
      input$Q11==1~"Low or no impact",
      input$Q11==2~"High impact in few areas OR moderate impact in many areas",
      input$Q11==3~"High impact in many areas"
    ),
    "QRP_RATIONALE.11" = input$R11,
    "QST_QUESTION.12" = "What level of impact could the species have on ecosystem function in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.12" = input$U12,
    "UNC_UNCERTAINTY.12" = case_when(
      input$U12==1~"Low certainty",
      input$U12==2~"Medium certainty",
      input$U12==3~"High certainty"
    ),
    "QRS_RISK_SCORE.12" = input$Q12,
    "QRS_DESCRIPTION.12" = case_when(
      input$Q12==1~"Low or no impact",
      input$Q12==2~"High impact in few areas OR moderate impact in many areas",
      input$Q12==3~"High impact in many areas"
    ),
    "QRP_RATIONALE.12" = input$R12,
    "QST_QUESTION.13" = "What level of impact could the species' associated diseases, parasites, or travellers have on other species in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.13" =input$U13 ,
    "UNC_UNCERTAINTY.13" = case_when(
      input$U13==1~"Low certainty",
      input$U13==2~"Medium certainty",
      input$U13==3~"High certainty"
    ),
    "QRS_RISK_SCORE.13" = input$Q13,
    "QRS_DESCRIPTION.13" = case_when(
      input$Q13==1~"Low or no impact",
      input$Q13==2~"High impact in few areas OR moderate impact in many areas",
      input$Q13==3~"High impact in many areas"
    ),
    "QRP_RATIONALE.13" = input$R13,
    "QST_QUESTION.14" = "What level of genetic impact could the species have on other species in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.14" = input$U14,
    "UNC_UNCERTAINTY.14" = case_when(
      input$U14==1~"Low certainty",
      input$U14==2~"Medium certainty",
      input$U14==3~"High certainty"
    ),
    "QRS_RISK_SCORE.14" = input$Q14,
    "QRS_DESCRIPTION.14" = case_when(
      input$Q14==1~"Low or no impact",
      input$Q14==2~"High impact in few areas OR moderate impact in many areas",
      input$Q14==3~"High impact in many areas"
    ),
    "QRP_RATIONALE.14" = input$R14,
    "QST_QUESTION.15" = "What level of impact could the species have on at-risk or depleted species in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.15" = input$U15,
    "UNC_UNCERTAINTY.15" = case_when(
      input$U15==1~"Low certainty",
      input$U15==2~"Medium certainty",
      input$U15==3~"High certainty"
    ),
    "QRS_RISK_SCORE.15" = input$Q15,
    "QRS_DESCRIPTION.15" = case_when(
      input$Q15==1~"Low or no impact",
      input$Q15==2~"High impact in few areas OR moderate impact in many areas",
      input$Q15==3~"High impact in many areas"
    ),
    "QRP_RATIONALE.15" = input$R15,
    "QST_QUESTION.16" = " What level of impact could the species have on aquaculture and commercially fished species in the assessment area?",
    "UNC_UNCERTAINTY_SCORE.16" = input$U16,
    "UNC_UNCERTAINTY.16" = case_when(
      input$U16==1~"Low certainty",
      input$U16==2~"Medium certainty",
      input$U16==3~"High certainty"
    ),
    "QRS_RISK_SCORE.16" = input$Q16,
    "QRS_DESCRIPTION.16" = case_when(
      input$Q16==1~"Low or no impact",
      input$Q16==2~"High impact in few areas OR moderate impact in many areas",
      input$Q16==3~"High impact in many areas"
    ),
    "QRP_RATIONALE.16" = input$R16,
    "QST_QUESTION.17" = "Is the species known or generally considered to be invasive anywhere in the world?",
    "UNC_UNCERTAINTY_SCORE.17" = input$U17,
    "UNC_UNCERTAINTY.17" = case_when(
      input$U17==1~"Low certainty",
      input$U17==2~"Medium certainty",
      input$U17==3~"High certainty"
    ),
    "QRS_RISK_SCORE.17" = input$Q17,
    "QRS_DESCRIPTION.17" = case_when(
      input$Q17==1~"No",
      input$Q17==2~"No, but has traits related to invasivenessMedium certainty",
      input$Q17==3~"Yes"
    ),
    "QRP_RATIONALE.17" = input$R17
    )
    
  })
  
  
  #The meow region map from the pre-assessment info. Clicking on a blue polygon will create a opo-up with the region name.
  output$meow.region.map<-renderLeaflet({
    leaflet(meow_regions) %>% 
      setView(lng=-106.9299, lat=51.9788, zoom=3)%>%
      addTiles() %>% 
      addPolygons(popup = meow_regions$ECOREGION)
  })
  
  #reactive input from choosing either MEOW or Provincial regions. This code updates the regions to select from. 
  observeEvent(
    input$regionSelect,{
    if(input$regionSelect=="Province"){
      updateSelectInput(session, "A23",label="Please select a province", choices = prov)
    }
  if(input$regionSelect=="MEOW"){
    updateSelectInput(session,"A23",label="Please select a MEOW Region", choices = meowR)
  }
    }

  )
  
  
  #box and whisker plot of the CMIST-database
  output$cmist.plot<- renderPlot({
    cm_plot<-
      ggplot(data=CMISTdata, aes(x=Taxonomic_Group, y=ASU_ADJ_RISK_SCORE, fill=Taxonomic_Group))+
    geom_boxplot(show.legend = FALSE)+
      scale_fill_viridis(discrete=TRUE, alpha=0.6)+
      geom_jitter(aes(shape=Taxonomic_Group),size=2, alpha=0.9, show.legend = FALSE)+
      ggtitle("CMIST Database Risk Scores by Taxonomic Group")+
      xlab("Taxonomic Group")+
      ylab("Adjusted CMIST Risk Score")+
      theme_bw()
    cm_plot
  })
  
  #This generates text from the selected points from the box and whisker plot
  plotH_sel<-eventReactive(input$plot_click1,{
    res2<-nearPoints(CMISTdata, input$plot_click1, #xvar="Taxonomic_Group",
                     #yvar = "ASU_ADJ_RISK_SCORE",
                     allRows = FALSE, 
                     maxpoints = 1,
                     threshold = 10)
    list(paste("CMIST Score",res2$ASU_ADJ_RISK_SCORE, sep=":"),
         paste("Species",res2$SPC_GENUS.SPC_SPECIES, sep=":"),
         paste("Study Area",res2$ASA_STUDY_AREA, sep=":"))
  })
  
  #This prints the text from above
  output$plotH_info<-renderPrint({
    plotH_sel()
    })
  
  #Scatter plot of score from each cmist and the cmist database
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
    #plot when no filters are applied
    if(input$filters=="no"){
      p<-p +
      geom_point(data=CMISTdata,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)
    }
    #plot updated for the specified region
    if(input$filters=="region"){
      #browser()
      filterData<-
        CMISTdata%>%
        dplyr::select(Region,SPC_GENUS.SPC_SPECIES, ASU_RAW_IMPACT_INVASION, ASU_RAW_LIKELIHOOD_INVASION, ASU_ADJ_RISK_SCORE )%>%
        dplyr::filter(Region==input$filterRegion)
      
      p<-p +
        geom_point(data=filterData,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)
    }
    #plot updated for the selected species from the CMIST database
    if(input$filters=="species"){
      #browser()
      filterData2<-
        CMISTdata%>%
        dplyr::select(Region,SPC_GENUS.SPC_SPECIES, ASU_RAW_IMPACT_INVASION, ASU_RAW_LIKELIHOOD_INVASION, ASU_ADJ_RISK_SCORE )%>%
        dplyr::filter(SPC_GENUS.SPC_SPECIES==input$filterSpecies)
      p<-p +
        geom_point(data=filterData2,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)
    }
    #plot updated for selected region and species
    if(input$filters=="both"){
      filterData3<-
        CMISTdata%>%
        dplyr::select(Region,SPC_GENUS.SPC_SPECIES, ASU_RAW_IMPACT_INVASION, ASU_RAW_LIKELIHOOD_INVASION, ASU_ADJ_RISK_SCORE )%>%
        dplyr::filter(Region==input$filterRegion & SPC_GENUS.SPC_SPECIES==input$filterSpecies)
      p<-p +
        geom_point(data=filterData3,aes(x=ASU_RAW_IMPACT_INVASION, y=ASU_RAW_LIKELIHOOD_INVASION), colour="black", position = "jitter", alpha=0.6)
    }

     p
  })

  #reactive text from selecting a point from scatter plot
  selCmist<-eventReactive(input$plot_hover,{
    res<-nearPoints(CMISTdata, input$plot_hover, xvar ="ASU_RAW_IMPACT_INVASION", yvar ="ASU_RAW_LIKELIHOOD_INVASION", allRows = FALSE)
    list(paste("CMIST Score",res$ASU_ADJ_RISK_SCORE, sep=":"), 
         paste("Species",res$SPC_GENUS.SPC_SPECIES, sep=":"), 
         paste("Study Area",res$ASA_STUDY_AREA, sep=":"))
  })
  
  output$plot_hoverinfo<- renderPrint({selCmist()})
  
  #generates CMIST score summary from the CMIST assessment
  output$cmist_score<-renderPrint({
    req(input$plot.click)
    text.cm<-summaryScore()
    test.cm.score<-text.cm$CMIST_Score
  })
  
  #summary table of CMIST Score
  output$table1<- renderTable({summaryScore()})
  
  #summary table of pre-assessment info
  output$table2<- renderTable({summaryPrep()})
  
  #summary table of cmist assessment info
  output$table3<-renderTable({summaryValue()})
  
  #summary table of references
  output$table4<-renderTable({summaryRef()})
  
  #downloadable csv document
  
  output$downloadData<- downloadHandler(
    
    filename=function(){
      
      paste(input$downloadName, ".csv", sep="")
    },
    
    content=
      function(file){
        
        wb<- write.xlsx(summaryPrep(), file, sheetName="Pre_Assessment_Info")
                addWorksheet(wb, "CMIST_Data")
                addWorksheet(wb, "References")
                addWorksheet(wb, "CMIST_Score")
                addWorksheet(wb, "For_Database")

                writeData(wb, "Pre_Assessment_Info", summaryPrep())
                writeData(wb, "CMIST_Data", summaryValue())
                writeData(wb, "References", summaryRef())
                writeData(wb, "CMIST_Score", summaryScore())
                writeData(wb, "For_Database", get_data())
                
        saveWorkbook(wb, file, overwrite = TRUE)

        
      }
    
  )
  
  #Downloadable PDF of the summary tab
  
  # output$downloadPDF<- downloadHandler(
  #   filename= function(){
  #     paste(input$downloadName, ".pdf", sep="")
  #     #browser()
  #     rmarkdown::render("CMIST_Summary.Rmd",
  #                       param=get_data(),
  #                       envir = new.env(parent = globalenv()),
  #                       output_file=paste(input$downloadName, '.pdf', sep=''))
  # 
  # 
  #   },
  #   
  #   content=
  #     function(file){
  #       doc<-docx()
  #       data<- get_data()
  #       doc<-addFlexTable(doc, vanilla.table(data))
  #       writeDoc(doc, file)
  #     }
  #   contentType = "application/pdf"
  #     )
  
  
  #leaflet map from the explore database tab
  output$leafletmap<- renderLeaflet({
     
   lf<- Canada_map%>%
      addTiles()%>%
      addPolygons(data=mist_sf_map,
                  weight=2,
                  col='blue',
                  fillColor = mist_sf_map,
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
  
  #data table of the CMIST database
  output$table5<- DT::renderDataTable({
    DT::datatable(CMISTdata)
  })
  
  #uploading previously saved work
  observeEvent(input$load_inputs, {
    # Load inputs
    uploaded_inputs <- read.csv(input$inputsLocation)
    # Update each input
    for(i in 1:nrow(uploaded_inputs)){
      updateNumericInput(session,
                         inputId = uploaded_inputs$inputId[i],
                         value = uploaded_inputs$value[i])
    }
  })
  
  # #Saving inputs 
  # observeEvent(input$save_inputs, {
  #   # Define inputs to save
  #   inputs_to_save <- c('A1',
  #                       'A2',
  #                       'A3',
  #                       'A4',
  #                       'A5',
  #                       'A6',
  #                       'A7',
  #                       'A8',
  #                       'A9',
  #                       'A10',
  #                       'A11',
  #                       'A12',
  #                       'A13',
  #                       'A14',
  #                       'A15',
  #                       'A16',
  #                       'A17',
  #                       'A18',
  #                       'A19',
  #                       'A29',
  #                       'A20',
  #                       'A30',
  #                       'A31',
  #                       'A21',
  #                       'A22',
  #                       'A23',
  #                       'A24',
  #                       'A25',
  #                       'A26',
  #                       'A27',
  #                       'A28',
  #                       'Q1',
  #                       'Q2',
  #                       'Q3',
  #                       'Q4',
  #                       'Q5',
  #                       'Q6',
  #                       'Q7',
  #                       'Q8',
  #                       'Q9',
  #                       'Q10',
  #                       'Q11',
  #                       'Q12',
  #                       'Q13',
  #                       'Q14',
  #                       'Q15',
  #                       'Q16',
  #                       'Q17',
  #                       'U1',
  #                       'U2',
  #                       'U3',
  #                       'U4',
  #                       'U5',
  #                       'U6',
  #                       'U7',
  #                       'U8',
  #                       'U9',
  #                       'U10',
  #                       'U11',
  #                       'U12',
  #                       'U13',
  #                       'U14',
  #                       'U15',
  #                       'U16',
  #                       'U17',
  #                       'C1',
  #                       'C2',
  #                       'C3',
  #                       'C4',
  #                       'C5',
  #                       'C6',
  #                       'C7',
  #                       'C8',
  #                       'C9',
  #                       'C10',
  #                       'C11',
  #                       'C12',
  #                       'C13',
  #                       'C14',
  #                       'C15',
  #                       'C16',
  #                       'C17',
  #                       'C18',
  #                       'C19',
  #                       'C20',
  #                       'C21',
  #                       'C22',
  #                       'C23',
  #                       'C24',
  #                       'C25',
  #                       'C26',
  #                       'C27',
  #                       'C28',
  #                       'C29',
  #                       'C30',
  #                       'C31',
  #                       'C32',
  #                       'C33',
  #                       'C34',
  #                       'C35',
  #                       'C36',
  #                       'C37',
  #                       'C38',
  #                       'C39',
  #                       'C40',
  #                       'C41',
  #                       'C42',
  #                       'C43',
  #                       'C44',
  #                       'C45',
  #                       'C46',
  #                       'C47',
  #                       'C48',
  #                       'C49',
  #                       'C50',
  #                       'C51',
  #                       'C52',
  #                       'C53',
  #                       'C54',
  #                       'C55',
  #                       'C56',
  #                       'C57',
  #                       'C58',
  #                       'C59',
  #                       'C60',
  #                       'C61',
  #                       'C62',
  #                       'C63',
  #                       'C64',
  #                       'C65',
  #                       'C66',
  #                       'C67',
  #                       'C68',
  #                       'C69',
  #                       'C70',
  #                       'C71',
  #                       'C72',
  #                       'C73',
  #                       'C74',
  #                       'C75',
  #                       'C76',
  #                       'C77',
  #                       'C78',
  #                       'C79',
  #                       'C80',
  #                       'C81',
  #                       'C82',
  #                       'C83',
  #                       'C84',
  #                       'C85',
  #                       'C86',
  #                       'C87',
  #                       'C88',
  #                       'C89',
  #                       'C90',
  #                       'C91',
  #                       'C92',
  #                       'C93',
  #                       'C94',
  #                       'C95',
  #                       'C96',
  #                       'C97',
  #                       'C98',
  #                       'C99',
  #                       'C100'
  #                       
  #   )
  #   # Declare inputs
  #   inputs <- NULL
  #   # Append all inputs before saving to folder
  #   for(input.i in inputs_to_save){
  #     inputs <- append(inputs, input[[input.i]])
  #   }
  #   # Inputs data.frame
  #   inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
  #   # Save Inputs
  #   write.csv(inputs_data_frame, file = input$inputsLocation, row.names = FALSE)
  # }) 
  
  
}
)

shinyApp(ui=ui, server=server,  
        enableBookmarking = "url"
         )


