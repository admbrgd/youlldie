
# This is a Shiny web application named "youlldie"

#Install required packages:
#install.packages("shiny")
#install.packages("hrbrthemes")
#install.packages("viridis")
#install.packages("gridExtra")
#install.packages("ggrepel)
#install.packages("plotly")
#install.packages("ggplot2")
#install.packages("dplyr")

#load required packages:
library(shiny)
library(hrbrthemes)
library(viridis)
library(gridExtra)
library(ggrepel)
library(plotly)
library(ggplot2)
library(dplyr)
#library(stats)
#library(Rfast)
#library(doBy)

#////////////////////////////////////////////////////////////////////////////////
# the following section is to build a ui object that lays out a webpage (html) for the app (it converts R -> html)
#////////////////////////////////////////////////////////////////////////////////

ui<-fluidPage(
  
  
  #  headerPanel("You'll die"),
  
  
  
  ################################################################################
  # The following section is to add input function corresponding to Risk Factor ordered by Domains
  ################################################################################
  
  sidebarPanel(
    style = "overflow-y:scroll;position:relative;max-height:100vh",
    #DEMOGRAPHICS-------------------------------------------------------------------
    fluidRow(
      tags$h4("DEMOGRAPHICS")),
    
    #Risk Factor: Current Age (CAGE)
    fluidRow(
      sliderInput(inputId="cage",
                  label="Age",
                  value=0, min=0, max=100)
      #,plotOutput("bubble")
    ),
    
    #Risk Factor: Sex (SEX)
    fluidRow(
      radioButtons(inputId="sex", label="Sex", choices=list("Male",
                                                            "Female"), 
                   selected=character(0))
      
    ),
    
    #Risk Factor: Race (RACE)
    fluidRow(
      radioButtons(inputId="race", label="Race", choices=list("Caucasian (White)",
                                                              "African (Black)",
                                                              "Asian",
                                                              "Middle Eastern (Indian)",
                                                              "Native American",
                                                              "Other"), 
                   selected=character(0))
    ),
    
    #SOCIAL STATUS------------------------------------------------------------------
    fluidRow(
      tags$h4("SOCIAL STATUS")),
    
    #Risk Factor: Income Group (INC)
    fluidRow(
      radioButtons(inputId="inc", label="Income Group", choices=list("Poor",
                                                                     "Lower-middle",
                                                                     "Middle",
                                                                     "Upper-middle",
                                                                     "Rich"),
                   selected=character(0))
    ),
    
    #Risk Factor: Highest achieved education degree (EDU)
    fluidRow(
      radioButtons(inputId="edu", label="What is your highest grade/level of school/degree?", choices=list("No Formal Schooling",
                                                                                                           "Primary Education (Elementary School)",
                                                                                                           "Secondary Education (High School)",
                                                                                                           "Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program",
                                                                                                           "Master's degree",
                                                                                                           "Doctoral degree"),
                   selected=character(0))
    ),
    
    #SS1  #Marital Status (x)
    #SS2  #Number of Kids (x)
    
    #LIFESTYLE----------------------------------------------------------------------
    
    fluidRow(
      tags$h4("LIFESTYLE")),
    
    #Risk Factor: Weekly Drinks (DRK)
    fluidRow(
      sliderInput(inputId="drk",
                  label="Number of drinks per week",
                  value=0, min=0, max=20)
      #,plotOutput("bubble")
    ),
    
    
    #Risk Factor: Weekly Smokes (SMK)
    fluidRow(
      sliderInput(inputId="smk",
                  label="Number of smokes per week",
                  value=0, min=0, max=140)
      #,plotOutput("bubble")
    ),
    
    
    #Risk Factor: Number of moderate intensity physical activity minutes per week (MPA)
    fluidRow(
      sliderInput(inputId="mpa",
                  label="Number of minutes of moderate intensity physical activity per week",
                  value=0, min=0, max=300)
      #,plotOutput("bubble")
    ),
    
    
    #Risk Factor: Number of high intensity physical activity minutes per week (HPA)
    fluidRow(
      sliderInput(inputId="hpa",
                  label="Number of minutes of vigorous intensity physical activity per week",
                  value=0, min=0, max=100)
      #,plotOutput("bubble")
    ),
    
    #VITALS-------------------------------------------------------------------------
    
    fluidRow(
      tags$h4("VITALS")),
    
    #Risk Factor: Systolic Blood Pressure mm/Hg (SYS)
    #              fluidRow(
    #                 sliderInput(inputId="sys",
    #                             label="Systolic Blood Pressure (mm/HG)",
    #                             value=0, min=0, max=200)
    #                 #,plotOutput("bubble")
    #               ),
    
    
    #Risk Factor: Systolic Blood Pressure mm/Hg (SYS)
    fluidRow(
      radioButtons(inputId="sys", label="Blood Pressure", choices=list("Normal (SBP <120 mmHG)",
                                                                       "Elevated (SBP 120-129 mmHG)",
                                                                       "High Blood Pressure Stage 1 (SBP 130-140 mmHG)",
                                                                       "High Blood Pressure Stage 2 (SBP >140 mmHG)"),
                   selected=character(0))
    ),            
    
    
    
    
    
    #Risk Factor: Body Mass Index (BMI)
    fluidRow(
      radioButtons(inputId="bmi", label="Body Mass Index", choices=list("Underweight (<18.5)",
                                                                        "Normal Weight (18.5-24.9)",
                                                                        "Overweight (25-29.9)",
                                                                        "Obese (>30)"),
                   selected=character(0))
    ),
    
    
    #Risk Factor: weight (WGH)
    
    #               fluidRow(
    #                 numericInput(inputId="wgh",
    #                              label="Weight",
    #                              value=0, min=0, max=500, step=5)
    #               ),
    #               
    #               fluidRow(  
    #                 selectInput(inputId="wghu",
    #                             label="Weight Unit",
    #                             choices=list("kg","lbs"),
    #                             multiple=FALSE)
    #               ),
    #               
    #               #Risk Factor: height (HGH)
    #               
    #               fluidRow(
    #                 numericInput(inputId="hgh",
    #                              label="Height",
    #                              value=0, min=0, max=500, step=5)
    #               ),
    #               
    #               fluidRow(  
    #                 selectInput(inputId="hghu",
    #                             label="Height Unit",
    #                             choices=list("cm","lbs"),
    #                             multiple=FALSE)
    #               ),
    
    #MEDICAL HISTORY----------------------------------------------------------------
    
    fluidRow(
      tags$h4("MEDICAL HISTORY")),
    fluidRow(
      tags$p("Are you currently living with the following conditions?")),
    
    
    #HBP  #High Blood Pressure (Y/N)
    
    #               fluidRow(
    #                 radioButtons(inputId="hbp",
    #                              label="High Blood Pressure", 
    #                              choices=list("Yes",
    #                                           "No"),
    #                              selected=character(0))
    #               ),
    
    #HBC  #High Blood Cholesterol (Y/N)
    
    fluidRow(
      radioButtons(inputId="hbc",
                   label="High Blood Cholesterol", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #CVD  #CVD
    fluidRow(
      radioButtons(inputId="cvd",
                   label="Cardiovascular Disease", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #COP  #COPD
    
    fluidRow(
      radioButtons(inputId="copd",
                   label="Chronic Obstructive Pulmonary Disease", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #DIA  #Diabetes (Y/N)
    
    fluidRow(
      radioButtons(inputId="dia",
                   label="Diabetes", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #DEP  #Depression (Y/N)
    
    fluidRow(
      radioButtons(inputId="dep",
                   label="Depression", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #CAN  #Cancer
    fluidRow(
      radioButtons(inputId="can",
                   label="Cancer", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #CAN  #Alzheimer
    fluidRow(
      radioButtons(inputId="alz",
                   label="Alzheimer", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #FAMILY HISTORY-----------------------------------------------------------------
    
    fluidRow(
      tags$h4("FAMILY HISTORY")),
    fluidRow(
      tags$p("Has someone in your family (Father, Mother, Brother, Sister experienced the following conditions?")),
    
    
    #HBP  #High Blood Pressure (Y/N)
    fluidRow(
      radioButtons(inputId="fhbp",
                   label="Family History of High Blood Pressure", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #HBC  #High Blood Cholesterol (Y/N)
    fluidRow(
      radioButtons(inputId="fhbc",
                   label="Family History of High Blood Cholesterol", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #CVD  #CVD
    fluidRow(
      radioButtons(inputId="fcvd",
                   label="Family History of Cardiovascular Disease", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #COP  #COPD
    fluidRow(
      radioButtons(inputId="fcopd",
                   label="Family History of Chronic Obstructive Pulmonary Disease", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #FDI  #Diabetes
    fluidRow(
      radioButtons(inputId="fdia",
                   label="Family History of Diabetes", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #FDE  #Depression
    fluidRow(
      radioButtons(inputId="fdep",
                   label="Family History of Depression", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #CAN  #Cancer
    fluidRow(
      radioButtons(inputId="fcan",
                   label="Family History of Cancer", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #ALZ  #Alzheimer
    fluidRow(
      radioButtons(inputId="falz",
                   label="Family History of Alzheimer", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #CONCOMITAN MEDICATIONS---------------------------------------------------------
    
    fluidRow(
      tags$h4("CONCOMITANT MEDICATIONS")),
    
    #CMI  #Immunosuppression
    
    fluidRow(
      radioButtons(inputId="cmi",
                   label="Are you taking immunosuppressants?", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #CMA  #Infectious Agents ?????
    
    
  ),
  
  
  
  mainPanel(
    #    fluidRow(
    #      tags$h1("You'll die")),
    
    #    fluidRow(
    
    
    
    #    tags$div(tags$h3("You actually have", tags$strong(textOutput("textprob1", inline = TRUE),"%"),
    #                     "chances of dying from", tags$strong(textOutput("textcause1", inline = TRUE)),
    #                     "at the age of", tags$strong(textOutput("textage1", inline = TRUE))
    #    )
    #    ),
    
#    tags$div(tags$h2("You'll die")),
#    tags$div(tags$h5("From something. At some point. For sure.")),
    
#    tags$div(tags$h5("Your cause and age of death depends on your inherited risk factors and your lifestyle choices.",
                     
#                     tags$strong("Enter your profile in the grey area and see how your chances of dying from different causes stack up."), 
#                     "Then, if
#              you want, make some lifestyle changes to improve your life and you know...live longer. And don't get scared.
#              This app is not a death sentence generator. It just illustrates what the numbers
#              look like. And you're not a number right? No you're not. You are a complex unique being with a 
#              willpower that transcends the boundaries of funky-looking bubbles. 
#              Yes you are. But you'll die. For sure.")),
    
    
    tags$div(tags$h5("Based on the information you provided, you have, statistically speaking...")),
    
    
    tags$div(
      tags$strong(textOutput("textprob1", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause1", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage1", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob2", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause2", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage2", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob3", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause3", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage3", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob4", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause4", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage4", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob5", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause5", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage5", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob6", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause6", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage6", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob7", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause7", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage7", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob8", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause8", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage8", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob9", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause9", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage9", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob10", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause10", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage10", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob11", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause11", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage11", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob12", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause12", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage12", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob13", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause13", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage13", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob14", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause14", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage14", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob15", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause15", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage15", inline = TRUE)),
      br(),
      tags$strong(textOutput("textprob16", inline = TRUE),"%"),
      "chance of dying from", tags$strong(textOutput("textcause16", inline = TRUE)),
      "at the age of",tags$strong(textOutput("textage16", inline = TRUE)),
      br(),
      
      
    ),
    #     ),
    
    #     textOutput("text2"),
    
    #    fluidRow(
    plotlyOutput("bubble2", height="auto", width="auto"),
    #  ),
    
    #    fluidRow(
    
    #tags$div(tags$h4("Enter your profile in the grey area and watch the graph above change.")),
    #  ),
    
    
    
    
    
    
    
    
  ),
  
  
  
  #  position = "left" # this code is to move the SidePanel vs MainPanel
  
)

#////////////////////////////////////////////////////////////////////////////////
# the following section is to build the dataframe used to build the plot
#////////////////////////////////////////////////////////////////////////////////

#Provide the list of leading causes of death here:
cod55 <- data.frame(cause =  c("Cardiovascular Diseases",
                               "Coronary Heart Diseases",
                               "Stroke",
                               "Cancer",
                               "COVID-19",
                               "Alzheimer’s Disease",
                               "Chronic Lower Respiratory Diseases",
                               "Diabetes",
                               "Drug Overdose",
                               "Motor Vehicle Accident",
                               "Fall",
                               "Influenza and Pneumonia",
                               "Kidney Diseases",
                               "Suicide",
                               "Liver Diseases",
                               "Septicemia"),
                    
                    #enter the average age of death from each conditions here:                 
                    age =  c(67.3,  #Cardiovascular Diseases
                             76.0,  #Coronary Heart Diseases
                             70.5,  #Stroke
                             65.0,  #Cancer
                             79.0,  #COVID-19
                             78.0,  #Alzheimer’s Disease
                             62.0,  #Chronic Lower Respiratory Diseases
                             74.6,  #Diabetes
                             40.0,  #Drug Overdose
                             40.0,  #Motor Vehicle Accident
                             70.0,  #Fall
                             70.0,  #Influenza and Pneumonia
                             73.0,  #Kidney Diseases
                             30.0,  #Suicide
                             52.0,  #Liver Diseases
                             65.0), #Septicemia
                    
                    #enter the baseline risk of dying from each condition (n/100,000) here:                  
                    risk =  c(224.4, #Cardiovascular Diseases
                              91.8, #Coronary Heart Diseases
                              38.8,  #Stroke
                              148.1, #Cancer
                              85.0, #COVID-19
                              32.4,  #Alzheimer’s Disease
                              36.4,  #Chronic Lower Respiratory Diseases
                              24.8,  #Diabetes
                              25.8,  #Drug Overdose
                              13.1,  #Motor Vehicle Accident
                              10.3,  #Fall
                              13.0,  #Influenza and Pneumonia
                              12.7,  #Kidney Diseases
                              13.5,  #Suicide
                              13.3,  #Liver Diseases
                              9.7), #Septicemia
                    
                    #enter the population (n/100,000) dying form each condition per year here:                
                    pop =  c(813804, #Cardiovascular Diseases
                             406351, #Coronary Heart Diseases
                             160264, #Stroke
                             608570, #Cancer
                             350831, #COVID-19
                             132242, #Alzheimer’s Disease
                             152657, #Chronic Lower Respiratory Diseases
                             102188, #Diabetes
                             91800,  #Drug Overdose
                             42915,  #Motor Vehicle Accident
                             40114,  #Fall
                             53544,  #Influenza and Pneumonia
                             52547,  #Kidney Diseases
                             45940,  #Suicide
                             51642,  #Liver Diseases
                             40050) #Septicemia
)

#################################################################################
# The Section below serve to define the "server" function for the server to create/use the R components for the app.
#################################################################################

server <- function(input, output){
  
  #converting the cause of death dataframe (cod) into a reactive function / dataframe that changes according to inputs.  
  
  cod77<-reactive({cod55 %>%
      
      #################################################################################
    #################################################################################
    # Setting impact of Risk Factors on AGE OF DEATH   
    #################################################################################
    #################################################################################
    
    mutate(age=c(
      
      cod55[cod55$cause=="Cardiovascular Diseases","age"]
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.955}
        else if(input$sex=="Female"){1.045})
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.00157))
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.00109))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000219))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000438))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      #      -(if(input$sys < 130){0}
      #        else if(input$sys >= 130 & input$sys < 140){1}
      #        else if(input$sys >= 140 & input$sys < 150){3}
      #        else if(input$sys >= 150 & input$sys < 160){5}
      #        else if(input$sys >= 160 & input$sys < 170){7} 
      #        else if(input$sys >= 170){10})
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.984}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.967}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.934})
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.942}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.975}
        else if(input$bmi=="Obese (>30)"){0.949})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      ,                 
      
      
      ################################################################################
      # Coronary Heart Diseases
      ################################################################################
      cod55[cod55$cause=="Coronary Heart Diseases","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.974}
        else if(input$sex=="Female"){1.026})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.00157))    
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.00109))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000219))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000438))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.984}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.967}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.934})
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.942}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.975}
        else if(input$bmi=="Obese (>30)"){0.949})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      
      ,
      
      ################################################################################
      # Stroke
      ################################################################################
      cod55[cod55$cause=="Stroke","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.965}
        else if(input$sex=="Female"){1.035})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.00157))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.000894))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000168))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000336))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.982}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.958}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.907})
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.942}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.978}
        else if(input$bmi=="Obese (>30)"){0.931})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      
      ,
      
      
      ################################################################################
      # Cancer                       
      ################################################################################
      cod55[cod55$cause=="Cancer","age"]
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.923}
        else if(input$sex=="Female"){1.077})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.00437))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.00149))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000164))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000328))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.985}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.970}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.944})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.937}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.978}
        else if(input$bmi=="Obese (>30)"){0.960})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      
      ,
      
      
      
      ################################################################################
      # COVID-19                      
      ################################################################################
      cod55[cod55$cause=="COVID-19","age"]
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.997}
        else if(input$sex=="Female"){1.038})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){1}
        else if(input$race=="Asian"){1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.00875))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.00242))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000320))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000640))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.985}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.970}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.942})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.926}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.977}
        else if(input$bmi=="Obese (>30)"){0.953})
      
      # MEDICAL HISTORY ==============================================================
      
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      ,
      
      
      #################################################################################
      #Alzeihmer's Disease
      #################################################################################
      cod55[cod55$cause=="Alzheimer’s Disease","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.974}
        else if(input$sex=="Female"){1.026})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.000602))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000105))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000210))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      #################################################################################
      # Chronic Lower Respiratory Diseases
      #################################################################################
      cod55[cod55$cause=="Chronic Lower Respiratory Diseases","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.00109))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.00109))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000223))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000446))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.985}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.970}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.942})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.931}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.014}
        else if(input$bmi=="Obese (>30)"){1.020})
      
      # MEDICAL HISTORY ==============================================================
      
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      
      #################################################################################
      # Diabetes
      #################################################################################
      cod55[cod55$cause=="Diabetes","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.000613))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000261))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000522))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.983}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.961}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.923})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.926}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      ,
      
      
      
      #################################################################################
      # Drug Overdose
      #################################################################################
      cod55[cod55$cause=="Drug Overdose","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.950}
        else if(input$sex=="Female"){1.025})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.00175))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.000530))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000211))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000422))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.979}
        else if(input$bmi=="Obese (>30)"){0.949})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      
      ,
      
      
      #################################################################################
      # Motor Vehicle Accident
      #################################################################################
      cod55[cod55$cause=="Motor Vehicle Accident","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.01137))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.000609))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      
      #################################################################################
      # Fall
      #################################################################################
      cod55[cod55$cause=="Fall","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.00122))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.000677))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000177))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000354))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.979}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.951}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.894})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.912}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.980}
        else if(input$bmi=="Obese (>30)"){0.967})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      
      #################################################################################
      # Influenza and Pneumonia
      #################################################################################
      cod55[cod55$cause=="Influenza and Pneumonia","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){1}
        else if(input$race=="Asian"){1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.00875))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.00162))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000109))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000218))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.985}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.970}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.942})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.902}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.974}
        else if(input$bmi=="Obese (>30)"){0.949})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      
      #################################################################################
      # Kidney Diseases
      #################################################################################
      cod55[cod55$cause=="Kidney Diseases","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.000435))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.0000842))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000168))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.984}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.967}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.934})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.936}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.977}
        else if(input$bmi=="Obese (>30)"){0.951})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      ,
      
      
      #################################################################################
      # Suicide
      #################################################################################
      cod55[cod55$cause=="Suicide","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.0175))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.000724))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000303))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000606))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      ,
      
      
      #################################################################################
      # Liver Diseases
      #################################################################################
      cod55[cod55$cause=="Liver Diseases","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.0175))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.000538))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000253))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000506))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.978}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.948}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.886})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.980}
        else if(input$bmi=="Obese (>30)"){0.957})
      
      # MEDICAL HISTORY ==============================================================
      
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      ,
      
      
      #################################################################################
      # Septicemia
      #################################################################################
      cod55[cod55$cause=="Septicemia","age"]
      
      
      
      # DEMOGRAPHICS =================================================================
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1-(input$drk*0.00131))      
      
      #Risk Factor: Smokes: smk
      *(1-(input$smk*0.000922))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1+(input$mpa*0.000223))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1+(input$hpa*0.000446))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.978}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.948}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.886})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.923}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
    )
    
    #///////////////////////////////////////////////////////////////////////////////
    # This piece of code is used so that all conditions Age of Death are anchored by the Current Age input
    # This is important for how plot reacts to the input Current Age
    #///////////////////////////////////////////////////////////////////////////////
    ,age = (ifelse(age-5<=input$cage,input$cage+5,age))
    
    ) %>%
      
      
      #################################################################################
    #################################################################################
    # Setting impact of Risk Factors on RISK OF DEATH from different cause of death  
    #################################################################################
    #################################################################################
    
    mutate(risk=c( 
      
      
      ################################################################################
      # Cardiovascular Diseases
      ################################################################################
      
      cod55[cod55$cause=="Cardiovascular Diseases","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #      *(if(input$cage==0){1}
      #        else if(input$cage>0&input$cage<5){0.00832} 
      #        else if(input$cage>=5&input$cage<15){0.00476}
      #        else if(input$cage>=15&input$cage<25){0.01843}
      #        else if(input$cage>=25&input$cage<35){0.06124}
      #        else if(input$cage>=35&input$cage<45){0.20987}
      #        else if(input$cage>=45&input$cage<55){0.62782}
      #        else if(input$cage>=55&input$cage<65){1.50000}
      #        else if(input$cage>=65&input$cage<75){3.50416}
      #        else if(input$cage>=75&input$cage<85){10.2652}
      #        else if(input$cage>=85){35.9744}
      #        )
      
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.219}
        else if(input$sex=="Female"){0.816})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.980}
        else if(input$race=="African (Black)"){1.389}
        else if(input$race=="Asian"){0.582}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.672}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.04))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0175))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.0016))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.0032))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.26}
        else if(input$bmi=="Obese (>30)"){1.76})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.06}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){2.57}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){4.98}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2.00}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.02}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ################################################################################
      # Coronary Heart Diseases
      ################################################################################
      
      
      cod55[cod55$cause=="Coronary Heart Diseases","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.375}
        else if(input$sex=="Female"){0.698})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.001}
        else if(input$race=="African (Black)"){1.233}
        else if(input$race=="Asian"){0.596}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.941}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.04))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0175))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.0016))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.0032))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.26}
        else if(input$bmi=="Obese (>30)"){1.76})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.06}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){2.57}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){4.98}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.02}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ################################################################################
      # Stroke
      ################################################################################
      
      
      cod55[cod55$cause=="Stroke","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.026}
        else if(input$sex=="Female"){0.964})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.964}
        else if(input$race=="African (Black)"){1.441}
        else if(input$race=="Asian"){0.809}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.670}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.04))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0126))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00133))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00266))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.3}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.7}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.21})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.5}
        else if(input$bmi=="Obese (>30)"){2})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.08}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){2.44}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){3.34}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.83}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Cancer
      ################################################################################
      
      cod55[cod55$cause=="Cancer","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.184}
        else if(input$sex=="Female"){0.863})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.016}
        else if(input$race=="African (Black)"){1.134}
        else if(input$race=="Asian"){0.614}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.636}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.58}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.58}
        else if(input$edu=="Primary Education (Elementary School)"){1.58}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.2))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.06523))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.0006))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.0012))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.21}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.33})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.29}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.285}
        else if(input$bmi=="Obese (>30)"){1.57})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){4.38}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){2.5}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      
      ################################################################################
      # COVID-19
      ################################################################################
      
      cod55[cod55$cause=="COVID-19","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.267}
        else if(input$sex=="Female"){0.784})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.934}
        else if(input$race=="African (Black)"){1.645}
        else if(input$race=="Asian"){0.741}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.518}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.38}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.58}
        else if(input$edu=="Primary Education (Elementary School)"){1.58}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.045))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0511))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00253))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00507))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.36})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.5}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.34}
        else if(input$bmi=="Obese (>30)"){1.85})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.77}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.55}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      
      ################################################################################
      # Alzheimer’s Disease
      ################################################################################
      
      cod55[cod55$cause=="Alzheimer’s Disease","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.781}
        else if(input$sex=="Female"){1.142})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.043}
        else if(input$race=="African (Black)"){0.951}
        else if(input$race=="Asian"){0.540}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.485}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0052))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.000833))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00166))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1.7}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Chronic Lower Respiratory Diseases
      ################################################################################
      
      cod55[cod55$cause=="Chronic Lower Respiratory Diseases","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.104}
        else if(input$sex=="Female"){0.923})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.069}
        else if(input$race=="African (Black)"){0.816}
        else if(input$race=="Asian"){0.280}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.657}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.0125))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.115))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00156))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00313))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.4})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.4}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.8}
        else if(input$bmi=="Obese (>30)"){0.77})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){3.42}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.59}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1.57}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      ,
      
      ################################################################################
      # Diabetes
      ################################################################################
      
      cod55[cod55$cause=="Diabetes","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.258}
        else if(input$sex=="Female"){0.786})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.903}
        else if(input$race=="African (Black)"){1.859}
        else if(input$race=="Asian"){0.750}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.512}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.7}
        else if(input$inc=="Middle"){1.5}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2.4}
        else if(input$edu=="Primary Education (Elementary School)"){1.6}
        else if(input$edu=="Secondary Education (High School)"){1.4}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0055))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00126))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00253))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.24}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.58}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.82})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.5}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){3.0}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){2.04}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.77}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){5}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      
      ################################################################################
      # Drug Overdose
      ################################################################################
      
      cod55[cod55$cause=="Drug Overdose","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.426}
        else if(input$sex=="Female"){0.578})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.058}
        else if(input$race=="African (Black)"){1.236}
        else if(input$race=="Asian"){0.155}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.946}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.6}
        else if(input$inc=="Lower-middle"){1.375}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.925}
        else if(input$inc=="Rich"){0.7})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.9}
        else if(input$edu=="Doctoral degree"){0.8})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.05))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.00342))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00166))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00333))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.24}
        else if(input$bmi=="Obese (>30)"){2})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Motor Vehicle Accident
      ################################################################################
      
      cod55[cod55$cause=="Motor Vehicle Accident","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.481}
        else if(input$sex=="Female"){0.534})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.992}
        else if(input$race=="African (Black)"){1.397}
        else if(input$race=="Asian"){0.313}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.344}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2.5}
        else if(input$edu=="Primary Education (Elementary School)"){2.0}
        else if(input$edu=="Secondary Education (High School)"){1.5}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.6})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.6))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0054))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){2}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1.5}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){2})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      ,
      
      ################################################################################
      # Fall
      ################################################################################
      
      cod55[cod55$cause=="Fall","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.214}
        else if(input$sex=="Female"){0.816})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.087}
        else if(input$race=="African (Black)"){0.495}
        else if(input$race=="Asian"){0.524}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.796}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2}
        else if(input$edu=="Primary Education (Elementary School)"){2}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.5}
        else if(input$edu=="Doctoral degree"){0.5})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.02))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0071))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.0014))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.0028))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.5}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){2.0}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.5})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.8}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.15}
        else if(input$bmi=="Obese (>30)"){1.31})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Influenza and Pneumonia
      ################################################################################
      
      cod55[cod55$cause=="Influenza and Pneumonia","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.215}
        else if(input$sex=="Female"){0.846})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.985}
        else if(input$race=="African (Black)"){1.292}
        else if(input$race=="Asian"){0.792}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.077}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.4}
        else if(input$inc=="Lower-middle"){1.2}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.8}
        else if(input$inc=="Rich"){0.6})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.6}
        else if(input$edu=="Primary Education (Elementary School)"){1.4}
        else if(input$edu=="Secondary Education (High School)"){1.2}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.6})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.45))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0311))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.000866))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00493))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.36})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){2}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.5}
        else if(input$bmi=="Obese (>30)"){2})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.77}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.55}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){2.15}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Kidney Diseases
      ################################################################################
      
      cod55[cod55$cause=="Kidney Diseases","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.213}
        else if(input$sex=="Female"){0.843})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.898}
        else if(input$race=="African (Black)"){2.024}
        else if(input$race=="Asian"){0.638}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.882}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.001))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.000666))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00133))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.3}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.34}
        else if(input$bmi=="Obese (>30)"){1.94})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1.64}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.22}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1.75}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.29}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Suicide
      ################################################################################
      
      cod55[cod55$cause=="Suicide","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.630}
        else if(input$sex=="Female"){0.407})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.126}
        else if(input$race=="African (Black)"){0.556}
        else if(input$race=="Asian"){0.474}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.244}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*1.0))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0083))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.0024))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.0048))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.5}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.85}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){2.58}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Liver Diseases
      ################################################################################
      
      cod55[cod55$cause=="Liver Diseases","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.316}
        else if(input$sex=="Female"){0.707})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.083}
        else if(input$race=="African (Black)"){0.662}
        else if(input$race=="Asian"){0.308}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){2.850}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*1.0))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0036))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00133))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00266))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.56}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){2.13}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.69})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.16}
        else if(input$bmi=="Obese (>30)"){1.69})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){4.7}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Septicemia
      ################################################################################
      
      cod55[cod55$cause=="Septicemia","risk"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.113}
        else if(input$sex=="Female"){0.918})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.959}
        else if(input$race=="African (Black)"){1.680}
        else if(input$race=="Asian"){0.464}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.948}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2}
        else if(input$edu=="Primary Education (Elementary School)"){2}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){2}
        else if(input$edu=="Master's degree"){0.5}
        else if(input$edu=="Doctoral degree"){0.5})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.025))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0133))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00156))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00313))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.56}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1.4}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.4}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2.2}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1.3}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.5}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1.5}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      
      
    )
    
    
    ) %>%
      
      #################################################################################
    #################################################################################
    # Setting impact of Risk Factors on POPULATION associated with different cods   
    #################################################################################
    #################################################################################   
    
    
    
    mutate(pop=c( 
      
      ################################################################################
      # Cardiovascular Diseases
      ################################################################################
      
      cod55[cod55$cause=="Cardiovascular Diseases","pop"]
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.219}
        else if(input$sex=="Female"){0.816})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.980}
        else if(input$race=="African (Black)"){1.389}
        else if(input$race=="Asian"){0.582}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.672}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.04))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0175))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.0016))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.0032))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.26}
        else if(input$bmi=="Obese (>30)"){1.76})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.06}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){4.98}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2.00}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.02}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ################################################################################
      # Coronary Heart Diseases
      ################################################################################
      
      
      cod55[cod55$cause=="Coronary Heart Diseases","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.375}
        else if(input$sex=="Female"){0.698})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.001}
        else if(input$race=="African (Black)"){1.233}
        else if(input$race=="Asian"){0.596}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.941}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.04))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0175))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.0016))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.0032))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.26}
        else if(input$bmi=="Obese (>30)"){1.76})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.06}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){4.98}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.02}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ################################################################################
      # Stroke
      ################################################################################
      
      
      cod55[cod55$cause=="Stroke","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.026}
        else if(input$sex=="Female"){0.964})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.964}
        else if(input$race=="African (Black)"){1.441}
        else if(input$race=="Asian"){0.809}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.670}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.04))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0126))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00133))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00266))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.3}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.7}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.21})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.5}
        else if(input$bmi=="Obese (>30)"){2})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.08}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){3.34}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.83}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Cancer
      ################################################################################
      
      cod55[cod55$cause=="Cancer","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.184}
        else if(input$sex=="Female"){0.863})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.016}
        else if(input$race=="African (Black)"){1.134}
        else if(input$race=="Asian"){0.614}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.636}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.58}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.58}
        else if(input$edu=="Primary Education (Elementary School)"){1.58}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.2))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.06523))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.000333))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.000666))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.21}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.33})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.29}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.285}
        else if(input$bmi=="Obese (>30)"){1.57})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){4.38}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){2.5}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      
      ################################################################################
      # COVID-19
      ################################################################################
      
      cod55[cod55$cause=="COVID-19","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.267}
        else if(input$sex=="Female"){0.784})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.934}
        else if(input$race=="African (Black)"){1.645}
        else if(input$race=="Asian"){0.741}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.518}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.38}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.58}
        else if(input$edu=="Primary Education (Elementary School)"){1.58}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.045))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0511))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00253))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00507))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.36})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.5}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.34}
        else if(input$bmi=="Obese (>30)"){1.85})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.77}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.55}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      
      ################################################################################
      # Alzheimer’s Disease
      ################################################################################
      
      cod55[cod55$cause=="Alzheimer’s Disease","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.781}
        else if(input$sex=="Female"){1.142})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.043}
        else if(input$race=="African (Black)"){0.951}
        else if(input$race=="Asian"){0.540}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.485}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0052))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.000833))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00166))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1.7}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Chronic Lower Respiratory Diseases
      ################################################################################
      
      cod55[cod55$cause=="Chronic Lower Respiratory Diseases","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.104}
        else if(input$sex=="Female"){0.923})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.069}
        else if(input$race=="African (Black)"){0.816}
        else if(input$race=="Asian"){0.280}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.657}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.0125))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.115))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00156))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00313))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.4})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.4}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.8}
        else if(input$bmi=="Obese (>30)"){0.77})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){3.42}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.59}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1.57}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      ,
      
      ################################################################################
      # Diabetes
      ################################################################################
      
      cod55[cod55$cause=="Diabetes","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.258}
        else if(input$sex=="Female"){0.786})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.903}
        else if(input$race=="African (Black)"){1.859}
        else if(input$race=="Asian"){0.750}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.512}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.7}
        else if(input$inc=="Middle"){1.5}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2.4}
        else if(input$edu=="Primary Education (Elementary School)"){1.6}
        else if(input$edu=="Secondary Education (High School)"){1.4}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0055))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00126))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00253))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.24}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.58}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.82})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.5}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){3.0}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){2.04}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.77}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){5}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      
      ################################################################################
      # Drug Overdose
      ################################################################################
      
      cod55[cod55$cause=="Drug Overdose","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.426}
        else if(input$sex=="Female"){0.578})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.058}
        else if(input$race=="African (Black)"){1.236}
        else if(input$race=="Asian"){0.155}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.946}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.6}
        else if(input$inc=="Lower-middle"){1.375}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.925}
        else if(input$inc=="Rich"){0.7})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.9}
        else if(input$edu=="Doctoral degree"){0.8})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.24}
        else if(input$bmi=="Obese (>30)"){2})
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      -(if(is.null(input$sys)){0}
        else if(input$sys=="Normal (SBP <120 mmHG)"){3}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){5}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){10})
      
      
      #Risk Factor: Body Mass Index: bmi
      +(if(is.null(input$bmi)){0}
        else if(input$bmi=="Underweight (<18.5)"){5}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){0}
        else if(input$bmi=="Overweight (25-29.9)"){10}
        else if(input$bmi=="Obese (>30)"){30})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Motor Vehicle Accident
      ################################################################################
      
      cod55[cod55$cause=="Motor Vehicle Accident","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.481}
        else if(input$sex=="Female"){0.534})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.992}
        else if(input$race=="African (Black)"){1.397}
        else if(input$race=="Asian"){0.313}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.344}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2.5}
        else if(input$edu=="Primary Education (Elementary School)"){2.0}
        else if(input$edu=="Secondary Education (High School)"){1.5}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.6})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.6))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0054))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){2}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1.5}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){2})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      ,
      
      ################################################################################
      # Fall
      ################################################################################
      
      cod55[cod55$cause=="Fall","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.214}
        else if(input$sex=="Female"){0.816})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.087}
        else if(input$race=="African (Black)"){0.495}
        else if(input$race=="Asian"){0.524}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.796}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2}
        else if(input$edu=="Primary Education (Elementary School)"){2}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.5}
        else if(input$edu=="Doctoral degree"){0.5})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.02))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0071))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.0014))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.0028))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.5}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){2.0}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.5})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.8}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.15}
        else if(input$bmi=="Obese (>30)"){1.31})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Influenza and Pneumonia
      ################################################################################
      
      cod55[cod55$cause=="Influenza and Pneumonia","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.215}
        else if(input$sex=="Female"){0.846})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.985}
        else if(input$race=="African (Black)"){1.292}
        else if(input$race=="Asian"){0.792}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.077}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.4}
        else if(input$inc=="Lower-middle"){1.2}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.8}
        else if(input$inc=="Rich"){0.6})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.6}
        else if(input$edu=="Primary Education (Elementary School)"){1.4}
        else if(input$edu=="Secondary Education (High School)"){1.2}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.6})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.45))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0311))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.000866))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00493))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.36})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){2}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.5}
        else if(input$bmi=="Obese (>30)"){2})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.77}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.55}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){2.15}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Kidney Diseases
      ################################################################################
      
      cod55[cod55$cause=="Kidney Diseases","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.213}
        else if(input$sex=="Female"){0.843})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.898}
        else if(input$race=="African (Black)"){2.024}
        else if(input$race=="Asian"){0.638}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.882}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.001))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.000666))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00133))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.3}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.34}
        else if(input$bmi=="Obese (>30)"){1.94})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.22}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1.75}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.29}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Suicide
      ################################################################################
      
      cod55[cod55$cause=="Suicide","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.630}
        else if(input$sex=="Female"){0.407})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.126}
        else if(input$race=="African (Black)"){0.556}
        else if(input$race=="Asian"){0.474}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.244}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*1.0))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0083))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.0024))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.0048))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.5}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.85}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){2.58}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Liver Diseases
      ################################################################################
      
      cod55[cod55$cause=="Liver Diseases","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.316}
        else if(input$sex=="Female"){0.707})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.083}
        else if(input$race=="African (Black)"){0.662}
        else if(input$race=="Asian"){0.308}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){2.850}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*1.0))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0036))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00133))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00266))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.56}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){2.13}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.69})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.16}
        else if(input$bmi=="Obese (>30)"){1.69})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){4.7}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      
      
      ,
      
      ################################################################################
      # Septicemia
      ################################################################################
      
      cod55[cod55$cause=="Septicemia","pop"]
      
      
      # DEMOGRAPHICS ===============================================================
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #Risk Factor: Sex: sex
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.113}
        else if(input$sex=="Female"){0.918})
      
      
      #Risk Factor: Race: race
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.959}
        else if(input$race=="African (Black)"){1.680}
        else if(input$race=="Asian"){0.464}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.948}
        else if(input$race=="Other"){1})
      
      # SOCIAL STATUS ================================================================
      
      #Risk Factor: Income Group: inc
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #Risk Factor: #Education (highest achieved degree): edu
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2}
        else if(input$edu=="Primary Education (Elementary School)"){2}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){2}
        else if(input$edu=="Master's degree"){0.5}
        else if(input$edu=="Doctoral degree"){0.5})
      
      #SS1  #Marital/Significant Relationship Status (x) (TBD)
      
      
      # LIFESTYLE ====================================================================
      
      #Risk Factor: Drinks: drk
      *(1+(input$drk*0.025))      
      
      #Risk Factor: Smokes: smk
      *(1+(input$smk*0.0133))
      
      #Risk Factor: Number of moderate intensity physical activity per week: mpa----> remind users that + does not cancel - lifestyle choices
      *(1-(input$mpa*0.00156))
      
      #Risk Factor: Number of high intensity physical activity per week: hpa
      *(1-(input$hpa*0.00313))
      
      # VITALS =======================================================================
      
      #Risk Factor: Systolic Blood Pressure: sys
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      
      #Risk Factor: Body Mass Index: bmi
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.56}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      # MEDICAL HISTORY ==============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: hbp
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: hbc
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: cvd
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1.4}
        else if(input$cvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: copd
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.4}
        else if(input$copd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: dia
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2.2}
        else if(input$dia=="No"){1})
      
      #Risk Factor: Medical History - Depression: dep
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1.3}
        else if(input$dep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: can
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.5}
        else if(input$can=="No"){1})
      
      
      #Risk Factor: Medical History - Alzheimer: alz
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1.5}
        else if(input$alz=="No"){1})
      
      
      # FAMILY HISTORY ===============================================================
      
      #Risk Factor: Medical History - High Blood Pressure: fhbp
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #Risk Factor: Medical History - High Blood Cholesterol: fhbc
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #Risk Factor: Medical History - Cardiovascular Disease: fcvd
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #Risk Factor: Medical History - Chronic Obstructive Pulmonary Disease: fcopd
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #Risk Factor: Medical History - Diabetes: fdia
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #Risk Factor: Medical History - Depression: fdep
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #Risk Factor: Medical History - Cancer: fcan
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #Risk Factor: Medical History - Alzheimer: falz
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      
      # CONCOMITANT MEDICATIONS ======================================================
      
      #Risk Factor: Immunosuppressants: cmi
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
    )) %>%
      
      # Use "mutate()" to set values for ggplot
      
      mutate(risk = round(risk)) %>%
      mutate(age = round(age)) %>%
      mutate(annotation=cause) %>%
      # calculating the probability of dying from the different cause using their respective risks,
      # removing the risk related to Stroke and CHD because those risks are also part of the CVD risk
      mutate(probability = round(risk/(sum(risk[cause!="Stroke" & cause!="Coronary Heart Diseases"]))*100,1)) %>%
      arrange(desc(pop)) %>%
      mutate(cause = factor(cause, cause)) %>%
      mutate(text = paste("Cause: ", cause, "\nRisk (n/100,000): ", risk, "\nAverage Age of Death: ", age, sep=""))
    #     mutate(text = paste("Cause: ", cause, "\nPopulation (M): ", pop, "\nRisk: ", risk, "\nAge of Death: ", age, sep=""))    
    
    
    
    
  })  
  
  
  ###########################################################################################
  #
  #
  #THE NEW REALITY IS HERE
  #
  #
  ###########################################################################################
  
  
  
  # ({
  
  #output$textcause <- renderText({ paste("Probably from", cod77()[which.max(cod77()$risk),5] ,"at the age of", cod77()[which.max(cod77()$risk),"age"] ) })  
  
  #   output$textcause <- renderText({ cod77()[which.max(cod77()$risk),5] }) 
  
  #    })
  
  #  })
  
  
  #  ({
  
  #maxcod <- max(cod55$age)
  
  #    output$textage <- renderText({ cod77()[which.max(cod77()$risk),"age"]  })
  #output$text <- renderText({ max(cod77()$age) })
  #  })
  
  
  
  
  
  #    output$textcause2 <- renderText({ cod77()[Rfast::nth(cod77()$risk, 6, descending = T),"age"]  })
  
  #    output$textcause2 <- renderText({ cod77()[which.maxn(cod77()$risk,6), 5]  })
  
  #create textoutput for all of the causes in order of increasing risk.
  ({  output$textcause1 <- renderText({ cod77()[order(-cod77()$risk)[1],5] }) })
  ({  output$textcause2 <- renderText({ cod77()[order(-cod77()$risk)[2],5] }) })
  ({  output$textcause3 <- renderText({ cod77()[order(-cod77()$risk)[3],5] }) })
  ({  output$textcause4 <- renderText({ cod77()[order(-cod77()$risk)[4],5] }) })
  ({  output$textcause5 <- renderText({ cod77()[order(-cod77()$risk)[5],5] }) })
  ({  output$textcause6 <- renderText({ cod77()[order(-cod77()$risk)[6],5] }) })
  ({  output$textcause7 <- renderText({ cod77()[order(-cod77()$risk)[7],5] }) })
  ({  output$textcause8 <- renderText({ cod77()[order(-cod77()$risk)[8],5] }) })
  ({  output$textcause9 <- renderText({ cod77()[order(-cod77()$risk)[9],5] }) })
  ({  output$textcause10 <- renderText({ cod77()[order(-cod77()$risk)[10],5] }) })
  ({  output$textcause11 <- renderText({ cod77()[order(-cod77()$risk)[11],5] }) })
  ({  output$textcause12 <- renderText({ cod77()[order(-cod77()$risk)[12],5] }) })
  ({  output$textcause13 <- renderText({ cod77()[order(-cod77()$risk)[13],5] }) })
  ({  output$textcause14 <- renderText({ cod77()[order(-cod77()$risk)[14],5] }) })
  ({  output$textcause15 <- renderText({ cod77()[order(-cod77()$risk)[15],5] }) })
  ({  output$textcause16 <- renderText({ cod77()[order(-cod77()$risk)[16],5] }) })
  
  #create textoutput for all of the causes probability in order of increasing risk.
  ({  output$textprob1 <- renderText({ cod77()[order(-cod77()$risk)[1],6] }) })
  ({  output$textprob2 <- renderText({ cod77()[order(-cod77()$risk)[2],6] }) })
  ({  output$textprob3 <- renderText({ cod77()[order(-cod77()$risk)[3],6] }) })
  ({  output$textprob4 <- renderText({ cod77()[order(-cod77()$risk)[4],6] }) })
  ({  output$textprob5 <- renderText({ cod77()[order(-cod77()$risk)[5],6] }) })
  ({  output$textprob6 <- renderText({ cod77()[order(-cod77()$risk)[6],6] }) })
  ({  output$textprob7 <- renderText({ cod77()[order(-cod77()$risk)[7],6] }) })
  ({  output$textprob8 <- renderText({ cod77()[order(-cod77()$risk)[8],6] }) })
  ({  output$textprob9 <- renderText({ cod77()[order(-cod77()$risk)[9],6] }) })
  ({  output$textprob10 <- renderText({ cod77()[order(-cod77()$risk)[10],6] }) })
  ({  output$textprob11 <- renderText({ cod77()[order(-cod77()$risk)[11],6] }) })
  ({  output$textprob12 <- renderText({ cod77()[order(-cod77()$risk)[12],6] }) })
  ({  output$textprob13 <- renderText({ cod77()[order(-cod77()$risk)[13],6] }) })
  ({  output$textprob14 <- renderText({ cod77()[order(-cod77()$risk)[14],6] }) })
  ({  output$textprob15 <- renderText({ cod77()[order(-cod77()$risk)[15],6] }) })
  ({  output$textprob16 <- renderText({ cod77()[order(-cod77()$risk)[16],6] }) })
  
  #create textoutput for all of the causes age in order of increasing risk.
  ({  output$textage1 <- renderText({ cod77()[order(-cod77()$risk)[1],"age"] }) })
  ({  output$textage2 <- renderText({ cod77()[order(-cod77()$risk)[2],"age"] }) })
  ({  output$textage3 <- renderText({ cod77()[order(-cod77()$risk)[3],"age"] }) })
  ({  output$textage4 <- renderText({ cod77()[order(-cod77()$risk)[4],"age"] }) })
  ({  output$textage5 <- renderText({ cod77()[order(-cod77()$risk)[5],"age"] }) })
  ({  output$textage6 <- renderText({ cod77()[order(-cod77()$risk)[6],"age"] }) })
  ({  output$textage7 <- renderText({ cod77()[order(-cod77()$risk)[7],"age"] }) })
  ({  output$textage8 <- renderText({ cod77()[order(-cod77()$risk)[8],"age"] }) })
  ({  output$textage9 <- renderText({ cod77()[order(-cod77()$risk)[9],"age"] }) })
  ({  output$textage10 <- renderText({ cod77()[order(-cod77()$risk)[10],"age"] }) })
  ({  output$textage11 <- renderText({ cod77()[order(-cod77()$risk)[11],"age"] }) })
  ({  output$textage12 <- renderText({ cod77()[order(-cod77()$risk)[12],"age"] }) })
  ({  output$textage13 <- renderText({ cod77()[order(-cod77()$risk)[13],"age"] }) })
  ({  output$textage14 <- renderText({ cod77()[order(-cod77()$risk)[14],"age"] }) })
  ({  output$textage15 <- renderText({ cod77()[order(-cod77()$risk)[15],"age"] }) })
  ({  output$textage16 <- renderText({ cod77()[order(-cod77()$risk)[16],"age"] }) })
  
  
  
  
  
  
  
  ({
    #    observeEvent(input$dimension,{  #this line is to make the prot's height responsive to window's height.
    
    output$bubble2 <- renderPlotly({
      ggplotly(              
        #                 mutate(annotation=cause) %>%
        #                 arrange(desc(pop)) %>%
        #                 mutate(cause = factor(cause, cause)) %>%
        #                 mutate(text = paste("Cause: ", cause, "\nPopulation (M): ", pop, "\nRisk: ", risk, "\nAge of Death: ", age, sep="")) %>%
        ggplot(cod77(), aes(x=age, y=probability, size = pop, color = cause, text=text)) +
          geom_point(alpha=0.6) + #use this line to make bubble +or- transparent
          scale_size(range = c(2, 30), name="Population (M)") +
          scale_color_viridis(discrete=TRUE, guide='none') +
          theme_ipsum() +
          #        theme_bw(legend.key.size = unit(0.1, "cm")) +
          #        theme(legend.key.size = unit(3, "lines")) +
          #   theme(legend.spacing.y = unit(0.05, 'cm')) +
          #   guides(fill = guide_legend(byrow = TRUE)) +
          #layout(legend=list('top') +
          #theme(legend.position='top') +
          #layout(legend = list(orientation='h',)) +
          scale_x_continuous(limits = c(input$cage, 100), breaks=c(10,20,30,40,50,60,70,80,90,100)) + #use this line to specify the x-axis range and tick positions
          scale_y_continuous(limits=c(0, NA), expand = expansion(mult = c(NA, 0.20)))
        #theme(legend.position="top", legend.title = element_blank())
        #layout(legend=list(orientation="h"))
        #use this last line to specify the y-axis range and clipping data below limit
        #        ,
        #        height="100%"
        ,
        tooltip="text") %>% 
        #layout(legend=list(orientation="h",x = 0.45, y = 0.2))
        
        #the code below is related to legend display.
        layout(legend=list(xanchor='Right',
                           #x=-0.1,
                           yanchor='top',
                           #y=1.8,
                           #orientation='h',
                           title = NA,
                           itemsizing='constant'
                           #spacing.y = unit(0.1, "cm"),
                           #guide_legend = "byrow"
                           #spacing.x=unit(0.1,"cm")
        )) 
      
      
      
      
      #the code below sorts of work to put the legend on the top of the chart, but it does not resize perfectly.
      
      #layout(legend=list(xanchor='left',
      #                   x=-0.1,
      #                   yanchor='top',
      #                   y=1.8,
      #                   orientation='h',
      #                   title = NA,
      #                   itemsizing='constant'))
      #   })
      
    })
    
    
    
  })
  
  
  
  
}      

#this knits together the ui and the server function.
shinyApp(ui = ui, server = server)

#Use this code to deploy the app (you can also click the "publish app" button after running the app:
# rsconnect::deployApp('C:/Users/ad905068/OneDrive - BioMarin/Documents/R_for_BMRN/Shiny2')

################################################################################
# BELOW IS TRASH
################################################################################

#---------------------------------------------------------------------------------
#BELOW IS A COPY OF THE WORKING SERVER FUNCTION. DONT TOUCH IT

#this is a function for the server to use to create the R components for the app.

#inactivate/activate all below

#server <- function(input, output){
#    output$bubble <- renderPlotly({
#        ggplotly(data %>%
#            mutate(annotation=country) %>%
#            mutate(gdpPercap=round(gdpPercap,0)) %>%
#            mutate(pop=round(pop/1000000,2)) %>%
#            mutate(lifeExp=round(lifeExp,1)) %>%
#            arrange(desc(pop)) %>%
#            mutate(country = factor(country, country)) %>%
#            mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
#            ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
#                geom_point(alpha=0.7) +
#                scale_size(range = c(1.4, 19), name="Population (M)") +
#                scale_color_viridis(discrete=TRUE, guide=FALSE) +
#                theme_ipsum() +
#                theme(legend.position="none") +
#                scale_x_continuous(limits = c(2000, 40000)), #use this last line to specify the x-axis range and clipping data below limit
#                #coord_cartesian(xlim = c(10000, 40000)), #use this last line to specify the x-axis range wothout clipping data below limit
#        tooltip="text")
#    })
#}      

#ABOVE IS A COPY OF THE WORKING SERVER FUNCTION. DONT TOUCH IT
#---------------------------------------------------------------------------------



#////////////////////////////////////////////////////////////////////////////////
# Potential Risk Factors
#////////////////////////////////////////////////////////////////////////////////

#DEMOGRAPHICS
#AGE  #Current Age
#SEX  #Sex
#RACE  #Race

#SOCIAL STATUS
#INC  #Income Group
#EDU  #Education (highest achieved degree)
#SS1  #Marital Status (x)
#SS2  #Number of Kids (x)

#LIFESTYLE
#DRK  #Weekly Drinks
#SMK  #Weekly Smokes
#HPA  #Weekly Hours of high intensity physical activity
#MPA  #Weekly Hours of moderate intensity physical activity

#VITALS
#BMI  #Body Mass Index
#WGH  #Weight
#HGH  #Height
#SYS  #Systolic Blood Pressure mm/Hg

#MEDICAL HISTORY
#HBP  #High Blood Pressure (Y/N)
#HBC  #High Blood Cholesterol (Y/N)
#DIA  #Diabetes (Y/N)
#DEP  #Depression (Y/N)

#CONMEDS
#CMI  #Immunosuppression
#CMA  #Infectious Agents

#FAMILY HISTORY
#CVD  #CVD
#CAN  #Cancer
#COP  #COPD
#ALZ  #Alzheimer
#FDI  #Diabetes
#FDE  #Depression
