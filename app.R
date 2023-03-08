# This is a Shiny web application named "youlldie" that runs a model for statistically predicting 
# one's cause and age of death based on one's inherited risk factors and lifestyle choices.

# The steps followed to build this model are the following:

# 1. A dataframe is built to tabulate the most common causes of death and their 
# baseline AGE OF DEATH (AGE), RISK OF DEATH (RISK) and RATE OF DEATH (RATE). 
# * Baseline AGE corresponds to the average age of death associated with each cause of death 
# * Baseline RISK corresponds to the death rate associated with each causes of 
# death (n / 100,000) divided by the sum of the death rates associated with all 
# causes of death. As such, RISK is a probability that death from a given cause 
# will happen. It is a value confined between 0 and 1.  
# * Baseline RATE corresponds to the total population dying from each cause of 
# death per year. It is also known as the Crude Death Rate.    

# 2. Values associated with the risk factors parameters impacts on the baseline 
# AGE, RISK and RATE for different causes of death are set. For examples, "male"
# and "female" are the two parameters of the risk factor "sex". The value for 
# the impact of the "male" parameter on AGE is <1 for the cause of death 
# "cardiovascular diseases" because male die from cadiovascular diseases at a 
# younger age than the average population composed of males and female. The 
# values of the risk factors parameters impacts act as multiplier of the baseline
# AGE, RISK and RATE. Namely, risk factors parameters with impact values >1 
# increase the baseline AGE, RISK and RATE values whereas risk factors parameters 
# with impact values <1 decrease the baseline AGE, RISK and RATE values. Risk 
# factors parameters that increase AGE and decrease RISK and RATE are beneficial. 
# Risk factors parameter that decrease AGE and increase RISK and RATE are detrimental. 

# 3. The calculations performed above yield an updated dataframe which is plotted 
# as a bubble plot with the AGE of Death on the x-axis and the RISK of Death on 
# the y-axis. The size of the bubbles corresponds to the RATE of death aka the Crude Death Rate.

# Packages ---------------------------------------------------------------------
# load required packages:
library(shiny)
library(hrbrthemes)
library(viridis)
library(gridExtra)
library(ggrepel)
library(plotly)
library(ggplot2)
library(dplyr)

# UI ---------------------------------------------------------------------------
# the following section is to build a ui object that lays out a webpage (html) for the app (it converts R -> html)

ui <- fluidPage(
  
  ## Risk Factors ----------------------------------------------------------------
  # The following section is to add input functions corresponding to Risk Factors (e.g.: Sex, Race, # of drinks/week, etc.) 
  # Individual Risk Factors are grouped by Domain (e.g.: DEMOGRAPHICS, SOCIAL STATUS, etc.)
  
  sidebarPanel(
    style = "overflow-y:scroll;position:relative;max-height:100vh",
    
    ### DEMOGRAPHICS ---------------------------------------------------------------
    
    fluidRow(
      tags$h4("DEMOGRAPHICS")),
    
    #### Current Age ----
    
    fluidRow(
      sliderInput(inputId="cage",
                  label="Age",
                  value=0, min=0, max=100)
    ),
    
    #### Sex ----
    
    fluidRow(
      radioButtons(inputId="sex", label="Sex", choices=list("Male",
                                                            "Female"), 
                   selected=character(0))
    ),
    
    #### Race ----
    
    fluidRow(
      radioButtons(inputId="race", label="Race", choices=list("Caucasian (White)",
                                                              "African (Black)",
                                                              "Asian",
                                                              "Middle Eastern (Indian)",
                                                              "Native American",
                                                              "Other"), 
                   selected=character(0))
    ),
    
    #### World Region ----
    
    fluidRow(
      radioButtons(inputId="wbr", label="World Region", choices=list("East Asia & Pacific",
                                                                     "Europe & Central Asia",
                                                                     "Latin America & Caribbean",
                                                                     "Middle East & North Africa",
                                                                     "North America",
                                                                     "South Asia",
                                                                     "Sub-Saharan Africa"), 
                   selected=character(0))
    ),
    
    
    ### SOCIAL STATUS --------------------------------------------------------------
    
    fluidRow(
      tags$h4("SOCIAL STATUS")),
    
    #### Income Group ----
    
    fluidRow(
      radioButtons(inputId="inc", label="Income Group", choices=list("Poor",
                                                                     "Lower-middle",
                                                                     "Middle",
                                                                     "Upper-middle",
                                                                     "Rich"),
                   selected=character(0))
    ),
    
    #### Education ----
    
    fluidRow(
      radioButtons(inputId="edu", label="What is your highest grade/level of school/degree?", choices=list("No Formal Schooling",
                                                                                                           "Primary Education (Elementary School)",
                                                                                                           "Secondary Education (High School)",
                                                                                                           "Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program",
                                                                                                           "Master's degree",
                                                                                                           "Doctoral degree"),
                   selected=character(0))
    ),
    
    ### LIFESTYLE ------------------------------------------------------------------
    
    fluidRow(
      tags$h4("LIFESTYLE")),
    
    #### Weekly Drinks ----
    
    fluidRow(
      sliderInput(inputId="drk",
                  label="Number of drinks per week",
                  value=0, min=0, max=20)
    ),
    
    #### Weekly Smokes ----
    
    fluidRow(
      sliderInput(inputId="smk",
                  label="Number of smokes per week",
                  value=0, min=0, max=140)
    ),
    
    #### Number of moderate intensity physical activity minutes per week ----
    
    fluidRow(
      sliderInput(inputId="mpa",
                  label="Number of minutes of moderate intensity physical activity per week",
                  value=0, min=0, max=300)
    ),
    
    #### Number of high intensity physical activity minutes per week ----
    
    fluidRow(
      sliderInput(inputId="hpa",
                  label="Number of minutes of vigorous intensity physical activity per week",
                  value=0, min=0, max=100)
    ),
    
    #### Number of Hours of Sleep per Day ----
    
    fluidRow(
      sliderInput(inputId="hsd",
                  label="Number of hours of sleep per day",
                  value=7, min=0, max=24)
    ),  
    
    
    ### VITALS ---------------------------------------------------------------------
    
    fluidRow(
      tags$h4("VITALS")),
    
    #### Systolic Blood Pressure (mm/Hg) ----
    
    fluidRow(
      radioButtons(inputId="sys", label="Blood Pressure", choices=list("Normal (SBP <120 mmHG)",
                                                                       "Elevated (SBP 120-129 mmHG)",
                                                                       "High Blood Pressure Stage 1 (SBP 130-140 mmHG)",
                                                                       "High Blood Pressure Stage 2 (SBP >140 mmHG)"),
                   selected=character(0))
    ),            
    
    #### Body Mass Index ----
    
    fluidRow(
      radioButtons(inputId="bmi", label="Body Mass Index", choices=list("Underweight (<18.5)",
                                                                        "Normal Weight (18.5-24.9)",
                                                                        "Overweight (25-29.9)",
                                                                        "Obese (>30)"),
                   selected=character(0))
    ),
    
    ### MEDICAL HISTORY ------------------------------------------------------------
    
    fluidRow(
      tags$h4("MEDICAL HISTORY")),
    fluidRow(
      tags$p("Are you currently living with the following conditions?")),
    
    #### High Blood Cholesterol ----
    
    fluidRow(
      radioButtons(inputId="hbc",
                   label="High Blood Cholesterol", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Cardiovascular Disease (CVD) ----
    
    fluidRow(
      radioButtons(inputId="cvd",
                   label="Cardiovascular Disease", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Chronic Obstructive Pulmonary Disease (COPD) ----
    
    fluidRow(
      radioButtons(inputId="copd",
                   label="Chronic Obstructive Pulmonary Disease", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Diabetes ----
    
    fluidRow(
      radioButtons(inputId="dia",
                   label="Diabetes", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Depression ----
    
    fluidRow(
      radioButtons(inputId="dep",
                   label="Depression", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Cancer ----
    
    fluidRow(
      radioButtons(inputId="can",
                   label="Cancer", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Alzheimer ----
    
    fluidRow(
      radioButtons(inputId="alz",
                   label="Alzheimer", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    ### FAMILY HISTORY ===---------------------------------------------------------
    
    fluidRow(
      tags$h4("FAMILY HISTORY")),
    fluidRow(
      tags$p("Has someone in your family (Father, Mother, Brother, Sister experienced the following conditions?")),
    
    #### Family History of High Blood Pressure ----
    
    fluidRow(
      radioButtons(inputId="fhbp",
                   label="Family History of High Blood Pressure", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Family History of High Blood Cholesterol ----
    
    fluidRow(
      radioButtons(inputId="fhbc",
                   label="Family History of High Blood Cholesterol", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Family History of Cardiovascular Disease (CVD) ----
    
    fluidRow(
      radioButtons(inputId="fcvd",
                   label="Family History of Cardiovascular Disease", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Family History of Chronic Obstructive Pulmonary Disease (COPD) ----
    
    fluidRow(
      radioButtons(inputId="fcopd",
                   label="Family History of Chronic Obstructive Pulmonary Disease", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Family History of Diabetes ----
    
    fluidRow(
      radioButtons(inputId="fdia",
                   label="Family History of Diabetes", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Family History of Depression ----
    
    fluidRow(
      radioButtons(inputId="fdep",
                   label="Family History of Depression", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Family History of Cancer ----
    
    fluidRow(
      radioButtons(inputId="fcan",
                   label="Family History of Cancer", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    #### Family History of Alzheimer ----
    
    fluidRow(
      radioButtons(inputId="falz",
                   label="Family History of Alzheimer", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
    ### CONCOMITAN MEDICATIONS ----------------------------------------------------
    
    fluidRow(
      tags$h4("CONCOMITANT MEDICATIONS")),
    
    #### Immunosuppression ---
    
    fluidRow(
      radioButtons(inputId="cmi",
                   label="Are you taking immunosuppressants?", 
                   choices=list("Yes",
                                "No"),
                   selected=character(0))
    ),
    
  ),
  
  ## Main Panel -------------------------------------------------------------------
  # The following section is to indicate what to display on the mainPanel
  
  mainPanel(
    
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
    
    ## Plot Window Fitting ----
    # Here are parameters related to the bubble-plot's window fitting
    
    plotlyOutput("bubble", height="auto", width="auto"),
    
  ),
  
)

# DATAFRAME --------------------------------------------------------------------
# The following section is to build the dataframe that the plot is based on

cod <- data.frame(
  
  ## CAUSES OF DEATH ----
  # Provide the list of leading causes of death here:
  cause = c("Cardiovascular Diseases",
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
  
  ## AGE OF DEATH ----                  
  # Enter the average age of death from each cause of death here:                 
  age = c(67.3,  #Cardiovascular Diseases
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
  
  ## RISK OF DEATH ----                
  # Enter the baseline risk of dying from each cause of death (n/100,000) here:                  
  risk = c(224.4, #Cardiovascular Diseases
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
  
  ## RATE OF DEATH ----                  
  # Enter the total population dying form each cause of death per year (Crude Death Rate) here:                
  pop = c(813804, #Cardiovascular Diseases
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

# SERVER -----------------------------------------------------------------------
# The Section below serve to define the "server" function for the server to create/use the R components for the app.

server <- function(input, output){
  
  #converting the cause of death dataframe (cod) into a reactive function / dataframe that changes according to inputs.  
  
  cod_react<-reactive({cod %>%
      
      # AGE OF DEATH -----------------------------------------------------------------
    # Setting the impact of Risk Factors on the baseline AGE OF DEATH for different causes of death  
    
    mutate(age=c(
      
      ## Cardiovascular Diseases -----------------------------------------------------
      
      cod[cod$cause=="Cardiovascular Diseases","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.955}
        else if(input$sex=="Female"){1.045})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      
      *(1-(input$drk*0.00157))
      
      #### Smokes per week ---
      *(1-(input$smk*0.00109))
      
      #### Moderate intensity physical activity ---
      *(1+(input$mpa*0.000219))
      
      #### High intensity physical activity ---
      *(1+(input$hpa*0.000438))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.70}
        else if(input$hsd==2){0.75}
        else if(input$hsd==3){0.80}
        else if(input$hsd==4){0.85}
        else if(input$hsd==5){0.90}
        else if(input$hsd==6){0.95}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.85}
        else if(input$hsd==10){0.80}       
        else if(input$hsd==11){0.75}
        else if(input$hsd==12){0.70}
        else if(input$hsd==13){0.65}
        else if(input$hsd==14){0.60}
        else if(input$hsd==15){0.55}
        else if(input$hsd>=16){0.50}
      )    
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.984}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.967}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.934})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.942}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.975}
        else if(input$bmi=="Obese (>30)"){0.949})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      ### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,                 
      
      ## Coronary Heart Diseases -----------------------------------------------------
      
      cod[cod$cause=="Coronary Heart Diseases","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.974}
        else if(input$sex=="Female"){1.026})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.00157))    
      
      #### Smokes per week ---
      *(1-(input$smk*0.00109))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000219))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000438))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.984}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.967}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.934})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.942}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.975}
        else if(input$bmi=="Obese (>30)"){0.949})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Stroke ----------------------------------------------------------------------
      
      cod[cod$cause=="Stroke","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.965}
        else if(input$sex=="Female"){1.035})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.00157))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.000894))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000168))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000336))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.70}
        else if(input$hsd==2){0.75}
        else if(input$hsd==3){0.80}
        else if(input$hsd==4){0.85}
        else if(input$hsd==5){0.90}
        else if(input$hsd==6){0.95}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.85}
        else if(input$hsd==10){0.80}       
        else if(input$hsd==11){0.75}
        else if(input$hsd==12){0.70}
        else if(input$hsd==13){0.65}
        else if(input$hsd==14){0.60}
        else if(input$hsd==15){0.55}
        else if(input$hsd>=16){0.50}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.982}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.958}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.907})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.942}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.978}
        else if(input$bmi=="Obese (>30)"){0.931})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Cancer ----------------------------------------------------------------------                       
      
      cod[cod$cause=="Cancer","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.923}
        else if(input$sex=="Female"){1.077})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.00437))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.00149))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000164))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000328))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.985}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.970}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.944})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.937}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.978}
        else if(input$bmi=="Obese (>30)"){0.960})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## COVID-19 --------------------------------------------------------------------                     
      
      cod[cod$cause=="COVID-19","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.997}
        else if(input$sex=="Female"){1.038})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){1}
        else if(input$race=="Asian"){1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.00875))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.00242))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000320))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000640))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.985}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.970}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.942})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.926}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.977}
        else if(input$bmi=="Obese (>30)"){0.953})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Alzeihmer's Disease ---------------------------------------------------------
      
      cod[cod$cause=="Alzheimer’s Disease","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.974}
        else if(input$sex=="Female"){1.026})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.000602))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000105))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000210))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      
      ## Chronic Lower Respiratory Diseases ------------------------------------------
      
      cod[cod$cause=="Chronic Lower Respiratory Diseases","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.00109))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.00109))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000223))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000446))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.985}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.970}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.942})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.931}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.014}
        else if(input$bmi=="Obese (>30)"){1.020})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Diabetes --------------------------------------------------------------------
      
      cod[cod$cause=="Diabetes","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.000613))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000261))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000522))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.983}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.961}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.923})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.926}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Drug Overdose ---------------------------------------------------------------
      
      cod[cod$cause=="Drug Overdose","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.950}
        else if(input$sex=="Female"){1.025})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.00175))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.000530))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000211))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000422))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.979}
        else if(input$bmi=="Obese (>30)"){0.949})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Motor Vehicle Accident ------------------------------------------------------
      
      cod[cod$cause=="Motor Vehicle Accident","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.01137))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.000609))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.70}
        else if(input$hsd==2){0.75}
        else if(input$hsd==3){0.80}
        else if(input$hsd==4){0.85}
        else if(input$hsd==5){0.90}
        else if(input$hsd==6){0.95}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1}
        else if(input$hsd==9){1}
        else if(input$hsd==10){1}       
        else if(input$hsd==11){1}
        else if(input$hsd==12){1}
        else if(input$hsd==13){1}
        else if(input$hsd==14){1}
        else if(input$hsd==15){1}
        else if(input$hsd>=16){1}
      ) 
      
      
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Fall ------------------------------------------------------------------------
      
      cod[cod$cause=="Fall","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.00122))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.000677))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000177))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000354))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.979}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.951}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.894})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.912}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.980}
        else if(input$bmi=="Obese (>30)"){0.967})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Influenza and Pneumonia -----------------------------------------------------
      
      cod[cod$cause=="Influenza and Pneumonia","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){1}
        else if(input$race=="Asian"){1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.00875))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.00162))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000109))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000218))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.70}
        else if(input$hsd==2){0.75}
        else if(input$hsd==3){0.80}
        else if(input$hsd==4){0.85}
        else if(input$hsd==5){0.90}
        else if(input$hsd==6){0.95}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.95}
        else if(input$hsd==9){0.90}
        else if(input$hsd==10){0.85}       
        else if(input$hsd==11){0.80}
        else if(input$hsd==12){0.75}
        else if(input$hsd==13){0.70}
        else if(input$hsd==14){0.65}
        else if(input$hsd==15){0.60}
        else if(input$hsd>=16){0.55}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.985}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.970}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.942})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.902}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.974}
        else if(input$bmi=="Obese (>30)"){0.949})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Kidney Diseases -------------------------------------------------------------
      
      cod[cod$cause=="Kidney Diseases","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.000435))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.0000842))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000168))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.984}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.967}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.934})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.936}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.977}
        else if(input$bmi=="Obese (>30)"){0.951})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Suicide ---------------------------------------------------------------------
      
      cod[cod$cause=="Suicide","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.0175))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.000724))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000303))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000606))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Liver Diseases --------------------------------------------------------------
      
      cod[cod$cause=="Liver Diseases","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.0175))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.000538))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000253))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000506))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.978}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.948}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.886})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.980}
        else if(input$bmi=="Obese (>30)"){0.957})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Septicemia ------------------------------------------------------------------
      
      cod[cod$cause=="Septicemia","age"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1}
        else if(input$sex=="Female"){1})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1}
        else if(input$race=="African (Black)"){0.93}
        else if(input$race=="Asian"){1.1}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.85}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){0.975}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){0.964}
        else if(input$edu=="Primary Education (Elementary School)"){0.982}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1.018}
        else if(input$edu=="Master's degree"){1.036}
        else if(input$edu=="Doctoral degree"){1.054})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1-(input$drk*0.00131))      
      
      #### Smokes per week ---
      *(1-(input$smk*0.000922))
      
      #### Moderate intensity physical activity per week ---
      *(1+(input$mpa*0.000223))
      
      #### High intensity physical activity per week ---
      *(1+(input$hpa*0.000446))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){0.83}
        else if(input$hsd==2){0.86}
        else if(input$hsd==3){0.89}
        else if(input$hsd==4){0.92}
        else if(input$hsd==5){0.95}
        else if(input$hsd==6){0.98}
        else if(input$hsd==7){1}
        else if(input$hsd==8){0.98}
        else if(input$hsd==9){0.95}
        else if(input$hsd==10){0.92}       
        else if(input$hsd==11){0.89}
        else if(input$hsd==12){0.86}
        else if(input$hsd==13){0.83}
        else if(input$hsd==14){0.80}
        else if(input$hsd==15){0.77}
        else if(input$hsd>=16){0.74}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){0.978}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){0.948}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){0.886})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){0.923}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
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
      
      # RISK OF DEATH ----------------------------------------------------------------
    # Setting the impact of Risk Factors on the baseline RISK OF DEATH from different causes of death  
    
    mutate(risk=c( 
      
      ## Cardiovascular Diseases -----------------------------------------------------
      
      cod[cod$cause=="Cardiovascular Diseases","risk"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.219}
        else if(input$sex=="Female"){0.816})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.980}
        else if(input$race=="African (Black)"){1.389}
        else if(input$race=="Asian"){0.582}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.672}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.189}
        else if(input$wbr=="Europe & Central Asia"){1.888}
        else if(input$wbr=="Latin America & Caribbean"){0.708}
        else if(input$wbr=="Middle East & North Africa"){0.876}
        else if(input$wbr=="North America"){1.181}
        else if(input$wbr=="South Asia"){0.758}
        else if(input$wbr=="Sub-Saharan Africa"){0.400})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.04))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0175))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.0016))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.0032))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){3.70}
        else if(input$hsd==2){3.19}
        else if(input$hsd==3){2.68}
        else if(input$hsd==4){2.17}
        else if(input$hsd==5){1.66}
        else if(input$hsd==6){1.15}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.40}
        else if(input$hsd==9){1.80}
        else if(input$hsd==10){2.20}       
        else if(input$hsd==11){2.60}
        else if(input$hsd==12){3.00}
        else if(input$hsd==13){3.40}
        else if(input$hsd==14){3.80}
        else if(input$hsd==15){4.20}
        else if(input$hsd>=16){4.60}
      )  
      
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.26}
        else if(input$bmi=="Obese (>30)"){1.76})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.06}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){2.57}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){4.98}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2.00}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.02}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Coronary Heart Diseases -----------------------------------------------------
      
      cod[cod$cause=="Coronary Heart Diseases","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.375}
        else if(input$sex=="Female"){0.698})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.001}
        else if(input$race=="African (Black)"){1.233}
        else if(input$race=="Asian"){0.596}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.941}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.189}
        else if(input$wbr=="Europe & Central Asia"){1.888}
        else if(input$wbr=="Latin America & Caribbean"){0.708}
        else if(input$wbr=="Middle East & North Africa"){0.876}
        else if(input$wbr=="North America"){1.181}
        else if(input$wbr=="South Asia"){0.758}
        else if(input$wbr=="Sub-Saharan Africa"){0.400})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.04))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0175))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.0016))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.0032))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.26}
        else if(input$bmi=="Obese (>30)"){1.76})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.06}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){2.57}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){4.98}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.02}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Stroke ----------------------------------------------------------------------
      
      cod[cod$cause=="Stroke","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.026}
        else if(input$sex=="Female"){0.964})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.964}
        else if(input$race=="African (Black)"){1.441}
        else if(input$race=="Asian"){0.809}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.670}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.189}
        else if(input$wbr=="Europe & Central Asia"){1.888}
        else if(input$wbr=="Latin America & Caribbean"){0.708}
        else if(input$wbr=="Middle East & North Africa"){0.876}
        else if(input$wbr=="North America"){1.181}
        else if(input$wbr=="South Asia"){0.758}
        else if(input$wbr=="Sub-Saharan Africa"){0.400})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.04))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0126))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00133))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00266))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){2.20}
        else if(input$hsd==2){2.00}
        else if(input$hsd==3){1.80}
        else if(input$hsd==4){1.60}
        else if(input$hsd==5){1.40}
        else if(input$hsd==6){1.20}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.07}
        else if(input$hsd==9){1.14}
        else if(input$hsd==10){1.21}       
        else if(input$hsd==11){1.28}
        else if(input$hsd==12){1.35}
        else if(input$hsd==13){1.42}
        else if(input$hsd==14){1.49}
        else if(input$hsd==15){1.56}
        else if(input$hsd>=16){1.63}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.3}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.7}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.21})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.5}
        else if(input$bmi=="Obese (>30)"){2})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.08}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){2.44}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){3.34}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.83}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Cancer ----------------------------------------------------------------------
      
      cod[cod$cause=="Cancer","risk"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.184}
        else if(input$sex=="Female"){0.863})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.016}
        else if(input$race=="African (Black)"){1.134}
        else if(input$race=="Asian"){0.614}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.636}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.280}
        else if(input$wbr=="Europe & Central Asia"){1.790}
        else if(input$wbr=="Latin America & Caribbean"){0.870}
        else if(input$wbr=="Middle East & North Africa"){0.457}
        else if(input$wbr=="North America"){1.745}
        else if(input$wbr=="South Asia"){0.507}
        else if(input$wbr=="Sub-Saharan Africa"){0.351})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.58}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.58}
        else if(input$edu=="Primary Education (Elementary School)"){1.58}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.2))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.06523))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.0006))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.0012))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.21}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.33})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.29}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.285}
        else if(input$bmi=="Obese (>30)"){1.57})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){4.38}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){2.5}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## COVID-19 --------------------------------------------------------------------
      
      cod[cod$cause=="COVID-19","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.267}
        else if(input$sex=="Female"){0.784})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.934}
        else if(input$race=="African (Black)"){1.645}
        else if(input$race=="Asian"){0.741}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.518}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.141}
        else if(input$wbr=="Europe & Central Asia"){1.816}
        else if(input$wbr=="Latin America & Caribbean"){1.636}
        else if(input$wbr=="Middle East & North Africa"){0.512}
        else if(input$wbr=="North America"){2.457}
        else if(input$wbr=="South Asia"){0.258}
        else if(input$wbr=="Sub-Saharan Africa"){0.181})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.38}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.58}
        else if(input$edu=="Primary Education (Elementary School)"){1.58}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.045))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0511))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00253))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00507))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.36})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.5}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.34}
        else if(input$bmi=="Obese (>30)"){1.85})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.77}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.55}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Alzheimer’s Disease ---------------------------------------------------------
      
      cod[cod$cause=="Alzheimer’s Disease","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.781}
        else if(input$sex=="Female"){1.142})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.043}
        else if(input$race=="African (Black)"){0.951}
        else if(input$race=="Asian"){0.540}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.485}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.135}
        else if(input$wbr=="Europe & Central Asia"){1.928}
        else if(input$wbr=="Latin America & Caribbean"){0.980}
        else if(input$wbr=="Middle East & North Africa"){0.446}
        else if(input$wbr=="North America"){1.888}
        else if(input$wbr=="South Asia"){0.378}
        else if(input$wbr=="Sub-Saharan Africa"){0.246})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0052))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.000833))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00166))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1.7}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Chronic Lower Respiratory Diseases ------------------------------------------
      
      cod[cod$cause=="Chronic Lower Respiratory Diseases","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.104}
        else if(input$sex=="Female"){0.923})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.069}
        else if(input$race=="African (Black)"){0.816}
        else if(input$race=="Asian"){0.280}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.657}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.403}
        else if(input$wbr=="Europe & Central Asia"){0.962}
        else if(input$wbr=="Latin America & Caribbean"){0.721}
        else if(input$wbr=="Middle East & North Africa"){0.395}
        else if(input$wbr=="North America"){1.489}
        else if(input$wbr=="South Asia"){1.665}
        else if(input$wbr=="Sub-Saharan Africa"){0.365})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.0125))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.115))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00156))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00313))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.4})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.4}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.8}
        else if(input$bmi=="Obese (>30)"){0.77})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){3.42}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.59}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1.57}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Diabetes --------------------------------------------------------------------
      
      cod[cod$cause=="Diabetes","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.258}
        else if(input$sex=="Female"){0.786})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.903}
        else if(input$race=="African (Black)"){1.859}
        else if(input$race=="Asian"){0.750}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.512}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.883}
        else if(input$wbr=="Europe & Central Asia"){0.938}
        else if(input$wbr=="Latin America & Caribbean"){1.627}
        else if(input$wbr=="Middle East & North Africa"){0.767}
        else if(input$wbr=="North America"){1.105}
        else if(input$wbr=="South Asia"){0.958}
        else if(input$wbr=="Sub-Saharan Africa"){0.723})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.7}
        else if(input$inc=="Middle"){1.5}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2.4}
        else if(input$edu=="Primary Education (Elementary School)"){1.6}
        else if(input$edu=="Secondary Education (High School)"){1.4}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0055))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00126))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00253))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.35}
        else if(input$hsd==1){1.30}
        else if(input$hsd==2){1.25}
        else if(input$hsd==3){1.20}
        else if(input$hsd==4){1.15}
        else if(input$hsd==5){1.10}
        else if(input$hsd==6){1.05}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.07}
        else if(input$hsd==9){1.14}
        else if(input$hsd==10){1.21}       
        else if(input$hsd==11){1.28}
        else if(input$hsd==12){1.35}
        else if(input$hsd==13){1.42}
        else if(input$hsd==14){1.49}
        else if(input$hsd==15){1.56}
        else if(input$hsd>=16){1.63}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.24}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.58}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.82})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.5}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){3.0}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){2.04}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.77}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){5}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Drug Overdose ---------------------------------------------------------------
      
      cod[cod$cause=="Drug Overdose","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.426}
        else if(input$sex=="Female"){0.578})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.058}
        else if(input$race=="African (Black)"){1.236}
        else if(input$race=="Asian"){0.155}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.946}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.211}
        else if(input$wbr=="Europe & Central Asia"){0.635}
        else if(input$wbr=="Latin America & Caribbean"){0.135}
        else if(input$wbr=="Middle East & North Africa"){0.402}
        else if(input$wbr=="North America"){5.392}
        else if(input$wbr=="South Asia"){0.159}
        else if(input$wbr=="Sub-Saharan Africa"){0.067})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.6}
        else if(input$inc=="Lower-middle"){1.375}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.925}
        else if(input$inc=="Rich"){0.7})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.9}
        else if(input$edu=="Doctoral degree"){0.8})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.05))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.00342))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00166))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00333))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.24}
        else if(input$bmi=="Obese (>30)"){2})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      ,
      
      ## Motor Vehicle Accident ------------------------------------------------------
      
      cod[cod$cause=="Motor Vehicle Accident","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.481}
        else if(input$sex=="Female"){0.534})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.992}
        else if(input$race=="African (Black)"){1.397}
        else if(input$race=="Asian"){0.313}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.344}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.035}
        else if(input$wbr=="Europe & Central Asia"){0.575}
        else if(input$wbr=="Latin America & Caribbean"){1.114}
        else if(input$wbr=="Middle East & North Africa"){1.664}
        else if(input$wbr=="North America"){0.747}
        else if(input$wbr=="South Asia"){0.852}
        else if(input$wbr=="Sub-Saharan Africa"){1.012})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2.5}
        else if(input$edu=="Primary Education (Elementary School)"){2.0}
        else if(input$edu=="Secondary Education (High School)"){1.5}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.6})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.6))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0054))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){3.31}
        else if(input$hsd==1){2.98}
        else if(input$hsd==2){2.65}
        else if(input$hsd==3){2.32}
        else if(input$hsd==4){1.99}
        else if(input$hsd==5){1.66}
        else if(input$hsd==6){1.33}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1}
        else if(input$hsd==9){1}
        else if(input$hsd==10){1}       
        else if(input$hsd==11){1}
        else if(input$hsd==12){1}
        else if(input$hsd==13){1}
        else if(input$hsd==14){1}
        else if(input$hsd==15){1}
        else if(input$hsd>=16){1}
      )   
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){2}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1.5}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){2})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Fall ------------------------------------------------------------------------
      
      cod[cod$cause=="Fall","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.214}
        else if(input$sex=="Female"){0.816})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.087}
        else if(input$race=="African (Black)"){0.495}
        else if(input$race=="Asian"){0.524}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.796}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2}
        else if(input$edu=="Primary Education (Elementary School)"){2}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.5}
        else if(input$edu=="Doctoral degree"){0.5})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.02))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0071))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.0014))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.0028))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){1.78}
        else if(input$hsd==2){1.65}
        else if(input$hsd==3){1.52}
        else if(input$hsd==4){1.39}
        else if(input$hsd==5){1.26}
        else if(input$hsd==6){1.13}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.06}
        else if(input$hsd==9){1.12}
        else if(input$hsd==10){1.18}       
        else if(input$hsd==11){1.24}
        else if(input$hsd==12){1.30}
        else if(input$hsd==13){1.36}
        else if(input$hsd==14){1.42}
        else if(input$hsd==15){1.48}
        else if(input$hsd>=16){1.54}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.5}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){2.0}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.5})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.8}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.15}
        else if(input$bmi=="Obese (>30)"){1.31})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Influenza and Pneumonia -----------------------------------------------------
      
      cod[cod$cause=="Influenza and Pneumonia","risk"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.215}
        else if(input$sex=="Female"){0.846})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.985}
        else if(input$race=="African (Black)"){1.292}
        else if(input$race=="Asian"){0.792}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.077}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.768}
        else if(input$wbr=="Europe & Central Asia"){0.932}
        else if(input$wbr=="Latin America & Caribbean"){1.144}
        else if(input$wbr=="Middle East & North Africa"){0.465}
        else if(input$wbr=="North America"){0.788}
        else if(input$wbr=="South Asia"){0.972}
        else if(input$wbr=="Sub-Saharan Africa"){1.931})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.4}
        else if(input$inc=="Lower-middle"){1.2}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.8}
        else if(input$inc=="Rich"){0.6})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.6}
        else if(input$edu=="Primary Education (Elementary School)"){1.4}
        else if(input$edu=="Secondary Education (High School)"){1.2}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.6})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.45))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0311))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.000866))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00493))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.35}
        else if(input$hsd==1){3.30}
        else if(input$hsd==2){2.90}
        else if(input$hsd==3){2.50}
        else if(input$hsd==4){2.10}
        else if(input$hsd==5){1.70}
        else if(input$hsd==6){1.30}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.20}
        else if(input$hsd==9){1.50}
        else if(input$hsd==10){1.80}       
        else if(input$hsd==11){2.10}
        else if(input$hsd==12){2.40}
        else if(input$hsd==13){2.70}
        else if(input$hsd==14){3.00}
        else if(input$hsd==15){3.30}
        else if(input$hsd>=16){3.60}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.36})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){2}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.5}
        else if(input$bmi=="Obese (>30)"){2})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.77}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.55}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){2.15}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      ,
      
      ## Kidney Diseases -------------------------------------------------------------
      
      cod[cod$cause=="Kidney Diseases","risk"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.213}
        else if(input$sex=="Female"){0.843})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.898}
        else if(input$race=="African (Black)"){2.024}
        else if(input$race=="Asian"){0.638}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.882}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.852}
        else if(input$wbr=="Europe & Central Asia"){0.924}
        else if(input$wbr=="Latin America & Caribbean"){1.557}
        else if(input$wbr=="Middle East & North Africa"){0.864}
        else if(input$wbr=="North America"){1.502}
        else if(input$wbr=="South Asia"){0.765}
        else if(input$wbr=="Sub-Saharan Africa"){0.538})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.001))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.000666))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00133))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.3}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.34}
        else if(input$bmi=="Obese (>30)"){1.94})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1.64}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.22}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1.75}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.29}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Suicide ---------------------------------------------------------------------
      
      cod[cod$cause=="Suicide","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.630}
        else if(input$sex=="Female"){0.407})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.126}
        else if(input$race=="African (Black)"){0.556}
        else if(input$race=="Asian"){0.474}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.244}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.910}
        else if(input$wbr=="Europe & Central Asia"){1.544}
        else if(input$wbr=="Latin America & Caribbean"){0.727}
        else if(input$wbr=="Middle East & North Africa"){0.445}
        else if(input$wbr=="North America"){1.425}
        else if(input$wbr=="South Asia"){1.288}
        else if(input$wbr=="Sub-Saharan Africa"){0.662})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*1.0))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0083))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.0024))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.0048))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.5}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.85}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){2.58}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Liver Diseases --------------------------------------------------------------
      
      cod[cod$cause=="Liver Diseases","risk"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.316}
        else if(input$sex=="Female"){0.707})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.083}
        else if(input$race=="African (Black)"){0.662}
        else if(input$race=="Asian"){0.308}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){2.850}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.829}
        else if(input$wbr=="Europe & Central Asia"){1.212}
        else if(input$wbr=="Latin America & Caribbean"){1.124}
        else if(input$wbr=="Middle East & North Africa"){1.013}
        else if(input$wbr=="North America"){0.999}
        else if(input$wbr=="South Asia"){0.963}
        else if(input$wbr=="Sub-Saharan Africa"){0.860})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*1.0))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0036))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00133))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00266))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.56}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){2.13}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.69})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.16}
        else if(input$bmi=="Obese (>30)"){1.69})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){4.7}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Septicemia ------------------------------------------------------------------
      
      cod[cod$cause=="Septicemia","risk"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.113}
        else if(input$sex=="Female"){0.918})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.959}
        else if(input$race=="African (Black)"){1.680}
        else if(input$race=="Asian"){0.464}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.948}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.5}
        else if(input$wbr=="Europe & Central Asia"){1.5}
        else if(input$wbr=="Latin America & Caribbean"){2}
        else if(input$wbr=="Middle East & North Africa"){2}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){4}
        else if(input$wbr=="Sub-Saharan Africa"){6})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2}
        else if(input$edu=="Primary Education (Elementary School)"){2}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){2}
        else if(input$edu=="Master's degree"){0.5}
        else if(input$edu=="Doctoral degree"){0.5})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.025))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0133))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00156))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00313))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.56}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1.4}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.4}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2.2}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1.3}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.5}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1.5}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
    )
    
    ) %>%
      
      # RATE DEATH ----------------------------------------------------------------
    # Setting the impact of Risk Factors on the baseline population dying form each 
    # cause of death per year (Crude Death Rate)
    
    mutate(pop=c( 
      
      ## Cardiovascular Diseases -----------------------------------------------------
      
      cod[cod$cause=="Cardiovascular Diseases","pop"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.219}
        else if(input$sex=="Female"){0.816})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.980}
        else if(input$race=="African (Black)"){1.389}
        else if(input$race=="Asian"){0.582}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.672}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.189}
        else if(input$wbr=="Europe & Central Asia"){1.888}
        else if(input$wbr=="Latin America & Caribbean"){0.708}
        else if(input$wbr=="Middle East & North Africa"){0.876}
        else if(input$wbr=="North America"){1.181}
        else if(input$wbr=="South Asia"){0.758}
        else if(input$wbr=="Sub-Saharan Africa"){0.400})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.04))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0175))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.0016))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.0032))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){3.70}
        else if(input$hsd==2){3.19}
        else if(input$hsd==3){2.68}
        else if(input$hsd==4){2.17}
        else if(input$hsd==5){1.66}
        else if(input$hsd==6){1.15}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.40}
        else if(input$hsd==9){1.80}
        else if(input$hsd==10){2.20}       
        else if(input$hsd==11){2.60}
        else if(input$hsd==12){3.00}
        else if(input$hsd==13){3.40}
        else if(input$hsd==14){3.80}
        else if(input$hsd==15){4.20}
        else if(input$hsd>=16){4.60}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.26}
        else if(input$bmi=="Obese (>30)"){1.76})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.06}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){2.57}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){4.98}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2.00}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.02}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Coronary Heart Diseases -----------------------------------------------------
      
      cod[cod$cause=="Coronary Heart Diseases","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.375}
        else if(input$sex=="Female"){0.698})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.001}
        else if(input$race=="African (Black)"){1.233}
        else if(input$race=="Asian"){0.596}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.941}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.189}
        else if(input$wbr=="Europe & Central Asia"){1.888}
        else if(input$wbr=="Latin America & Caribbean"){0.708}
        else if(input$wbr=="Middle East & North Africa"){0.876}
        else if(input$wbr=="North America"){1.181}
        else if(input$wbr=="South Asia"){0.758}
        else if(input$wbr=="Sub-Saharan Africa"){0.400})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.04))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0175))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.0016))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.0032))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.26}
        else if(input$bmi=="Obese (>30)"){1.76})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.06}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){2.57}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){4.98}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.02}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Stroke ----------------------------------------------------------------------
      
      cod[cod$cause=="Stroke","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.026}
        else if(input$sex=="Female"){0.964})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.964}
        else if(input$race=="African (Black)"){1.441}
        else if(input$race=="Asian"){0.809}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.670}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.189}
        else if(input$wbr=="Europe & Central Asia"){1.888}
        else if(input$wbr=="Latin America & Caribbean"){0.708}
        else if(input$wbr=="Middle East & North Africa"){0.876}
        else if(input$wbr=="North America"){1.181}
        else if(input$wbr=="South Asia"){0.758}
        else if(input$wbr=="Sub-Saharan Africa"){0.400})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.42}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.42}
        else if(input$edu=="Primary Education (Elementary School)"){1.42}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.04))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0126))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00133))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00266))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){2.20}
        else if(input$hsd==2){2.00}
        else if(input$hsd==3){1.80}
        else if(input$hsd==4){1.60}
        else if(input$hsd==5){1.40}
        else if(input$hsd==6){1.20}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.07}
        else if(input$hsd==9){1.14}
        else if(input$hsd==10){1.21}       
        else if(input$hsd==11){1.28}
        else if(input$hsd==12){1.35}
        else if(input$hsd==13){1.42}
        else if(input$hsd==14){1.49}
        else if(input$hsd==15){1.56}
        else if(input$hsd>=16){1.63}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.3}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.7}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.21})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.18}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.5}
        else if(input$bmi=="Obese (>30)"){2})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.08}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){2.44}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){3.34}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.83}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){2.5}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Cancer ----------------------------------------------------------------------
      
      cod[cod$cause=="Cancer","pop"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.184}
        else if(input$sex=="Female"){0.863})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.016}
        else if(input$race=="African (Black)"){1.134}
        else if(input$race=="Asian"){0.614}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.636}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.280}
        else if(input$wbr=="Europe & Central Asia"){1.790}
        else if(input$wbr=="Latin America & Caribbean"){0.870}
        else if(input$wbr=="Middle East & North Africa"){0.457}
        else if(input$wbr=="North America"){1.745}
        else if(input$wbr=="South Asia"){0.507}
        else if(input$wbr=="Sub-Saharan Africa"){0.351})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.58}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.58}
        else if(input$edu=="Primary Education (Elementary School)"){1.58}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.2))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.06523))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.0006))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.0012))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.21}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.33})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.29}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.285}
        else if(input$bmi=="Obese (>30)"){1.57})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){4.38}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){2.5}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## COVID-19 --------------------------------------------------------------------
      
      cod[cod$cause=="COVID-19","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.267}
        else if(input$sex=="Female"){0.784})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.934}
        else if(input$race=="African (Black)"){1.645}
        else if(input$race=="Asian"){0.741}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.518}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.141}
        else if(input$wbr=="Europe & Central Asia"){1.816}
        else if(input$wbr=="Latin America & Caribbean"){1.636}
        else if(input$wbr=="Middle East & North Africa"){0.512}
        else if(input$wbr=="North America"){2.457}
        else if(input$wbr=="South Asia"){0.258}
        else if(input$wbr=="Sub-Saharan Africa"){0.181})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.38}
        else if(input$inc=="Lower-middle"){1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.58}
        else if(input$edu=="Primary Education (Elementary School)"){1.58}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.045))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0511))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00253))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00507))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.36})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.5}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.34}
        else if(input$bmi=="Obese (>30)"){1.85})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.77}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.55}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Alzheimer’s Disease ---------------------------------------------------------
      
      cod[cod$cause=="Alzheimer’s Disease","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){0.781}
        else if(input$sex=="Female"){1.142})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.043}
        else if(input$race=="African (Black)"){0.951}
        else if(input$race=="Asian"){0.540}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.485}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.135}
        else if(input$wbr=="Europe & Central Asia"){1.928}
        else if(input$wbr=="Latin America & Caribbean"){0.980}
        else if(input$wbr=="Middle East & North Africa"){0.446}
        else if(input$wbr=="North America"){1.888}
        else if(input$wbr=="South Asia"){0.378}
        else if(input$wbr=="Sub-Saharan Africa"){0.246})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0052))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.000833))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00166))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1.7}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Chronic Lower Respiratory Diseases ------------------------------------------
      
      cod[cod$cause=="Chronic Lower Respiratory Diseases","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.104}
        else if(input$sex=="Female"){0.923})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.069}
        else if(input$race=="African (Black)"){0.816}
        else if(input$race=="Asian"){0.280}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.657}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.403}
        else if(input$wbr=="Europe & Central Asia"){0.962}
        else if(input$wbr=="Latin America & Caribbean"){0.721}
        else if(input$wbr=="Middle East & North Africa"){0.395}
        else if(input$wbr=="North America"){1.489}
        else if(input$wbr=="South Asia"){1.665}
        else if(input$wbr=="Sub-Saharan Africa"){0.365})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.0125))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.115))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00156))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00313))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.4})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.4}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){0.8}
        else if(input$bmi=="Obese (>30)"){0.77})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){3.42}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.59}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1.57}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Diabetes --------------------------------------------------------------------
      
      cod[cod$cause=="Diabetes","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.258}
        else if(input$sex=="Female"){0.786})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.903}
        else if(input$race=="African (Black)"){1.859}
        else if(input$race=="Asian"){0.750}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.512}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.883}
        else if(input$wbr=="Europe & Central Asia"){0.938}
        else if(input$wbr=="Latin America & Caribbean"){1.627}
        else if(input$wbr=="Middle East & North Africa"){0.767}
        else if(input$wbr=="North America"){1.105}
        else if(input$wbr=="South Asia"){0.958}
        else if(input$wbr=="Sub-Saharan Africa"){0.723})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.7}
        else if(input$inc=="Middle"){1.5}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2.4}
        else if(input$edu=="Primary Education (Elementary School)"){1.6}
        else if(input$edu=="Secondary Education (High School)"){1.4}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){1}
        else if(input$edu=="Doctoral degree"){1})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0055))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00126))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00253))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.35}
        else if(input$hsd==1){1.30}
        else if(input$hsd==2){1.25}
        else if(input$hsd==3){1.20}
        else if(input$hsd==4){1.15}
        else if(input$hsd==5){1.10}
        else if(input$hsd==6){1.05}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.07}
        else if(input$hsd==9){1.14}
        else if(input$hsd==10){1.21}       
        else if(input$hsd==11){1.28}
        else if(input$hsd==12){1.35}
        else if(input$hsd==13){1.42}
        else if(input$hsd==14){1.49}
        else if(input$hsd==15){1.56}
        else if(input$hsd>=16){1.63}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.24}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.58}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.82})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.5}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){3.0}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){2.04}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.77}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){5}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Drug Overdose ---------------------------------------------------------------
      
      cod[cod$cause=="Drug Overdose","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.426}
        else if(input$sex=="Female"){0.578})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.058}
        else if(input$race=="African (Black)"){1.236}
        else if(input$race=="Asian"){0.155}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.946}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.211}
        else if(input$wbr=="Europe & Central Asia"){0.635}
        else if(input$wbr=="Latin America & Caribbean"){0.135}
        else if(input$wbr=="Middle East & North Africa"){0.402}
        else if(input$wbr=="North America"){5.392}
        else if(input$wbr=="South Asia"){0.159}
        else if(input$wbr=="Sub-Saharan Africa"){0.067})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.6}
        else if(input$inc=="Lower-middle"){1.375}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.925}
        else if(input$inc=="Rich"){0.7})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.9}
        else if(input$edu=="Doctoral degree"){0.8})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.05))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.00342))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00166))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00333))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      )
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.24}
        else if(input$bmi=="Obese (>30)"){2})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      ,
      
      ## Motor Vehicle Accident ------------------------------------------------------
      
      cod[cod$cause=="Motor Vehicle Accident","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.481}
        else if(input$sex=="Female"){0.534})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.992}
        else if(input$race=="African (Black)"){1.397}
        else if(input$race=="Asian"){0.313}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.344}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.035}
        else if(input$wbr=="Europe & Central Asia"){0.575}
        else if(input$wbr=="Latin America & Caribbean"){1.114}
        else if(input$wbr=="Middle East & North Africa"){1.664}
        else if(input$wbr=="North America"){0.747}
        else if(input$wbr=="South Asia"){0.852}
        else if(input$wbr=="Sub-Saharan Africa"){1.012})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2.5}
        else if(input$edu=="Primary Education (Elementary School)"){2.0}
        else if(input$edu=="Secondary Education (High School)"){1.5}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.6})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.6))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0054))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){3.31}
        else if(input$hsd==1){2.98}
        else if(input$hsd==2){2.65}
        else if(input$hsd==3){2.32}
        else if(input$hsd==4){1.99}
        else if(input$hsd==5){1.66}
        else if(input$hsd==6){1.33}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1}
        else if(input$hsd==9){1}
        else if(input$hsd==10){1}       
        else if(input$hsd==11){1}
        else if(input$hsd==12){1}
        else if(input$hsd==13){1}
        else if(input$hsd==14){1}
        else if(input$hsd==15){1}
        else if(input$hsd>=16){1}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){2}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1.5}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){2})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Fall ------------------------------------------------------------------------
      
      cod[cod$cause=="Fall","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.214}
        else if(input$sex=="Female"){0.816})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.087}
        else if(input$race=="African (Black)"){0.495}
        else if(input$race=="Asian"){0.524}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.796}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1}
        else if(input$wbr=="Europe & Central Asia"){1}
        else if(input$wbr=="Latin America & Caribbean"){1}
        else if(input$wbr=="Middle East & North Africa"){1}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){1}
        else if(input$wbr=="Sub-Saharan Africa"){1})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2}
        else if(input$edu=="Primary Education (Elementary School)"){2}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.5}
        else if(input$edu=="Doctoral degree"){0.5})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.02))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0071))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.0014))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.0028))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){0.65}
        else if(input$hsd==1){1.78}
        else if(input$hsd==2){1.65}
        else if(input$hsd==3){1.52}
        else if(input$hsd==4){1.39}
        else if(input$hsd==5){1.26}
        else if(input$hsd==6){1.13}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.06}
        else if(input$hsd==9){1.12}
        else if(input$hsd==10){1.18}       
        else if(input$hsd==11){1.24}
        else if(input$hsd==12){1.30}
        else if(input$hsd==13){1.36}
        else if(input$hsd==14){1.42}
        else if(input$hsd==15){1.48}
        else if(input$hsd>=16){1.54}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.5}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){2.0}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.5})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.8}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.15}
        else if(input$bmi=="Obese (>30)"){1.31})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.66}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Influenza and Pneumonia -----------------------------------------------------
      
      cod[cod$cause=="Influenza and Pneumonia","pop"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.215}
        else if(input$sex=="Female"){0.846})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.985}
        else if(input$race=="African (Black)"){1.292}
        else if(input$race=="Asian"){0.792}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.077}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.768}
        else if(input$wbr=="Europe & Central Asia"){0.932}
        else if(input$wbr=="Latin America & Caribbean"){1.144}
        else if(input$wbr=="Middle East & North Africa"){0.465}
        else if(input$wbr=="North America"){0.788}
        else if(input$wbr=="South Asia"){0.972}
        else if(input$wbr=="Sub-Saharan Africa"){1.931})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.4}
        else if(input$inc=="Lower-middle"){1.2}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.8}
        else if(input$inc=="Rich"){0.6})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.6}
        else if(input$edu=="Primary Education (Elementary School)"){1.4}
        else if(input$edu=="Secondary Education (High School)"){1.2}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){1}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.6})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.45))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0311))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.000866))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00493))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.35}
        else if(input$hsd==1){3.30}
        else if(input$hsd==2){2.90}
        else if(input$hsd==3){2.50}
        else if(input$hsd==4){2.10}
        else if(input$hsd==5){1.70}
        else if(input$hsd==6){1.30}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.20}
        else if(input$hsd==9){1.50}
        else if(input$hsd==10){1.80}       
        else if(input$hsd==11){2.10}
        else if(input$hsd==12){2.40}
        else if(input$hsd==13){2.70}
        else if(input$hsd==14){3.00}
        else if(input$hsd==15){3.30}
        else if(input$hsd>=16){3.60}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.2}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.36})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){2}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.5}
        else if(input$bmi=="Obese (>30)"){2})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.77}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.55}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){2.15}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      ,
      
      ## Kidney Diseases -------------------------------------------------------------
      
      cod[cod$cause=="Kidney Diseases","pop"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.213}
        else if(input$sex=="Female"){0.843})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.898}
        else if(input$race=="African (Black)"){2.024}
        else if(input$race=="Asian"){0.638}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.882}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.852}
        else if(input$wbr=="Europe & Central Asia"){0.924}
        else if(input$wbr=="Latin America & Caribbean"){1.557}
        else if(input$wbr=="Middle East & North Africa"){0.864}
        else if(input$wbr=="North America"){1.502}
        else if(input$wbr=="South Asia"){0.765}
        else if(input$wbr=="Sub-Saharan Africa"){0.538})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.001))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.000666))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00133))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.16}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1.34}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1.56})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.3}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.34}
        else if(input$bmi=="Obese (>30)"){1.94})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1.64}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.22}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1.75}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.29}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Suicide ---------------------------------------------------------------------
      
      cod[cod$cause=="Suicide","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.630}
        else if(input$sex=="Female"){0.407})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.126}
        else if(input$race=="African (Black)"){0.556}
        else if(input$race=="Asian"){0.474}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){1.244}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.910}
        else if(input$wbr=="Europe & Central Asia"){1.544}
        else if(input$wbr=="Latin America & Caribbean"){0.727}
        else if(input$wbr=="Middle East & North Africa"){0.445}
        else if(input$wbr=="North America"){1.425}
        else if(input$wbr=="South Asia"){1.288}
        else if(input$wbr=="Sub-Saharan Africa"){0.662})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*1.0))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0083))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.0024))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.0048))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.5}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){2.85}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){2.58}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Liver Diseases --------------------------------------------------------------
      
      cod[cod$cause=="Liver Diseases","pop"]
      
      ### DEMOGRAPHICS ===
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.316}
        else if(input$sex=="Female"){0.707})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){1.083}
        else if(input$race=="African (Black)"){0.662}
        else if(input$race=="Asian"){0.308}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){2.850}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){0.829}
        else if(input$wbr=="Europe & Central Asia"){1.212}
        else if(input$wbr=="Latin America & Caribbean"){1.124}
        else if(input$wbr=="Middle East & North Africa"){1.013}
        else if(input$wbr=="North America"){0.999}
        else if(input$wbr=="South Asia"){0.963}
        else if(input$wbr=="Sub-Saharan Africa"){0.860})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){1.2}
        else if(input$inc=="Lower-middle"){1.1}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){0.9}
        else if(input$inc=="Rich"){0.8})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){1.2}
        else if(input$edu=="Primary Education (Elementary School)"){1.1}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){0.9}
        else if(input$edu=="Master's degree"){0.8}
        else if(input$edu=="Doctoral degree"){0.7})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*1.0))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0036))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00133))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00266))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1.56}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){2.13}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){2.69})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1.16}
        else if(input$bmi=="Obese (>30)"){1.69})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1.1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){1}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){4.7}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
      ,
      
      ## Septicemia ------------------------------------------------------------------
      
      cod[cod$cause=="Septicemia","pop"]
      
      ### DEMOGRAPHICS ===
      
      #Risk Factor: Current Age: cage
      #+(input$cage/2)  
      
      #### Sex ---
      *(if(is.null(input$sex)){1}
        else if(input$sex=="Male"){1.113}
        else if(input$sex=="Female"){0.918})
      
      #### Race ---
      *(if(is.null(input$race)){1}
        else if(input$race=="Caucasian (White)"){0.959}
        else if(input$race=="African (Black)"){1.680}
        else if(input$race=="Asian"){0.464}
        else if(input$race=="Middle Eastern (Indian)"){1}
        else if(input$race=="Native American"){0.948}
        else if(input$race=="Other"){1})
      
      #### World Region ---
      *(if(is.null(input$wbr)){1}
        else if(input$wbr=="East Asia & Pacific"){1.5}
        else if(input$wbr=="Europe & Central Asia"){1.5}
        else if(input$wbr=="Latin America & Caribbean"){2}
        else if(input$wbr=="Middle East & North Africa"){2}
        else if(input$wbr=="North America"){1}
        else if(input$wbr=="South Asia"){4}
        else if(input$wbr=="Sub-Saharan Africa"){6})
      
      ### SOCIAL STATUS ===
      
      #### Income Group ---
      *(if(is.null(input$inc)){1}
        else if(input$inc=="Poor"){2}
        else if(input$inc=="Lower-middle"){1.5}
        else if(input$inc=="Middle"){1}
        else if(input$inc=="Upper-middle"){1}
        else if(input$inc=="Rich"){1})
      
      #### Education ---
      *(if(is.null(input$edu)){1}
        else if(input$edu=="No Formal Schooling"){2}
        else if(input$edu=="Primary Education (Elementary School)"){2}
        else if(input$edu=="Secondary Education (High School)"){1}
        else if(input$edu=="Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program"){2}
        else if(input$edu=="Master's degree"){0.5}
        else if(input$edu=="Doctoral degree"){0.5})
      
      ### LIFESTYLE ===
      
      #### Drinks per week---
      *(1+(input$drk*0.025))      
      
      #### Smokes per week ---
      *(1+(input$smk*0.0133))
      
      #### Moderate intensity physical activity per week ---
      *(1-(input$mpa*0.00156))
      
      #### High intensity physical activity per week ---
      *(1-(input$hpa*0.00313))
      
      #### Hours of sleep per day ---
      *(if(input$hsd==0){1.42}
        else if(input$hsd==1){1.36}
        else if(input$hsd==2){1.30}
        else if(input$hsd==3){1.24}
        else if(input$hsd==4){1.18}
        else if(input$hsd==5){1.12}
        else if(input$hsd==6){1.06}
        else if(input$hsd==7){1}
        else if(input$hsd==8){1.10}
        else if(input$hsd==9){1.30}
        else if(input$hsd==10){1.50}       
        else if(input$hsd==11){1.70}
        else if(input$hsd==12){1.90}
        else if(input$hsd==13){2.10}
        else if(input$hsd==14){2.30}
        else if(input$hsd==15){2.50}
        else if(input$hsd>=16){2.70}
      ) 
      
      ### VITALS ===
      
      #### Systolic Blood Pressure ---
      *(if(is.null(input$sys)){1}
        else if(input$sys=="Normal (SBP <120 mmHG)"){1}
        else if(input$sys=="Elevated (SBP 120-129 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 1 (SBP 130-140 mmHG)"){1}
        else if(input$sys=="High Blood Pressure Stage 2 (SBP >140 mmHG)"){1})
      
      #### Body Mass Index ---
      *(if(is.null(input$bmi)){1}
        else if(input$bmi=="Underweight (<18.5)"){1.56}
        else if(input$bmi=="Normal Weight (18.5-24.9)"){1}
        else if(input$bmi=="Overweight (25-29.9)"){1}
        else if(input$bmi=="Obese (>30)"){1})
      
      ### MEDICAL HISTORY ===
      
      #### High Blood Pressure ---
      *(if(is.null(input$hbp)){1}
        else if(input$hbp=="Yes"){1}
        else if(input$hbp=="No"){1})
      
      #### High Blood Cholesterol ---
      *(if(is.null(input$hbc)){1}
        else if(input$hbc=="Yes"){1}
        else if(input$hbc=="No"){1})
      
      #### Cardiovascular Disease ---
      *(if(is.null(input$cvd)){1}
        else if(input$cvd=="Yes"){1.4}
        else if(input$cvd=="No"){1})
      
      #### Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$copd)){1}
        else if(input$copd=="Yes"){1.4}
        else if(input$copd=="No"){1})
      
      #### Diabetes ---
      *(if(is.null(input$dia)){1}
        else if(input$dia=="Yes"){2.2}
        else if(input$dia=="No"){1})
      
      #### Depression ---
      *(if(is.null(input$dep)){1}
        else if(input$dep=="Yes"){1.3}
        else if(input$dep=="No"){1})
      
      #### Cancer ---
      *(if(is.null(input$can)){1}
        else if(input$can=="Yes"){1.5}
        else if(input$can=="No"){1})
      
      #### Alzheimer ---
      *(if(is.null(input$alz)){1}
        else if(input$alz=="Yes"){1.5}
        else if(input$alz=="No"){1})
      
      ### FAMILY HISTORY ===
      
      #### Family History of High Blood Pressure ---
      *(if(is.null(input$fhbp)){1}
        else if(input$fhbp=="Yes"){1}
        else if(input$fhbp=="No"){1})
      
      #### Family History of High Blood Cholesterol ---
      *(if(is.null(input$fhbc)){1}
        else if(input$fhbc=="Yes"){1}
        else if(input$fhbc=="No"){1})
      
      #### Family History of Cardiovascular Disease ---
      *(if(is.null(input$fcvd)){1}
        else if(input$fcvd=="Yes"){1}
        else if(input$fcvd=="No"){1})
      
      #### Family History of Chronic Obstructive Pulmonary Disease ---
      *(if(is.null(input$fcopd)){1}
        else if(input$fcopd=="Yes"){1}
        else if(input$fcopd=="No"){1})
      
      #### Family History of Diabetes ---
      *(if(is.null(input$fdia)){1}
        else if(input$fdia=="Yes"){1}
        else if(input$fdia=="No"){1})
      
      #### Family History of Depression ---
      *(if(is.null(input$fdep)){1}
        else if(input$fdep=="Yes"){1}
        else if(input$fdep=="No"){1})
      
      #### Family History of Cancer ---
      *(if(is.null(input$fcan)){1}
        else if(input$fcan=="Yes"){1}
        else if(input$fcan=="No"){1})
      
      #### Family History of Alzheimer ---
      *(if(is.null(input$falz)){1}
        else if(input$falz=="Yes"){1}
        else if(input$falz=="No"){1})
      
      ### CONCOMITANT MEDICATIONS ===
      
      #### Immunosuppressants ---
      *(if(is.null(input$cmi)){1}
        else if(input$cmi=="Yes"){1}
        else if(input$cmi=="No"){1})
      
    )
    
    ) %>%
      
      #///////////////////////////////////////////////////////////////////////////////       
      # Use "mutate()" to set values for ggplot
      #///////////////////////////////////////////////////////////////////////////////
      
      mutate(risk = round(risk)) %>%
      mutate(age = round(age)) %>%
      mutate(annotation = cause) %>%
      # calculating the probability of dying from the different cause using their respective risks,
      # removing the risk related to Stroke and CHD because those risks are also part of the CVD risk
      mutate(probability = round(risk/(sum(risk[cause!="Stroke" & cause!="Coronary Heart Diseases"]))*100,1)) %>%
      arrange(desc(pop)) %>%
      mutate(cause = factor(cause, cause)) %>%
      mutate(text = paste("Cause: ", cause, "\nCrude Death Rate: ", pop, "\nRisk (%): ", probability, "\nAverage Age of Death: ", age, sep=""))
    
  })  
  
  #///////////////////////////////////////////////////////////////////////////////   
  #create textoutput for all of the causes in order of increasing risk.
  
  ({  output$textcause1 <- renderText({ cod_react()[order(-cod_react()$risk)[1],5] }) })
  ({  output$textcause2 <- renderText({ cod_react()[order(-cod_react()$risk)[2],5] }) })
  ({  output$textcause3 <- renderText({ cod_react()[order(-cod_react()$risk)[3],5] }) })
  ({  output$textcause4 <- renderText({ cod_react()[order(-cod_react()$risk)[4],5] }) })
  ({  output$textcause5 <- renderText({ cod_react()[order(-cod_react()$risk)[5],5] }) })
  ({  output$textcause6 <- renderText({ cod_react()[order(-cod_react()$risk)[6],5] }) })
  ({  output$textcause7 <- renderText({ cod_react()[order(-cod_react()$risk)[7],5] }) })
  ({  output$textcause8 <- renderText({ cod_react()[order(-cod_react()$risk)[8],5] }) })
  ({  output$textcause9 <- renderText({ cod_react()[order(-cod_react()$risk)[9],5] }) })
  ({  output$textcause10 <- renderText({ cod_react()[order(-cod_react()$risk)[10],5] }) })
  ({  output$textcause11 <- renderText({ cod_react()[order(-cod_react()$risk)[11],5] }) })
  ({  output$textcause12 <- renderText({ cod_react()[order(-cod_react()$risk)[12],5] }) })
  ({  output$textcause13 <- renderText({ cod_react()[order(-cod_react()$risk)[13],5] }) })
  ({  output$textcause14 <- renderText({ cod_react()[order(-cod_react()$risk)[14],5] }) })
  ({  output$textcause15 <- renderText({ cod_react()[order(-cod_react()$risk)[15],5] }) })
  ({  output$textcause16 <- renderText({ cod_react()[order(-cod_react()$risk)[16],5] }) })
  
  #create textoutput for all of the causes probability in order of increasing risk.
  ({  output$textprob1 <- renderText({ cod_react()[order(-cod_react()$risk)[1],6] }) })
  ({  output$textprob2 <- renderText({ cod_react()[order(-cod_react()$risk)[2],6] }) })
  ({  output$textprob3 <- renderText({ cod_react()[order(-cod_react()$risk)[3],6] }) })
  ({  output$textprob4 <- renderText({ cod_react()[order(-cod_react()$risk)[4],6] }) })
  ({  output$textprob5 <- renderText({ cod_react()[order(-cod_react()$risk)[5],6] }) })
  ({  output$textprob6 <- renderText({ cod_react()[order(-cod_react()$risk)[6],6] }) })
  ({  output$textprob7 <- renderText({ cod_react()[order(-cod_react()$risk)[7],6] }) })
  ({  output$textprob8 <- renderText({ cod_react()[order(-cod_react()$risk)[8],6] }) })
  ({  output$textprob9 <- renderText({ cod_react()[order(-cod_react()$risk)[9],6] }) })
  ({  output$textprob10 <- renderText({ cod_react()[order(-cod_react()$risk)[10],6] }) })
  ({  output$textprob11 <- renderText({ cod_react()[order(-cod_react()$risk)[11],6] }) })
  ({  output$textprob12 <- renderText({ cod_react()[order(-cod_react()$risk)[12],6] }) })
  ({  output$textprob13 <- renderText({ cod_react()[order(-cod_react()$risk)[13],6] }) })
  ({  output$textprob14 <- renderText({ cod_react()[order(-cod_react()$risk)[14],6] }) })
  ({  output$textprob15 <- renderText({ cod_react()[order(-cod_react()$risk)[15],6] }) })
  ({  output$textprob16 <- renderText({ cod_react()[order(-cod_react()$risk)[16],6] }) })
  
  #create textoutput for all of the causes age in order of increasing risk.
  ({  output$textage1 <- renderText({ cod_react()[order(-cod_react()$risk)[1],"age"] }) })
  ({  output$textage2 <- renderText({ cod_react()[order(-cod_react()$risk)[2],"age"] }) })
  ({  output$textage3 <- renderText({ cod_react()[order(-cod_react()$risk)[3],"age"] }) })
  ({  output$textage4 <- renderText({ cod_react()[order(-cod_react()$risk)[4],"age"] }) })
  ({  output$textage5 <- renderText({ cod_react()[order(-cod_react()$risk)[5],"age"] }) })
  ({  output$textage6 <- renderText({ cod_react()[order(-cod_react()$risk)[6],"age"] }) })
  ({  output$textage7 <- renderText({ cod_react()[order(-cod_react()$risk)[7],"age"] }) })
  ({  output$textage8 <- renderText({ cod_react()[order(-cod_react()$risk)[8],"age"] }) })
  ({  output$textage9 <- renderText({ cod_react()[order(-cod_react()$risk)[9],"age"] }) })
  ({  output$textage10 <- renderText({ cod_react()[order(-cod_react()$risk)[10],"age"] }) })
  ({  output$textage11 <- renderText({ cod_react()[order(-cod_react()$risk)[11],"age"] }) })
  ({  output$textage12 <- renderText({ cod_react()[order(-cod_react()$risk)[12],"age"] }) })
  ({  output$textage13 <- renderText({ cod_react()[order(-cod_react()$risk)[13],"age"] }) })
  ({  output$textage14 <- renderText({ cod_react()[order(-cod_react()$risk)[14],"age"] }) })
  ({  output$textage15 <- renderText({ cod_react()[order(-cod_react()$risk)[15],"age"] }) })
  ({  output$textage16 <- renderText({ cod_react()[order(-cod_react()$risk)[16],"age"] }) })
  
  
  # PLOT -------------------------------------------------------------------------
  # Set bubble plot (ggplot) parameters
  
  ({
    output$bubble <- renderPlotly({
      ggplotly(              
        ggplot(cod_react(), aes(x=age, y=probability, size = pop, color = cause, text=text)) +
          geom_point(alpha=0.6) + #use this line to make bubble +or- transparent
          scale_size(range = c(2, 30), name="Population (M)") +
          scale_color_viridis(discrete=TRUE, guide='none') +
          theme_ipsum() +
          scale_x_continuous(limits = c(input$cage, 100), breaks=c(10,20,30,40,50,60,70,80,90,100)) + #use this line to specify the x-axis range and tick positions
          scale_y_continuous(limits=c(0, NA), expand = expansion(mult = c(NA, 0.20)))
        ,
        tooltip="text") %>% 
        #the code below is related to legend display.
        layout(legend=list(xanchor='Right',
                           yanchor='top',
                           title = NA,
                           itemsizing='constant'
        )) 
      
    })
    
  })
  
}      

#this knits together the ui and the server function.
shinyApp(ui = ui, server = server)

# The app code ends here
