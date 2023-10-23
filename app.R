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
library(tidyr)
library(bslib)

# run this to perform rsconnect update
# install.packages('rsconnect')
# library(rsconnect)

# UI ---------------------------------------------------------------------------
# the following section is to build a ui object that lays out a webpage (html) for the app (it converts R -> html)

ui <- page_sidebar(
  title = "YOU'LL DIE",
  ## Risk Factors ----------------------------------------------------------------
  # The following section is to add input functions corresponding to Risk Factors (e.g.: Sex, Race, # of drinks/week, etc.) 
  # Individual Risk Factors are grouped by Domain (e.g.: DEMOGRAPHICS, SOCIAL STATUS, etc.)
  

  sidebar = sidebar(width = 400, open = "open",
    accordion(style = "--bs-accordion-bg: unset;", multiple = FALSE,
      ### DEMOGRAPHICS ---------------------------------------------------------------
      accordion_panel(
        "DEMOGRAPHICS",
        
        #### Current Age ----
        
        sliderInput(inputId="cage",
          label="Age",
          value=0, min=0, max=100),
        
        #### Sex ----
        
        radioButtons(inputId="sex", label="Sex", choices=list("Male",
          "Female"), 
          selected=character(0)),
        
        #### Race ----
        
        radioButtons(inputId="race", label="Race", choices=list("White",
          "Black",
          "Asian",
          "Native American",
          "Other"), 
          selected=character(0)),
        
        #### World Region ----
        
        radioButtons(inputId="wbr", label="World Region", choices=list("East Asia & Pacific",
          "Europe & Central Asia",
          "Latin America & Caribbean",
          "Middle East & North Africa",
          "North America",
          "South Asia",
          "Sub-Saharan Africa"), 
          selected=character(0)),
        
        
      ),
      ### SOCIAL STATUS --------------------------------------------------------------
      accordion_panel(
        "SOCIAL STATUS",
        
        #### Income Group ----
        
        radioButtons(inputId="inc", label="Income Group", choices=list("Poor",
          "Lower-middle",
          "Middle",
          "Upper-middle",
          "Rich"),
          selected=character(0)),
        
        #### Education ----
        
        radioButtons(inputId="edu", label="Education", choices=list("No Formal Schooling",
          "Primary Education (Elementary School)",
          "Secondary Education (High School)",
          "Tertiary Education (Bachelor's degree, Professional, Occupational, Technical or Vocational program",
          "Master's degree",
          "Doctoral degree"),
          selected=character(0)),
      ),
      ### LIFESTYLE ------------------------------------------------------------------
      accordion_panel(
        "LIFESTYLE",
        
        #### Weekly Drinks ----
        
        sliderInput(inputId="drk",
          label="Number of drinks per week",
          value=0, min=0, max=20),
        
        #### Weekly Smokes ----
        
        sliderInput(inputId="smk",
          label="Number of smokes per week",
          value=0, min=0, max=140),
        
        #### Number of moderate intensity physical activity minutes per week ----
        
        sliderInput(inputId="mpa",
          label="Number of minutes of moderate intensity physical activity per week",
          value=0, min=0, max=300),
        
        #### Number of high intensity physical activity minutes per week ----
        
        sliderInput(inputId="hpa",
          label="Number of minutes of vigorous intensity physical activity per week",
          value=0, min=0, max=100),
        
        #### Number of Hours of Sleep per Day ----
        
        sliderInput(inputId="hsd",
          label="Number of hours of sleep per day",
          value=7, min=0, max=24),
        
      ),
      ### VITALS ---------------------------------------------------------------------
      accordion_panel(
        "VITALS",
        
        #### Systolic Blood Pressure (mm/Hg) ----
        
        radioButtons(inputId="sys", label="Blood Pressure", choices=list("Normal (SBP <120 mmHG)",
          "Elevated (SBP 120-129 mmHG)",
          "High Blood Pressure Stage 1 (SBP 130-140 mmHG)",
          "High Blood Pressure Stage 2 (SBP >140 mmHG)"),
          selected=character(0)),
        
        #### Body Mass Index ----
        
        radioButtons(inputId="bmi", label="Body Mass Index", choices=list("Underweight (<18.5)",
          "Normal Weight (18.5-24.9)",
          "Overweight (25-29.9)",
          "Obese (>30)"),
          selected=character(0)),
      ),
      ### MEDICAL HISTORY ------------------------------------------------------------
      accordion_panel(
        "MEDICAL HISTORY",
        tags$p("Are you currently living with the following conditions?"),
        
        #### High Blood Cholesterol ----
        
        radioButtons(inputId="hbc",
          label="High Blood Cholesterol", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Cardiovascular Disease (CVD) ----
        
        radioButtons(inputId="cvd",
          label="Cardiovascular Disease", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Chronic Obstructive Pulmonary Disease (COPD) ----
        
        radioButtons(inputId="copd",
          label="Chronic Obstructive Pulmonary Disease", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Diabetes ----
        
        radioButtons(inputId="dia",
          label="Diabetes", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Depression ----
        
        radioButtons(inputId="dep",
          label="Depression", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Cancer ----
        
        radioButtons(inputId="can",
          label="Cancer", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Alzheimer ----
        
        radioButtons(inputId="alz",
          label="Alzheimer", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
      ),
      ### FAMILY HISTORY ===---------------------------------------------------------
      accordion_panel(
        "FAMILY HISTORY",
        tags$p("Has someone in your family (Father, Mother, Brother, Sister experienced the following conditions?"),
        
        #### Family History of Cardiovascular Disease (CVD) ----
        
        radioButtons(inputId="fcvd",
          label="Family History of Cardiovascular Disease", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Family History of Chronic Obstructive Pulmonary Disease (COPD) ----
        
        radioButtons(inputId="fcopd",
          label="Family History of Chronic Obstructive Pulmonary Disease", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Family History of Diabetes ----
        
        radioButtons(inputId="fdia",
          label="Family History of Diabetes", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Family History of Depression ----
        
        radioButtons(inputId="fdep",
          label="Family History of Depression", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Family History of Cancer ----
        
        radioButtons(inputId="fcan",
          label="Family History of Cancer", 
          choices=list("Yes",
            "No"),
          selected=character(0)),
        
        #### Family History of Alzheimer ----
        
        radioButtons(inputId="falz",
          label="Family History of Alzheimer", 
          choices=list("Yes",
            "No"),
          selected=character(0))
      )
    )

  ),
  
  ## Main Panel -------------------------------------------------------------------
  # The following section is to indicate what to display on the mainPanel
  
  layout_columns(
    
    tags$div(
      tags$h5("Based on the information entered in the grey area, statistically, you have:"),
      lapply(1:16, function(i) {
        tagList(
          tags$strong(textOutput(paste0("textprob", i), inline = TRUE),"%"),
          "chance of dying from", tags$strong(textOutput(paste0("textcause", i), inline = TRUE)),
          "at the age of",tags$strong(textOutput(paste0("textage", i), inline = TRUE)),
          br(),
        )
      })
    ),
    
    ## Plot Window Fitting ----
    # Here are parameters related to the bubble-plot's window fitting
    
    plotlyOutput("bubble", height="600", width="auto"),
    
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

factors <- readr::read_csv("factors.csv")
factors_cont <- readr::read_csv("factors_cont.csv")

# Given user input, take a copy of the factors.csv data and keep only the
# rows that match user input
filter_df_by_inputs <- function(df, input) {
  selected <- logical(nrow(df))
  for (name in unique(df[["var"]])) {
    if (!is.null(input[[name]])) {
      selected <- selected | (df$var == name & df$value == as.character(input[[name]]))
    }
  }
  df[selected,]
}

# Given user input, take a copy of the factors_cont.csv data and multiply
# each factor by the user input
multiply_by_inputs <- function(df, input) {
  vars <- unique(df[["var"]])
  values <- vapply(vars, \(v) {
    if (is.null(input[[v]])) {
      NA
    } else {
      input[[v]]
    }
  }, numeric(1))
  df %>%
    left_join(data.frame(var=vars, value=values), by="var") %>%
    mutate(multiplier = 1+(value*multiplier)) %>%
    select(-value) %>%
    group_by(category, cause) %>%
    summarise(multiplier = prod(multiplier), .groups="drop")
}


# SERVER -----------------------------------------------------------------------
# The Section below serve to define the "server" function for the server to create/use the R components for the app.

server <- function(input, output){
  
  #converting the cause of death dataframe (cod) into a reactive function / dataframe that changes according to inputs.  

  observe({
    print(cod_react())
  })
  cod_react<-reactive({
    current_factors <- filter_df_by_inputs(factors, input) %>%
      group_by(category, cause) %>%
      summarise(multiplier = prod(multiplier), .groups = "drop") %>%
      pivot_wider(names_from=category, values_from=multiplier, names_prefix="f_")
    
    current_factors_cont <- multiply_by_inputs(factors_cont, input) |>
      pivot_wider(names_from=category, values_from=multiplier, names_prefix="f_")
    
    cod %>%
      left_join(current_factors, by = "cause") %>%
      mutate(
        age = age * f_age,
        pop = pop * f_pop,
        risk = risk * f_risk
      ) %>%
      select(-f_age, -f_pop, -f_risk) %>%
      left_join(current_factors_cont, by = "cause") %>%
      mutate(
        age = age * f_age,
        pop = pop * f_pop,
        risk = risk * f_risk
      ) %>%
      select(-f_age, -f_pop, -f_risk) %>%
      mutate(age = (ifelse(age-5<=input$cage,input$cage+5,age))) %>%

      
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
  
  cod_react_ordered <- reactive({
    cod_react()[order(-cod_react()$risk),]
  })
  
  #///////////////////////////////////////////////////////////////////////////////   
  #create textoutput for all of the causes in order of increasing risk.
  
  lapply(1:16, function(i) {
    output[[paste0("textcause", i)]] <- renderText({ cod_react_ordered()[i,5] })
    output[[paste0("textprob", i)]] <- renderText({ cod_react_ordered()[i,6] })
    output[[paste0("textage", i)]] <- renderText({ cod_react_ordered()[i,"age"] })
  })
  
  # PLOT -------------------------------------------------------------------------
  # Set bubble plot (ggplot) parameters
  
  ({
    output$bubble <- renderPlotly({
      ggplotly(              
        ggplot(cod_react(), aes(x=age, y=probability, size = pop, color = cause, text=text)) +
          geom_point(alpha=0.6) + #use this line to make bubble +or- transparent
          geom_text(aes(label = cause), size=3, color="black") +
          scale_size(range = c(2, 30), name="Population (M)") +
          #coord_fixed() + 
          scale_color_viridis(discrete=TRUE, guide='none') +
          theme_ipsum() +
          scale_x_continuous(limits = c(input$cage, 100), breaks=c(10,20,30,40,50,60,70,80,90,100)) + #use this line to specify the x-axis range and tick positions
          scale_y_continuous(limits=c(0, NA), expand = expansion(mult = c(NA, 0.20))) +
          theme(legend.position="none"), 
        tooltip="text"
      )

      #        ,
      #        tooltip="text")
      #the code below is related to legend display.
      #       layout(legend=list(xanchor='Right',
      #                           yanchor='top',
      #                           title = NA,
      #                           itemsizing='constant'
      #        )) 
      
    })
    
  })
}      

#this knits together the ui and the server function.
shinyApp(ui = ui, server = server)

# The app code ends here
