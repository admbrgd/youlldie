# youlldie

youlldie is an R Shiny web app that statistically predicts one's cause and age of death based on one's inherited risk factors and lifestyle choices. It displays how and when death will likely occur in individual cases based on current scientific knowledge. It aims to provide a realistic perspective on death and incite users to adopt healthy lifestyles.

The app is currently available at https://youlldie.com

A combination of publicly available databases and peer-reviewed articles were used in the programming of the statistical model behind the app. Those are available on the apps [References](https://youlldie.com/references/) page. When contributing, please provide reference for your claims and make sure it is peer-reviewed material.

## The Code

The code behind the app follows the standard Shiny application structure. Please see the following for more information on Shiny apps: https://shiny.rstudio.com/tutorial/.

Essentially, the code has a "ui" component that serves to control the webpage display and a "server" component that provides the instructions to the server to create the output.

As such, all the code contained within "ui <- fluidPage()" serve to present input fields to users. Those input fields correspond to risk factors that have an impacts on the age, risk and population that dies from each cause of death. "ui <- fluidPage()" also contains some codes to display the outputs but the actual calculations take place within the "server <- function(input, output){}" portion of the code.

## The Calculation

The calculation is done as described below. The values that are used as inputs are drawn from an extensive review of peer-reviewed scientific literature available on the apps [References](https://youlldie.com/references/) page. 

1. A dataframe is built to tabulate the 16 most common cause of death and their baseline AGE OF DEATH, RISK OF DEATH and POPULATION. 
    * Baseline AGE OF DEATH correspond to the average age of death form each cause of death, 
    * Baseline RISK OF DEATH corresponds to the average risk of dying from each cause of death (n / 100,000)
    * Baseline POPULATION corresponds to the total population dying from each cause of death per year.    

2. The impacts of individual Risk Factors on the baseline AGE OF DEATH for different causes of death is set.

3. The impacts of individual Risk Factors on the baseline RISK OF DEATH from different causes of death is set.  

4. The impacts of individual Risk Factors on the baseline POPULATION associated with different causes of death is set.

## The Output

The calculations performed above yield an updated dataframe which is displayed. Namely: 

* The updated dataframe is plotted as a bubble plot with the AGE OF DEATH on the x-axis and the RISK of DEATH on the y-axis. The size of the bubbles corresponds to the POPULATION.

* The probability of dying from the different causes of death is calculated by dividing the RISK OF DEATH from the individual causes of death by the sum of the RISK OF DEATH from any causes of death.

## Template

You can find a template for setting a given cause of death AGE OF DEATH, RISK OF DEATH or POPULATION at the end of the code.
