# youlldie

youlldie is an R Shiny web app that statistically predicts one's cause and age of death based on one's inherited risk factors and lifestyle choices. It displays how and when death will likely occur in individual cases based on current scientific knowledge. It aims to provide a realistic perspective on death and incite users to adopt healthy lifestyles.

The app is currently available at https://youlldie.com

## The Code

The code behind the app follows the standard Shiny application structure. Please see the following for more information on Shiny apps: https://shiny.rstudio.com/tutorial/.

Essentially, the code has a "ui" component that serves to control the webpage display and a "server" component that provides the instructions to the server to create the output.

As such, all the code contained within "ui <- fluidPage()" serve to present input fields to users. Those input fields correspond to risk factors that have an impacts on the age, risk and population that dies from each cause of death. "ui <- fluidPage()" also contains some codes to display the outputs but the actual calculations take place within the "server <- function(input, output){}" portion of the code.

## The Calculation

The calculation is done as described below. The values that are used as inputs are drawn from an extensive review of publicly available databases and peer-reviewed articles. Those are available on the apps [References](https://youlldie.com/references/) page.

1. A dataframe is built to tabulate the most common causes of death and their baseline AGE OF DEATH (AGE), RISK OF DEATH (RISK) and RATE OF DEATH (RATE). 
    * Baseline AGE corresponds to the average age of death associated with each cause of death 
    * Baseline RISK corresponds to the death rate associated with each causes of death (n / 100,000) divided by the sum of the death rates associated with all causes of death. As such, RISK is a probability that death from a given cause will happen. It is a value confined between 0 and 1.  
    * Baseline RATE corresponds to the total population dying from each cause of death per year. It is also known as the Crude Death Rate.    

2. Values associated with the risk factors parameters impacts on the baseline AGE, RISK and RATE for different causes of death are set. For examples, "male" and "female" are the two parameters of the risk factor "sex". The value for the impact of the "male" parameter on AGE is <1 for the cause of death "cardiovascular diseases" because male die from cadiovascular diseases at a younger age than the average population composed of males and female. The values of the risk factors parameters impacts act as multiplier of the baseline AGE, RISK and RATE. Namely, risk factors parameters with impact values >1 increase the baseline AGE, RISK and RATE values whereas risk factors parameters with impact values <1 decrease the baseline AGE, RISK and RATE values. Risk factors parameters that increase AGE and decrease RISK and RATE are beneficial. Risk factors parameter that decrease AGE and increase RISK and RATE are detrimental. 

## The Output

The calculations performed above yield an updated dataframe which is plotted as a bubble plot with the AGE of Death on the x-axis and the RISK of Death on the y-axis. The size of the bubbles corresponds to the RATE of death aka the Crude Death Rate.
