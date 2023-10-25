# youlldie

youlldie is a web app that statistically predicts your life expectancy. Specifically, it determines your risk of death from different causes based on your inherited risk factors, environment and lifestyle choices. The algorithm behind youlldie leverages information gathered from peer-reviewed literature and public databases.

The app is currently available at https://youlldie.com

## Vision for the project

This project aims to 
1. Develop an accurate life expectancy model
2. Provide a realistic perspective of death to users and health workers
3. Incite users to adopt healthy lifestyles

**Practically**, the goal of this project is to develop a tool that calculates the risk of death from different causes and the average age of death from each causes for a given population characterized by certain risk factors. The risk factors are entered into the apps by users. Specifically, without any input, it provide the life expectancy of the world's population. Every input refines the life expectancy prediction to a more specific population. As such, this tool aims to provide a statisitcally acurate and realistic perspective of death to the general public and the medical profession. It also aims to incite users to adopt healthy lifestyles.

## Roadmap

### Global Healthcare Database Driven

Currently, the determination of the basal risk of death from the different causes, the average age of death from each causes and the multipliers associated with each risk factors is based on an extensive review of the litterature and public databases. The references are documented here: https://youlldie.com/references/. To have a more accurate predictions, those parameters could be determined in near real time from data provided from healthcare systems. Specifically, if the app could use HL7-compliant datasets pulled from the world's healthcare systems as input files, the app's output would be very interesting. Aligning with this vision, the app is currently designed to be data-driven. Namely, it is able to make use of external files (see repo files: factor.csv and factor_cont.csv file).

As such, one of the current challenges is the dissemination of the app and the acquisition of endorsement from the world health authorities and their health system vendors.

### Data Transfer Specifications

A standar format of acquiring global health data needs to be determined. The variables required are listed below:

* Death ID
* Primary cause of death
* Gender
* Race
* Age of Death
* World Bank Region
* Financial Status
* Highest level of Schooling
* Number of Drinks per week
* Number of smokes per week
* Number of moderate-intensity physical activity minutes per week
* Number of vigorous-intensity physical activity minutes per week
* Number of hours of sleep per day
* Systolic Blood Pressure
* Body Mass Index	
* High Blood Cholesterol (Yes/No)
* Cardiovascular Disease (Yes/No)
* Chronic Obstructive Pulmunary Disease (Yes/No)
* Diabetes (Yes/No)
* Depression (Yes/No)
* Cancer (Yes/No)
* Alzeimer (Yes/No)
* Family History of Cardiovascular Disease (Yes/No)
* Family History of Family History of Chronic Obstructive
* Pulmunary (Yes/No)
* Family History of Diabetes (Yes/No)
* Family History of Depression (Yes/No)
* Family History of Cancer (Yes/No)
* Family History of Alzeimer (Yes/No)

**Technically**, the contribution from the open-source community helps making this app a statistically accurate tool. The algorythm behind the app represents model of human longevity which can in turn be used to measure the impact of external factors on human health and ultimately improve the human condition. 

**Phylosophically**, the purpose of the app is to make people contemplate their position in the stream of life. To borrow some words form Alan Watts, everybody should, sometime in their lifetime, consider death. Observe skulls and skeletons and wonder what it will be like to go to sleep and never wake up.Never! That is a very gloomy thing for contemplation. But it’s just like manure. Just as manure fertilizes the plants and so on, so the contemplation of death and the acceptance of death is very highly generative of creating life. You’ll get wonderful things out of that. Death is important to think about. It must not be swept under the carpet. Thinking about and accepting death brings a trust in life. It incites one to let go. Stop clinging to constantly changing things that cannot be clung to. Recognize oneself as an unstable particle in the constantly changing flux of eternal life. Acknowledge the union and inseparability from everything else that there is. The contemplation of death allows one to change point of view and to find oneself. It awakens the senses. Thinking about death gives the opportunity to understand what life is all about and see what this universe is for. It is conducive to liberation. Understanding that everything is in the right place is the opportunity presented by the contemplation of death.

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
