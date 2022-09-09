# COVID-19 SEIR model for Bolivia

## Live version

https://rafalopezv.io/covid19_bolivia/

## Methodological annex

https://rafalopezv.io/static/covid/covid_19_bolivia.pdf

## Technical approach

- We wanted to test:
  + If there was a variation of the R0 estimate before and after quarantine was declared. We detected a decrease from R0=6 to R0=2.4 
  + How many days the quarantine postponed the saturation of public and private health services. We found there was a delay of 29 days
  + How many days the peak of the pandemic could be postponed due to teh quarantine declaration. We found that the peak was delayed in 98 days
  + We run several simulations to find the R0 that couuld reproduce the first 6 weeks of COVID.-19 infections and run the model based on that and some other fixed criteria such as probability of contagion
  
## How to reproduce the code

- Run [modelo.R](https://github.com/rafalopezv/COVID-19-SEIR-model-for-Bolivia/blob/master/modelo.R)
- Run [visualizaciones.R](https://github.com/rafalopezv/COVID-19-SEIR-model-for-Bolivia/blob/master/visualizaciones.R) to reproduce gif animations 

## Languaje

- Code comments and live version are in spanish


![animacion_con_cuarentena](https://user-images.githubusercontent.com/17109075/189262639-9b2092a7-941e-428d-8c23-e9176d52efca.gif)
