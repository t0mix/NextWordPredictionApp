This app has been built for my Data Science Capstone project and has been kept as simple as possible. It takes a phrase as input and outputs a prediction of the next word.

Check this presentation on R Pubs: http://rpubs.com/t0mix/76819

Files:

 model.R - preprocessing and prediction functions
 freqs.RData - prediction data
 modelCreation.R - only if new prediction data should be generated
 server.R and ui.R - shiny app files implementing the model.R and  using the prediction data from freqs.RData

App is running on ShinyApps.io: https://t0mix.shinyapps.io/NextWordPredictionApp/

Author: Thomas Tsang
Date: April 26, 2015