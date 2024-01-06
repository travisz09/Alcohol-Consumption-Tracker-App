# Alcohol-Consumption-Tracker-App
Conveniently log and visualize your alcohol consumption.

<em>DISCLAIMER!</em>
I am not a doctor. This app was developed for my personal use, and is being shared for free to anybody who might find it useful. This app is not intended to diagnose, treat, cure, or prevent any disease. Please consult with a health care professional before making any changes to your drinking habits. Please <a href='https://www.samhsa.gov/find-help/national-helpline'>seek help</a> from qualified professionals if you think you may be suffering from substance abuse issues. <font style="color:red"><b>DON'T DRINK AND DRIVE!</b></font>

This R <a href='https://shiny.posit.co/'>Shiny<a> App is a convenient way for me to quickly log my drinking habits from anywhere. I specifically wanted a convenient way to update from my smartphone and grew tired of using the GoogleSheets app (not mobile friendly).
  
To use this app you will need:
  1) R installed, with the required package "shiny".
  2) A local directory from which to deploy the app.
  3) A free <a href='https://www.shinyapps.io/'>shinyapps.io<a> account to host your app.
  4) A free <a href='https://www.google.com/sheets/about/'>Google Sheet<a> with 8 columns as follows; 'date', 'time', 'drink', 'brand', 'style', 'abv', 'volume', 'notes'.
  
To launch this app:
  1) Save the files in any directory on your machine.
  2) Follow the steps to set up your shinyapp.io account and add the authentication token to the deploy_app.R file.
  3) Link to your GoogleSheet file in the three places indicated in the app.R file.
  4) Optional. Change the timezone settings for users not in the PST timezone (US West Coast).
  5) Run the deploy_app.R script.
  6) Save the address of your new app and enjoy.

Future features updates will include data summary statistics and visualizations.
