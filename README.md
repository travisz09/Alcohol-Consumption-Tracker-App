# Alcohol-Consumption-Tracker-App
A mobile-friendly R Shiny application for logging alcohol consumption and viewing estimated BAC and sobriety information.

## Disclaimer
<em>DISCLAIMER!</em>
I am not a doctor. This app was developed for my personal use, and is being shared for free to anybody who might find it useful. This app is not intended to diagnose, treat, cure, or prevent any disease. Please consult with a health care professional before making any changes to your drinking habits. Please [seek help](https://www.samhsa.gov/find-help/national-helpline) from qualified professionals if you think you may be suffering from substance abuse issues. <font style="color:red"><b>DON'T DRINK AND DRIVE!</b></font>

## About
This R [Shiny](https://shiny.posit.co/) App is a convenient way to keep track of my drinking habits from anywhere. Whether you are a regular drinker, or just have an occasional drink, knowing your habits can help you make good decisions. Using a few simple inputs, this app will keep a record of your drinks, **on a private GSheet that you own**, estimate your blood alcohol concentration (BAC), and even keep track of your sobriety! *Son of a bitch, If I can't get clean, I'm gonna drink my life away!*

## Features
- Mobile-friendly drink logging. *Have a drink on me*. 
- Google Sheets persistence
- Flexible drink/brand/style history, whether you like bourbon, or scotch, or beer
- Estimated BAC (*seeing double-double*)
- Estimated time until sober / time sober. *So starting today, I'm breaking out of this cage*.

## Requirements
- R 4.4+
- Google account
- Google Sheet
- Google Cloud service account
- Google Sheets API enabled
- Posit Connect Cloud account (for hosted deployment)

## Google Sheets Schema
| Column | Description |
| --- | --- |
| date | Drink date |
| time | Drink time |
| drink | Drink category |
| brand | Brand |
| style | Style/type |
| abv | Alcohol by volume (%) |
| volume | Volume (oz) |
| notes | Optional notes |

## Local Setup

1. Clone repository.
2. Restore packages:

   `renv::restore()`

3. Configure Google service account.
   - Create and download a service-account JSON key.
   - Enable the Google Sheets API.
4. Share Sheet with service-account email.
5. Configure environment variables.
   - Base-64 encode the JSON
   - use `usethis::edit_r_environ(scope = "user")` to load the encoded JSON into .Renviron file
6. Run application.

## Configuration

Required environment variables:

| Variable | Value|
|------------------------------|------------------------------|
| `GOOGLE_SERVICE_ACCOUNT_B64` | Base64-encoded service-account JSON |
| `GOOGLE_SHEET_ID` | Google Sheet ID |

> [WARNING!]
> Never commit or publish your Google service-account credentials. Keep .Renviron and service-account JSON files outside version control. *The careless whispers of a good friend*.
  
## Running Locally

`shiny::runApp()`

## Testing

`testthat::test_dir("tests/testthat")`

## Deployment

Instructions for Posit Connect Cloud:

- deploy through Positron
- configure the two secret variables
- do not deploy `.Renviron` or credential JSON
- publish `app.R`

## Known Limitations / Future Work

- Drink sessions currently rely on separate date/time fields.
- Sessions spanning midnight need improved handling. *Sleepin' with my clothes on*. 
- BAC is an estimate, not a measurement.

### TODOs:
- Create page of figures and summary statistics
- Password protect

## License
MIT

> Hangovers only last a day, but memories last a lifetime.
