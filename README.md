# World Values Survey Explorer

An interactive Shiny dashboard for exploring data from the **World Values Survey (WVS) Wave 6**, 
covering 60 countries and over 89,000 respondents.

## 🌐 Live App
👉 [Click here to open the app](https://alyssa-hyde.shinyapps.io/wvs-explorer/)

## 📋 About the App
This dashboard allows users to select a country and explore three themes:

- **Democracy** — How people rate different aspects of democracy (V228A–V228I).  
  Scale: 1 = Not at all democratic, 4 = Definitely democratic.
- **News Consumption** — How often people consume news from different sources (V217–V224).  
  Scale: 1 = Daily, 5 = Never.
- **Attitudes to Science** — People's views on science and technology (V192–V197).  
  Scale: 1 = Strongly disagree, 10 = Strongly agree.

Each section includes:
1. An interactive chart for the selected country
2. An interactive table with country-level averages/proportions
3. A global comparison table across all 60 countries

## 📁 File Structure
wvs-explorer/
├── app.R              # Shiny app
├── prep_data.R        # Data cleaning and preparation script
├── data/
│   └── wvs_prepared.RData   # Cleaned data used by the app
└── README.md

## 🗂️ Data Source
World Values Survey Wave 6 (2010–2014)  
[www.worldvaluessurvey.org](http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp)

## 📦 R Packages Used
- `shiny` — web app framework
- `shinydashboard` — dashboard layout
- `tidyverse` — data manipulation
- `plotly` — interactive charts
- `DT` — interactive tables

## 🚀 How to Run Locally
1. Clone this repository
2. Download the raw WVS Wave 6 data from the link above and place it in `data/`
3. Run `prep_data.R` to generate `wvs_prepared.RData`
4. Run `app.R` in RStudio