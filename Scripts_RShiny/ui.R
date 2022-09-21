# Import libraries:
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(surveillance)
# Define UI: Three tabs (Forecasts, Predictors, and Scenarios)
# In each tab, a set of input that impact what output is plotted
shinyUI(fluidPage(
    tags$style(".smaller--p { font-size: 90%;}"
    ),
    tags$style('body {font-size: 12px;}'),
    tags$style(".irs--shiny .irs-handle {top: 17px; width: 18px; height: 18px;}"),
    tags$style(".irs--shiny .irs-min, .irs--shiny .irs-max,
               .irs--shiny .irs-single, .irs-grid-text {font-size: 8px;}"),

    shinyjs::useShinyjs(),
    tags$head(
        tags$style(HTML(".leaflet-container { background: white; }"))
    ),
    titlePanel(windowTitle = "Covid-19 local outbreak risk in the EU/EEA",
               h3("Covid-19 local outbreak risk in the EU/EEA")),
    tabsetPanel(
        # First Panel: Forecasts
        tabPanel("Forecasts", fluid = TRUE,
                 # Tab title
                 h4("Forecasted COVID-19 case incidence over the next 28 days"),
                 
                 p("Click on the map to plot local forecasts; click outside the map to move back to national level forecasts"),
                 # Outputs of this panel = Incidence map and time series 
                 fluidRow(column(10, offset = 1,
                                 splitLayout(cellWidths = c("33%", "67%"),
                                             leafletOutput("map", height = "300px"),
                                             plotOutput("preds", height = "300px"))
                 )),
                 
                 
                 hr(),
                 
                 HTML("<p>Use the control widgets below to change the country, geographical resolution, and format of the plots. 
                      Check our <a href='https://github.com/alxsrobert/RShiny_covid_ECDC/raw/main/RShiny%20App%20user%20guide.pdf'>
                      user guide</a> for more details </p>"),
                 # Set of inputs:
                 fluidRow(column(3, offset = 1, 
                                 # Which country is to be plotted (default = France)
                                 selectInput("country", "Country", 
                                             choices = list("France" = 1, "Czechia" = 2, "Italy" = 3), 
                                             selected = 1),
                                 # Should the time-series be age-stratied (default = no)
                                 checkboxInput("age", "Plot age-stratified forecasts", value = FALSE),
                                 # Should the y-axis of the time series be log-scale (default = no)
                                 checkboxInput("log", "Use logarithmic y-axis in the time-series plot", value = FALSE)),
                          column(4,
                                 # What Geographical resolution should be plotted (default = Fine (NUTS-3))
                                 radioButtons("nuts", "Geographical resolution",
                                              choices = list("Coarse (NUTS-2)" = 1, "Fine (NUTS-3)" = 2),
                                              selected = 2, inline = T),
                                 # What quantile of the prediction should be plotted (default = Median)
                                 sliderInput("quant", "Prediction quantile shown on the map:",
                                             min = 0, max = 100, value = 50, step = 10)),
                          column(3,
                                 # How many data points should be visible in the time series (default 28 days)
                                 sliderInput("weeks", "Number of observed weeks in the time-series plot:", min = 1, max = 20, 
                                             value = 4, step = 1, width = "100%"),
                                 # What prediction date should be used (default = latest available date)
                                 sliderInput("prev", "Number of weeks before the last observed data point from which to predict:", 
                                             min = 0, max = 12, value = 0, step = 1, width = "100%")
                          )),
                 p(class = "smaller--p", "NUTS = Nomenclature of Territorial Units for Statistics"),
                 p(class = "smaller--p", 
                   "Age-stratified forecasts were not generated for Italy because subnational age-stratified surveillance data were not available"),
                 p(class = "smaller--p", "Large daily variations in the forecasts are due to a day-of-the-week effect in the reported data (e.g. fewer cases are reported on Sundays)"),
                 p(class = "smaller--p", "Administrative boundaries, \u00A9 EuroGeographics, \u00A9 TurkStat. Source: European Commission - Eurostat/GISCO, 
                   The boundaries and names shown on this map do not imply official endorsement or acceptance by the European Union"),
        ),
        
        # Second Panel: Predictors
        tabPanel("Predictors", fluid = TRUE,
                 
                 # Tab title
                 h4("Latest forecasted COVID-19 case incidence; Local risks of onwards transmission and importation"),
                 
                 HTML("<p>Use the control widgets below to change the country, geographical resolution, and format of the maps.  
                      Check our <a href='https://github.com/alxsrobert/RShiny_covid_ECDC/raw/main/RShiny%20App%20user%20guide.pdf'>
                      user guide</a> for more details </p>"),
                 # Set of inputs:
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,
                         # Which country is to be plotted (default = France)
                         selectInput("country2", "Country", 
                                     choices = list("France" = 1, "Czechia" = 2, "Italy" = 3), selected = 1),
                         # What Geographical resolution should be plotted (default = Fine (NUTS-3))
                         radioButtons("nuts2", "Geographical resolution",
                                      choices = list("Coarse (NUTS-2)" = 1, "Fine (NUTS-3)" = 2),selected = 2, inline = T),
                         # What quantile of the prediction should be plotted (default = Median)
                         sliderInput("quant2", "Prediction quantile shown on the incidence map:", min = 0,
                                     max = 100, value = 50, step = 10),
                         # What type of map should be plotted (default = Incidence)
                         radioButtons("type", "Type of incidence map (changes the left-side map)",
                                      choices = list("14-day Incidence per 100,000 inhabitants" = 1, 
                                                     "Number of cases over the next 14 days" = 2, 
                                                     "Changes compared to the last week of data" = 3),
                                      selected = 1),
                     ),
                     
                     mainPanel(width = 9,
                               splitLayout(cellWidths = c("34%", "33%", "33%"),
                                           plotOutput("map2"), plotOutput("ar"), plotOutput("en"))
                     )
                 ),
                 p(class = "smaller--p", "NUTS = Nomenclature of Territorial Units for Statistics"),
                 p(class = "smaller--p", "Administrative boundaries, \u00A9 EuroGeographics, \u00A9 TurkStat. Source: European Commission - Eurostat/GISCO,
                   The boundaries and names shown on this map do not imply official endorsement or acceptance by the European Union")

        ),
        
        # Third Panel: Scenarios
        tabPanel("Scenarios", fluid = TRUE,
                 
                 # Tab title
                 h4("Impact of changes in transmission on 28-day forecasts"),
                 
                 p("Click on the map to plot local forecasts; click outside the map to move back to national level forecasts"),
                 
                 # Show generated simulations
                 fluidRow(column(10, offset = 1,
                                 splitLayout(cellWidths = c("33%", "67%"), 
                                             leafletOutput("map3", height = "300px"), 
                                             plotOutput("preds2", height = "300px"))
                 )),
                 
                 hr(),
                 
                 HTML("<p>Use the control widgets below to change the country, geographical resolution, format of the plots and level of transmission.
                      Check our <a href='https://github.com/alxsrobert/RShiny_covid_ECDC/raw/main/RShiny%20App%20user%20guide.pdf'>
                      user guide</a> for more details </p>"),
                 # Set of inputs:
                 fluidRow(column(3, offset = 1, 
                                 # Which country is to be plotted (default = France)
                                 selectInput("country3", "Country", 
                                             choices = list("France" = 1, "Czechia" = 2, "Italy" = 3), selected = 1),
                                 # Should the time-series be age-stratied (default = no)
                                 checkboxInput("age2", "Plot age-stratified forecasts", value = FALSE),
                                 # Should the y-axis of the time series be log-scale (default = no)
                                 checkboxInput("log2", "Use logarithmic y-axis in the time-series plot", value = FALSE),
                                 # What Geographical resolution should be plotted (default = Fine (NUTS-3))
                                 radioButtons("nuts3", "Geographical resolution",
                                              choices = list("Coarse (NUTS-2)" = 1, "Fine (NUTS-3)" = 2),selected = 2, inline = T)),
                          column(4,
                                 # How many data points should be visible on the time series (default 28 days)
                                 sliderInput("weeks2", "Number of observed weeks in the time-series plot:", min = 1, 
                                             max = 10, value = 4, step = 1, width = "85%"),
                                 # What quantile of the prediction should be plotted (default = Median)
                                 sliderInput("quant3", "Prediction quantile shown on the map:", 
                                             min = 0, max = 100, value = 50, step = 10, width = "85%"),
                                 # Impact of an increase in transmission (default = 0%) 
                                 sliderInput("transmi", "Increase in transmission due to changes in behaviour or new variant (%):", 
                                             min = 0, max = 40, value = 0, step = 20, width = "85%")),
                          column(3,
                                 # Impact of an decrease in transmission due to NPIs (default = 0%) 
                                 sliderInput("NPI", "Drop in transmission due to NPIs (%):", 
                                             min = 0, max = 40, value = 0, step = 20, width = "100%"),
                                 # Age group targeted by the NPIs (default = Everyone) 
                                 radioButtons("target", "Population targeted by the NPIs", 
                                              choices = c("Everyone" = 1, "<20 yo" = 2, 
                                                          "20-60 yo" = 3,">60 yo" = 4),
                                              selected = 1, inline = T),
                                 # Should the number of importations (i.e stemming from the 
                                 # endemic component) be set to 0? (default = No)
                                 checkboxInput("import", "Remove importations from outside the selected country", value = F, width = "100%"),
                                 # What is the delay until NPIs become effective (Default = One week)
                                 radioButtons("delay", "Date of NPIs becoming effective", 
                                              choices = list("One week delay" = 1, "Two week delay" = 2), 
                                              selected = 1, inline = T)
                          )),
                 
                 p(class = "smaller--p", "NUTS = Nomenclature of Territorial Units for Statistics; NPIs = non-pharmaceutical interventions"),
                 p(class = "smaller--p", "Age-stratified forecasts were not generated for Italy because subnational age-stratified surveillance data were not available"),
                 p(class = "smaller--p", "Large daily variations in the forecasts are due to a day-of-the-week effect in the reported data (e.g. fewer cases are reported on Sundays)"),
                 p(class = "smaller--p", "NPIs correspond to control 
                 measures (e.g. business and school closures, gathering bans..) 
                 aiming to slow the spread of COVID-19. The values presented here 
                 should be interpreted with caution since the efficacy of Non-Pharmaceutical
                 Interventions depends on various factors, such as behaviour and 
                 adherence, that are not explicitly incorporated in our model."),
                 p(class = "smaller--p", "Administrative boundaries, \u00A9 EuroGeographics, \u00A9 TurkStat. Source: European Commission - Eurostat/GISCO,
                   The boundaries and names shown on this map do not imply official endorsement or acceptance by the European Union"),
        )
        
    )
))
