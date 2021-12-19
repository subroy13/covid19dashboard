library(shiny)
library(shinyWidgets)
library(shinybusy)
library(bs4Dash)

library(dplyr)
library(readr)
library(tidyr)

library(EpiEstim)
library(sf)

library(ggplot2)
library(plotly)

source('./path_details.R')

# Load the custom modules
source('./ui/uiNavbar.R')
source('./ui/uiHomeTab.R')
source('./ui/uiSummaryTab.R')
source('./ui/uiCountTab.R')
source('./ui/uiR0Tab.R')
source('./ui/uiPredTab.R')

source('./server/serverDataMaker.R')
source('./server/serverSummaryTab.R')
source('./server/serverCountTab.R')
source('./server/serverR0Tab.R')
source('./server/serverPredTab.R')


IndiaList <- read_csv(paste0(DATA_PATH, 'datasets/india-details.csv'))

ui <- bs4DashPage(
    title = "COVID 19 Dashboard",
    sidebar_collapsed = TRUE,
    enable_preloader = TRUE,
    loading_duration = 1,
    loading_background = "#ffebcd",
    controlbar_overlay = FALSE,
    navbar = bs4DashNavbar(
        skin = "dark",
        status = "dark",
        rightUi = uiOutput("lastupdatedtimestamp", inline = FALSE)
    ),
    sidebar = uiNavbar,
    body = bs4DashBody(
        tags$head(
            tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100;0,300;0,400;0,500;0,700;0,900;1,100;1,300;1,400;1,500;1,700;1,900&family=Lato:ital,wght@1,900&display=swap"),
            tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
            tags$div(HTML("<script type='text/x-mathjax-config' >
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$']]}
                });
                </script>
                "))
        ),
        bs4TabItems(
            uiHomeTab,
            uiSummaryTab(unique(IndiaList$State)),
            uiCountTab(id = "confirmed", tabName = "confirmed", label = "Confirmed", color = "danger", stateList = unique(IndiaList$State)),
            uiCountTab(id = "active", tabName = "active", label = "Active", color = "warning", stateList = unique(IndiaList$State) ),
            uiCountTab(id = "recovered", tabName = "recovered", label = "Recovered", color = "success", stateList = unique(IndiaList$State) ),
            uiCountTab(id = "deaths", tabName = "deaths", label = "Deaths", color = "dark", stateList = unique(IndiaList$State) ),
            uiR0Tab(IndiaList),
            uiPredTab(IndiaList)
        )
    ),
    footer = bs4DashFooter(
        fixed = FALSE,
        copyrights = a(
            href = "https://www.iimv.ac.in/", 
            target = "_blank", "Indian Institute of Management, Visakhapatnam"
        ),
        right_text = "2020"
    )
)

server <- function(input, output, session) {
    aList <- update_data(as.POSIXct(format(Sys.time(), tz = "Asia/Kolkata"), tz = "Asia/Kolkata") , session)
    
    df <- aList[[1]]
    distdf <- aList[[2]]
    
    signTime <- readRDS(paste0(DATA_PATH, 'datasets/signTime.Rds'))  # read in the signature time
    
    latest_df <- df %>% dplyr::filter(Date == max(Date))     # extract latest observations
    latest_distdf <- distdf %>% dplyr::filter(Date == max(Date))   # extract latest observations for district level
    
    
    output$lastupdatedtimestamp <- renderUI({
        tags$p(paste("Last updated on: ", as.character(format(signTime["data"], tz = "Asia/Kolkata")) ), style = "color:white; margin-bottom:0; margin-right:1rem;")
    })
    
    # update homepage
    observeEvent(input[["see-more-btn"]], {
        updatebs4TabItems(session, inputId = "mainMenu", selected = 2)
    })
    
    observeEvent(input[["pred-more-btn"]], {
        updatebs4TabItems(session, inputId = "mainMenu", selected = 9)
    })
    
    # Update summary page
    serverSummaryTab(input, output, session, IndiaList, latest_df, latest_distdf)
    
    # Update Counts page
    serverCountTab(input, output, session, "confirmed", df, unique(IndiaList$State), label = "Confirmed")
    serverCountTab(input, output, session, "active", df, unique(IndiaList$State), label = "Active")
    serverCountTab(input, output, session, "recovered", df, unique(IndiaList$State), label = "Recovered")
    serverCountTab(input, output, session, "deaths", df, unique(IndiaList$State), label = "Deaths")
    
    # server R0 page
    serverR0Tab(input, output, session, IndiaList, df, distdf)
    
    # server prediction page
    serverPredTab(input, output, session, IndiaList, df, distdf, user_time = as.POSIXct(format(Sys.time(), tz = "Asia/Kolkata"), tz = "Asia/Kolkata") )
    
}


shinyApp(ui, server)













