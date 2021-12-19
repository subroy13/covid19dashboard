uiSummaryTab <- function(stateList) {
    
    bs4TabItem(tabName = "summary",
            fluidRow(
                bs4Card(
                    inputId = "home-glance-card",
                    title = uiOutput("home-glance-card-title"),
                    collapsible = FALSE,
                    closable = FALSE,
                    maximizable = FALSE,
                    fluidRow(
                        tags$div(
                            class = "col-sm-3",
                            id = "confirmedBox",
                            bs4InfoBox(
                                title = uiOutput("confirmedBox-title"), 
                                value = "Confirmed Cases", 
                                icon = "heartbeat",
                                iconStatus = "primary",
                                status = NULL,
                                width = 12
                            )
                        ),
                        tags$div(
                            class = "col-sm-3",
                            id = "activeBox",
                            bs4InfoBox(
                                title = uiOutput("activeBox-title"), 
                                value = "Active Cases", 
                                icon = "bed",
                                iconStatus = "warning",
                                width = 12
                            )
                        ),
                        tags$div(
                            class = "col-sm-3",
                            id = "recoveredBox",
                            bs4InfoBox(
                                title = uiOutput("recoveredBox-title"), 
                                value = "Recovered Cases", 
                                icon = "medkit", 
                                iconStatus = "success",
                                width = 12
                            )
                        ),
                        tags$div(
                            class = "col-sm-3",
                            id = "deceasedBox",
                            bs4InfoBox(
                                title = uiOutput("deceasedBox-title"), 
                                value = "Death Cases", 
                                icon = "skull", 
                                iconStatus = "danger",
                                width = 12
                            )
                        )
                    ),
                    width = 12
                )
            ),
            
            fluidRow(
                bs4Card(
                    title = "COVID - 19 Situation in India and its States",
                    width = 12,
                    collapsible = FALSE,
                    closable = FALSE,
                    maximizable = TRUE,
                    fluidRow(
                        tags$div(class = "col-lg-4",
                                 cardPad(
                                     tags$div(
                                         class = "inner-pane",
                                         p("This map indicates 3 types of display for Confirmed, Active, Recovered and Death Cases:"),
                                         tags$ol(
                                             tags$li(tags$b("Count of Cases:"), "which is the latest number of cases"),
                                             tags$li(tags$b("Cases per million population:"), "which shows the prevalence of number of cases"),
                                             tags$li(tags$b("Cases per area:"), "which denotes the density of cases per square kilometer area")
                                         ),
                                         pickerInput("input-home-var", label = "Select Variable to Display",
                                                     choices = c("Confirmed", "Active", "Recovered", "Deaths")),
                                         pickerInput("input-home-type", label = "Select Type of Transformation",
                                                     choices = c("Count of Cases" = 1, "Cases per million population" = 2,
                                                                 "Cases per area (sq. km.)" = 3)),
                                         pickerInput("input-home-state", label = "Select States to Display",
                                                     choices = c("Whole India", stateList ),
                                                     options = pickerOptions(
                                                         liveSearch = TRUE,
                                                         size = 8
                                                     )
                                         )
                                     )
                                 )   
                        ),
                        tags$div(class = "col-lg-8", 
                                 plotOutput("plot-home-map", width = "100%", height = 700)     
                        )
                    )
                )
            )
            
    )  # end of TAB
}
    
    
                     
    
        