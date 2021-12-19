uiCountTab <- function(id, tabName = "confirmed", label = "Confirmed", color = "warning", stateList = NULL) {

    # Counting page
    tabItem(tabName = tabName,
        tags$h1(paste(label, "Cases in India due to COVID-19"), class = "tab-section-heading"),
        fluidRow(
            bs4Card(
                # select states
                title = "Select States",
                collapsible = FALSE,
                closable = FALSE,
                maximizable = FALSE,
                fluidRow(
                    column(
                        width = 4,
                        cardPad(
                            tags$div(
                                h3("Selection of States to filter the Graphs"),
                                p("Click on the states on left pane to add it to the right pane and display"),
                                p("Click on the states on the right pane to remove it from the display"),
                                class = "inner-pane"
                            ),
                            color = paste(color)
                        )
                    ),
                    column(width = 8,
                            checkboxInput( paste(id, "stateAll", sep = "_") , label = "Add all states to selection"),
                            multiInput(
                                inputId = paste(id, "stateSelect", sep = "_"), label = "Select States from Left pane to Right pane to toggle selection",
                                selected = sample(as.character(stateList), size = 5),
                                choiceNames = as.character(stateList), 
                                choiceValues = as.character(stateList), 
                                width = "100%"
                            ) 
                    )
                ), width = 12  
            )
        ),
                
        fluidRow(
            # toggle input to cumulative cases
            bs4Card(
                title = paste("Number of", label, "Cases by States"),
                width = 12,
                collapsible = FALSE,
                closable = FALSE,
                maximizable = TRUE,
                fluidRow(
                    column(
                        width = 3,
                        cardPad(
                            tags$div(class = "inner-pane",
                                     dateRangeInput( paste(id, "date", sep = "_"), label = "Choose Range of Date", start = "2020-03-15", end = Sys.Date() ),
                                     tags$label("Change into Logarithmic Scale", class = "control-label"),
                                     switchInput(
                                         inputId = paste(id, "log", sep = "_"),
                                         value = FALSE,
                                         onLabel = "YES",
                                         offLabel = "NO"
                                     ),
                                     withMathJax()
                            ),
                            color = paste(color)
                        )
                        
                    ),
                    column(
                        width = 9,
                        plotlyOutput(paste(id, "plotCumulative", sep = "_"))
                    )
                )
            )
        ),
                    
        fluidRow(
            # toggle input to cases increments
            bs4Card(
                title = paste("Number of New", label, "Cases by States per day"),
                width = 12,
                collapsible = FALSE,
                closable = FALSE,
                maximizable = TRUE,
                fluidRow(
                    column(
                        width = 3,
                        cardPad(
                            tags$div(class = "inner-pane", 
                                     dateRangeInput(paste(id, "date2", sep = "_"), label = "Choose Range of Date", start = "2020-03-15", end = Sys.Date() ),
                                     tags$label("Do you want Percentage Change", class = "control-label"),
                                     switchInput(
                                         inputId = paste(id, "percent", sep = "_"),
                                         value = FALSE,
                                         onLabel = "YES",
                                         offLabel = "NO"
                                     ),
                                     withMathJax()
                            ),
                            color = paste(color)
                        )
                        
                    ),
                    column(
                        width = 9,
                        plotlyOutput(paste(id, "plotIncrement", sep = "_"))
                    )
                )
            )
        )    
    )


}