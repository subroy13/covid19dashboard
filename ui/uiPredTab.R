uiPredTab <- function(IndiaList) {
    tabItem(tabName = "prediction",
            tags$h1("Prediction of COVID-19 Spread", class = "tab-section-heading"),
            fluidRow(
                bs4Card(
                    title = "Step 1: Choose the State (or UT) and District to build Prediction",
                    inputId = "pred-step1",
                    width = 12,
                    collapsible = TRUE,
                    maximizable = FALSE,
                    closable = FALSE,
                    pickerInput(
                            inputId = "input-pred-state",
                            label = "A : Choose a State or UT of India",
                            choices = c("All India", unique(IndiaList$State)),
                            options = pickerOptions(
                                liveSearch = TRUE,
                                size = 8
                            )
                    ),
                    pickerInput(
                            inputId = "input-pred-district",
                            label = "Select District of India",
                            choices = c("All India"),
                            options = pickerOptions(
                                liveSearch = TRUE,
                                size = 8
                            )
                    ),
                    tags$div(
                        tags$label("C : Click SUBMIT to view prediction", class = "control-label d-block"),
                        actionBttn(
                            inputId = "submit-pred",
                            label = "SUBMIT",
                            style = "unite", 
                            color = "success"
                        )
                    ),
                    footer = "Note: This step should not take more than a minute. Please bear with us. The estimated time for this step at State level is 1 minute and at the District level is 30 seconds."
                )
            ), 
            fluidRow(
                bs4Card(
                    title = 'Step 2: Choose the scenario to view',
                    inputId = "pred-step2",
                    width = 12,
                    collapsible = TRUE,
                    closable = FALSE,
                    maximizable = FALSE,
                    fluidRow(
                        tags$div(class = "col-lg-3",
                                 cardPad(
                                     tags$div(
                                         class = "inner-pane",
                                         helpText("Please choose the hypothetical scenarios of different social distancing behaviors of general public to see the relevant prediction under that scenario."),
                                         pickerInput(
                                             inputId = "input-pred-distancing",
                                             label = "Select the Level of Social Distancing during Covid-19",
                                             choices = c("No social distancing" = 1, "Minimal social distancing" = 2, "Strict social distancing" = 3)
                                         ),
                                         tags$label("Do you want to view 95% Confidence Interval?", class="control-label"),
                                         switchInput(
                                             "input-pred-ci", 
                                             value = FALSE,
                                             onLabel = "Yes",
                                             offLabel = "No"
                                         ),
                                         pickerInput(
                                             inputId = "input-pred-var",
                                             label = "Select Variable to display",
                                             choices = c("Both", "Active Cases Only")
                                         )
                                     )
                                 )     
                                 
                        ),
                        tags$div(
                            class = "col-lg-9",
                            tags$div(
                                uiOutput("title-pred"),
                                uiOutput("plottext-pred"),
                                class = "px-5 text-justify pb-5"
                            ),
                            plotlyOutput("plot-pred")
                        )
                    ),
                    footer = "Note: This step should only take a few seconds to process unlike Step 1."
                ),
                fluidRow(
                    tags$div(
                    tags$b("About the model for prediction:"),
                    tags$p("The eSIR i.e. Extended SIR model, proposed by",  
                               tags$a("Wang et. al.", href = "https://www.medrxiv.org/content/10.1101/2020.02.29.20029421v1", target = "_blank"),
                               "is a direct generalization of the usual SIR model. It incorporates 
                               various types of time-varying quarantine protocols, 
                               including government-level macro isolation policies 
                               using a function $\\pi(t)$ that parametrizes it.  
                               While applying the eSIR model, we have estimated the $\\pi(t)$-function by normalizing 
                               time-variant estimation of the reproduction number $R_t$."),
                    class = "col-md-10 mx-auto mb-4 mt-4 text-justify"
                    )
                )
            )
            
        )
}

