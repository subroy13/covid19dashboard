uiR0Tab <- function(IndiaList) {
    tabItem(tabName = "R0",
            tags$h1("Estimation of Time Varying Reproduction Rates of COVID-19", class = "tab-section-heading"),
                   fluidRow(
                       bs4Card(title = "Time Varying Reproduction Rate of Whole India",
                        collapsible = FALSE,
                        maximizable = TRUE,
                        closable = FALSE,
                           width = 12,
                           fluidRow(
                               column(width = 4,
                                      cardPad(
                                          tags$div(class = "inner-pane",
                                            h4("What is Reproduction Rate"),
                                            p("Reproduction Rate is the average number of secondary infections that a person infected with COVID-19 spreads, before recovering from it or before dying.")   
                                          )
                                      )
                               ),
                               column(width = 8,
                                      plotlyOutput("plot-R0-india")    
                               )
                           )
                       )
                   ),
                   
                   fluidRow(
                       bs4Card(title = "Time Varying Reproduction Rates of different States of India",
                               collapsible = FALSE,
                               maximizable = TRUE,
                               closable = FALSE,
                           width = 12,
                           fluidRow(
                               column(width = 4,
                                      cardPad(
                                          tags$div(
                                              class = "inner-pane",
                                              pickerInput(
                                                  inputId = "input-R0-state",
                                                  label = "Select State of India",
                                                  choices = unique(IndiaList$State),
                                                  options = pickerOptions(
                                                      liveSearch = TRUE,
                                                      size = 7)
                                              ),
                                              p("The significance of Reproduction Rate $R_0$ is in determining whether an infectious disease is endemic or pandemic."),
                                              tags$ol(
                                                  tags$li("$R_0 > 1$ means a single person spreads it to more than one person in average, suggesting explode of the pandemic"),
                                                  tags$li("$R_0 < 1$ means a single person spreads it to less than one person in average, suggesting controlled state of infectious disease")
                                              )
                                          )
                                      )
                               ),
                               column(width = 8,
                                      uiOutput("title-R0-state"),
                                      plotlyOutput("plot-R0-state")    
                               )
                           )
                       )
                   ),
                   
                   fluidRow(
                       box(title = "Time Varying Reproduction Rates of different Districts of India",
                           collapsible = FALSE,
                           maximizable = TRUE,
                           closable = FALSE,
                           width = 12,    
                           fluidRow(
                               column(width = 4,
                                      cardPad(
                                          tags$div(
                                              class = "inner-pane",
                                              pickerInput(
                                                  inputId = "input-R0-district",
                                                  label = "Select District of India",
                                                  choices = IndiaList$District,
                                                  options = pickerOptions(
                                                      liveSearch = TRUE,
                                                      size = 7
                                                  )
                                              ),
                                              tags$p("The estimation process of a time varying $R_0$ is obtained from the 
                                                     observed incidence curve of the epidemic and by modelling the distribution 
                                                     of serial intervals i.e. the time between the onset of symptoms in a primary case 
                                                     and the onset of symptoms in secondary cases. 
                                                     Details can be found in ", 
                                                     tags$a("Cori et. al.", href = "https://academic.oup.com/aje/article/178/9/1505/89262", target = "_blank"))
                                          )
                                      )
                               ),
                               column(width = 8,
                                      uiOutput("title-R0-district"),
                                      plotlyOutput("plot-R0-district")    
                               )
                           )
                       )
                   )
                   
    )
    
}






