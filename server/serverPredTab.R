source('./path_details.R')
source('./mcmc_functions.R')

serverPredTab <- function(input, output, session, IndiaList, df, distdf, user_time) {
    init.params <- list(R0 = 3.15, R0_sd = 1, gamma0 = 0.0117, gamma0_sd = 0.1)
    control.params <- list(nchain=4, nadapt=5e4, ndraw=1e3, thin=20, nburnin=1e3)
    # control.params <- list(nchain=2, nadapt=5e3, ndraw=1e3, thin=10, nburnin=1e2)
    
    # update pickerInputs
    observeEvent(
        input[["input-pred-state"]],
        {
            if (input[["input-pred-state"]] != "All India") {
                updatePickerInput(session, "input-pred-district", 
                    label = paste("B: Choose a district of", input[["input-pred-state"]]),
                    choices = c("Whole State", IndiaList$District[IndiaList$State == input[["input-pred-state"]]  ]) )
            }
            else {
                updatePickerInput(session, "input-pred-district", 
                    label = "Please select the state first to view the districts",
                    choices = c("All India") )
            }
        }
    )

    # generate reActive data
    pred <- reactiveValues(data = NULL, date_df = NULL, possibleMsg = NULL, title = NULL, fileExist = FALSE, errorMsg = NULL)
    
    # reactive computation for clicking on submitButton
    observeEvent(
        input[["submit-pred"]],
        {
            signTime <- readRDS(paste0(DATA_PATH, 'datasets/signTime.Rds'))  # read in the signature time

            # get the state name and district name
            state_name <- input[["input-pred-state"]]
            district_name <- input[["input-pred-district"]]

            # get the name combination based on state name and district name
            if (state_name == "All India") {
                comb_name <- "India"
            }
            else if (district_name == "Whole State") {
                comb_name <- state_name
            }
            else {
                # this time you have really selected a district
                comb_name <- paste(state_name, district_name, sep = "_")
            }

            if ((signTime[comb_name] + 86399 < user_time) | !file.exists(paste0(DATA_PATH, "datasets/predictions/",comb_name,".Rds"))) {
                # check if we need to update the computation

                R0List <- readRDS(paste0(DATA_PATH, 'datasets/minimalR0.Rds'))   # load state level R0

                if (state_name == "All India") {
                    subdf <- df %>% dplyr::group_by(Date) %>% 
                                dplyr::summarise(Confirmed = sum(Confirmed, na.rm = T), Active = sum(Active, na.rm = T), Recovered = sum(Recovered, na.rm = T), Deaths = sum(Deaths, na.rm = T))
                    N <- 131e7   # population of India
                }
                else if (district_name == "Whole State") {
                    subdf <- df %>% dplyr::filter(State == state_name)
                    N <- sum(IndiaList$Population[IndiaList$State == state_name], na.rm = T)
                }
                else {
                    subdf <- distdf %>% dplyr::filter(State == state_name & District == district_name)
                    N <- IndiaList$Population[IndiaList$State == state_name & IndiaList$District == district_name]
                }
                
                tryCatch({
                    # do the MCMC
                    predList <- pred.eSIR(subdf, N, R0List, init.params, control.params, session)
                    
                    # save the prediction
                    saveRDS(predList, paste0(DATA_PATH, "datasets/predictions/",comb_name,".Rds") ) 
                    pred$fileExist = TRUE
                    pred$errorMsg = NULL
                },
                error = function(cond) {
                    err_cond <- paste(cond, collapse = " ; ")
                    print(err_cond)
                    if (err_cond == "Error in create_pred(I, R, N, pi0, minimal_pi0, df$Date[1], init.params, : No Prediction is possible as either there has been no cases in this district or all the cases have recovered.\n") {
                            pred$errorMsg = err_cond
                        } else {
                            pred$errorMsg = NULL
                        }
                        pred$fileExist = FALSE
                    }
                )

                # update the latest signature time
                new_SignTime <- as.POSIXct( paste0(as.Date(user_time), " 00:00:00 IST"), tz = "Asia/Kolkata" )
                signTime[comb_name] <- new_SignTime    # update the signature time for whole state level
                saveRDS(signTime, paste0(DATA_PATH, 'datasets/signTime.Rds'))
                
                remove_modal_spinner(session = session)
                
            }
            else {
                # else I simply read in the stored MCMC results
                predList <- readRDS(paste0(DATA_PATH, "datasets/predictions/",comb_name,".Rds"))
                pred$fileExist = TRUE
                pred$errorMsg = NULL
            }

            if (pred$fileExist) {
                pred$data <- predList[[1]]
                pred$date_df <- predList[[2]]
                pred$possibleMsg <- predList[[3]]
            } else {
                pred$data <- NULL
                pred$date_df <- NULL
                pred$possibleMsg <- NULL
            }
            
            # SETUP THE TITLE
            if (pred$fileExist) {
                if (input[["input-pred-state"]] == "All India") {
                    pred$title <- "eSIR Prediction for the Whole India"
                }
                else if (input[["input-pred-district"]] == "Whole State") {
                    pred$title <- paste("eSIR Prediction for state :", input[["input-pred-state"]] )
                }
                else {
                    pred$title <- paste("eSIR Prediction for district :", input[["input-pred-district"]], "in state :", input[["input-pred-state"]] )
                }
            } else {
                if (!is.null(pred$errorMsg)) {
                    pred$title <- "Prediction for this location is not available at this moment. No Prediction is possible as either there has been no cases in this district or all the cases have recovered or all the cases are currently active."
                } else {
                    pred$title <- "Prediction for this location is not available at this moment. "
                }
            }

        }, ignoreInit = TRUE
    )

    # update the title
    output[["title-pred"]] <- renderUI({
        if (is.null(pred$title)) return()
        
        h4(pred$title)
    })
    
    # update the supporting text
    output[["plottext-pred"]] <- renderUI({
        if (is.null(pred$date_df) | is.null(pred$possibleMsg)) return()
        
        j <- as.numeric(input[["input-pred-distancing"]])
            
        if (pred$possibleMsg[j]) {
            # if true then NO msg
            pos_text <- NULL
        }
        else {
            # if false, then not possible
            # check if j = 1, (if yes then no message)
            if (j == 1) {
                pos_text <- NULL
            } else {
                pos_text <- "This situation is not possible since the current level of social distancing among masses do not show indication of such strict social distancing measures."
            }
        }
            
        pred_date_df <- pred$date_df %>% dplyr::filter(Situation == paste(input[["input-pred-distancing"]]) )
        pred_df <- pred$data %>% dplyr::filter(Situation == paste(input[["input-pred-distancing"]]))
        
        # check for a 2nd wave
        peak_est <- pred_date_df$Estimate[pred_date_df$Variable == "HighDate"]
        peak_plot <- pred_df$Date[which.max(pred_df$Estimate.Infected)]
        if (peak_plot > peak_est + 7) {
            if (is.null(pos_text)) {
                pos_text <- "The estimate of peak infection date is for the first wave of the infection. It seems, there could be a second wave as well."
            }
        }
        
        
        
        if (!is.null(pos_text)) {
            tagList(
                tags$blockquote(pos_text),
                tags$hr(),
                tags$p(
                    "The estimated date when number of active cases (infected and currently hospitalized) is maximum, i.e. 
                the date from which the number of active cases should start to decline is ",
                    tags$b(format(pred_date_df$Estimate[pred_date_df$Variable == "HighDate"], "%d %B %Y") ),
                    ". However, assuming errors in estimation, the date when number of 
                active cases is at peak lies between ",
                    tags$b(format(pred_date_df$Lower[pred_date_df$Variable == "HighDate"], "%d %B %Y")), 
                    " and ",
                    tags$b(format(pred_date_df$Upper[pred_date_df$Variable == "HighDate"], "%d %B %Y")), 
                    " with 95% certainty."
                ),
                tags$hr(),
                tags$p(
                    "The estimated date when the number of recoveries will cross the number of active cases is ",
                    tags$b(format(pred_date_df$Estimate[pred_date_df$Variable == "CrossDate"], "%d %B %Y") ),
                    ". However, assuming errors in estimation, the date when number of 
                recovered persons cross the number of active cases lies between ",
                    tags$b(format(pred_date_df$Lower[pred_date_df$Variable == "CrossDate"], "%d %B %Y")), 
                    " and ",
                    tags$b(format(pred_date_df$Upper[pred_date_df$Variable == "CrossDate"], "%d %B %Y")), 
                    " with 95% certainty."
                )
            )
        } else {
            tagList(
                tags$hr(),
                tags$p(
                    "The estimated date when number of active cases (Infected and hospitalized) is maximum, i.e. 
                the date from which the number of active cases should start to decline is ",
                    tags$b(format(pred_date_df$Estimate[pred_date_df$Variable == "HighDate"], "%d %B %Y") ),
                    ". However, assuming errors in estimation, the date when number of 
                active cases is at peak lies between ",
                    tags$b(format(pred_date_df$Lower[pred_date_df$Variable == "HighDate"], "%d %B %Y")), 
                    " and ",
                    tags$b(format(pred_date_df$Upper[pred_date_df$Variable == "HighDate"], "%d %B %Y")), 
                    " with 95% certainty."
                ),
                tags$hr(),
                tags$p(
                    "The estimated date when the number of recoveries will cross the number of active cases is ",
                    tags$b(format(pred_date_df$Estimate[pred_date_df$Variable == "CrossDate"], "%d %B %Y") ),
                    ". However, assuming errors in estimation, the date when number of 
                recovered persons cross the number of active cases lies between ",
                    tags$b(format(pred_date_df$Lower[pred_date_df$Variable == "CrossDate"], "%d %B %Y")), 
                    " and ",
                    tags$b(format(pred_date_df$Upper[pred_date_df$Variable == "CrossDate"], "%d %B %Y")), 
                    " with 95% certainty."
                )
            )
        }
        
    })
    
    
    # update the plot
    output[["plot-pred"]] <- renderPlotly({
        if (is.null(pred$data)) return()

        input_situ <- paste(input[["input-pred-distancing"]])
        tmpdf_state <- pred$data %>% dplyr::filter(Situation %in% c("obs", input_situ))
        tmpdf_state$Situation <- ifelse(tmpdf_state$Situation == "obs", "Observed", "Predicted")
    
        if (input[["input-pred-ci"]]) {
            if (input[["input-pred-var"]] == "Both") {
                p <- ggplot(tmpdf_state, aes(x = Date)) +
                    geom_ribbon(aes(ymin = Lower.Infected, ymax = Upper.Infected), fill = "red", alpha = 0.1) +
                    geom_line(aes(y = Estimate.Infected, linetype = Situation, color = "Active")) +
                    geom_ribbon(aes(ymin = Lower.Removed, ymax = Upper.Removed), fill = "darkgreen", alpha = 0.1) +
                    geom_line(aes(y = Estimate.Removed, linetype = Situation, color = "Recovered")) +
                    theme_bw() +
                    scale_color_manual(guide = "legend", name = "Variable", 
                                       values = c("Active" = "red", "Recovered" = "darkgreen"),
                                       labels = c("Removed", "Active")) +
                    xlab("Date") + ylab("Estimates")# +
                    # scale_x_date(breaks = scales::pretty_breaks(n = 10)) 
                    
            }
            else {
                p <- ggplot(tmpdf_state, aes(x = Date)) +
                    geom_ribbon(aes(ymin = Lower.Infected, ymax = Upper.Infected), fill = "red", alpha = 0.1) +
                    geom_line(aes(y = Estimate.Infected, linetype = Situation), color = "red") +
                    theme_bw() +
                    xlab("Date") + ylab("Estimates") # +
                    # scale_x_date(breaks = scales::pretty_breaks(n = 10))
                    
            }
            
        }
        else {
            if (input[["input-pred-var"]] == "Both") {
                p <- ggplot(tmpdf_state, aes(x = Date)) +
                    geom_line(aes(y = Estimate.Infected, linetype = Situation, color = "Active")) +
                    geom_line(aes(y = Estimate.Removed, linetype = Situation, color = "Recovered")) +
                    theme_bw() +
                    scale_color_manual(guide = "legend", name = "Variable", 
                                       values = c("Active" = "red", "Recovered" = "darkgreen"),
                                       labels = c("Removed", "Active")) +
                    xlab("Date") + ylab("Estimates") # +
                    # scale_x_date(breaks = scales::pretty_breaks(n = 10))
                    
            }
            else {
                p <- ggplot(tmpdf_state, aes(x = Date)) +
                    geom_line(aes(y = Estimate.Infected, linetype = Situation), color = "red") +
                    theme_bw() +
                    xlab("Date") + ylab("Estimates") # +
                    # scale_x_date(breaks = scales::pretty_breaks(n = 10))
                    
            }
            
        }
            
        ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = 1.1), margin = list(t = 50))
    })
   
}