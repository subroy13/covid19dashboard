source('./path_details.R')

serverCountTab <- function(input, output, session, id, df, stateList, label = "Confirmed") {

    observeEvent(
        input[[paste(id, "stateAll", sep = "_")]], {
            if (input[[paste(id, "stateAll", sep = "_")]]) {
                updateCheckboxInput(session, paste(id, "stateAll", sep = "_"), label = "Remove all states from selection")
                updateMultiInput(session, paste(id, "stateSelect", sep = "_"), selected = as.character(stateList))
            }
            else {
                updateCheckboxInput(session, paste(id, "stateAll", sep = "_"), label = "Add all states to selection")
                updateMultiInput(session, paste(id, "stateSelect", sep = "_"), selected = sample(as.character(stateList), size = 1))
            }
        }, ignoreInit = TRUE
    )
    
    
    output[[paste(id, "plotCumulative", sep = "_")]] <- renderPlotly({
        start_date <- input[[paste(id, "date", sep = "_")]][1]
        end_date <- input[[paste(id, "date", sep = "_")]][2]
        selected_states <- input[[paste(id, "stateSelect", sep = "_")]]
        
        tmpdf <- df %>% dplyr::filter(Date >= start_date & Date <= end_date & State %in% selected_states)
        tmpdf[, "Var"] <- tmpdf[, label]   # create a dummy variable for plotting
        
        if (input[[paste(id, "log", sep = "_")]]) {
            p <- ggplot(tmpdf, aes(x = Date)) +
                geom_point(aes(y = log(Var + 1), color = State)) +
                geom_line(aes(y = log(Var + 1), color = State)) +
                theme_bw() +
                xlab("Date") +
                ylab(paste("Counts of", label, "Cases (in Log scale)")) # +
                # scale_x_date(breaks = scales::pretty_breaks(n = 15))
        }
        else {
            p <- ggplot(tmpdf, aes(x = Date)) +
                geom_point(aes(y = Var, color = State)) +
                geom_line(aes(y = Var, color = State)) +
                theme_bw() +
                xlab("Date") +
                ylab(paste("Counts of", label, "Cases")) # +
                # scale_x_date(breaks = scales::pretty_breaks(n = 15))
            
        }
        
        ggplotly(p) %>% layout(legend = list("orientation" = "h", "x" = 0, "y" = -0.25),
                               margin = list(b = 80))
        
    })
    
    output[[paste(id, "plotIncrement", sep = "_")]] <- renderPlotly({
        start_date_2 <- input[[paste(id, "date2", sep = "_")]][1]
        end_date_2 <- input[[paste(id, "date2", sep = "_")]][2]
        selected_states <- input[[paste(id, "stateSelect", sep = "_")]]
                
        tmpdf <- df %>% dplyr::filter(Date >= start_date_2 & Date <= end_date_2 & State %in% selected_states)
        tmpdf[, "Var"] <- tmpdf[, label]   # create a dummy variable for plotting
            
        if (input[[paste(id, "percent", sep = "_")]]) {
            tmpdf <- tmpdf %>% group_by(State) %>%
                mutate(New = (Var - lag(Var))/lag(Var) * 100 ) %>%
                drop_na()
            
            p <- ggplot(tmpdf, aes(x = Date)) +
                geom_point(aes(y = New, color = State)) +
                geom_line(aes(y = New, color = State)) +
                theme_bw() +
                xlab("Date") +
                ylab(paste("Percent change of", label, "Cases")) #+
                # scale_x_date(breaks = scales::pretty_breaks(n = 15))
        }
        else {
            tmpdf <- tmpdf %>% group_by(State) %>%
                mutate(New = (Var - lag(Var)) ) %>%
                drop_na()
            
            p <- ggplot(tmpdf, aes(x = Date)) +
                geom_point(aes(y = New, color = State)) +
                geom_line(aes(y = New, color = State)) +
                theme_bw() +
                xlab("Date") +
                ylab(paste("Counts of New", label, "Cases per day"))# +
                # scale_x_date(breaks = scales::pretty_breaks(n = 15))
        }
        
        ggplotly(p) %>% layout(legend = list("orientation" = "h", "x" = 0, "y" = -0.25),
                               margin = list(b = 80))
        
    })
    
}