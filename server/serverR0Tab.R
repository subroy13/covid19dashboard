source('./path_details.R')


serverR0Tab <- function(input, output, session, IndiaList, df, distdf) {
    
    # take a window length of 4 days
    w <- 4

    # plotly output for whole india
    output[["plot-R0-india"]] <- renderPlotly({
        tot_df <- df %>% group_by(Date) %>% summarise(Confirmed = sum(Confirmed, na.rm = T))
        incid_india <- ifelse(diff(tot_df$Confirmed) < 0, 0, diff(tot_df$Confirmed))
        t_start_ind <- 2:(length(incid_india) - w)
        t_end_ind <- t_start_ind + w
        
        res <- estimate_R(incid_india, method = "parametric_si", config = make_config(list(mean_si = 4.7, std_si = 2.9, t_start = t_start_ind, t_end = t_end_ind)))
        res <- as_tibble(res$R) %>% 
            mutate(Date = tot_df$Date[1] + t_start - 1, Estimate = `Mean(R)`,
                   Lower.95 = `Quantile.0.025(R)`, Upper.95 = `Quantile.0.975(R)`) 
        fit <- loess(Estimate ~ t_start, data = res)
        
        res %>%
            mutate(Smoothed = fit$fitted) %>%
            plot_ly(x = ~Date) %>%
            add_ribbons(ymin = ~`Lower.95`, ymax = ~`Upper.95`, name = "95% CI", 
                        line = list(color = 'rgba(90, 90, 90, 0.2)'),
                        fillcolor = 'rgba(90, 90, 90, 0.2)') %>%
            add_trace(y = ~Estimate, name = "Estimated R0", line = list(color = 'red'), marker = list(color = 'red'), mode = 'markers+lines', type = 'scatter') %>%
            add_lines(y = ~Smoothed, name = "Smoothed R0", line = list(color = 'green', dash = 'dot')) %>%
            layout(legend = list(orientation = "h", x = 0, y = 1.1), margin = list(t = 50))
            
    })

    # update plot title for selected state
    output[["title-R0-state"]] <- renderUI({
        selected_state <- input[["input-R0-state"]]
        
        h4(paste("Time Varying Reproduction Rate for", selected_state))
    })

    # update plotly for select state
    output[["plot-R0-state"]] <- renderPlotly({
        state_df <- df %>% dplyr::filter(State == input[["input-R0-state"]])
        incid_state <- ifelse(diff(state_df$Confirmed) < 0, 0, diff(state_df$Confirmed))
        t_start_state <- 2:(length(incid_state) - w)
        t_end_state <- t_start_state + w
        
        res1 <- estimate_R(incid_state, method = "parametric_si", config = make_config(list(mean_si = 4.7, std_si = 2.9, t_start = t_start_state, t_end = t_end_state)))
        res1 <- as_tibble(res1$R) %>% 
            mutate(Date = state_df$Date[1] + t_start - 1, Estimate = `Mean(R)`,
                   Lower.95 = `Quantile.0.025(R)`, Upper.95 = `Quantile.0.975(R)`) 
        fit1 <- loess(Estimate ~ t_start, data = res1)
        
        res1 %>%
            mutate(Smoothed = fit1$fitted) %>%
            plot_ly(x = ~Date) %>%
            add_ribbons(ymin = ~`Lower.95`, ymax = ~`Upper.95`, name = "95% CI", 
                        line = list(color = 'rgba(90, 90, 90, 0.2)'),
                        fillcolor = 'rgba(90, 90, 90, 0.2)') %>%
            add_trace(y = ~Estimate, name = "Estimated R0", line = list(color = 'red'), marker = list(color = 'red'), mode = 'markers+lines', type = 'scatter') %>%
            add_lines(y = ~Smoothed, name = "Smoothed R0", line = list(color = 'green', dash = 'dot')) %>%
            layout(legend = list(orientation = "h", x = 0, y = 1.1), margin = list(t = 50))
    })

    # update choices for district level picker input
    observeEvent(
        input[["input-R0-state"]],
        updatePickerInput(session, "input-R0-district", 
                          label = paste("Select District of", input[["input-R0-state"]]),
                          choices = IndiaList$District[IndiaList$State == input[["input-R0-state"]]  ])
    )
    
    # update plot heading for district level
    output[["title-R0-district"]] <- renderUI({
        selected_dist <- input[["input-R0-district"]]
        
        h4(paste("Time Varying Reproduction Rate for", selected_dist))
    })
    
    
    # update plot for district level
    output[["plot-R0-district"]] <- renderPlotly({
        dist_df <- distdf %>% dplyr::filter(State == input[["input-R0-state"]] & District == input[["input-R0-district"]])
        incid_dist <- ifelse(diff(dist_df$Confirmed) < 0, 0, diff(dist_df$Confirmed))
        t_start_dist <- 2:(length(incid_dist) - w)
        t_end_dist <- t_start_dist + w
        
        res2 <- estimate_R(incid_dist, method = "parametric_si", config = make_config(list(mean_si = 4.7, std_si = 2.9, t_start = t_start_dist, t_end = t_end_dist)))
        res2 <- as_tibble(res2$R) %>% 
            mutate(Date = dist_df$Date[1] + t_start - 1, Estimate = `Mean(R)`,
                   Lower.95 = `Quantile.0.025(R)`, Upper.95 = `Quantile.0.975(R)`) 
        fit2 <- loess(Estimate ~ t_start, data = res2)
        
        res2 %>%
            mutate(Smoothed = fit2$fitted) %>%
            plot_ly(x = ~Date) %>%
            add_ribbons(ymin = ~`Lower.95`, ymax = ~`Upper.95`, name = "95% CI", 
                        line = list(color = 'rgba(90, 90, 90, 0.2)'),
                        fillcolor = 'rgba(90, 90, 90, 0.2)') %>%
            add_trace(y = ~Estimate, name = "Estimated R0", line = list(color = 'red'), marker = list(color = 'red'), mode = 'markers+lines', type = 'scatter') %>%
            add_lines(y = ~Smoothed, name = "Smoothed R0", line = list(color = 'green', dash = 'dot')) %>%
            layout(legend = list(orientation = "h", x = 0, y = 1.1), margin = list(t = 50))
    })

}








