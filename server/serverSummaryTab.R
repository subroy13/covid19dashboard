source('./path_details.R')

serverSummaryTab <- function(input, output, session, IndiaList, latest_df, latest_distdf) {
    
    output[["home-glance-card-title"]] <- renderUI({
        paste("COVID - 19 Situation at a Glance for ", input[["input-home-state"]])
    })
    
    output[["confirmedBox-title"]] <- renderUI({ 
            if (input[["input-home-state"]] == "Whole India") {
                h4(sum(latest_df$Confirmed))
            } else {
                h4(latest_df$Confirmed[latest_df$State == input[["input-home-state"]]][1])
            }
        })
    
    output[["activeBox-title"]] <- renderUI({ 
        if (input[["input-home-state"]] == "Whole India") {
            h4(sum(latest_df$Active))
        } else {
            h4(latest_df$Active[latest_df$State == input[["input-home-state"]]][1])
        }
    })
    
    output[["recoveredBox-title"]] <- renderUI({ 
        if (input[["input-home-state"]] == "Whole India") {
            h4(sum(latest_df$Recovered))
        } else {
            h4(latest_df$Recovered[latest_df$State == input[["input-home-state"]]][1])
        }
    })
    
    output[["deceasedBox-title"]] <- renderUI({ 
        if (input[["input-home-state"]] == "Whole India") {
            h4(sum(latest_df$Deaths))
        } else {
            h4(latest_df$Deaths[latest_df$State == input[["input-home-state"]]][1])
        }
    })
    

    output[["plot-home-map"]] <- renderPlot({
        color <- switch(input[["input-home-var"]], 
                        "Confirmed" = "#FFDB6D", 
                        "Active" = "#FF0000", 
                        "Recovered" = "#00FF00",
                        "Deaths" = "#999999")
        
        
        if (input[["input-home-state"]] == "Whole India") {
            india_map <- readRDS(paste0(DATA_PATH, 'datasets/maps/India.Rds'))
            latest_df[, "Value"] <- latest_df[, input[["input-home-var"]] ]
            tmpdf <- india_map %>% left_join(latest_df %>% select(State, Value), by = c("st_nm" = "State"))
            
            tmpdf <- tmpdf %>% 
                left_join(IndiaList %>% 
                              group_by(State) %>% 
                              summarise(Population = sum(Population, na.rm = T), Area = sum(Area, na.rm = T)), by = c("st_nm" = "State"))
            
        }
        else {
            state_map <- readRDS(paste0(DATA_PATH, 'datasets/maps/',input[["input-home-state"]],'.Rds'))
            latest_distdf <- latest_distdf %>% dplyr::filter(State == input[["input-home-state"]])
            latest_distdf[, "Value"] <- latest_distdf[, input[["input-home-var"]] ]
            tmpdf <- state_map %>% left_join(latest_distdf %>% select(District, Value), by = c("district" = "District"))
            
            tmpdf <- tmpdf %>% left_join(IndiaList %>% dplyr::filter(State == input[["input-home-state"]]), by = c("district" = "District"))
            
            # replace NA values of population by statewise averages
            tmpdf$Population <- ifelse(is.na(tmpdf$Population), mean(tmpdf$Population, na.rm = T), tmpdf$Population)
            tmpdf$Area <- ifelse(is.na(tmpdf$Area), mean(tmpdf$Area, na.rm = T), tmpdf$Area)
        }
        
        if (input[["input-home-type"]] == 2) {
            tmpdf$Value <- tmpdf$Value / tmpdf$Population * 1e6
        }
        if (input[["input-home-type"]] == 3) {
            tmpdf$Value <- tmpdf$Value / tmpdf$Area
        }
        
        ggplot(data = tmpdf) +
            geom_sf(aes(fill = Value)) +
            scale_fill_gradient(trans = "sqrt", low = "white", high = color) +
            xlab("Longitude") + ylab("Latitude") +
            guides(fill = guide_colorbar(title = paste(input[["input-home-var"]], "Cases"), frame.colour = "black", reverse = TRUE, barwidth = 0.5, barheight = 15)) +
            theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))
        
        
    })
    
}








