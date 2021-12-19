library(readr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(EpiEstim)

source('./path_details.R')

update_data <- function(user_time, session) {
    # it checks the signature (i.e. the last updated time) for the data entry
    signTimeList <- readRDS(paste0(DATA_PATH, 'datasets/signTime.Rds'))
    signTime <- signTimeList["data"]
    
    # compares against the user time, and if signTime is > 1 days old, need to update data
    if ((signTime + 86399 < user_time) | (!file.exists(paste0(DATA_PATH, 'datasets/india-state-level.csv'))) ) {
        
        # read in the state codes and names
        stateList <- c(
            "an" = "Andaman and Nicobar Islands", "ap" = "Andhra Pradesh", "ar" = "Arunachal Pradesh", "as" = "Assam",
            "br" = "Bihar", "ch" = "Chandigarh", "ct" = "Chhattisgarh", "dl" = "Delhi", "dn" = "Dadra and Nagar Haveli and Daman and Diu",
            "ga" = "Goa", "gj" = "Gujarat", "hp" = "Himachal Pradesh", "hr" = "Haryana", "jh" = "Jharkhand", "jk" = "Jammu and Kashmir",
            "ka" = "Karnataka", "kl" = "Kerala", "la" = "Ladakh", "ld" = "Lakshadweep", "mh" = "Maharashtra", "ml" = "Meghalaya",
            "mn" = "Manipur", "mp" = "Madhya Pradesh", "mz" = "Mizoram", "nl" = "Nagaland", "or" = "Odisha", "pb" = "Punjab",
            "py" = "Puducherry", "rj" = "Rajasthan", "sk" = "Sikkim", "tg" = "Telangana", "tn" = "Tamil Nadu",
            "tr" = "Tripura", "up" = "Uttar Pradesh", "ut" = "Uttarakhand", "wb" = "West Bengal")
        
        
        # Name of the official districts of India (with Population and Area)
        distList <- read_csv(paste0(DATA_PATH, 'datasets/india-details.csv'))
        
        # UPDATE STATE LEVEL DATA
        url1 <- "https://api.covid19india.org/states_daily.json"     # statewise daily
        df <- fromJSON(url1)$states_daily
        
        df <- as_tibble(df) %>%
            mutate(date = as.Date(date, format = "%d-%b-%y")) %>%
            pivot_longer(-c(date, status), names_to = "state", values_to = "value") %>%
            mutate(state = stateList[state]) %>%
            drop_na() %>%
            pivot_wider(id_cols = c(date, state), names_from = status, values_from = value) %>%
            mutate(Confirmed = as.numeric(Confirmed),
                   Recovered = as.numeric(Recovered),
                   Deceased = as.numeric(Deceased)) %>%
            replace_na(list(Confirmed = 0, Recovered = 0, Deceased = 0)) %>%
            group_by(state) %>%
            mutate(Confirmed = cumsum(Confirmed),
                   Recovered = cumsum(Recovered), 
                   Deceased = cumsum(Deceased)) %>%
            ungroup() %>%
            mutate(Active = Confirmed - Recovered - Deceased) %>%
            select(Date = date, State = state, Confirmed, Active, Recovered, Deaths = Deceased)
        
        write_csv(df, path = paste0(DATA_PATH, "datasets/india-state-level.csv"))
        
        
        # UPDATE DISTRICT LEVEL DATA (New Update for change in API)
        url2 <- "https://api.covid19india.org/csv/latest/districts.csv"   # districtwise new data
        df2 <- read_csv(url2)
        
        tmpdf <- df2 %>% mutate(Active = Confirmed - Recovered - Deceased) %>%
            select(Date, State, District, Confirmed, Active, Recovered, Deaths = Deceased)
        
        # custom spell check
        tmpdf$State[which(tmpdf$State == "Dadra and Nagar Haveli")] <- "Dadra and Nagar Haveli and Daman and Diu"
        tmpdf$District[which(tmpdf$State == "Maharashtra" & tmpdf$District == "Raigarh")] = "Raigad"
        tmpdf$District[which(tmpdf$State == "Andaman and Nicobar Islands")] <- "South Andaman"
        
        
        # a named vector containing the wrong names and correct names
        correct_spell_dict <- c("Y.S.R." = "Y.S.R. Kadapa", "Y.S.R Kadapa" = "Y.S.R. Kadapa", 
                                "Other State" = NA, "Other States" = NA, "Other States*" = NA, "Unknown" = NA,
                                "South Salmara Mancachar" = "South Salmara Mankachar", "Kaimur Bhabua" = "Kaimur", 
                                "Ahmadabad" = "Ahmedabad","Banas Kantha" = "Banaskantha", 
                                "Chota Udaipur" = "Chhota Udaipur", "Kachchh" = "Kutch", "Mahesana" = "Mehsana",
                                "Panch Mahals" = "Panchmahal", "Sabar Kantha" = "Sabarkantha", "Charki Dadri" = "Charkhi Dadri",
                                "Kodarma" = "Koderma", "Bengaluru" = "Bengaluru Urban", 
                                "Other Region" = NA, "Other Region*" = NA,
                                "Ahmadnagar" = "Ahmednagar", "Bid" = "Beed", "Buldana" = "Buldhana", "Gondiya" = "Gondia",
                                "Baleshwar" = "Balasore", "Debagarh" = "Deogarh", "Jajapur" = "Jajpur", 
                                "Firozpur" = "Ferozepur", "BSF Camp" = NA, "Chittaurgarh" = "Chittorgarh", 
                                "Dhaulpur" = "Dholpur", "Evacuees" = NA, "Evacuees*" = NA, "Italians" = NA, 
                                "Italians*" = NA, "Other state" = NA, "Others state" = NA, "Airport Quarantine" = NA,
                                "Railway Quarantine" = NA, "Kanniyakumari" = "Kanyakumari", "The Nilgiris" = "Nilgiris", 
                                "Kumuram Bheem Asifabad" = "Komaram Bheem", "Jagitial" = "Jagtial", 
                                "Jangoan" = "Jangaon", "Bara Banki" = "Barabanki", "Mahrajganj" = "Maharajganj", "Medinipur East" = "Purba Medinipur",
                                "Bandipore" = "Bandipora", "Baramula" = "Baramulla", "Shupiyan" = "Shopian", 
                                "Shopiyan" = "Shopian", "Saraikela-Kharsawan" = "Seraikela-Kharsawan", "Badgam" = "Budgam",
                                "Punch" = "Poonch", "Chamarajanagara" = "Chamarajanagar", "Chikkaballapura" = "Chikkaballapur",
                                "Nabarangapur" = "Nabarangpur", "Shrawasti" = "Shravasti", "Kheri" = "Lakhimpur Kheri")
        
        # rename the wrong spellings
        tmpdf$District <- ifelse(tmpdf$District %in% names(correct_spell_dict), 
                                 correct_spell_dict[tmpdf$District],
                                 tmpdf$District)
        
        tmpdf <- tmpdf %>% filter(District %in% distList$District)
        nDate <- length(unique(tmpdf$Date))
        tmpdf <- tmpdf %>% right_join(tibble(State = rep(distList$State, nDate), District = rep(distList$District, nDate), Date = rep(unique(tmpdf$Date), each = nrow(distList)) ), by = c("Date", "State", "District")) %>% 
            replace_na(list(Confirmed = 0, Active = 0, Recovered = 0, Deaths = 0))
        
        write_csv(tmpdf, path = paste0(DATA_PATH, "datasets/india-district-level.csv"))
        
        # UPDATE R0 for States (Computation of minimal R0)
        statenames <- unique(df$State)
        t_start <- 2:(sum(df$State == "West Bengal") - 1 - 4)    # window length = 4
        t_end <- t_start + 4
        
        R0List <- sapply(statenames, function(x){
            incid = sort(df$Confirmed[df$State == x])
            res <- estimate_R(diff(incid), method = "parametric_si", config = make_config(list(mean_si = 4.7, std_si = 2.9, t_start = t_start, t_end = t_end)))
            fit <- loess(`Mean(R)` ~ t_start, data = res$R)
            return(fit$fitted)    # get smoothed R0 (starting from day 2)
        })
        
        R0List <- R0List[9, ]   # row 9 corresponds to March 23rd
        
        R0List <- pmin(R0List, 3)   # minimal R0 must be less than or equal to 3 = Base R0
        
        saveRDS(R0List, file = paste0(DATA_PATH, 'datasets/minimalR0.Rds'))
        
        
        # Now, update the signature time
        new_SignTime <- as.POSIXct( paste0(as.Date(user_time), " 00:00:00 IST"), tz = "Asia/Kolkata" )
        signTimeList["data"] <- new_SignTime    # update the signature time for whole India level
        
        saveRDS(signTimeList, file = paste0(DATA_PATH, 'datasets/signTime.Rds'))
        
        return(list(df, tmpdf))  # return a list of statewise count, district counts
        
    }
    
    else {
        
        # else simply read in the data from the saved files
        df <- read_csv(paste0(DATA_PATH, 'datasets/india-state-level.csv'))
        distdf <- read_csv(paste0(DATA_PATH, 'datasets/india-district-level.csv'))
        
        return(list(df, distdf))
    }
    
}