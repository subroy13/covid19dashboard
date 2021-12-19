source('./path_details.R')

# This script updates the prediction for eSIR Model

# Loading the required packages
library(readr)
library(dplyr)
library(ggplot2)
library(rjags)
library(gtools)
library(EpiEstim)
library(dclone)
library(forecast)


# obtain the prediction for given Statename or Districtname
pred.eSIR <- function(df, N, R0List, init.params, control.params, session) {
  # define all functions within this function environment
  
  # Utility function for lognormal distribution
  lognorm.param<-function(mu0,var0){
    var <- log(var0/mu0^2+1)
    mu <- log(mu0)-var/2
    return(round(c(mu,var),3))
  }
  
  # performing Gibbs Sampler
  do.Gibbs <- function(I, R, pi0, init.params, control.params, session = NULL) {
    T_obs <- length(I)
    
    lognorm_gamma_param <- lognorm.param(init.params$gamma0, init.params$gamma0_sd)
    lognorm_R0_param <- lognorm.param(init.params$R0, init.params$R0_sd)
    
    model.string <- paste0("
             model{
                   for(t in 2:(T_obs+1)){
                   Km[t-1,1] <- -beta*pi[t-1]*theta[t-1,1]*theta[t-1,2]
                   Km[t-1,9] <- gamma*theta[t-1,2]
                   Km[t-1,5] <- -Km[t-1,1]-Km[t-1,9]
                   Km[t-1,2] <- -beta*pi[t-1]*(theta[t-1,1]+0.5*Km[t-1,1])*(theta[t-1,2]+0.5*Km[t-1,5])
                   Km[t-1,10] <- gamma*(theta[t-1,2]+0.5*Km[t-1,5])
                   Km[t-1,6] <- -Km[t-1,2]-Km[t-1,10]
                   Km[t-1,3] <- -beta*pi[t-1]*(theta[t-1,1]+0.5*Km[t-1,2])*(theta[t-1,2]+0.5*Km[t-1,6])
                   Km[t-1,11] <- gamma*(theta[t-1,2]+0.5*Km[t-1,6])
                   Km[t-1,7] <- -Km[t-1,3]-Km[t-1,11]
                   Km[t-1,4] <- -beta*pi[t-1]*(theta[t-1,1]+Km[t-1,3])*(theta[t-1,2]+Km[t-1,7])
                   Km[t-1,12] <- gamma*(theta[t-1,2]+Km[t-1,7])
                   Km[t-1,8] <- -Km[t-1,4]-Km[t-1,12]
                   alpha[t-1,1] <- theta[t-1,1]+(Km[t-1,1]+2*Km[t-1,2]+2*Km[t-1,3]+Km[t-1,4])/6
                   alpha[t-1,2] <- theta[t-1,2]+(Km[t-1,5]+2*Km[t-1,6]+2*Km[t-1,7]+Km[t-1,8])/6
                   alpha[t-1,3] <- theta[t-1,3]+(Km[t-1,9]+2*Km[t-1,10]+2*Km[t-1,11]+Km[t-1,12])/6
                   theta[t,1:3] ~ ddirch(k * alpha[t-1,1:3])
                   I[t-1] ~ dbeta(lambdaI*theta[t,2],lambdaI*(1-theta[t,2])) T(0.000000001,0.999999999) 
                   R[t-1] ~ dbeta(lambdaR*theta[t,3],lambdaR*(1-theta[t,3])) T(0.000000001,0.999999999) 
                  }
                  theta[1,1] <-  1- theta[1,2]- theta[1,3]
                  theta[1,2] ~ dbeta(",1,",",1/I[1],") T(0.000000001,0.999999999) 
                  theta[1,3] ~ dbeta(",1,",",1/R[1],") T(0.000000001,0.999999999) 
                  gamma ~  dlnorm(",lognorm_gamma_param[1],",",1/lognorm_gamma_param[2],")
                  R0 ~ dlnorm(",lognorm_R0_param[1],",",1/lognorm_R0_param[2],")
                  beta <- R0*gamma
                  k ~  dgamma(2,0.0001)
                  lambdaI ~ dgamma(2,0.0001)
                  lambdaR ~ dgamma(2,0.0001)
               }
            ")
    
    model.spec <- textConnection(model.string)
    
    if (!is.null(session)) {
      show_modal_spinner(text = "Building Parallel chains for Bayesian Model", session = session, spin = "self-building-square", color = "#fc034e")
    }
    
    if (parallelizable) {
      # update the posterior for burn in times, for this time, do not monitor anything
      jags_sample <- jags.parfit(cl = 4, 
                                 model=model.spec, 
                                 data=list('I'=I,'R'=R,'T_obs'=T_obs,'pi'= pi0), 
                                 n.chains =control.params$nchain, 
                                 n.adapt = control.params$nadapt, 
                                 params=c('theta','gamma','R0','beta','I','lambdaI','lambdaR','k'),
                                 n.iter = control.params$ndraw * control.params$nchain, 
                                 thin = control.params$thin)
    } else {
      # update the posterior for burn in times, for this time, do not monitor anything
      jags_sample <- jags.fit(model=model.spec, 
                                 data=list('I'=I,'R'=R,'T_obs'=T_obs,'pi'= pi0), 
                                 n.chains =control.params$nchain, 
                                 n.adapt = control.params$nadapt, 
                                 params=c('theta','gamma','R0','beta','I','lambdaI','lambdaR','k'),
                                 n.iter = control.params$ndraw * control.params$nchain, 
                                 thin = control.params$thin)
      
    }
    
    jags_sample <- jags_sample[[1]]
    
    if (!is.null(session)) {
      remove_modal_spinner(session = session)
    }
    
    return(jags_sample)
  }
  
  # forecasting for a single scenario
  pred.Gibbs <- function(mcmc_sample, pi_new, T_obs, control.params, I, R) {
    T_new <- length(pi_new)
    
    # extract components from mcmc samples
    theta_pre <- array(mcmc_sample[,-(1:(T_obs+6))], dim = c(control.params$mclen, T_obs+1, 3))
    R0_pre <- (mcmc_sample[,"R0"])
    gamma_pre <-(mcmc_sample[,"gamma"])
    beta_pre <- (mcmc_sample[,"beta"])
    lambdaI_pre <- (mcmc_sample[,"lambdaI"])
    lambdaR_pre <- (mcmc_sample[,"lambdaR"])
    k_pre <- (mcmc_sample[,"k"])
    
    I_post <- matrix(NA, nrow=control.params$mclen, ncol=T_new)
    R_post <- matrix(NA, nrow=control.params$mclen, ncol=T_new)
    theta_post <- array(0,dim=c(control.params$mclen,T_new,3))  
    
    for(l in 1:control.params$mclen){
      
      thetalt1 <- theta_pre[l, T_obs+1, 1]
      thetalt2 <- theta_pre[l, T_obs+1, 2]
      thetalt3 <- theta_pre[l, T_obs+1, 3]
      betal <- beta_pre[l]
      gammal <- gamma_pre[l]
      kl <- k_pre[l]
      lambdaIl <- lambdaI_pre[l]
      lambdaRl <- lambdaR_pre[l]
      if (betal< 0 | gammal< 0 | thetalt1< 0 | thetalt2< 0 |thetalt3< 0) { 
        next 
      }

      for(t in 1:T_new ){
        # perform runge kutta
        Km <- numeric(12)
        alpha_post <- numeric(3)
        
        Km[1] <- -betal*pi_new[t]*thetalt1*thetalt2
        Km[9] <- gammal*thetalt2
        Km[5] <- -Km[1]-Km[9]
        
        Km[2] <- -betal*pi_new[t]*(thetalt1+0.5*Km[1])*(thetalt2+0.5*Km[5])
        Km[10] <- gammal*(thetalt2+0.5*Km[5])
        Km[6] <- -Km[2]-Km[10]
        
        Km[3] <- -betal*pi_new[t]*(thetalt1+0.5*Km[2])*(thetalt2+0.5*Km[6])
        Km[11] <- gammal*(thetalt2+0.5*Km[6])
        Km[7] <- -Km[3]-Km[11]
        
        Km[4] <- -betal*pi_new[t]*(thetalt1+Km[3])*(thetalt2+Km[7])
        Km[12] <- gammal*(thetalt2+Km[7])
        Km[8] <- -Km[4]-Km[12]
        
        alpha_post[1] <- thetalt1+(Km[1]+2*Km[2]+2*Km[3]+Km[4])/6
        alpha_post[2] <- thetalt2+(Km[5]+2*Km[6]+2*Km[7]+Km[8])/6
        alpha_post[3] <- thetalt3+(Km[9]+2*Km[10]+2*Km[11]+Km[12])/6
        
        thetalt_tmp <- rdirichlet(1, kl * abs(alpha_post))
        thetalt1 <- theta_post[l,t,1]<- thetalt_tmp[1]
        thetalt2 <- theta_post[l,t,2]<- thetalt_tmp[2]
        thetalt3 <- theta_post[l,t,3]<- thetalt_tmp[3]
        
        I_post[l,t] <- rbeta(1,lambdaIl*thetalt2,lambdaIl*(1-thetalt2))
        R_post[l,t] <- rbeta(1,lambdaIl*thetalt3,lambdaIl*(1-thetalt3))
        
      }
      
    }
    
    # extract the highest pandemic date
    highDate <- quantile(sapply(1:control.params$mclen, FUN = function(x){ which.max( c(I[1:T_obs], I_post[x, ]) ) }), probs = c(0.05, 0.5, 0.95), na.rm = T)
    
    # extract the crossing date
    crossDate <- quantile(sapply(1:control.params$mclen, FUN = function(x){ tail(which( c(R[1:T_obs], R_post[x, ]) < c(I[1:T_obs], I_post[x, ]) ),1) }),
                          probs = c(0.05, 0.5, 0.95), na.rm = T)
    
    
    # extract CI and estimates for Infected and Recovered state
    I_post_mean <- colMeans(theta_post[, , 2], na.rm = T)
    R_post_mean <- colMeans(theta_post[, , 3], na.rm = T)

    I_post <- apply(I_post, 2, FUN = quantile, probs = c(0.025, 0.5, 0.975), na.rm = T)
    I_post[2, ] <- I_post_mean

    R_post <- apply(R_post, 2, FUN = quantile, probs = c(0.025, 0.5, 0.975), na.rm = T)
    R_post[2, ] <- R_post_mean
    
    return(list(I_post, R_post, highDate, crossDate))   # return the estimate and CI for only post part
  }
  
  # combinining these MCMC sampler and forecasting
  create_pred <- function(I, R, N, pi0, minimal_pi0, start_date, init.params, control.params, session) {
    
    ## CHECK FROM HERE
    # take first time I and R both are positive
    firstPosIndex <- which((I > 0) & (R > 0))[1]
    if(is.na(firstPosIndex)){stop("No Prediction is possible as either there has been no cases in this district or all the cases have recovered.")}
    
    # truncate to required indices
    Dates <- start_date - 1 + (firstPosIndex:length(pi0))
    I_obs <- I[firstPosIndex:length(pi0)] / N
    R_obs <- R[firstPosIndex:length(pi0)] / N
    
    if(0 %in% I_obs){
      I_obs <- (I[firstPosIndex:length(pi0)]+0.4) / N
      R_obs <- (R[firstPosIndex:length(pi0)]+0.4) / N
    }
    
    pi0 <- pi0[firstPosIndex:length(pi0)]
    pi0 <- pmax(pi0, 0.01)    # if pi is < 0.01 for some time, truncate to 0.01
    
    T_obs <- length(I_obs)
    
    mcmc_sample <- do.Gibbs(I_obs, R_obs, pi0, init.params, control.params, session)  # perform Gibbs sampling
    
    # create the estimate for observed time
    theta_pre <- array(mcmc_sample[, -(1:(T_obs+6))], dim = c(control.params$mclen, T_obs+1, 3))
    I_pre_mean <- colMeans(theta_pre[, -1, 2], na.rm = T)
    R_pre_mean <- colMeans(theta_pre[, -1, 3], na.rm = T)

    I_pre <- apply(theta_pre[, -1, 2], MARGIN = 2, FUN = quantile, probs = c(0.025, 0.5, 0.975), na.rm = T)
    I_pre[2, ] <- I_pre_mean

    R_pre <- apply(theta_pre[, -1, 3], MARGIN = 2, FUN = quantile, probs = c(0.025, 0.5, 0.975), na.rm = T)
    R_pre[2, ] <- R_pre_mean

    # create tibble holding these
    pred_df <- tibble(Date = Dates, 
                      Lower.Infected = I_pre[1, ], Estimate.Infected = I_pre[2, ], Upper.Infected = I_pre[3, ],
                      Lower.Removed = R_pre[1, ], Estimate.Removed = R_pre[2, ], Upper.Removed = R_pre[3, ],
                      Situation = rep("obs", T_obs))
    last_train_date <- tail(pred_df$Date, 1)
    
    
    # create date df
    pred_date_df <- tibble(Situation = "situ", Variable = "var", 
                           Lower = as.Date('2020-05-01'), 
                           Estimate = as.Date('2020-05-01'), 
                           Upper = as.Date('2020-05-01') )
    
    
    # now check on with the dates
    pi_scenario <- c(1, minimal_pi0, 0.1)
    possibleMsg <- logical(3)   # a vector to hold whether possible to have such scenario
    
    # now simulate for 9 scenarios
    show_modal_spinner(text = "Simulating Scenarios", session = session, spin = "circle", color = "#262624")
    
    for (j in 1:3) {
      pi_new <- rep(pi_scenario[j], 90)  # prediction for next 90 days
      pi_new <- pmax(pi_new, 0.01)    # if pi is < 0.01 for some time, truncate to 0.01
      
      if (pi_scenario[j] >= tail(pi0, 1)) {
        possibleMsg[j] <- TRUE
      }
      
      predList <- pred.Gibbs(mcmc_sample, pi_new, T_obs, control.params, I/N, R/N)  # get mcmc prediction
      
      # extract dates
      highDate <- start_date + predList[[3]] - 1
      crossDate <- start_date + predList[[4]] - 1
      
      # add new rows
      pred_df <- pred_df %>% add_row(
        Date = last_train_date + (1:length(pi_new)),
        Lower.Infected = predList[[1]][1, ], Estimate.Infected = predList[[1]][2, ], 
        Upper.Infected = predList[[1]][3, ],
        Lower.Removed = predList[[2]][1, ], Estimate.Removed = predList[[2]][2, ], 
        Upper.Removed = predList[[2]][3, ],
        Situation = rep(paste(j), length(pi_new) ) )
      
      # add new rows to date dataframe
      pred_date_df <- pred_date_df %>% add_row(
        Situation = rep(paste(j), 2),
        Variable = c("HighDate", "CrossDate"),
        Lower = c(highDate[1], crossDate[1]),
        Estimate = c(highDate[2], crossDate[2]),
        Upper = c(highDate[3], crossDate[3])
      )
      
    }
    
    remove_modal_spinner(session = session)
    
    pred_date_df <- pred_date_df[-1, ]   # first one is dummy row
    
    # return this predicted dataframe
    return(list(pred_df, pred_date_df, possibleMsg))
  }
  
  # MAIN BODY OF THIS FUNCTION
  # initial controls
  # Defining control parameters
  control.params$mclen <- round(control.params$ndraw / control.params$thin) * control.params$nchain    #number of MCMC draws in total
  
  # df is a dataframe with Date, State, and Confirmed, Recovered, Deaths, Active columns
  I <- df$Active
  R <- df$Recovered + df$Deaths
  
  # First note that, Y = I + R
  incid = df$Confirmed
  
  t_start <- 2:(length(incid) - 1 - 4)    # window length = 4
  t_end <- t_start + 4
  incidence_curve <- ifelse(diff(incid) < 0, 0, diff(incid))
  
  res <- estimate_R(incidence_curve, method = "parametric_si", config = make_config(list(mean_si = 4.7, std_si = 2.9, t_start = t_start, t_end = t_end)))
  fit <- loess(`Mean(R)` ~ t_start, data = res$R)
  R0_est <- fit$fitted     # get smoothed R0 (starting from day 2)
  
  pi0 <- c(R0_est[1], R0_est) / 3
  
  # now for minimal R0, you pick the R0 of the corresponding dates
  if (!is.null(df$State)) {
    minimal_pi0 <- R0List[df$State[1]] / 3
  }
  else {
    minimal_pi0 <- 2.144195 / 3      # for India
  }
  
  predList <- create_pred(I, R, N, pi0, minimal_pi0, df$Date[1], init.params, control.params, session)
  pred_df <- predList[[1]]
  pred_date_df <- predList[[2]]
  possibleMsg <- predList[[3]]
  
  show_modal_spinner(text = "Combining Simulation scenarios and Saving Predictions", session = session)
  
  pred_df <- pred_df %>%
    mutate(
      Lower.Infected = round(N * Lower.Infected),
      Upper.Infected = round(N * Upper.Infected),
      Estimate.Infected = round(N * Estimate.Infected),
      Lower.Removed = round(N * Lower.Removed),
      Upper.Removed = round(N * Upper.Removed),
      Estimate.Removed = round(N * Estimate.Removed)
    )
  
  return(list(pred_df, pred_date_df, possibleMsg))
}


