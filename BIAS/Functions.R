Non_inf_prob_continuous = function(MID, 
                                   sigma_trt, sigma_ctrl,  
                                   alpha, n_trt, n_ctrl, 
                                   m_trt, m_ctrl, 
                                   yBar_trt, yBar_ctrl,
                                   outcome_is_good){
  MID = abs(MID)
  
  if(!outcome_is_good){
    temp1 = yBar_trt
    yBar_trt = yBar_ctrl
    yBar_ctrl = temp1
    
    temp2 = sigma_trt
    sigma_trt = sigma_ctrl
    sigma_ctrl = temp2
    
    temp3 = n_trt
    n_trt = n_ctrl
    n_ctrl = temp3
    
    temp4 = m_trt
    m_trt = m_ctrl
    m_ctrl = temp4
  }
  
  temp1 = MID - sqrt(sigma_trt^2/m_trt + 
                       sigma_ctrl^2/m_ctrl)*qnorm(1 - alpha) 
  temp2 = yBar_trt - yBar_ctrl
  temp3 = sqrt(sigma_trt^2*(1/m_trt + 1/n_trt) + 
                 sigma_ctrl^2*(1/m_ctrl + 1/n_ctrl))
  
  pnorm((temp1 + temp2)/temp3)
}







Non_inf_prob_continuous_seamless = function(MID, 
                                            sigma_trt, sigma_ctrl,  
                                            alpha, n_trt, n_ctrl, 
                                            m_trt, m_ctrl, 
                                            yBar_trt, yBar_ctrl,
                                            outcome_is_good){
  MID = abs(MID)
  
  if(!outcome_is_good){
    temp1 = yBar_trt
    yBar_trt = yBar_ctrl
    yBar_ctrl = temp1
    
    temp2 = sigma_trt
    sigma_trt = sigma_ctrl
    sigma_ctrl = temp2
    
    temp3 = n_trt
    n_trt = n_ctrl
    n_ctrl = temp3
    
    temp4 = m_trt
    m_trt = m_ctrl
    m_ctrl = temp4
  }
  
  delta = yBar_trt - yBar_ctrl + MID
  sigma_ctrl2 = sigma_ctrl^2/(m_ctrl + n_ctrl)
  sigma_ctrl2_post = sigma_ctrl2*m_ctrl/n_ctrl
  sigma_trt2 = sigma_trt^2/(m_trt + n_trt)
  sigma_trt2_post = sigma_trt2*m_trt/n_trt
  sigma = sqrt(sigma_ctrl2 + sigma_trt2)
  sigma_post = sqrt(sigma_ctrl2_post + sigma_trt2_post)
  
  pnorm((delta - qnorm(1-alpha)*sigma)/sigma_post)
}









rbetabinom = function(n_draws, n, alpha, beta){
  p = rbeta(n_draws, alpha, beta)
  rbinom(n=n_draws, size=n, prob=p)
}






Non_inf_prob_of_success_binom_approx = function(m_ctrl, m_trt, 
                                                y_ctrl, y_trt, 
                                                n_ctrl, n_trt, 
                                                alpha, MID,
                                                outcome_is_good){
  MID = abs(MID)
  
  if(outcome_is_good){
    temp1 = y_trt
    y_trt = y_ctrl
    y_ctrl = temp1
    
    temp2 = n_trt
    n_trt = n_ctrl
    n_ctrl = temp2
    
    temp3 = m_trt
    m_trt = m_ctrl
    m_ctrl = temp3
  }
  
  p_ctrl = y_ctrl/n_ctrl
  p_trt = y_trt/n_trt
  sigma = sqrt(p_ctrl*(1-p_ctrl)/m_ctrl + p_trt*(1-p_trt)/m_trt)
  sigma_post = sqrt(p_ctrl*(1-p_ctrl)*(1/m_ctrl + 1/n_ctrl)
                    + p_trt*(1-p_trt)*(1/m_trt + 1/n_trt))
  
  p_delta = p_ctrl - p_trt
  POS_non_inf_approx = pnorm((MID - qnorm(1-alpha)*sigma + p_delta)/sigma_post)
  
  return(POS_non_inf_approx)
}








Non_inf_prob_of_success_binom_approx_seamless = function(m_ctrl, m_trt, 
                                                         y_ctrl, y_trt, 
                                                         n_ctrl, n_trt, 
                                                         alpha, MID,
                                                         outcome_is_good){
  MID = abs(MID)
  
  if(outcome_is_good){
    temp1 = y_trt
    y_trt = y_ctrl
    y_ctrl = temp1
    
    temp2 = n_trt
    n_trt = n_ctrl
    n_ctrl = temp2
    
    temp3 = m_trt
    m_trt = m_ctrl
    m_ctrl = temp3
  }
  
  p_ctrl = y_ctrl/n_ctrl
  p_trt = y_trt/n_trt
  delta = p_ctrl - p_trt + MID
  
  sigma2_ctrl = p_ctrl*(1-p_ctrl)/(m_ctrl + n_ctrl)
  sigma2_ctrl_post = sigma2_ctrl*m_ctrl/n_ctrl
  sigma2_trt = p_trt*(1-p_trt)/(m_trt + n_trt)
  sigma2_trt_post = sigma2_trt*m_trt/n_trt
  sigma = sqrt(sigma2_ctrl + sigma2_trt)
  sigma_post = sqrt(sigma2_ctrl_post + sigma2_trt_post)
  
  pnorm((delta - qnorm(1-alpha)*sigma)/sigma_post)
}








Non_inf_prob_of_success_binom_sim = function(N, m_ctrl, m_trt, 
                                             y_ctrl, y_trt, n_ctrl, 
                                             n_trt, alpha, MID,
                                             outcome_is_good = F){
  MID = abs(MID)
  
  if(outcome_is_good){
    temp1 = y_trt
    y_trt = y_ctrl
    y_ctrl = temp1
    
    temp2 = n_trt
    n_trt = n_ctrl
    n_ctrl = temp2
    
    temp3 = m_trt
    m_trt = m_ctrl
    m_ctrl = temp3
  }
  
  y_ctrl_new = rbetabinom(N, m_ctrl, y_ctrl + 1, n_ctrl - y_ctrl + 1)
  p_ctrl_new = y_ctrl_new/m_ctrl
  y_trt_new = rbetabinom(N, m_trt, y_trt + 1, n_trt - y_trt + 1)
  p_trt_new = y_trt_new/m_trt
  sigma = sqrt(p_ctrl_new*(1-p_ctrl_new)/m_ctrl + p_trt_new*(1-p_trt_new)/m_trt)
  
  p_delta_new = p_ctrl_new - p_trt_new + MID
  sigma[sigma == 0] = Inf
  
  POS_sup_simulation = sum(p_delta_new/sigma > qnorm(1-alpha))/N
  
  
  return(POS_sup_simulation)
}






samp_size_continuous = function(MID, 
                                sigma_trt, sigma_ctrl,  
                                alpha, n_trt, n_ctrl, 
                                yBar_trt, yBar_ctrl,
                                p_target,
                                outcome_is_good = T){
  
  MID = abs(MID)
  
  if(!outcome_is_good){
    temp1 = yBar_trt
    yBar_trt = yBar_ctrl
    yBar_ctrl = temp1
    
    temp2 = sigma_trt
    sigma_trt = sigma_ctrl
    sigma_ctrl = temp2
    
    temp3 = n_trt
    n_trt = n_ctrl
    n_ctrl = temp3
    
    temp4 = m_trt
    m_trt = m_ctrl
    m_ctrl = temp4
  }
  
  temp1 = MID + yBar_trt - yBar_ctrl
  temp2 = sqrt(sigma_trt^2*(1+1/n_trt) + 
                 sigma_ctrl^2*(1+1/n_ctrl))*qnorm(p_target)
  temp3 = sqrt(sigma_trt^2 + sigma_ctrl^2)*qnorm(1 - alpha)
  
  ceiling((temp2 + temp3)^2/temp1^2)
}






min_samp_size_p_success_plot_continuous = function(MID, 
                                                   sigma_trt, sigma_ctrl,  
                                                   alpha, n_trt, n_ctrl, 
                                                   yBar_trt, yBar_ctrl,
                                                   p_target,
                                                   outcome_is_good){
  m_vec = 1:1000
  Ps = Non_inf_prob_continuous(MID, 
                               sigma_trt, sigma_ctrl,  
                               alpha, n_trt, n_ctrl, 
                               m_vec, m_vec, 
                               yBar_trt, yBar_ctrl,
                               outcome_is_good)
  test = which(Ps >= p_target)
  M = max(m_vec)
  
  while(length(test) == 0 && M < 1e4){
    m_vec_new = (M + 1):(M + 500)
    Ps_new = Non_inf_prob_continuous(MID, 
                                     sigma_trt, sigma_ctrl,  
                                     alpha, n_trt, n_ctrl, 
                                     m_vec_new, m_vec_new, 
                                     yBar_trt, yBar_ctrl,
                                     outcome_is_good)
    test = c(test, M + 1 + which(Ps_new >= p_target))
    m_vec = c(m_vec, m_vec_new)
    Ps = c(Ps, Ps_new)
    
    M = max(m_vec_new)
  }
  
  m_min = ifelse(M < 1e4, min(test), 1e4 - m_vec[1] + 1)
  m_max = min(length(Ps), 2*m_min)
  
  
  df = data.frame(m = m_vec, P = Ps)
  df = df[1:m_max,]
  df1 = data.frame(x = c(1, m_min, m_min), 
                   y = c(Ps[m_min], Ps[m_min], 0))
  
  
  p = ggplot(df, aes(m, P)) + 
    geom_line(size = 2) + 
    geom_line(df1, mapping = aes(x, y), 
              linetype = 'dashed', col = 'red') + 
    geom_point(aes(x = m_min, y = Ps[m_min]), 
               shape=21, size=5, fill="white", 
               colour = "dodgerblue4", stroke = 2) + 
    xlab("Future sample size (per arm)") + 
    ylab("Predictive Power") + 
    ggtitle(paste0("For ", p_target*100, 
                   "% BPP use ",
                   m_min, " samples per arm")) + 
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=18, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=20, hjust = .5,
                                    face="bold.italic")) +
    scale_y_continuous(expand = c(0.01, 0.02)) + 
    scale_x_continuous(expand = c(0.01, 0))
  
  p
}






min_samp_size_p_success_seamless_plot_continuous = function(MID, 
                                                            sigma_trt, sigma_ctrl,  
                                                            alpha, n_trt, n_ctrl, 
                                                            yBar_trt, yBar_ctrl,
                                                            p_target,
                                                            outcome_is_good){
  m_vec = 1:1000
  Ps = Non_inf_prob_continuous_seamless(MID, 
                                        sigma_trt, sigma_ctrl,  
                                        alpha, n_trt, n_ctrl, 
                                        m_vec, m_vec, 
                                        yBar_trt, yBar_ctrl,
                                        outcome_is_good)
  test = which(Ps >= p_target)
  M = max(m_vec)
  
  while(length(test) == 0 && M < 1e4){
    m_vec_new = (M + 1):(M + 500)
    Ps_new = Non_inf_prob_continuous_seamless(MID, 
                                              sigma_trt, sigma_ctrl,  
                                              alpha, n_trt, n_ctrl, 
                                              m_vec_new, m_vec_new, 
                                              yBar_trt, yBar_ctrl,
                                              outcome_is_good)
    test = c(test, M + 1 + which(Ps_new >= p_target))
    m_vec = c(m_vec, m_vec_new)
    Ps = c(Ps, Ps_new)
    
    M = max(m_vec_new)
  }
  
  m_min = ifelse(M < 1e4, min(test), 1e4 - m_vec[1] + 1)
  m_max = min(length(Ps), 2*m_min)
  
  
  df = data.frame(m = m_vec, P = Ps)
  df = df[1:m_max,]
  df1 = data.frame(x = c(1, m_min, m_min), 
                   y = c(Ps[m_min], Ps[m_min], 0))
  
  plot_title = ifelse(Ps[m_min - m_vec[1] + 1] >= p_target,
                      paste0("For ", p_target*100, 
                             "% BPP use ",
                             m_min, " samples per arm"),
                      paste0("Not even 10,000 samples per arm will suffice for a BPP of ",
                             p_target*100, "%")
  )
  
  
  ggplot(df, aes(m, P)) + 
    geom_line(size = 2) + 
    geom_line(df1, mapping = aes(x, y), 
              linetype = 'dashed', col = 'red') + 
    geom_point(aes(x = m_min, y = Ps[m_min]), 
               shape=21, size=5, fill="white", 
               colour = "dodgerblue4", stroke = 2) + 
    xlab("Future sample size (per arm)") + 
    ylab("Predictive Power") + 
    ggtitle(plot_title) + 
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=18, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=20, hjust = .5,
                                    face="bold.italic")) + 
    scale_y_continuous(expand = c(0.01, 0.02)) + 
    scale_x_continuous(expand = c(0.01, 0))
}









min_samp_size_p_success_plot_binomial = function(y_ctrl, y_trt, 
                                                 n_ctrl, n_trt, 
                                                 alpha, MID,
                                                 p_target,
                                                 outcome_is_good){
  m_vec = 30:500
  Ps = Non_inf_prob_of_success_binom_approx(m_vec, m_vec, 
                                            y_ctrl, y_trt, 
                                            n_ctrl, n_trt, 
                                            alpha, MID,
                                            outcome_is_good)
  test = m_vec[which(Ps >= p_target)]
  M = max(m_vec)
  
  while(length(test) == 0 && M < 1e4){
    m_vec_new = (M + 1):(M + 500)
    Ps_new = Non_inf_prob_of_success_binom_approx(m_vec_new, 
                                                  m_vec_new, 
                                                  y_ctrl, 
                                                  y_trt, 
                                                  n_ctrl, 
                                                  n_trt, 
                                                  alpha, 
                                                  MID,
                                                  outcome_is_good)
    test = c(test, M + 1 + which(Ps_new >= p_target))
    m_vec = c(m_vec, m_vec_new)
    Ps = c(Ps, Ps_new)
    
    M = max(m_vec_new)
  }
  m_min = ifelse(M < 1e4, min(test), 1e4 - m_vec[1] + 1)
  m_max = min(length(Ps) + min(m_vec) - 1, 2*m_min)
  
  m_y = m_min - min(m_vec) + 1
  
  
  df = data.frame(m = m_vec, P = Ps)
  df = df[1:m_max,]
  df1 = data.frame(x = c(min(m_vec), m_min, m_min), 
                   y = c(Ps[m_y], Ps[m_y], 0))
  
  plot_title = ifelse(Ps[m_min - m_vec[1] + 1] >= p_target,
                      paste0("For ", p_target*100, 
                             "% BPP use ",
                             m_min, " samples per arm"),
                      paste0("Not even 10,000 samples per arm will suffice for a BPP of ",
                             p_target*100, "%")
  )
  
  ggplot(df, aes(m, P)) + 
    geom_line(size = 2) + 
    geom_line(df1, mapping = aes(x, y), 
              linetype = 'dashed', col = 'red') + 
    geom_point(aes(x = m_min, y = Ps[m_y]), 
               shape=21, size=5, fill="white", 
               colour = "dodgerblue4", stroke = 2) + 
    xlab("Future sample size (per arm)") + 
    ylab("Predictive Power") + 
    ggtitle(plot_title) + 
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=18, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=20, hjust = .5,
                                    face="bold.italic")) + 
    scale_y_continuous(expand = c(0.01, 0.02)) + 
    scale_x_continuous(expand = c(0.01, 0))
}







min_samp_size_p_success_seamless_plot_binomial = function(y_ctrl, 
                                                          y_trt, 
                                                          n_ctrl, 
                                                          n_trt, 
                                                          alpha, 
                                                          MID,
                                                          p_target,
                                                          outcome_is_good){
  m_vec = 30:500
  Ps = Non_inf_prob_of_success_binom_approx_seamless(m_vec, m_vec, 
                                                     y_ctrl, y_trt, 
                                                     n_ctrl, n_trt, 
                                                     alpha, MID,
                                                     outcome_is_good)
  test = m_vec[which(Ps >= p_target)]
  M = max(m_vec)
  
  while(length(test) == 0 && M < 1e4){
    m_vec_new = (M + 1):(M + 500)
    Ps_new = Non_inf_prob_of_success_binom_approx_seamless(m_vec_new, 
                                                           m_vec_new, 
                                                           y_ctrl, 
                                                           y_trt, 
                                                           n_ctrl, 
                                                           n_trt, 
                                                           alpha, 
                                                           MID,
                                                           outcome_is_good)
    test = c(test, M + 1 + which(Ps_new >= p_target))
    m_vec = c(m_vec, m_vec_new)
    Ps = c(Ps, Ps_new)
    
    M = max(m_vec_new)
  }
  m_min = ifelse(M < 1e4, min(test), 1e4 - m_vec[1] + 1)
  m_max = min(length(Ps) + min(m_vec) - 1, 2*m_min)
  
  m_y = m_min - min(m_vec) + 1
  
  
  df = data.frame(m = m_vec, P = Ps)
  df = df[1:m_max,]
  df1 = data.frame(x = c(min(m_vec), m_min, m_min), 
                   y = c(Ps[m_y], Ps[m_y], 0))
  
  plot_title = ifelse(Ps[m_min - m_vec[1] + 1] >= p_target,
                      paste0("For ", p_target*100, 
                             "% BPP use ",
                             m_min, " samples per arm"),
                      paste0("Not even 10,000 samples per arm will suffice for a BPP of ",
                             p_target*100, "%")
  )
  
  ggplot(df, aes(m, P)) + 
    geom_line(size = 2) + 
    geom_line(df1, mapping = aes(x, y), 
              linetype = 'dashed', col = 'red') + 
    geom_point(aes(x = m_min, y = Ps[m_y]), 
               shape=21, size=5, fill="white", 
               colour = "dodgerblue4", stroke = 2) + 
    xlab("Future sample size (per arm)") + 
    ylab("Predictive Power") + 
    ggtitle(plot_title) + 
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=18, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=20, hjust = .5,
                                    face="bold.italic")) + 
    scale_y_continuous(expand = c(0.01, 0.02)) + 
    scale_x_continuous(expand = c(0.01, 0))
}








beta_posterior_params = function(samp_size_vec, n_cases_vec){
  alpha = 1 + n_cases_vec
  beta = 1 + samp_size_vec - n_cases_vec
  
  return(list(alpha = alpha, beta = beta))
}






normal_posterior_params = function(samp_size_vec,
                                   ybar_vec, sd_vec){
  center = ybar_vec
  spread = sd_vec/sqrt(samp_size_vec)
  
  return(list(center = center, spread = spread))
}






beta_posterior_samp = function(samp_size_vec, 
                               n_cases_vec, N_MC){
  n_arms = length(samp_size_vec)
  
  params = beta_posterior_params(samp_size_vec, n_cases_vec)
  alpha = params$alpha
  beta = params$beta
  
  a = rep(alpha, each = N_MC)
  b = rep(beta, each = N_MC)
  
  M = matrix(rbeta(n_arms*N_MC, a, b), ncol = n_arms)
  return(M)
}







beta_posterior_density_df = function(samp_size_vec, 
                                     n_cases_vec){
  n_arms = length(samp_size_vec)
  
  params = beta_posterior_params(samp_size_vec, n_cases_vec)
  alpha = params$alpha
  beta = params$beta
  
  Arm = rep(1:n_arms, each = 200)
  a = rep(alpha, each = 200)
  b = rep(beta, each = 200)
  
  q = t(sapply(1:n_arms,
               function(i){
                 qbeta(c(.0005, .9995), alpha[i], beta[i])
               }))
  
  x = c(apply(q, 1, function(v){
    seq(v[1], v[2], length = 200)
  })
  )
  
  Density = dbeta(x, a, b)
  df = data.frame(Arm = as.factor(Arm), x = x, Density = Density)
  
  return(df)
}






normal_posterior_samp = function(samp_size_vec, ybar_vec, 
                                 sd_vec, N_MC){
  n_arms = length(samp_size_vec)
  
  params = normal_posterior_params(samp_size_vec,
                                   ybar_vec, sd_vec)
  center = params$center
  spread = params$spread
  
  a = rep(center, each = N_MC)
  b = rep(spread, each = N_MC)
  df = rep(samp_size_vec - 1, each = N_MC)
  
  M = matrix(a + b*rt(n_arms*N_MC, df), ncol = n_arms)
  return(M)
}






normal_posterior_density_df = function(samp_size_vec,
                                       ybar_vec, sd_vec){
  n_arms = length(samp_size_vec)
  
  params = normal_posterior_params(samp_size_vec,
                                   ybar_vec, sd_vec)
  center = params$center
  spread = params$spread
  
  Arm = rep(1:n_arms, each = 200)
  mu = rep(center, each = 200)
  se = rep(spread, each = 200)
  df = rep(samp_size_vec - 1, each = 200)
  x = mu + se*rep(seq(-3.5, 3.5, length = 200), n_arms)
  Density = dt((x- mu)/se, df)/se
  df = data.frame(Arm = as.factor(Arm), x = x, Density = Density)
  
  return(df)
}






posterior_dens_plot = function(df, response){
  str = ifelse(response == "Numeric", 
               "Mean response",
               "Event rate")
  
  ggplot(df, aes(x, Density, col = Arm)) +
    geom_area(df, position = 'identity',
              mapping = aes(x = x, y = Density, fill = Arm),
              alpha = .75, size = 1) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=18, hjust = .5,
                                    face="bold.italic")) +
    xlab(str) + 
    scale_fill_brewer(palette="Set1") + 
    scale_color_brewer(palette="Set1") + 
    scale_y_continuous(expand = c(0.01, 0.01)) + 
    scale_x_continuous(expand = c(0.01, 0)) + 
    ggtitle("Posterior distribution")
}






normal_CrI_plot = function(samp_size_vec, ybar_vec, sd_vec){
  params = normal_posterior_params(samp_size_vec,
                                   ybar_vec, sd_vec)
  center = params$center
  spread = params$spread
  d_f = samp_size_vec - 1
  
  df = center + spread*t(sapply(d_f, 
                                function(x){
                                  qt(c(.025, .5, .975), x)
                                }))
  df = data.frame(df)
  names(df) = c("LL", "Median", "UL")
  df$Arm = as.factor(1:nrow(df))
  df = df %>% mutate(CrI = paste0(sprintf("%.3f", Median), 
                                  " [", 
                                  sprintf("%.3f", LL), 
                                  ", ", 
                                  sprintf("%.3f", UL), 
                                  "]"))
  
  ggplot(df, 
         aes(x = Arm, y = Median, ymin = LL, ymax = UL,
             label = CrI)) + 
    geom_pointrange(aes(col = Arm), size = 1) +
    geom_errorbar(aes(ymin=LL, ymax=UL, col=Arm), width=.15, cex=1) +
    ylab("Mean response [95% CrI]") + 
    scale_y_continuous(expand = c(0.05, 0.05)) +
    geom_text(nudge_x = .2, size = 5, hjust="inward") + 
    coord_flip() +
    theme(legend.position = "none",
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=18, hjust = .5,
                                    face="bold.italic")) + 
    scale_color_brewer(palette="Set1")
}





beta_CrI_plot = function(samp_size_vec, n_cases_vec){
  params = beta_posterior_params(samp_size_vec, n_cases_vec)
  alpha = params$alpha
  beta = params$beta
  
  df = t(sapply(1:length(alpha), 
                function(i){
                  qbeta(c(.025, .5, .975), alpha[i], beta[i])
                }))
  df = data.frame(df)
  names(df) = c("LL", "Median", "UL")
  df$Arm = as.factor(1:nrow(df))
  df = df %>% mutate(CrI = paste0(sprintf("%.3f", Median), 
                                  " [", 
                                  sprintf("%.3f", LL), 
                                  ", ", 
                                  sprintf("%.3f", UL), 
                                  "]"))
  
  
  ggplot(df, 
         aes(x = Arm, y = Median, 
             ymin = LL, ymax = UL, label = CrI)) + 
    geom_pointrange(aes(col = Arm), size = 1) +
    geom_errorbar(aes(ymin=LL, ymax=UL, col=Arm), width=.15, cex=1) +
    ylab("Event rate [95% CrI]") + 
    scale_y_continuous(expand = c(0.05, 0.05)) +
    geom_text(nudge_x = .2, size = 5, hjust="inward") + 
    coord_flip() +
    theme(legend.position = "none",
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=18, hjust = .5,
                                    face="bold.italic")) + 
    scale_color_brewer(palette="Set1")
}






posterior_superiority = function(samp, outcome_is_good){
  if(!outcome_is_good) samp = - samp
  arms = 1:ncol(samp)
  
  tab = table(apply(samp, 1, which.max))
  miss = arms[!(arms %in% names(tab))]
  if(length(miss) > 0){
    tab = c(tab, rep(0, length(miss)))
    names(tab)[names(tab) == ''] = miss
    tab = tab[order(names(tab))]
  }
  
  return(as.numeric(tab/sum(tab)))
}





post_superiority_barplot = function(post_sup){
  df = data.frame(Arm = as.factor(1:length(post_sup)), post_sup = post_sup)
  ggplot(df, aes(Arm, post_sup, fill = Arm)) + 
    geom_bar(stat = 'identity') + 
    scale_fill_brewer(palette="Set1") + 
    ylab("Probability of Superiority") + 
    theme(legend.position = "none",
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    ) + 
    scale_y_continuous(expand = c(0, 0.01),
                       labels = scales::percent_format(accuracy = 1)#, limits = c(0, 1)
                       ) 
}







post_superiority_barplot_new = function(post_sup, power){
  df = data.frame(Arm = as.factor(1:length(post_sup)), 
                  Probability = post_sup,
                  Group = rep("Superiority", length(post_sup))
  )
  
  if(!is.na(power)){
    temp = post_sup^power
    alloc_prob = temp/sum(temp)
    temp_df = data.frame(Arm = as.factor(1:length(post_sup)), 
                         Probability = alloc_prob,
                         Group = rep("Allocation", length(post_sup))
    )
    df = rbind(df, temp_df)
  }
  
  p = ggplot(df, aes(Arm, Probability, fill = Group)) + 
    geom_bar(stat = 'identity', 
             position = "dodge") + 
    scale_fill_brewer(palette="Set1") + 
    ylab("Probability") + 
    theme(
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_line(colour = "grey92"),
      axis.title.x = element_text(size=18, face="bold"),
      axis.title.y = element_text(size=18, face="bold"),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16),
      legend.text=element_text(size=14),
      legend.title=element_text(size=16)
    ) + 
    scale_y_continuous(expand = c(0, 0.01),
                       labels = scales::percent_format(accuracy = 1) #, limits = c(0, 1)
                       ) 
  
  p = p + guides(fill = guide_legend(title = "Probability"))
  
  return(p)
}







prob_of_superiority_table = function(post_sup, power){
  is_max = post_sup == max(post_sup)
  is_min = post_sup == min(post_sup)

  
  arms = 1:length(post_sup)
  post_sup_pct = paste0(sprintf("%.1f", post_sup*100), "%")
  
  df = data.frame(Arm = arms, 
                  post_sup_pct = post_sup_pct) %>%
    mutate(
      Arm = cell_spec(Arm, "html", 
                      color = ifelse(is_max, "green", 
                                     ifelse(is_min, "red", "black")),
                      bold = is_min | is_max),
      post_sup_pct = cell_spec(post_sup_pct, "html", 
                               color = ifelse(is_max, "green", 
                                              ifelse(is_min, "red", "black")),
                               bold = is_min | is_max)
    )
  
  
  if(!is.na(power)){
    RAR = post_sup^power
    RAR = RAR/sum(RAR)
    RAR_pct = paste0(sprintf("%.1f", RAR*100), "%")
    
    df$Alloc_prob = RAR_pct
    
    df = df %>% mutate(
      Alloc_prob = cell_spec(Alloc_prob, "html", 
                             color = ifelse(is_max, "green", 
                                            ifelse(is_min, "red", "black")),
                             bold = is_min | is_max)
    )       
  }
  
  if(!is.na(power)){
    kb = knitr::kable(df, "html", align = "cc", 
                      col.names = c("Arm", 
                                    "Prob. of Superiority",
                                    "Allocation Prob."), 
                      escape = F) 
  } else{
    kb = knitr::kable(df, "html", align = "cc", 
                      col.names = c("Arm", 
                                    "Prob. of Superiority"), 
                      escape = F) 
  }
  
  kb %>%
    kable_styling(bootstrap_options = c("striped", "bordered"),
                  full_width = F)
}






ppos_selection_probs_table = function(probs, assurance){
  n_arms = length(probs)
  probs_pct = paste0(sprintf("%.1f", probs*100), "%")
  assur_pct = paste0(sprintf("%.1f", assurance*100), "%")
  
  df = data.frame(Arm = 1:n_arms, 
                  probs_pct = probs_pct, 
                  assur_pct = assur_pct)
  
  knitr::kable(df, "html", align = "ccc", 
               col.names = c("Arm Number", 
                             "Probability of Selection",
                             "Expected BPP"), 
               escape = F) %>%
    kable_styling(c("striped", "bordered"),
                  full_width = F)
}





PPOS_selection_continuous_seamless = function(ctrl_mean, trt_mean_vec, 
                                              sigma_ctrl, sigma_trt_vec,
                                              n_ctrl, n_trt_vec, 
                                              m_ctrl, m_trt_vec,
                                              alpha, MID, 
                                              outcome_is_good, 
                                              cutoff){
  MID = abs(MID)
  
  if(!outcome_is_good){
    temp1 = trt_mean_vec
    trt_mean_vec = ctrl_mean
    ctrl_mean = temp1
    
    temp2 = sigma_trt_vec
    sigma_trt_vec = sigma_ctrl
    sigma_ctrl = temp2
    
    temp3 = n_trt_vec
    n_trt_vec = n_ctrl
    n_ctrl = temp3
    
    temp4 = m_trt_vec
    m_trt_vec = m_ctrl
    m_ctrl = temp4
  }
  
  
  delta = trt_mean_vec - ctrl_mean
  
  sigma_ctrl2 = sigma_ctrl^2/n_ctrl
  sigma_ctrl2_star = sigma_ctrl^2/(m_ctrl + n_ctrl)
  sigma_ctrl2_post_star = sigma_ctrl2_star*m_ctrl/n_ctrl
  
  sigma_trt2 = sigma_trt_vec^2/n_trt_vec
  sigma_trt2_star = sigma_trt_vec^2/(m_trt_vec + n_trt_vec)
  sigma_trt2_post_star = sigma_trt2_star*m_trt_vec/n_trt_vec
  
  sigma = sqrt(sigma_ctrl2 + sigma_trt2)
  sigma_star = sqrt(sigma_ctrl2_star + sigma_trt2_star)
  sigma_post_star = sqrt(sigma_ctrl2_post_star + sigma_trt2_post_star)
  
  term1 = - qnorm(1 - alpha)* sigma_star
  term2 = - qnorm(cutoff)*sigma_post_star + MID + delta
  
  return(pnorm((term1 + term2)/sigma))
}






PPOS_selection_binomial_seamless = function(CER, RRR_vec, 
                                            n_ctrl, n_trt_vec, 
                                            m_ctrl, m_trt_vec,
                                            alpha, MID, 
                                            outcome_is_good, 
                                            cutoff){
  
  MID = abs(MID)
  p_trt_vec = CER*(1 - RRR_vec)
  
  if(outcome_is_good){
    temp1 = p_trt_vec
    p_trt_vec = CER
    CER = temp1
    
    temp2 = n_trt_vec
    n_trt_vec = n_ctrl
    n_ctrl = temp2
    
    temp3 = m_trt_vec
    m_trt_vec = m_ctrl
    m_ctrl = temp3
  }
  
  
  delta = CER - p_trt_vec
  
  sigma2_ctrl = CER*(1 - CER)/n_ctrl
  sigma2_ctrl_star = CER*(1 - CER)/(m_ctrl + n_ctrl)
  sigma2_ctrl_post_star = sigma2_ctrl_star*m_ctrl/n_ctrl
  
  sigma2_trt = p_trt_vec*(1 - p_trt_vec)/n_trt_vec
  sigma2_trt_star = p_trt_vec*(1 - p_trt_vec)/(m_trt_vec + n_trt_vec)
  sigma2_trt_post_star = sigma2_trt_star*m_trt_vec/n_trt_vec
  
  sigma = sqrt(sigma2_ctrl + sigma2_trt)
  sigma_star = sqrt(sigma2_ctrl_star + sigma2_trt_star)
  sigma_post_star = sqrt(sigma2_ctrl_post_star + sigma2_trt_post_star)
  
  term1 = - qnorm(1 - alpha)* sigma_star
  term2 = - qnorm(cutoff)*sigma_post_star + MID + delta
  
  return(pnorm((term1 + term2)/sigma))
}






PPOS_selection_continuous = function(ctrl_mean, trt_mean_vec, 
                                     sigma_ctrl, sigma_trt_vec,
                                     n_ctrl, n_trt_vec, 
                                     m_ctrl, m_trt_vec,
                                     alpha, MID, 
                                     outcome_is_good, 
                                     cutoff){
  
  MID = abs(MID)
  
  if(!outcome_is_good){
    temp1 = trt_mean_vec
    trt_mean_vec = ctrl_mean
    ctrl_mean = temp1
    
    temp2 = sigma_trt_vec
    sigma_trt_vec = sigma_ctrl
    sigma_ctrl = temp2
    
    temp3 = n_trt_vec
    n_trt_vec = n_ctrl
    n_ctrl = temp3
    
    temp4 = m_trt_vec
    m_trt_vec = m_ctrl
    m_ctrl = temp4
  }
  
  
  delta = trt_mean_vec - ctrl_mean
  
  sigma_ctrl2 = sigma_ctrl^2/n_ctrl
  sigma_ctrl2_star = sigma_ctrl^2/m_ctrl
  sigma_ctrl2_post_star = sigma_ctrl^2*(1/m_ctrl + 1/n_ctrl)
  
  sigma_trt2 = sigma_trt_vec^2/n_trt_vec
  sigma_trt2_star = sigma_trt_vec^2/m_trt_vec
  sigma_trt2_post_star = sigma_trt_vec^2*(1/m_trt_vec + 1/n_trt_vec)
  
  sigma = sqrt(sigma_ctrl2 + sigma_trt2)
  sigma_star = sqrt(sigma_ctrl2_star + sigma_trt2_star)
  sigma_post_star = sqrt(sigma_ctrl2_post_star + sigma_trt2_post_star)
  
  term1 = - qnorm(1 - alpha)* sigma_star
  term2 = - qnorm(cutoff)*sigma_post_star + MID + delta
  
  return(pnorm((term1 + term2)/sigma))
}





PPOS_selection_binomial = function(CER, RRR_vec, 
                                   n_ctrl, n_trt_vec, 
                                   m_ctrl, m_trt_vec,
                                   alpha, MID, 
                                   outcome_is_good, 
                                   cutoff){
  
  MID = abs(MID)
  p_trt_vec = CER*(1 - RRR_vec)
  
  if(outcome_is_good){
    p_trt_vec = CER*(1 + RRR_vec)
  }
  
  
  delta = CER - p_trt_vec
  
  sigma = sqrt(CER*(1 - CER)/n_ctrl + p_trt_vec*(1 - p_trt_vec)/n_trt_vec)
  sigma_star = sqrt(CER*(1 - CER)/m_ctrl + p_trt_vec*(1 - p_trt_vec)/m_trt_vec)
  sigma_post_star = sqrt(CER*(1 - CER)*(1/m_ctrl + 1/n_ctrl)
                         + p_trt_vec*(1 - p_trt_vec)*(1/m_trt_vec + 1/n_trt_vec))
  
  term1 = - qnorm(1 - alpha)* sigma_star
  term2 = - qnorm(cutoff)*sigma_post_star + MID + delta
  
  return(pnorm((term1 + term2)/sigma))
}





PPOS_continuous_selection_probs_plot_data = function(cuts, ctrl_mean, trt_mean_vec, 
                                                     sigma_ctrl, sigma_trt_vec,
                                                     n_ctrl, n_trt_vec, 
                                                     m_ctrl, m_trt_vec,
                                                     alpha, MID, outcome_is_good,
                                                     cutoff, is_seamless){
  
  if(is_seamless){
    curves = t(sapply(cuts, PPOS_selection_continuous_seamless,
                      ctrl_mean = ctrl_mean, trt_mean_vec = trt_mean_vec, 
                      sigma_ctrl = sigma_ctrl, sigma_trt_vec = sigma_trt_vec,
                      n_ctrl = n_ctrl, n_trt_vec = n_trt_vec, 
                      m_ctrl = m_ctrl, m_trt_vec = m_trt_vec,
                      alpha = alpha, MID = MID, 
                      outcome_is_good = outcome_is_good))
    
    added_cut = PPOS_selection_continuous_seamless(ctrl_mean, trt_mean_vec, 
                                                   sigma_ctrl, sigma_trt_vec,
                                                   n_ctrl, n_trt_vec, 
                                                   m_ctrl, m_trt_vec,
                                                   alpha, MID, 
                                                   outcome_is_good, 
                                                   cutoff)
  } else{
    curves = t(sapply(cuts, PPOS_selection_continuous,
                      ctrl_mean = ctrl_mean, trt_mean_vec = trt_mean_vec, 
                      sigma_ctrl = sigma_ctrl, sigma_trt_vec = sigma_trt_vec,
                      n_ctrl = n_ctrl, n_trt_vec = n_trt_vec, 
                      m_ctrl = m_ctrl, m_trt_vec = m_trt_vec,
                      alpha = alpha, MID = MID, 
                      outcome_is_good = outcome_is_good))
    
    added_cut = PPOS_selection_continuous(ctrl_mean, trt_mean_vec, 
                                          sigma_ctrl, sigma_trt_vec,
                                          n_ctrl, n_trt_vec, 
                                          m_ctrl, m_trt_vec,
                                          alpha, MID, 
                                          outcome_is_good, 
                                          cutoff)
  }
  
  Assurance = apply(curves, 2, mean)
  
  n_arms = ncol(curves)
  df = data.frame(Arm = as.factor(rep(1:n_arms, each = nrow(curves))),
                  Cutoff = rep(cuts, n_arms),
                  Prob_Selection = c(curves))
  
  added_df = data.frame(Arm = as.factor(1:n_arms), 
                        Cutoff = cutoff, 
                        Select_Prob = added_cut)
  
  extended_added_df = data.frame(Arm = rep(as.factor(1:n_arms), each = 3), 
                                 x = rep(c(-Inf, cutoff, cutoff), n_arms), 
                                 y = c(rbind(added_cut, added_cut, -Inf)))
  
  return(list(df = df, added_df = added_df,
              extended_added_df = extended_added_df,
              Assurance = Assurance))
}






PPOS_binomial_selection_probs_plot_data = function(cuts, CER, RRR_vec,
                                                   n_ctrl, n_trt_vec, 
                                                   m_ctrl, m_trt_vec,
                                                   alpha, MID, 
                                                   outcome_is_good,
                                                   cutoff, is_seamless){
  
  if(is_seamless){
    curves = t(sapply(cuts, PPOS_selection_binomial_seamless,
                      CER = CER, RRR_vec = RRR_vec, 
                      n_ctrl = n_ctrl, n_trt_vec = n_trt_vec, 
                      m_ctrl = m_ctrl, m_trt_vec = m_trt_vec,
                      alpha = alpha, MID = MID, 
                      outcome_is_good = outcome_is_good))
    
    added_cut = PPOS_selection_binomial_seamless(CER, RRR_vec, 
                                                 n_ctrl, n_trt_vec, 
                                                 m_ctrl, m_trt_vec,
                                                 alpha, MID, 
                                                 outcome_is_good, 
                                                 cutoff)
  } else{
    curves = t(sapply(cuts, PPOS_selection_binomial,
                      CER = CER, RRR_vec = RRR_vec, 
                      n_ctrl = n_ctrl, n_trt_vec = n_trt_vec, 
                      m_ctrl = m_ctrl, m_trt_vec = m_trt_vec,
                      alpha = alpha, MID = MID, 
                      outcome_is_good = outcome_is_good))
    
    added_cut = PPOS_selection_binomial(CER, RRR_vec, 
                                        n_ctrl, n_trt_vec, 
                                        m_ctrl, m_trt_vec,
                                        alpha, MID, 
                                        outcome_is_good, 
                                        cutoff)
  }
  
  Assurance = apply(curves, 2, mean)
  
  n_arms = ncol(curves)
  df = data.frame(Arm = as.factor(rep(1:n_arms, each = nrow(curves))),
                  Cutoff = rep(cuts, n_arms),
                  Prob_Selection = c(curves))
  
  added_df = data.frame(Arm = as.factor(1:n_arms), 
                        Cutoff = cutoff, 
                        Select_Prob = added_cut)
  
  extended_added_df = data.frame(Arm = rep(as.factor(1:n_arms), each = 3), 
                                 x = rep(c(-Inf, cutoff, cutoff), n_arms), 
                                 y = c(rbind(added_cut, added_cut, -Inf)))
  
  return(list(df = df, added_df = added_df,
              extended_added_df = extended_added_df,
              Assurance = Assurance))
}






PPOS_selection_probs_plot = function(data){
  df = data$df
  added_df = data$added_df
  extended_added_df = data$extended_added_df
  
  ggplot(df, aes(Cutoff, Prob_Selection, col = Arm)) + 
    geom_line() +  
    geom_line(aes(group = Arm), size=1.5) + 
    ggtitle("Probability of Selection vs. BPP Cutoff") +
    ylab("Probability of Selection") +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=18, hjust = .5,
                                    face="bold.italic")) + 
    geom_line(extended_added_df, 
              mapping = aes(x = x, y = y, col = Arm),
              linetype = 'dashed') + 
    geom_point(added_df, mapping = aes(x = Cutoff, 
                                       y = Select_Prob), 
               shape=21, size=5, fill="white", 
               colour = "black", stroke = 2) + 
    scale_color_brewer(palette="Set1") + 
    scale_y_continuous(expand = c(0.01, 0.02)) + 
    scale_x_continuous(expand = c(0.01, 0)) 
}






Power_binomial_density = function(CER, RRR_vec, 
                                  n_ctrl, n_trt_vec, 
                                  m_ctrl, m_trt_vec,
                                  alpha, MID, 
                                  outcome_is_good, 
                                  cutoff){
  
  MID = abs(MID)
  p_trt_vec = CER*(1 - RRR_vec)
  
  if(outcome_is_good){
    p_trt_vec = CER*(1 + RRR_vec)
  }
  
  
  delta = CER - p_trt_vec
  
  sigma = sqrt(CER*(1 - CER)/n_ctrl + p_trt_vec*(1 - p_trt_vec)/n_trt_vec)
  sigma_star = sqrt(CER*(1 - CER)/m_ctrl + p_trt_vec*(1 - p_trt_vec)/m_trt_vec)
  sigma_post_star = sqrt(CER*(1 - CER)*(1/m_ctrl + 1/n_ctrl)
                         + p_trt_vec*(1 - p_trt_vec)*(1/m_trt_vec + 1/n_trt_vec))
  
  term1 = - qnorm(1 - alpha)* sigma_star
  term2 = - qnorm(cutoff)*sigma_post_star + MID + delta
  
  return(dnorm(-(term1 + term2)/sigma)*sigma_post_star/sigma/dnorm(qnorm(cutoff)))
}







Power_binomial_seamless_density = function(CER, RRR_vec, 
                                           n_ctrl, n_trt_vec, 
                                           m_ctrl, m_trt_vec,
                                           alpha, MID, 
                                           outcome_is_good, 
                                           cutoff){
  
  MID = abs(MID)
  p_trt_vec = CER*(1 - RRR_vec)
  
  if(outcome_is_good){
    temp1 = p_trt_vec
    p_trt_vec = CER
    CER = temp1
    
    temp2 = n_trt_vec
    n_trt_vec = n_ctrl
    n_ctrl = temp2
    
    temp3 = m_trt_vec
    m_trt_vec = m_ctrl
    m_ctrl = temp3
  }
  
  
  delta = CER - p_trt_vec
  
  sigma2_ctrl = CER*(1 - CER)/n_ctrl
  sigma2_ctrl_star = CER*(1 - CER)/(m_ctrl + n_ctrl)
  sigma2_ctrl_post_star = sigma2_ctrl_star*m_ctrl/n_ctrl
  
  sigma2_trt = p_trt_vec*(1 - p_trt_vec)/n_trt_vec
  sigma2_trt_star = p_trt_vec*(1 - p_trt_vec)/(m_trt_vec + n_trt_vec)
  sigma2_trt_post_star = sigma2_trt_star*m_trt_vec/n_trt_vec
  
  sigma = sqrt(sigma2_ctrl + sigma2_trt)
  sigma_star = sqrt(sigma2_ctrl_star + sigma2_trt_star)
  sigma_post_star = sqrt(sigma2_ctrl_post_star + sigma2_trt_post_star)
  
  term1 = - qnorm(1 - alpha)* sigma_star
  term2 = - qnorm(cutoff)*sigma_post_star + MID + delta
  
  return(dnorm(-(term1 + term2)/sigma)*sigma_post_star/sigma/dnorm(qnorm(cutoff)))
}






Power_density_binomial_data = function(cuts, CER, RRR_vec,
                                       n_ctrl, n_trt_vec, 
                                       m_ctrl, m_trt_vec,
                                       alpha, MID, 
                                       outcome_is_good,
                                       cutoff, is_seamless){
  
  if(is_seamless){
    densities = t(sapply(cuts, Power_binomial_seamless_density,
                         CER = CER, RRR_vec = RRR_vec, 
                         n_ctrl = n_ctrl, n_trt_vec = n_trt_vec, 
                         m_ctrl = m_ctrl, m_trt_vec = m_trt_vec,
                         alpha = alpha, MID = MID, 
                         outcome_is_good = outcome_is_good))
  } else{
    densities = t(sapply(cuts, Power_binomial_density,
                         CER = CER, RRR_vec = RRR_vec, 
                         n_ctrl = n_ctrl, n_trt_vec = n_trt_vec, 
                         m_ctrl = m_ctrl, m_trt_vec = m_trt_vec,
                         alpha = alpha, MID = MID, 
                         outcome_is_good = outcome_is_good))
  }
  
  n_arms = ncol(densities)
  df = data.frame(Arm = as.factor(rep(1:n_arms, each = nrow(densities))),
                  Power = rep(cuts, n_arms),
                  Density = c(densities))
  
  return(df)
}







Power_normal_seamless_density = function(ctrl_mean, trt_mean_vec, 
                                         sigma_ctrl, sigma_trt_vec,
                                         n_ctrl, n_trt_vec, 
                                         m_ctrl, m_trt_vec,
                                         alpha, MID, 
                                         outcome_is_good, 
                                         cutoff){
  MID = abs(MID)
  
  if(!outcome_is_good){
    temp1 = trt_mean_vec
    trt_mean_vec = ctrl_mean
    ctrl_mean = temp1
    
    temp2 = sigma_trt_vec
    sigma_trt_vec = sigma_ctrl
    sigma_ctrl = temp2
    
    temp3 = n_trt_vec
    n_trt_vec = n_ctrl
    n_ctrl = temp3
    
    temp4 = m_trt_vec
    m_trt_vec = m_ctrl
    m_ctrl = temp4
  }
  
  
  delta = trt_mean_vec - ctrl_mean
  
  sigma_ctrl2 = sigma_ctrl^2/n_ctrl
  sigma_ctrl2_star = sigma_ctrl^2/(m_ctrl+n_ctrl)
  sigma_ctrl2_post_star = sigma_ctrl2_star*m_ctrl/n_ctrl
  
  sigma_trt2 = sigma_trt_vec^2/n_trt_vec
  sigma_trt2_star = sigma_trt_vec^2/(m_trt_vec+n_trt_vec)
  sigma_trt2_post_star = sigma_trt2_star*m_trt_vec/n_trt_vec
  
  sigma = sqrt(sigma_ctrl2 + sigma_trt2)
  sigma_star = sqrt(sigma_ctrl2_star + sigma_trt2_star)
  sigma_post_star = sqrt(sigma_ctrl2_post_star + sigma_trt2_post_star)
  
  term1 = - qnorm(1 - alpha)* sigma_star
  term2 = - qnorm(cutoff)*sigma_post_star + MID + delta
  
  return(dnorm(-(term1 + term2)/sigma)*sigma_post_star/sigma/dnorm(qnorm(cutoff)))
}





Power_normal_density = function(ctrl_mean, trt_mean_vec, 
                                sigma_ctrl, sigma_trt_vec,
                                n_ctrl, n_trt_vec, 
                                m_ctrl, m_trt_vec,
                                alpha, MID, 
                                outcome_is_good, 
                                cutoff){
  MID = abs(MID)
  
  if(!outcome_is_good){
    temp1 = trt_mean_vec
    trt_mean_vec = ctrl_mean
    ctrl_mean = temp1
    
    temp2 = sigma_trt_vec
    sigma_trt_vec = sigma_ctrl
    sigma_ctrl = temp2
    
    temp3 = n_trt_vec
    n_trt_vec = n_ctrl
    n_ctrl = temp3
    
    temp4 = m_trt_vec
    m_trt_vec = m_ctrl
    m_ctrl = temp4
  }
  
  
  delta = trt_mean_vec - ctrl_mean
  
  sigma_ctrl2 = sigma_ctrl^2/n_ctrl
  sigma_ctrl2_star = sigma_ctrl^2/m_ctrl
  sigma_ctrl2_post_star = sigma_ctrl^2*(1/m_ctrl + 1/n_ctrl)
  
  sigma_trt2 = sigma_trt_vec^2/n_trt_vec
  sigma_trt2_star = sigma_trt_vec^2/m_trt_vec
  sigma_trt2_post_star = sigma_trt_vec^2*(1/m_trt_vec + 1/n_trt_vec)
  
  sigma = sqrt(sigma_ctrl2 + sigma_trt2)
  sigma_star = sqrt(sigma_ctrl2_star + sigma_trt2_star)
  sigma_post_star = sqrt(sigma_ctrl2_post_star + sigma_trt2_post_star)
  
  term1 = - qnorm(1 - alpha)* sigma_star
  term2 = - qnorm(cutoff)*sigma_post_star + MID + delta
  
  return(dnorm(-(term1 + term2)/sigma)*sigma_post_star/sigma/dnorm(qnorm(cutoff)))
}






Power_density_normal_data = function(cuts, ctrl_mean, trt_mean_vec, 
                                     sigma_ctrl, sigma_trt_vec,
                                     n_ctrl, n_trt_vec, 
                                     m_ctrl, m_trt_vec,
                                     alpha, MID, outcome_is_good,
                                     cutoff, is_seamless){
  
  if(is_seamless){
    densities = t(sapply(cuts, Power_normal_seamless_density,
                         ctrl_mean = ctrl_mean, trt_mean_vec = trt_mean_vec, 
                         sigma_ctrl = sigma_ctrl, sigma_trt_vec = sigma_trt_vec,
                         n_ctrl = n_ctrl, n_trt_vec = n_trt_vec, 
                         m_ctrl = m_ctrl, m_trt_vec = m_trt_vec,
                         alpha = alpha, MID = MID, 
                         outcome_is_good = outcome_is_good))
  } else{
    densities = t(sapply(cuts, Power_normal_density,
                         ctrl_mean = ctrl_mean, trt_mean_vec = trt_mean_vec, 
                         sigma_ctrl = sigma_ctrl, sigma_trt_vec = sigma_trt_vec,
                         n_ctrl = n_ctrl, n_trt_vec = n_trt_vec, 
                         m_ctrl = m_ctrl, m_trt_vec = m_trt_vec,
                         alpha = alpha, MID = MID, 
                         outcome_is_good = outcome_is_good))
  }
 
  n_arms = ncol(densities)
  df = data.frame(Arm = as.factor(rep(1:n_arms, each = nrow(densities))),
                  Power = rep(cuts, n_arms),
                  Density = c(densities))
  
  return(df)
}





Power_density_plot = function(df){
  ggplot(df, aes(Power, Density, col = Arm)) + 
    geom_area(df, position = 'identity',
              mapping = aes(Power, Density, fill = Arm), 
              alpha = .75, size = 1) + 
    ggtitle("BPP distribution by arm") +
    ylab("Density") +
    xlab("Bayesian Predictive Power") + 
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=18, hjust = .5,
                                    face="bold.italic")) + 
     # theme(plot.margin = unit(c(5.5, 5.5, 5.5, 10), "points")) +
    scale_fill_brewer(palette="Set1") + 
    scale_color_brewer(palette="Set1") + 
    scale_y_continuous(expand = c(0, 0)) + 
    scale_x_continuous(expand = c(0, 0))
}






Selectivity_df = function(data, n_trts_to_select){
  D = reshape2::dcast(data = data$df, 
                      formula = Cutoff ~ Arm,
                      value.var = "Prob_Selection")
  probs = D[,-1]
  probs = probs[,order(data$Assurance, decreasing = T)]
  probs[,-c(1:n_trts_to_select)] = 1 - probs[,-c(1:n_trts_to_select)]
  
  des = apply(probs, 1, prod)
  df = data.frame(Cutoff = D[,1], Selectivity = des)
  return(df)
}





Selectivity_plot = function(df){
  
  cut_opt_ind = which.max(df$Selectivity)
  M = max(df$Selectivity)
  opt_cut = df$Cutoff[cut_opt_ind]
  tit = paste0("Optimal cutoff: ", sprintf("%.2f", opt_cut))
  
  df_cut = data.frame(x = c(opt_cut, opt_cut),
                      y = c(0, M))
  
  p = ggplot(df, aes(Cutoff, Selectivity)) + 
    geom_line(size = 2, col = "darkgreen") + 
    geom_line(df_cut, mapping = aes(x, y),
              linetype = 'dashed', col = 'dodgerblue4') +
    geom_point(aes(x = opt_cut, y = M), 
               shape=21, size=5, fill="white", 
               colour = "dodgerblue4", stroke = 2) + 
    ggtitle(tit) + 
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=18, hjust = .5,
                                    face="bold.italic")
          ) +
     #theme(plot.margin = unit(c(5.5, 50, 5.5, 5.5), "points")) + 
    scale_y_continuous(expand = expansion(mult = c(0, .1))) + 
    scale_x_continuous(expand = c(0, 0.01))
  return(list(p = p, opt_cut = opt_cut))
}






Non_inf_prob_of_success_survival = function(n1, n2, d, M, 
                                            HR, gamma, alpha,
                                            trt_is_good){
  N = n1 + n2
  r = n1/N
  z = qnorm(1 - alpha)
  p_event = d/N
  theta_hat = log(HR)
  if(trt_is_good){
    theta_hat = -theta_hat
    gamma = -gamma
  }
  d_between = M*p_event
  d_max = d + d_between
  
  se_post_between = sqrt(1/(r*(1-r))*(1/d + 1/d_between))
  se_test_between = sqrt(1/(r*(1-r)*d_between))
  
  BPP_between = pnorm((gamma - theta_hat - z*se_test_between)/se_post_between)
  
  return(BPP_between)
}





Non_inf_prob_of_success_survival_seamless = function(n1, n2, d, M, 
                                                     HR, gamma, alpha,
                                                     trt_is_good){
  N = n1 + n2
  r = n1/N
  z = qnorm(1 - alpha)
  p_event = d/N
  theta_hat = log(HR)
  if(trt_is_good){
    theta_hat = -theta_hat
    gamma = -gamma
  }
  d_between = M*p_event
  d_max = d + d_between
  
  se_post_within = sqrt(1/(r*(1-r))*(1/d - 1/d_max))
  se_test_within = sqrt(1/(r*(1-r)*d_max))
  
  BPP_within = pnorm((gamma - theta_hat - z*se_test_within)/se_post_within)
  
  return(BPP_within)
}





min_samp_size_p_success_plot_survival = function(n1, n2, d, 
                                                 HR, MID, alpha,
                                                 trt_is_good, 
                                                 p_target){
  m_vec = 30:500
  Ps = Non_inf_prob_of_success_survival(n1, n2, d, m_vec*2, 
                                        HR, MID, alpha,
                                        trt_is_good)
  test = m_vec[which(Ps >= p_target)]
  M = max(m_vec)
  
  while(length(test) == 0 && M < 1e4){
    m_vec_new = (M + 1):(M + 500)
    Ps_new = Non_inf_prob_of_success_survival(n1, n2, d, 
                                              m_vec_new*2, 
                                              HR, MID, alpha,
                                              trt_is_good)
    test = c(test, M + 1 + which(Ps_new >= p_target))
    m_vec = c(m_vec, m_vec_new)
    Ps = c(Ps, Ps_new)
    
    M = max(m_vec_new)
  }
  m_min = ifelse(M < 1e4, min(test), 1e4 - m_vec[1] + 1)
  m_max = min(length(Ps) + min(m_vec) - 1, 2*m_min)
  
  m_y = m_min - min(m_vec) + 1
  
  
  df = data.frame(m = m_vec, P = Ps)
  df = df[1:m_max,]
  df1 = data.frame(x = c(min(m_vec), m_min, m_min), 
                   y = c(Ps[m_y], Ps[m_y], 0))
  
  plot_title = ifelse(Ps[m_min - m_vec[1] + 1] >= p_target,
                      paste0("For ", p_target*100, 
                             "% BPP use an overall sample size of ", 2*m_min),
                      paste0("Not even 20,000 samples will suffice for a BPP of ",
                             p_target*100, "%")
  )
  
  ggplot(df, aes(m, P)) + 
    geom_line(size = 2) + 
    geom_line(df1, mapping = aes(x, y), 
              linetype = 'dashed', col = 'red') + 
    geom_point(aes(x = m_min, y = Ps[m_y]), 
               shape=21, size=5, fill="white", 
               colour = "dodgerblue4", stroke = 2) + 
    xlab("Future sample size (per arm)") + 
    ylab("Predictive Power") + 
    ggtitle(plot_title) + 
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=18, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=20, hjust = .5,
                                    face="bold.italic")) + 
    scale_y_continuous(expand = c(0.01, 0.02)) + 
    scale_x_continuous(expand = c(0.01, 0))
}





min_samp_size_p_success_plot_survival_seamless = function(n1, n2, d, 
                                                          HR, MID, alpha,
                                                          trt_is_good, 
                                                          p_target){
  m_vec = 30:500
  Ps = Non_inf_prob_of_success_survival_seamless(n1, n2, d, m_vec*2, 
                                                 HR, MID, alpha,
                                                 trt_is_good)
  test = m_vec[which(Ps >= p_target)]
  M = max(m_vec)
  
  while(length(test) == 0 && M < 1e4){
    m_vec_new = (M + 1):(M + 500)
    Ps_new = Non_inf_prob_of_success_survival_seamless(n1, n2, d, 
                                                       m_vec_new*2, 
                                                       HR, MID, alpha,
                                                       trt_is_good)
    test = c(test, M + 1 + which(Ps_new >= p_target))
    m_vec = c(m_vec, m_vec_new)
    Ps = c(Ps, Ps_new)
    
    M = max(m_vec_new)
  }
  m_min = ifelse(M < 1e4, min(test), 1e4 - m_vec[1] + 1)
  m_max = min(length(Ps) + min(m_vec) - 1, 2*m_min)
  
  m_y = m_min - min(m_vec) + 1
  
  
  df = data.frame(m = m_vec, P = Ps)
  df = df[1:m_max,]
  df1 = data.frame(x = c(min(m_vec), m_min, m_min), 
                   y = c(Ps[m_y], Ps[m_y], 0))
  
  plot_title = ifelse(Ps[m_min - m_vec[1] + 1] >= p_target,
                      paste0("For ", p_target*100, 
                             "% BPP use an overall sample size of ",
                             2*m_min),
                      paste0("Not even 20,000 samples will suffice for a BPP of ",
                             p_target*100, "%")
  )
  
  ggplot(df, aes(m, P)) + 
    geom_line(size = 2) + 
    geom_line(df1, mapping = aes(x, y), 
              linetype = 'dashed', col = 'red') + 
    geom_point(aes(x = m_min, y = Ps[m_y]), 
               shape=21, size=5, fill="white", 
               colour = "dodgerblue4", stroke = 2) + 
    xlab("Future sample size (per arm)") + 
    ylab("Predictive Power") + 
    ggtitle(plot_title) + 
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=18, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=20, hjust = .5,
                                    face="bold.italic")) + 
    scale_y_continuous(expand = c(0.01, 0.02)) + 
    scale_x_continuous(expand = c(0.01, 0))
}





PPOS_selection_survival = function(HR_vec, n_ctrl,
                                   n_trt_vec, m_vec,
                                   p_event, alpha, MID, 
                                   outcome_is_good, 
                                   cutoff){
  theta_vec = log(HR_vec)
  z = qnorm(1 - alpha)
  
  if(outcome_is_good){
    theta_vec = -theta_vec
    MID = -MID
  }
  
  r = n_trt_vec/(n_trt_vec + n_ctrl)
  d_vec = (n_ctrl + n_trt_vec)*p_event
  d_between = m_vec*p_event
  d_max = d_vec + d_between
  
  se_post_between = sqrt(1/(r*(1-r))*(1/d_vec + 1/d_between))
  se_test_between = sqrt(1/(r*(1-r)*d_between))
  se = sqrt(1/(r*(1-r)*d_vec))
  
  term1 = - qnorm(1 - alpha)*se_test_between
  term2 = - qnorm(cutoff)*se_post_between + MID - theta_vec
  
  return(pnorm((term1 + term2)/se))
}





PPOS_selection_survival_seamless = function(HR_vec, n_ctrl,
                                            n_trt_vec, m_vec,
                                            p_event, alpha, MID, 
                                            outcome_is_good, 
                                            cutoff){
  theta_vec = log(HR_vec)
  
  z = qnorm(1 - alpha)

  if(outcome_is_good){
    theta_vec = -theta_vec
    MID = -MID
  }

  r = n_trt_vec/(n_trt_vec + n_ctrl)
  d_vec = (n_ctrl + n_trt_vec)*p_event
  d_between = m_vec*p_event
  d_max = d_vec + d_between
  
  se_post_within = sqrt(1/(r*(1-r))*(1/d_vec - 1/d_max))
  se_test_within = sqrt(1/(r*(1-r)*d_max))
  se = sqrt(1/(r*(1-r)*d_vec))
  
  term1 = - qnorm(1 - alpha)*se_test_within
  term2 = - qnorm(cutoff)*se_post_within + MID - theta_vec
  
  return(pnorm((term1 + term2)/se))
}







PPOS_survival_selection_probs_plot_data = function(cuts, HR_vec, n_ctrl,
                                                   n_trt_vec, m_vec,
                                                   p_event, alpha, MID, 
                                                   outcome_is_good, 
                                                   cutoff, is_seamless){
  
  if(is_seamless){
    curves = t(sapply(cuts, PPOS_selection_survival_seamless,
                      HR_vec = HR_vec, 
                      n_ctrl = n_ctrl, 
                      n_trt_vec = n_trt_vec, 
                      m_vec = m_vec,
                      p_event = p_event,
                      alpha = alpha, 
                      MID = MID, 
                      outcome_is_good = outcome_is_good))
    
    added_cut = PPOS_selection_survival_seamless(HR_vec, n_ctrl,
                                                 n_trt_vec, m_vec,
                                                 p_event, alpha, MID, 
                                                 outcome_is_good, 
                                                 cutoff)
  } else{
    curves = t(sapply(cuts, PPOS_selection_survival,
                      HR_vec = HR_vec, 
                      n_ctrl = n_ctrl, 
                      n_trt_vec = n_trt_vec, 
                      m_vec = m_vec,
                      p_event = p_event,
                      alpha = alpha, 
                      MID = MID, 
                      outcome_is_good = outcome_is_good))
    
    added_cut = PPOS_selection_survival(HR_vec, n_ctrl,
                                        n_trt_vec, m_vec,
                                        p_event, alpha, MID, 
                                        outcome_is_good, 
                                        cutoff)
  }
  
  Assurance = apply(curves, 2, mean)
  
  n_arms = ncol(curves)
  df = data.frame(Arm = as.factor(rep(1:n_arms, each = nrow(curves))),
                  Cutoff = rep(cuts, n_arms),
                  Prob_Selection = c(curves))
  
  added_df = data.frame(Arm = as.factor(1:n_arms), 
                        Cutoff = cutoff, 
                        Select_Prob = added_cut)
  
  extended_added_df = data.frame(Arm = rep(as.factor(1:n_arms), each = 3), 
                                 x = rep(c(-Inf, cutoff, cutoff), n_arms), 
                                 y = c(rbind(added_cut, added_cut, -Inf)))
  
  return(list(df = df, added_df = added_df,
              extended_added_df = extended_added_df,
              Assurance = Assurance))
}





Power_survival_density = function(HR_vec, 
                                  n_ctrl, n_trt_vec, 
                                  m_vec, p_event,
                                  alpha, MID, 
                                  outcome_is_good, 
                                  cutoff){
  MID = abs(MID)
  theta_vec = log(HR_vec)
  
  if(outcome_is_good){
    theta_vec = - theta_vec
  }
  
  r = n_trt_vec/(n_trt_vec + n_ctrl)
  d_vec = (n_ctrl + n_trt_vec)*p_event
  d_between = m_vec*p_event
  d_max = d_vec + d_between
  
  se_post_between = sqrt(1/(r*(1-r))*(1/d_vec + 1/d_between))
  se_test_between = sqrt(1/(r*(1-r)*d_between))
  se = sqrt(1/(r*(1-r)*d_vec))
  
  term1 = - qnorm(1 - alpha)*se_test_between
  term2 = - qnorm(cutoff)*se_post_between + MID - theta_vec
  
  return(dnorm(-(term1 + term2)/se)*se_post_between/se/dnorm(qnorm(cutoff)))
}





Power_survival_seamless_density = function(HR_vec, 
                                           n_ctrl, n_trt_vec, 
                                           m_vec, p_event,
                                           alpha, MID, 
                                           outcome_is_good, 
                                           cutoff){
  MID = abs(MID)
  theta_vec = log(HR_vec)
  
  if(outcome_is_good){
    theta_vec = - theta_vec
  }
  
  r = n_trt_vec/(n_trt_vec + n_ctrl)
  d_vec = (n_ctrl + n_trt_vec)*p_event
  d_between = m_vec*p_event
  d_max = d_vec + d_between
  
  r = n_trt_vec/(n_trt_vec + n_ctrl)
  d_vec = (n_ctrl + n_trt_vec)*p_event
  d_between = m_vec*p_event
  d_max = d_vec + d_between
  
  se_post_within = sqrt(1/(r*(1-r))*(1/d_vec - 1/d_max))
  se_test_within = sqrt(1/(r*(1-r)*d_max))
  se = sqrt(1/(r*(1-r)*d_vec))
  
  term1 = - qnorm(1 - alpha)*se_test_within
  term2 = - qnorm(cutoff)*se_post_within + MID - theta_vec
  
  return(dnorm(-(term1 + term2)/se)*se_post_within/se/dnorm(qnorm(cutoff)))
}





Power_density_survival_data = function(cuts, HR_vec, 
                                       n_ctrl, n_trt_vec, 
                                       m_vec, p_event,
                                       alpha, MID, 
                                       outcome_is_good, 
                                       cutoff, is_seamless){
  
  if(is_seamless){
    densities = t(sapply(cuts, Power_survival_seamless_density,
                         HR_vec = HR_vec, 
                         n_ctrl = n_ctrl, n_trt_vec = n_trt_vec, 
                         m_vec = m_vec, p_event = p_event,
                         alpha = alpha, MID = MID, 
                         outcome_is_good = outcome_is_good))
  } else{
    densities = t(sapply(cuts, Power_survival_density,
                         HR_vec = HR_vec, 
                         n_ctrl = n_ctrl, n_trt_vec = n_trt_vec, 
                         m_vec = m_vec, p_event = p_event,
                         alpha = alpha, MID = MID, 
                         outcome_is_good = outcome_is_good))
  }
  
  n_arms = ncol(densities)
  df = data.frame(Arm = as.factor(rep(1:n_arms, each = nrow(densities))),
                  Power = rep(cuts, n_arms),
                  Density = c(densities))
  
  return(df)
}






predictive_df_continuous = function(sigma_trt, sigma_ctrl,  
                                    n_trt, n_ctrl, 
                                    m_trt, m_ctrl, 
                                    yBar_trt, yBar_ctrl,
                                    outcome_is_good){
  
  mu_pred = yBar_trt - yBar_ctrl
  sigma_pred = sqrt(sigma_trt^2*(1/m_trt + 1/n_trt) + 
                      sigma_ctrl^2*(1/m_ctrl + 1/n_ctrl))
  
  lims = mu_pred + 3.5*c(-1, 1)*sigma_pred
  eff_size = sort(c(seq(lims[1], lims[2], length = 200), MID))
  
  df = data.frame(
    eff_size = eff_size,
    dens = dnorm(eff_size, mu_pred, sigma_pred)
  )
  
  return(list(df = df, mu_pred = mu_pred, 
              sigma_pred = sigma_pred))
}






predictive_df_continuous_seamless = function(sigma_trt, sigma_ctrl,  
                                             n_trt, n_ctrl, 
                                             m_trt, m_ctrl, 
                                             yBar_trt, yBar_ctrl,
                                             outcome_is_good){
  
  mu_pred = yBar_trt - yBar_ctrl
  sigma_ctrl2 = sigma_ctrl^2/(m_ctrl + n_ctrl)
  sigma_ctrl2_post = sigma_ctrl2*m_ctrl/n_ctrl
  sigma_trt2 = sigma_trt^2/(m_trt + n_trt)
  sigma_trt2_post = sigma_trt2*m_trt/n_trt
  sigma_pred = sqrt(sigma_ctrl2_post + sigma_trt2_post)
  
  lims = mu_pred + 3.5*c(-1, 1)*sigma_pred
  eff_size = sort(c(seq(lims[1], lims[2], length = 200), MID))
  
  df = data.frame(
    eff_size = eff_size,
    dens = dnorm(eff_size, mu_pred, sigma_pred)
  )
  
  return(list(df = df, mu_pred = mu_pred, 
              sigma_pred = sigma_pred))
}





predictive_df_binomial = function(m_ctrl, m_trt, 
                                  y_ctrl, y_trt, 
                                  n_ctrl, n_trt, 
                                  MID, outcome_is_good){
  p_ctrl = y_ctrl/n_ctrl
  p_trt = y_trt/n_trt
  sigma_pred = sqrt(p_ctrl*(1-p_ctrl)*(1/m_ctrl + 1/n_ctrl)
                    + p_trt*(1-p_trt)*(1/m_trt + 1/n_trt))
  
  mu_pred = p_ctrl - p_trt
  
  lims = mu_pred + 3.5*c(-1, 1)*sigma_pred
  eff_size = sort(c(seq(lims[1], lims[2], length = 200), MID))
  
  df = data.frame(
    eff_size = eff_size,
    dens = dnorm(eff_size, mu_pred, sigma_pred)
  )
  
  return(list(df = df, mu_pred = mu_pred, 
              sigma_pred = sigma_pred))
}





predictive_density_plot = function(df_list, MID, outcome_is_good){
  df = df_list$df
  mu_pred = df_list$mu_pred
  sigma_pred = df_list$sigma_pred
    
  if(outcome_is_good){
    df_sub = df %>%
      filter(eff_size >= MID)
  } else{
    df_sub = df %>%
      filter(eff_size <= MID)
  }
  
  
  ggplot(df, aes(eff_size, dens)) + 
    geom_line(size = 1) + 
    geom_area(df_sub, mapping = aes(eff_size, dens), fill = '#66FFCC') + 
    geom_segment(x = MID, xend = MID, y = 0, yend = dnorm(0, mu_pred, sigma_pred)) + 
    geom_line(df, mapping = aes(eff_size, dens), col = 'blue', size = 2) + 
    xlab("Effect size") +
    ylab("Predictive density") + 
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=18, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.title = element_text(size=20, hjust = .5,
                                    face="bold.italic")) +
    scale_y_continuous(expand = c(0, 0)) + 
    scale_x_continuous(expand = c(0, 0))
}
