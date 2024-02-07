rm(list = ls())
extrafont::loadfonts(quiet = T)
library(ggplot2)
library(dplyr)
library(scales)
library(reshape)
library(kableExtra)
library(knitr)
library(markdown)
library(MASS)
library(survival)


wd = "C:\\Users\\oharari\\Dropbox\\Cytel\\Interim Analysis Tool\\BIAS_New"
#wd = "C:\\Users\\oharari\\Dropbox\\Interim Analysis Tool"
setwd(wd)


source("Functions.R")


n1 = 125
n2 = 75 
d = 150 
HR = .8
MID = 0 
alpha = .05
trt_is_good = F
p_target = .8

min_samp_size_p_success_plot_survival(n1, n2, d, 
                                      HR, MID, alpha,
                                      trt_is_good, 
                                      p_target)

min_samp_size_p_success_plot_survival_seamless(n1, n2, d, 
                                               HR, MID, alpha,
                                               trt_is_good, 
                                               p_target)

p_event = .75
HR_vec = c(1, .9, .8, .7)
n_ctrl = 250
n_trt_vec = c(250, 250, 250, 250)
m_vec = c(1500, 1500, 1500, 1500)
MID = 0
outcome_is_good = F
alpha = .05
cuts = seq(0, 1, length = 200)
cutoff = .6
n_trts_to_select = 2

data = PPOS_survival_selection_probs_plot_data(cuts, HR_vec, n_ctrl,
                                               n_trt_vec, m_vec,
                                               p_event, alpha, MID, 
                                               outcome_is_good, 
                                               cutoff, is_seamless = T)

PPOS_selection_probs_plot(data)


probs = PPOS_selection_survival_seamless(HR_vec, n_ctrl,
                                         n_trt_vec, m_vec,
                                         p_event, alpha, MID, 
                                         outcome_is_good, 
                                         cutoff)



yBar_trt = 2
yBar_ctrl = 1
n_trt = 50
n_ctrl = 50
rm_trt = 100
m_ctrl = 100
sigma_trt = 5
sigma_ctrl = 5
alpha = .05
MID = 0
P_target = .8
outcome_is_good = T


min_samp_size_p_success_plot_continuous(MID, 
                                        sigma_trt, sigma_ctrl,  
                                        alpha, n_trt, n_ctrl, 
                                        yBar_trt, yBar_ctrl,
                                        P_target,
                                        outcome_is_good)


min_samp_size_p_success_seamless_plot_continuous(MID, 
                                                 sigma_trt, 
                                                 sigma_ctrl,  
                                                 alpha, n_trt, 
                                                 n_ctrl, 
                                                 yBar_trt, 
                                                 yBar_ctrl,
                                                 P_target,
                                                 outcome_is_good)

Non_inf_prob_continuous(MID, 
                        sigma_trt, sigma_ctrl,  
                        alpha, n_trt, n_ctrl, 
                        m_trt, m_ctrl, 
                        yBar_trt, yBar_ctrl,
                        outcome_is_good)



Non_inf_prob_continuous_seamless(MID, 
                                 sigma_trt, sigma_ctrl,  
                                 alpha, n_trt, n_ctrl, 
                                 m_trt, m_ctrl, 
                                 yBar_trt, yBar_ctrl,
                                 outcome_is_good)


n_trt = 100
n_ctrl = 100
y_trt = 5
y_ctrl = 10                                                 
m_trt = 500
m_ctrl = 500
MID = 0
outcome_is_good = F
alpha = .05
P_target = .8



samp1 = rbetabinom(n_draws = 1e6, 
                           n = m_trt, 
                           alpha = y_trt + 1, 
                           beta = n_trt - y_trt + 1)/m_trt
samp2 = rbetabinom(n_draws = 1e6, 
                   n = m_ctrl, 
                   alpha = y_ctrl + 1, 
                   beta = n_ctrl - y_ctrl + 1)/m_ctrl

samp_accurate = samp2 - samp1

breaks = pretty(range(samp_accurate), 
                n = nclass.FD(samp_accurate), 
                min.n = 1)
bwidth = breaks[2]-breaks[1]

df = data.frame(y = samp_accurate)

p_hat_c = y_ctrl/n_ctrl
p_hat_t = y_trt/n_trt

var_approx_c = p_hat_c*(1 - p_hat_c)*(1/n_ctrl + 1/m_ctrl)
var_approx_t = p_hat_t*(1 - p_hat_t)*(1/n_trt + 1/m_trt)
se_approx = sqrt(var_approx_c + var_approx_t)

x = seq(-.2, .3, length = 200)
d = dnorm(x, p_hat_c - p_hat_t, se_approx)

df2 = data.frame(x = x, d = d)


p = ggplot(df, aes(y)) + 
  geom_histogram(#binwidth = bwidth*1.5,
                 binwidth = .01,
                 position='identity',
                 aes(y= ..density..),
                 fill = "grey",
                 col = 'black'
                 ) + 
  geom_line(df2, mapping = aes(x, d), 
            col = 'blue', size = 1) + 
  xlab(TeX("$\\hat{p}_{m_c}^{*c} - \\hat{p}_{m_t}^{*t}$")) +
  ylab('Posterior Density') + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "grey92"),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(hjust=1, size=14),
        axis.text.y = element_text(hjust=1, size=14),
        plot.title = element_text(size=20, hjust = .5,
                                  face="bold.italic")) +
  scale_y_continuous(expand = c(0, 0.05)) + 
  scale_x_continuous(expand = c(0, 0))

p
ggsave("Histogram.eps", p, width = 4, height = 3, device = cairo_ps)

df3 = data.frame(Posterior = 'Approximate',
                 x = x, 
                 cdf = pnorm(x, p_hat_c - p_hat_t, se_approx))
exact_cdf = ecdf(samp_accurate)
df4 = data.frame(Posterior = "Exact",
                 x = environment(exact_cdf)$x,
                 cdf = environment(exact_cdf)$y)

df = rbind(df3, df4)

p = ggplot(df, aes(x, cdf, group = Posterior)) + 
  geom_line(aes(linetype = Posterior, color = Posterior), 
            size = 1.5) + 
  scale_colour_manual(values = c("blue", "grey")) + 
  xlab(TeX("$\\hat{p}_{m_c}^{*c} - \\hat{p}_{m_t}^{*t}$")) +
  ylab('Posterior CDF') + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "grey92"),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(hjust=1, size=14),
        axis.text.y = element_text(hjust=1, size=14),
        plot.title = element_text(size=20, hjust = .5,
                                  face="bold.italic"),
        legend.position = c(0.8, 0.2)) +
  scale_y_continuous(expand = c(0, 0.01)) + 
  scale_x_continuous(expand = c(0, 0))

p
ggsave("CDF.eps", p, width = 4, height = 3, device = cairo_ps)

           


Non_inf_prob_of_success_binom_approx(m_ctrl, m_trt, 
                                     y_ctrl, y_trt, 
                                     n_ctrl, n_trt, 
                                     alpha, MID,
                                     outcome_is_good)


Non_inf_prob_of_success_binom_approx_seamless(m_ctrl, 
                                              m_trt, 
                                              y_ctrl, 
                                              y_trt, 
                                              n_ctrl, 
                                              n_trt, 
                                              alpha, 
                                              MID,
                                              outcome_is_good)


p = min_samp_size_p_success_plot_binomial(y_ctrl, 
                                          y_trt, 
                                          n_ctrl, 
                                          n_trt, 
                                          alpha, 
                                          MID,
                                          P_target,
                                          outcome_is_good)
p
ggsave("min_samp_size_p_success_plot_binomial.eps", p, 
       width = 6, height = 4)


p = min_samp_size_p_success_seamless_plot_binomial(y_ctrl, 
                                                   y_trt, 
                                                   n_ctrl, 
                                                   n_trt, 
                                                   alpha, 
                                                   MID,
                                                   P_target,
                                                   outcome_is_good)
p
ggsave("min_samp_size_p_success_seamless_plot_binomial.eps", p, 
       width = 6, height = 4)






y_trt = 5
y_ctrl = 50
MID = 0
outcome_is_good = F
m_trt = 3
m_ctrl = 3
N = 1e5

Non_inf_prob_of_success_binom_approx(m_ctrl, m_trt, 
                                     y_ctrl, y_trt, 
                                     n_ctrl, n_trt, 
                                     alpha, MID,
                                     outcome_is_good)

Non_inf_prob_of_success_binom_sim(N, m_ctrl, m_trt, 
                                  y_ctrl, y_trt, 
                                  n_ctrl, n_trt, 
                                  alpha, MID,
                                  outcome_is_good)



samp_size_vec = c(50, 50, 50)
n_cases_vec = c(10, 6, 14)
N_MC = 1e5
outcome_is_good = F
power = .5

samp = beta_posterior_samp(samp_size_vec, n_cases_vec, N_MC)


df = beta_posterior_density_df(samp_size_vec, n_cases_vec)
posterior_dens_plot(df, "Dichotomous")

beta_CrI_plot(samp_size_vec, n_cases_vec)
  
post_sup = posterior_superiority(samp, outcome_is_good)
prob_of_superiority_table(post_sup, power)


post_superiority_barplot_new(post_sup, power)




samp_size_vec = c(50, 50, 50)
ybar_vec = c(1, 1.2, 1.4)
sd_vec = c(2, 2, 2)
N_MC = 1e5
outcome_is_good = T
samp = normal_posterior_samp(samp_size_vec, ybar_vec, sd_vec, N_MC)


post_sup = posterior_superiority(samp, outcome_is_good)
post_superiority_barplot(post_sup)


df = normal_posterior_density_df(samp_size_vec, ybar_vec, sd_vec)
posterior_dens_plot(df, "Numeric")

normal_CrI_plot(samp_size_vec, ybar_vec, sd_vec)









CER = .125
RRR_vec = c(0, .15, .3, .45)
n_ctrl = 250
n_trt_vec = c(250, 250, 250, 250)
m_ctrl = 750
m_trt_vec = c(750, 750, 750, 750)
MID = 0
outcome_is_good = F
alpha = .05
cuts = seq(0, 1, length = 200)
cutoff = .7
n_trts_to_select = 2

data = PPOS_binomial_selection_probs_plot_data(cuts, CER, RRR_vec,
                                               n_ctrl, n_trt_vec, 
                                               m_ctrl, m_trt_vec,
                                               alpha, MID, 
                                               outcome_is_good,
                                               cutoff, 
                                               is_seamless = T)


probs = PPOS_selection_binomial_seamless(CER, RRR_vec, 
                                         n_ctrl, n_trt_vec, 
                                         m_ctrl, m_trt_vec,
                                         alpha, MID, 
                                         outcome_is_good, 
                                         cutoff)
assurance = data$Assurance
ppos_selection_probs_table(probs, assurance)


df = Selectivity_df(data, n_trts_to_select)
Dp = Selectivity_plot(df)
p = Dp$p
opt_cut = Dp$opt_cut
ggsave("Selectivity_plot_binomial_seamless.eps", p, width = 5, height = 3)


data = PPOS_binomial_selection_probs_plot_data(cuts, CER, RRR_vec,
                                               n_ctrl, n_trt_vec, 
                                               m_ctrl, m_trt_vec,
                                               alpha, MID, 
                                               outcome_is_good,
                                               opt_cut, 
                                               is_seamless = T)
p = PPOS_selection_probs_plot(data)
p
ggsave("PPOS_selection_binomial_seamless.eps", 
       p, width = 5.5, height = 3.5, device="eps")






data = PPOS_binomial_selection_probs_plot_data(cuts, CER, RRR_vec,
                                               n_ctrl, n_trt_vec, 
                                               m_ctrl, m_trt_vec,
                                               alpha, MID, 
                                               outcome_is_good,
                                               cutoff, 
                                               is_seamless = F)
probs = PPOS_selection_binomial(CER, RRR_vec, n_ctrl, n_trt_vec, 
                                m_ctrl, m_trt_vec, alpha, MID, 
                                outcome_is_good, cutoff)

df = Selectivity_df(data, n_trts_to_select)
Dp = Selectivity_plot(df)
p = Dp$p
opt_cut = Dp$opt_cut
ggsave("Selectivity_plot_binomial.eps", p, width = 5, height = 3)

assurance = data$Assurance
ppos_selection_probs_table(probs, assurance)



data = PPOS_binomial_selection_probs_plot_data(cuts, CER, RRR_vec,
                                               n_ctrl, n_trt_vec, 
                                               m_ctrl, m_trt_vec,
                                               alpha, MID, 
                                               outcome_is_good,
                                               opt_cut, 
                                               is_seamless = F)
probs = PPOS_selection_binomial(CER, RRR_vec, n_ctrl, n_trt_vec, 
                                m_ctrl, m_trt_vec, alpha, MID, 
                                outcome_is_good, opt_cut)
p = PPOS_selection_probs_plot(data)
p
ggsave("PPOS_selection_binomial.eps", p, width = 5.5, height = 3.5)



df = Power_density_binomial_data(cuts, CER, RRR_vec,
                                 n_ctrl, n_trt_vec, 
                                 m_ctrl, m_trt_vec,
                                 alpha, MID, 
                                 outcome_is_good,
                                 cutoff, is_seamless = T)

p = Power_density_plot(df)
p
ggsave("Power_density_binomial_seamless.eps", 
       p, width = 5.5, height = 3.5, device = "eps")


df = Power_density_binomial_data(cuts, CER, RRR_vec,
                                 n_ctrl, n_trt_vec, 
                                 m_ctrl, m_trt_vec,
                                 alpha, MID, 
                                 outcome_is_good,
                                 cutoff, is_seamless = F)

p = Power_density_plot(df)
p
ggsave("Power_density_binomial.eps", p, width = 5.5, height = 3.5)



ctrl_mean = 1
trt_mean_vec = c(1, 1.1, 1.2, 1.3)
sigma_ctrl = 1.5
sigma_trt_vec = c(1.5, 1.5, 1.5, 1.5)
n_ctrl = 100
n_trt_vec = c(100, 100, 100, 100)
m_ctrl = 500
m_trt_vec = c(500, 500, 500, 500)
MID = 0
outcome_is_good = T
alpha = .05
cuts = seq(0, 1, length = 1000)
cutoff = .75
is_seamless = T



data = PPOS_continuous_selection_probs_plot_data(cuts, ctrl_mean, trt_mean_vec, 
                                                 sigma_ctrl, sigma_trt_vec,
                                                 n_ctrl, n_trt_vec, m_ctrl, m_trt_vec,
                                                 alpha, MID, outcome_is_good,
                                                 cutoff, is_seamless = T)
p = PPOS_selection_probs_plot(data)
p


data = PPOS_continuous_selection_probs_plot_data(cuts, ctrl_mean, trt_mean_vec, 
                                                 sigma_ctrl, sigma_trt_vec,
                                                 n_ctrl, n_trt_vec, m_ctrl, m_trt_vec,
                                                 alpha, MID, outcome_is_good,
                                                 cutoff, is_seamless = F)
PPOS_selection_probs_plot(data)


df = Power_density_normal_data(cuts, ctrl_mean, trt_mean_vec, 
                               sigma_ctrl, sigma_trt_vec,
                               n_ctrl, n_trt_vec, 
                               m_ctrl, m_trt_vec,
                               alpha, MID, outcome_is_good,
                               cutoff, is_seamless = F)
Power_density_plot(df)






CER = .125
RRR_vec = c(0, .15, .3, .45)
n_ctrl = 250
n_trt_vec = c(250, 250, 250, 250)
m_ctrl = 750
m_trt_vec = c(750, 750, 750, 750)
MID = 0
outcome_is_good = F
alpha = .05
cuts = seq(0, 1, length = 200)
cutoff = .7
df = data.frame(Cutoff = numeric(),
                Selectivity = numeric(),
                BPP = character(),
                N_selected = character(),
                opt_cut = numeric(),
                M = numeric(),
                tit = character())

for(N_selected in 1:3){
  n_trts_to_select = N_selected
  for(BPP in c("Within-trial", "Cross-trial")){
    is_seamless = BPP == "Within-trial"
    data = PPOS_binomial_selection_probs_plot_data(cuts, CER, RRR_vec,
                                                   n_ctrl, n_trt_vec, 
                                                   m_ctrl, m_trt_vec,
                                                   alpha, MID, 
                                                   outcome_is_good,
                                                   cutoff, 
                                                   is_seamless)
    
    temp = Selectivity_df(data, n_trts_to_select)
    
    cut_opt_ind = which.max(temp$Selectivity)
    M = max(temp$Selectivity)
    opt_cut = temp$Cutoff[cut_opt_ind]
    tit = paste0("Optimal cutoff: ", sprintf("%.2f", opt_cut))
    
    temp$BPP = BPP
    temp$N_selected = paste0("Top ", N_selected, " Selected")
    temp$opt_cut = opt_cut
    temp$M = M
    temp$tit = tit
    
    df = rbind(df, temp)
  }
}

temp = data.frame(x = .25, y = 0.39,
                  BPP = rep(c("Within-trial", "Cross-trial"), 3), 
                  N_selected = rep(c("Top 1 Selected", "Top 2 Selected", "Top 3 Selected"), each = 2),
                  title = unique(df$tit))

temp2 = data.frame(BPP = rep(c("Within-trial", "Cross-trial"), 3), 
                   N_selected = rep(c("Top 1 Selected", "Top 2 Selected", "Top 3 Selected"), each = 2),
                   opt_cut = unique(df$opt_cut), M = unique(df$M))

p = ggplot(df, aes(Cutoff, Selectivity)) + 
  geom_line(size = 1, col = "darkgreen") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "grey92"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.text = element_text(size=8, face = "bold.italic"),
        plot.title = element_text(size=16, hjust = .5,
                                  face="bold.italic")
  ) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) + 
  scale_x_continuous(expand = c(0, 0.02)) + 
  facet_grid(N_selected~BPP) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) + 
  theme(panel.spacing = unit(1, "lines")) 

p = p + geom_text(
  data    = temp,
  mapping = aes(x, y, label = title, 
                group = NULL, fontface=1),
  size = 2.75
) + 
  geom_segment(data = temp2,
               mapping = aes(x = opt_cut, xend = opt_cut, 
                             y = -Inf, yend =  M),
               linetype = 'dashed', col = 'blue') + 
  geom_point(data = temp2,
             aes(x = opt_cut, y = M),
             shape=21, size=2, fill="white",
             colour = "dodgerblue4", stroke = 2)


ggsave("All_Selectivities.pdf", p, height = 5, width = 5)









yBar_trt = 2
yBar_ctrl = 1
n_trt = 50
n_ctrl = 50
m_trt = 100
m_ctrl = 100
sigma_trt = 5
sigma_ctrl = 5
MID = 0
outcome_is_good = T




df_list = predictive_df_continuous(sigma_trt, sigma_ctrl,  
                                   n_trt, n_ctrl, 
                                   m_trt, m_ctrl, 
                                   yBar_trt, yBar_ctrl,
                                   outcome_is_good)


predictive_density_plot(df_list, MID, outcome_is_good)



df_list = predictive_df_continuous_seamless(sigma_trt, sigma_ctrl,  
                                            n_trt, n_ctrl, 
                                            m_trt, m_ctrl, 
                                            yBar_trt, yBar_ctrl,
                                            outcome_is_good)


predictive_density_plot(df_list, MID, outcome_is_good)




n_trt = 100
n_ctrl = 100
y_trt = 5
y_ctrl = 10                                                 
m_trt = 500
m_ctrl = 500
MID = 0
outcome_is_good = F

df_list = predictive_df_binomial(m_ctrl, m_trt, 
                                 y_ctrl, y_trt, 
                                 n_ctrl, n_trt, 
                                 MID, outcome_is_good)

predictive_density_plot(df_list, MID, outcome_is_good)
