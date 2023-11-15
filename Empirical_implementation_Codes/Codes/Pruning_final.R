########################################
# Title: Forest trough Trees pruning
#
# Author: Bernardo Scarpelli
#
# Last updated : 25/04/2023
######################################

# Packages
library(tidyverse)
library(xts)
library(furrr)
library(tictoc)
library(glmnet)
library(tsibble)
library(lubridate)
library(Matrix)
library(doSNOW)
library(optimization)
library(POET)
library(parallel)
library(data.table)
library(lmtest)
library(sandwich)
####################################################################
# Three char
###################################################################

ex_ret_df <- readRDS("C:/Users/berna/OneDrive/Área de Trabalho/AA EESP/Dissertação/R_codes/Proj_port_selec/Empirical_implementation_Codes/input_data/retuns_df_full.rds") %>%  
  as.data.frame()



# Splitting the sample into train and tune

ex_ret_df$Date[136] -> end_sam

ex_ret_df$Date[136 + 68] -> end_tune


# train sample

ex_ret_df %>% 
  filter(Date<= end_sam) %>% 
  mutate_at(vars(contains("D1")), ~.*(1/sqrt(2))) %>% 
  mutate_at(vars(contains("D2")),~.*(1/2)) %>% 
  mutate_at(vars(contains("D3")), ~.*(1/sqrt(2^3))) %>%  
  dplyr::select(-Date) -> train

# Tune

ex_ret_df %>% 
  filter(Date>end_sam) %>% 
  filter(Date<=end_tune) -> tune


# sample mean

train %>% 
  summarise_all(mean) %>% 
  unlist() -> mu_vec

# sample covariance

train %>% 
  as.matrix() %>% 
  cov() -> cov_mat_samp


## Determining the number of factors

train %>% 
  as.matrix() %>% 
  getnfac(jj = 6) -> factors_list

nfact <- factors_list$ic[1,1]
# Covariance with the POET Estimator


train %>% 
  mutate_all(.funs = function(x) x - mean(x)) %>% 
  as.matrix() %>% 
  t() %>% 
  POET(K = nfact, C = 1) -> aux


cov_mat_poet <- aux$SigmaY

## Simulated annealing for the sample covariance matrix

simul_anel(c(150,0,0.0004),cov_mat_samp,mu_vec,2000,10000,100,tune) -> best_samp


best_samp[[1]] %>% 
  plot(type = "l")



lambda_samp <- best_samp[[2]]

# Simulated annealing for the poet cov mat

simul_anel(c(150,0,0.0004),cov_mat_poet,mu_vec,200,10000,400,tune) -> best_poet


best_poet[[1]] %>% 
  plot(type = "l")
                        

lambda_poet <- best_poet[[2]]




# updating the covariance matrix and evaluating out of sample performance
# Out of sample performance

# test df

ex_ret_df %>% 
  filter(Date>end_tune) %>% 
  dplyr::select(-Date)-> test


# cov mat update


ex_ret_df %>% 
  filter(Date<=end_tune) %>% 
  dplyr::select(-Date) %>% 
  mutate_at(vars(contains("D1")), ~.*(1/sqrt(2))) %>% 
  mutate_at(vars(contains("D2")),~.*(1/2)) %>% 
  mutate_at(vars(contains("D3")), ~.*(1/sqrt(2^3))) %>% 
  cov() -> cov_mat_os_samp

## Determining the number of factors

ex_ret_df %>% 
  filter(Date<=end_tune) %>% 
  dplyr::select(-Date) %>% 
  mutate_at(vars(contains("D1")), ~.*(1/sqrt(2))) %>% 
  mutate_at(vars(contains("D2")),~.*(1/2)) %>% 
  mutate_at(vars(contains("D3")), ~.*(1/sqrt(2^3))) %>% 
  as.matrix() %>% 
  getnfac(jj = 6) -> factors_list

nfact <- factors_list$ic[1,1]


#

ex_ret_df %>% 
  filter(Date<=end_tune) %>% 
  dplyr::select(-Date) %>%
  mutate_at(vars(contains("D1")), ~.*(1/sqrt(2))) %>% 
  mutate_at(vars(contains("D2")),~.*(1/2)) %>% 
  mutate_at(vars(contains("D3")), ~.*(1/sqrt(2^3))) %>% 
  mutate_all(.funs = function(x)x - mean(x)) %>% 
  t() %>% 
  POET(K = (nfact), C = 1) -> aux_2

cov_mat_os_poet <- aux_2$SigmaY


# mu_vec update



ex_ret_df %>% 
  filter(Date<=end_tune) %>%
  dplyr::select(-Date) %>% 
  mutate_at(vars(contains("D1")), ~.*(1/sqrt(2))) %>% 
  mutate_at(vars(contains("D2")),~.*(1/2)) %>% 
  mutate_at(vars(contains("D3")), ~.*(1/sqrt(2^3))) %>%
  summarise_all(mean) %>% 
  unlist() -> mu_vec_os




## Portfolio returns

# sample estimator
robust_mv(mu_vec_os,cov_mat_os_samp,lambda_poet) %>% 
  as.matrix()-> w_os_samp



as.matrix(test)%*%w_os_samp -> ret_os_samp

ret_os_samp %>% 
  cumsum() %>% 
  plot(type = "l")

# POET estimator

robust_mv(mu_vec_os,cov_mat_os_poet,lambda_poet) %>% 
  as.matrix()-> w_os_poet


as.matrix(test)%*%w_os_poet -> ret_os_poet

ret_os_poet %>% 
  cumsum() %>% 
  plot(type = "l")


#####



mean(ret_os_poet)/sd(ret_os_poet)

mean(ret_os_samp)/sd(ret_os_samp)



ex_ret_df %>% 
  filter(Date > end_tune) -> aux_date


data.frame(APT = ret_os_samp, APT_POET = ret_os_poet, Date = aux_date$Date) -> report_df


# market index excess return

ind_df <- read_csv("Empirical_implementation_Codes/input_data/ind_df.csv")

ind_df %>% 
  as.data.frame() %>% 
  rename(CDI = `CDI Acumulado Fechamento`) %>% 
  rename(IBOV = `IBOV Fechamento`) %>% 
  dplyr::select(c(Date,IBOV,CDI)) %>% 
  mutate_at(vars(-contains("Date")),~100*(log(.) - lag(log(.)))) %>% 
  na.omit() %>% 
  mutate(IBOV = IBOV - CDI) %>% 
  dplyr::select(c(Date,IBOV)) %>% 
  mutate(Date = yearmonth(Date)) -> ind_df

##

report_df %>% 
  mutate(Date = yearmonth(Date)) -> report_df

report_df %>% 
  inner_join(ind_df, by = "Date") -> report_df

## saving the Data frame

report_df %>% 
  saveRDS(file = file.path(getwd(),"full_char","p_vs_samp.rds"))

(w_os_samp == 0) %>% 
  sum()


(w_os_poet == 0) %>% 
  sum()


## Plotting the comparison between them


gg <- ggplot(data = report_df, aes(x = Date)) +
  theme_bw(base_size = 10) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Date") + ylab("Excess Return") +
  geom_line(
    aes(y = cumsum(APT), color = "APT"),
    size = 1
  ) +
  geom_line(
    aes(y = cumsum(APT_POET), color = "POET-APT"),
    size = 1
  ) +
  geom_line(
    aes(y = cumsum(IBOV), color = "IBOV"),
    size = 1
  ) +
  
  scale_colour_manual(values = c(
    "blue", "red", "black"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cumulative Excess Return")









report_df %>% 
  select(-Date) %>% 
  mutate(APT = APT/sd(report_df$APT)) %>% 
  mutate(APT_POET = APT_POET/sd(report_df$APT_POET)) %>% 
  mutate(IBOV = IBOV/sd(report_df$IBOV)) %>% 
  summarise_all(mean) %>% 
  unlist()-> sharpe


matrix(ncol = 3,nrow = 2) -> result_mat

result_mat[1,] <- sharpe

result_mat[2,] <- c(2,85,"-")

colnames(result_mat) <- c("APT", "POET-APT","IBOV")


rownames(result_mat) <- c("Sharpe","Zero Portfolios")



result_mat %>% 
  stargazer::stargazer()



### Regressions


lm(APT_POET~APT, data = report_df) %>% 
  coeftest(vcov. = sandwich) -> reg_1


lm(APT~APT_POET, data = report_df) %>% 
  lmtest::coeftest() -> reg_2


# Alpha mat

alpha <- c(reg_1[1,1], reg_2[1,1])
p_val <- c(reg_1[1,4], reg_2[1,4])



ret_mat <- matrix(nrow = 2, ncol = 2)

ret_mat[1,] <- alpha

ret_mat[2,] <- p_val

colnames(ret_mat) <- c("POET-APT ~ APT","APT ~ POET-APT")

rownames(ret_mat) <- c("Alpha","P-value")

ret_mat %>% 
  stargazer::stargazer()












































