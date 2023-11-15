##############################################################
# Title: Forest trough Trees Time varying Covariance Matrix
#
# Author: Bernardo Scarpelli
#
# Last updated : 25/04/2023
#############################################################

# Packages:

library(tidyverse)
library(janitor)
library(rlang)
library(expm)
library(tsibble)
library(fBasics)
library(MTS)
library(rugarch)
library(rmgarch)
library(data.table)
library(xts)
library(furrr)
library(tictoc)
library(glmnet)
library(tsibble)
library(lubridate)
library(Matrix)
library(doSNOW)
library(optimization)
library(parallel)
library(data.table)



####################################################################
# Three char
###################################################################

ex_ret_df <- readRDS("C:/Users/berna/OneDrive/Área de Trabalho/AA EESP/Dissertação/R_codes/Proj_port_selec/Empirical_implementation_Codes/input_data/retuns_df_full.rds") %>% 
  as.data.frame()

# Splitting the sample into train and tune

ex_ret_df$Date[136] -> end_sam

ex_ret_df$Date[136 + 68] -> end_tune



ex_ret_df %>% 
  mutate_at(vars(contains("D1")), ~.*(1/sqrt(2))) %>% 
  mutate_at(vars(contains("D2")),~.*(1/2)) %>% 
  mutate_at(vars(contains("D3")), ~.*(1/sqrt(2^3))) -> df_cov


# Date vec

ex_ret_df %>% 
  filter(Date>end_tune) -> aux_date

date_vol <- aux_date$Date

# List_of covariance matrices:

plan(multisession, workers = 4)
tic()
date_vol %>% 
  future_map(~tv_matrix_c(df_cov,.x,nfact)) -> cov_list
toc()
plan(NULL)



date_vol %>% 
  map(~filter(df_cov,Date<.x)) %>% 
  map(~select(.x,-Date)) %>% 
  map(~colMeans(.x)) -> mu_list


full_list <- list()
for(i in c(1:length(mu_list))){
  
  full_list[[i]] <- list(cov_list[[i]],mu_list[[i]])
}



lambda_tv <- lambda_samp


# Estimating the weights
full_list %>% 
  map(~robust_mv(.x[[2]],.x[[1]],lambda_tv)) -> w_list


w_list %>% 
  purrr::reduce(cbind) -> w_mat


(as.matrix(test)%*%w_mat) %>% 
  diag()-> ret_os_tv

ret_os_tv %>% 
  cumsum() %>% 
  plot(type = "l")


mean(ret_os_tv)/sd(ret_os_tv)

















