#####################################################
# Title: Forest trough Trees Recovering portfolio
#
# Author: Bernardo Scarpelli
#
# Last updated : 25/04/2023
#################################################
rm(list = ls())
library(tidyverse)
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

final_df <- readRDS("C:/Users/berna/OneDrive/Área de Trabalho/AA EESP/Dissertação/R_codes/Proj_port_selec/Nom_port/input_data/final_df.rds")


# Begining date of test
ex_ret_df <- readRDS("C:/Users/berna/OneDrive/Área de Trabalho/AA EESP/Dissertação/R_codes/Proj_port_selec/Nom_port/input_data/retuns_df_full.rds") %>% 
  as.data.frame()

# Splitting the sample into train and tune

ex_ret_df$Date[136 + 68] -> end_tune



# Removing unnecessary columns
final_df %>% 
  select(-c(CDI,Price,INEG)) -> df


# Date_vec

df %>% 
  filter(Date>end_tune) -> df

df$Date %>% 
  unique() -> date_vec


# Extracting the risk free
final_df %>% 
  select(c(Date,CDI)) %>% 
  unique() %>% 
  filter(Date>end_tune)-> aux_rf


rf <- aux_rf$CDI


# Creating tree portfolios of depth 2


colnames(df)[!(colnames(df)%in% c("Date","Stock","ret"))] -> vars

plan(multisession, workers = 4)

tic()
date_vec %>% 
  future_map((~trees(df,vars,1,.x, id = T))) -> id_list_1
toc()

names(id_list_1) <- date_vec

  

id_list_1 %>% 
  saveRDS(file = file.path(getwd(),"Nom_port","id_list_1.rds"))

plan(NULL)
# Creating tree portfolio of depth 1



plan(multisession, workers = 4)
tic()
date_vec %>% 
  future_map(~trees(df,vars,2,.x,id = T)) -> id_list_2
toc()
plan(NULL)


id_list_2 %>% 
  saveRDS(file = file.path(getwd(),"Nom_port","id_list_2.rds"))


# Now i will merge both lists into one


full_id_list <- list()


for(i in c(1:length(date_vec))){
  
  
  
  full_id_list[[i]] <- list(id_list_1[[i]],id_list_2[[i]]) %>% 
    unlist(recursive = F)
  
  
}



w_os_poet[1: (length(w_os_poet)-1)] %>% 
  saveRDS(file = file.path(getwd(), "Nom_port","w_os_poet.rds"))


weight_asset <- function(df_list,w){
  
  for(i in c(1: length(df_list))){
    
    df_list[[i]] %>% 
      mutate(weight = w[[i]]*weight) -> df_list[[i]]
    
  }
  
  df_list %>% 
    reduce(function(x,y) full_join(x,y, by = c("Stock", "ret"))) -> aux_l
  
  
  # Getting the weights vector for each asset
  
  aux_l %>% 
    dplyr::select(-c("Stock","ret")) %>% 
    rowSums(na.rm = T) -> w
  
  data.frame(Stock = aux_l$Stock,weight = w, ret = aux_l$ret) %>% 
    return()

  
  
}


full_id_list %>% 
  map(~weight_asset(.x,w_os_poet)) -> ret_w_vec


ret_v <- c()


for(i in c(1:length(ret_w_vec))){
  
  ret_w_vec[[i]]$weight -> w
  
  
  w <- (w/sum(w))
  
  
  (w*ret_w_vec[[i]]$ret) %>% 
    sum() -> ret_v[i]
}


(ret_v - c)%>% 
  cumsum() %>%
  plot(type = "l")


(ret_v-c) %>% 
  cumsum() %>% 
  tail()


ret_w_vec[[1]]





















