########################################
# Title: Building Tree portfolios
#
# Author: Bernardo Scarpelli
#
# Last updated : 25/04/2023
######################################


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

final_df <- readRDS("C:/Users/berna/OneDrive/Área de Trabalho/AA EESP/Dissertação/R_codes/Proj_port_selec/Data/Tree_portfolios/Input_data/final_df.rds")
# Removing unnecessary columns
final_df %>% 
  select(-c(CDI,Price,INEG)) -> df


# Date_vec

df %>% 
  filter(year(Date)>1994) -> df

df$Date %>% 
  unique() -> date_vec


date_vec[1] -> date
# Market excess return

plan(multisession, workers = 4)

date_vec %>% 
  future_map(~market_p(df,.x)) %>% 
  purrr::reduce(rbind) -> market_ret

plan(NULL)

# Extracting the risk free
final_df %>% 
  select(c(Date,CDI)) %>% 
  unique() %>% 
  filter(year(Date)>1994)-> aux_rf


rf <- aux_rf$CDI


# Creating tree portfolios of depth 2


colnames(df)[!(colnames(df)%in% c("Date","Stock","ret"))] -> vars

plan(multisession, workers = 4)
date_vec %>% 
  future_map(~unlist(trees(df,vars,2,.x))) %>% 
  purrr::reduce(rbind) -> data_leafs_2

data_leafs_2 %>% 
  saveRDS(file = file.path(getwd(),"full_char","fchar2_df.rds"))

plan(NULL)
# Creating tree portfolio of depth 1



plan(multisession, workers = 4)
tic()
date_vec %>% 
  future_map(~unlist(trees(df,vars,1,.x))) %>% 
  purrr::reduce(rbind)-> test
toc()

test %>% 
  saveRDS(file = file.path(getwd(),"full_char","fchar1_df.rds"))

plan(NULL)


## Creating the excess returns and the full data frame


fchar2_df <- readRDS("C:/Users/berna/OneDrive/Área de Trabalho/AA EESP/Dissertação/R_codes/Proj_port_selec/full_char/fchar2_df.rds")
fchar1_df <- readRDS("C:/Users/berna/OneDrive/Área de Trabalho/AA EESP/Dissertação/R_codes/Proj_port_selec/full_char/fchar1_df.rds") 



# Creating and id for the portfolio depth


fchar2_df %>% 
  as.data.frame() %>% 
  colnames() -> name_2

name_2 <- paste("D2",name_2, sep = "")

colnames(fchar2_df) <- name_2


fchar1_df %>% 
  as.data.frame() %>% 
  colnames() -> name_1

name_1 <- paste("D1",name_1, sep = "")

colnames(fchar1_df) <- name_1

## Adding the column date 

fchar2_df %>% 
  as.data.frame() %>% 
  mutate(Date = date_vec) -> fchar2_df

fchar1_df %>% 
  as.data.frame() %>% 
  mutate(Date = date_vec) -> fchar1_df

# Merging all the data frames together

inner_join(fchar1_df,fchar2_df, by = "Date")  -> full_char_df

# Creating the excess returns vector
full_char_df %>% 
  mutate(M =  as.vector(market_ret)) %>% 
  mutate_at(vars(-contains("Date")), ~. - rf) -> return_full


return_full %>% 
  saveRDS(file = file.path(getwd(),"full_char","retuns_df_full.rds"))

return_full <- readRDS("C:/Users/berna/OneDrive/Área de Trabalho/AA EESP/Dissertação/R_codes/Proj_port_selec/full_char/retuns_df_full.rds")



## Descriptive statistics


# Number of valid stocks

final_df %>% 
  filter(year(Date)>1994) -> df_stock_aux


df_stock_aux$Date %>% 
  unique() -> date_vec


date_vec %>% 
  future_map(~filter(df_stock_aux,Date == .x)) %>% 
  future_map(~na.omit(.x)) %>% 
  map(~nrow(.x)) %>% 
  unlist() -> valid_vec



data.frame(n_stock = valid_vec,Date = date_vec) -> df_nstock




gg <- ggplot(data = df_nstock, aes(x = Date)) +
  theme_bw(base_size = 10) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Date") + ylab("") +
  geom_line(
    aes(y = n_stock, color = "Valid Stocks"),
    size = 1
  ) +
  
  scale_colour_manual(values = c(
 "black"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Number of Valid Stocks")

gg


date_vec

df_stock_aux$Stock %>% 
  unique() %>% 
  length()

## Descriptive statistics of the tree portfolios


# Depth one portfolios

ex_ret_df %>% 
  select(starts_with("D1")) -> d1_df


# Depth two portfolios

ex_ret_df %>% 
  select(starts_with("D2")) -> d2_df


# SD d1

d1_df %>% 
  summarise_all(sd) %>% 
  unlist() -> sd_d1

df_1 <- data.frame(value = sd_d1, Group = rep("D1", length(sd_d1)))


# SD d2

d2_df %>% 
  summarise_all(sd) %>% 
  unlist() -> sd_d2

df_2 <- data.frame(value = sd_d2, Group = rep("D2", length(sd_d2)))

# Creating a data frame to plot the histograms


df_hist <- rbind(df_1,df_2)


gg_hist <- ggplot(df_hist, aes(x = value, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Standard Deviation Density",
       x = "Value",
       y = "Density") +
  theme_minimal()

gg_hist




# pre pruning transformation

## D1

d1_df %>% 
  mutate_all(~.*(1/sqrt(2))) -> df_1ad


df_1ad %>% 
  summarise_all(sd) %>% 
  unlist()-> sd_ad1

df_ad1 <- data.frame(value = sd_ad1, Group = rep("D1", length(sd_ad1)) )

## D2

d2_df %>% 
  mutate_all(~.*(1/2))-> df_2ad


df_2ad %>% 
  summarise_all(sd) %>% 
  unlist()-> sd_ad2

df_ad2 <- data.frame(value = sd_ad2, Group = rep("D2", length(sd_ad2)))


df_ad <- rbind(df_ad1,df_ad2)

# Plot

gg_ad <- ggplot(df_ad, aes(x = value, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "",
       x = "Value",
       y = "Density") +
  theme_minimal()

gg_ad











  














