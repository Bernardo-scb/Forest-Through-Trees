################################################################################
# Title: Building Characteristics, cleaning Data and building input base Data frame
#
# Author: Bernardo Scarpelli
#
# Last updated : 25/04/2023
##############################################################################




rm(list = ls())

library(tidyverse)
library(lubridate)
library(janitor)
library(tsibble)
library(readr)
library(rugarch)

### Reading data

Forest <- read_csv("Dados/full_char/Input_data/Forest.csv")



Forest %>% 
  as.data.frame(Forest) -> forest_df


colnames(forest_df) <- c("Stock", "Date","Size","Price","Value")

forest_df %>% 
  select(Date, everything()) -> forest_df

### Cleaning the name

ex <- "<XBSP>"

forest_df %>% 
  mutate(Stock = str_remove(Stock,ex)) -> forest_df


## Converting everything to the right data type

# fixing the date

forest_df %>% 
  mutate(Date = str_replace_all(Date, "Jan", "01")) %>% 
  mutate(Date = str_replace_all(Date, "Mar", "03"))  %>% 
  mutate(Date = str_replace_all(Date, "Jun", "06")) %>% 
  mutate(Date = str_replace_all(Date, "Jul", "07")) %>%
  mutate(Date = str_replace_all(Date, "Nov", "11")) %>% 
  mutate(Date = str_replace_all(Date, "Fev", "02")) %>%
  mutate(Date = str_replace_all(Date, "Abr", "04")) %>%
  mutate(Date = str_replace_all(Date, "Mai", "05")) %>%
  mutate(Date = str_replace_all(Date, "Ago", "08")) %>%
  mutate(Date = str_replace_all(Date, "Set", "09")) %>%
  mutate(Date = str_replace_all(Date, "Out", "10")) %>%
  mutate(Date = str_replace_all(Date, "Dez", "12")) %>% 
  mutate(Date = paste("1-", Date,sep = "")) %>% 
  mutate(Date = as.Date(Date,format = "%d-%m-%Y"))%>% 
  mutate(Date = yearmonth(Date))-> forest_df

# working with the price and characteristics

forest_df %>% 
  mutate(Size = as.numeric(Size)) %>% 
  mutate(Price = as.numeric(Price)) %>% 
  mutate(Value = as.numeric(Value)) -> forest_df


forest_df %>% 
  rename(BME = Value) -> forest_df

## lagging the characteristics


forest_df %>% 
  group_by(Stock) %>% 
  mutate(Size = dplyr::lag(Size)) %>%
  mutate(BME = dplyr::lag(BME)) %>% 
  ungroup() -> forest_df


## Creating momentum columns

forest_df %>% 
  group_by(Stock) %>% 
  mutate(Momentum = 100*(log(Price) - dplyr::lag(log(Price),12))) %>% 
  mutate(Momentum = dplyr::lag(Momentum)) %>% 
  mutate(STR = 100*((log(Price) - dplyr::lag(log(Price))))) %>% 
  mutate(STR = lag(STR)) %>% 
  mutate(LTR = 100*((log(Price) - dplyr::lag(log(Price),36)))) %>% 
  mutate(LTR = lag(LTR)) %>% 
  ungroup() -> forest_df



## calculating the return

forest_df %>% 
  group_by(Stock) %>% 
  mutate(ret = 100*(log(Price) - dplyr::lag(log(Price)))) %>% 
  ungroup()-> forest_df


## Getting rid of the first six months were no momentum information will be available
## For any of the stocks

forest_df %>% 
  group_by(Stock) %>% 
  filter(row_number()>36) %>% 
  ungroup() -> forest_df

########################################################
# Remaining Characteristics
######################################################



char_df <- read_csv("Dados/full_char/Input_data/char_df.csv")



char_df %>% 
  rename(Date = Data) %>% 
  rename(Beta = `Beta|60 meses|Em moeda orig`) %>% 
  rename(INEG = `Negociabilidade|1 meses|Em moeda orig` ) %>% 
  rename(VME = `Media|nulos = 0|do volume$|em 1 meses|Em moeda orig|em milhares`) %>% 
  rename(Stock = Ativo)-> char_df

### Function to create the expected volatility

exp_vol <- function(ret_vec){
  
  (!is.na(ret_vec)) %>% 
    sum() -> b
  
  if(b<10){
    rep(NA,length(ret_vec)) %>% 
      return()
  }else{
    
    # Identifying the first and last non NA values
    which(!is.na(ret_vec)) %>% 
      head(1) -> start_index
    
    which(!is.na(ret_vec)) %>% 
      tail(1) -> end_index
    
    # Replacing NA by the mean
    
    ret_vec[start_index:end_index] -> ret_garch
    
    ret_garch[is.na(ret_garch)] <- mean(ret_garch,na.rm = T)
    
    
    # Create GARCH specification
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(0, 0)))  
    
    
    vol <- rep(NA,length(ret_garch))
    
    
    for(i in c(5:(length(ret_garch) - 1))){
      
      fit <- ugarchfit(spec, data = ret_garch[1:i],solver = "hybrid")
      
      bol <- is.na(sigma(fit)[1])[1,1]
      
      if(bol == TRUE){
        break
      }else{
        vol[i+1] <- sigma(ugarchforecast(fit,n.ahead = 1))
        print(i)
        
      }
      
      
    }
    
    
    c(rep(NA,(start_index - 1)),vol,rep(NA,(length(ret_vec) - end_index))) %>% 
      return()
    
  }
}






forest_df %>% 
  group_by(Stock) %>% 
  mutate(Vol = exp_vol(ret)) %>% 
  ungroup()-> forest_df


forest_df %>% 
  group_by(Stock) %>% 
  filter(row_number()>5) %>% 
  ungroup() -> forest_df


###### Writing the complete data frame


char_df %>% 
  mutate(across(.cols = !any_of(c("Date","Stock")),.fns = ~as.numeric(.))) %>% 
  mutate(Stock = str_remove(Stock, ex))-> char_df



char_df %>% 
  mutate(Date = str_replace_all(Date, "Jan", "01")) %>% 
  mutate(Date = str_replace_all(Date, "Mar", "03"))  %>% 
  mutate(Date = str_replace_all(Date, "Jun", "06")) %>% 
  mutate(Date = str_replace_all(Date, "Jul", "07")) %>%
  mutate(Date = str_replace_all(Date, "Nov", "11")) %>% 
  mutate(Date = str_replace_all(Date, "Fev", "02")) %>%
  mutate(Date = str_replace_all(Date, "Abr", "04")) %>%
  mutate(Date = str_replace_all(Date, "Mai", "05")) %>%
  mutate(Date = str_replace_all(Date, "Ago", "08")) %>%
  mutate(Date = str_replace_all(Date, "Set", "09")) %>%
  mutate(Date = str_replace_all(Date, "Out", "10")) %>%
  mutate(Date = str_replace_all(Date, "Dez", "12")) %>% 
  mutate(Date = paste("1-", Date,sep = "")) %>% 
  mutate(Date = as.Date(Date,format = "%d-%m-%Y")) %>% 
  mutate(Date = yearmonth(Date)) %>% 
  arrange(by = Date)-> char_df



char_df %>% 
  group_by(Stock) %>% 
  mutate(across(.cols = !any_of(c("Date","Stock")),.fns = ~dplyr::lag(.))) %>% 
  ungroup()-> char_df



full_df <- char_df %>% 
  right_join(forest_df, by = c("Date","Stock"))


  

### Working with the risk free asset

cdi <- read_csv("Data/full_char/Input_data/CDI.csv")



cdi %>% 
  as.data.frame() %>% 
  rename(CDI = `Fechamento|ajust p/ prov|Em moeda orig` ) %>% 
  rename(Date = Data) %>% 
  select(c(Date, CDI)) %>%
  mutate(Date = str_replace_all(Date, "Jan", "01")) %>% 
  mutate(Date = str_replace_all(Date, "Mar", "03"))  %>% 
  mutate(Date = str_replace_all(Date, "Jun", "06")) %>% 
  mutate(Date = str_replace_all(Date, "Jul", "07")) %>%
  mutate(Date = str_replace_all(Date, "Nov", "11")) %>% 
  mutate(Date = str_replace_all(Date, "Fev", "02")) %>%
  mutate(Date = str_replace_all(Date, "Abr", "04")) %>%
  mutate(Date = str_replace_all(Date, "Mai", "05")) %>%
  mutate(Date = str_replace_all(Date, "Ago", "08")) %>%
  mutate(Date = str_replace_all(Date, "Set", "09")) %>%
  mutate(Date = str_replace_all(Date, "Out", "10")) %>%
  mutate(Date = str_replace_all(Date, "Dez", "12")) %>% 
  mutate(Date = paste("1-", Date,sep = "")) %>% 
  mutate(Date = as.Date(Date,format = "%d-%m-%Y")) %>% 
  mutate(Date = yearmonth(Date)) %>% 
  arrange(by = Date) -> cdi_df


cdi_df %>%
  mutate(CDI = 100*(log(CDI) - log(dplyr::lag(CDI)))) %>% 
  na.omit() %>% 
  right_join(full_df, by = "Date") -> final_df





saveRDS(final_df, file = file.path(getwd(),"full_char","Data_output","final_df.rds"))

















