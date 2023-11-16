########################################
# Title: Forest through Trees Functions
#
# Author: Bernardo Scarpelli
#
# Last updated : 25/04/2023
######################################

# Packages
rm(list = ls())
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


## Split portfolio

split_p <- function(df,var){
  
  # sorting
  
  df %>% 
    arrange(!!sym(var)) %>% 
    mutate(id = row_number()) -> df_1
  
  # bellow and above median
  
  df_1 %>% 
    filter(id <= median(id)) -> aux_1
  
  df_1 %>% 
    filter(id>median(id)) -> aux_2
  
  list(aux_1, aux_2) %>% 
    return()
  
}



## Given a order of splits Create the leafs portfolios


market_p <- function(df,date){
  
  df %>% 
    filter(Date == date) %>% 
    na.omit()-> df_1
  
  df_1 %>% 
    mutate(weight =  Size/unlist(summarise_at(df_1,"Size", sum))) %>% 
    mutate(ret = weight*ret) -> df_1
  
  df_1$ret %>% 
    sum() %>% 
    return()
  
}

leafs <- function(df,vars,date,id = F){
  
  # filtering the date
  
  df %>% 
    filter(Date == date) %>% 
    na.omit()-> df_1
  
  new_df <- list(df_1)
  
  
  for(i in vars){
    
    new_df %>% 
      map(~split_p(.x,i)) %>% 
      unlist(recursive = F)-> new_df
    
  }
  
  if(id == T){
    
    new_df %>% 
      map(~mutate(.x,weight = Size/unlist(summarise_at(.x,"Size", sum)))) %>% 
      map(~dplyr::select(.x, c("Stock","weight","ret"))) %>% 
      return()
    
  }else{
  

  
  ## Values weighting
  
  new_df %>% 
    map(~mutate(.x,weight = Size/unlist(summarise_at(.x,"Size", sum)))) %>% 
    map(~mutate(.x,ret = weight*ret)) -> new_df
  
 
                                                    
  new_df %>% 
    map(~summarise_at(.x,"ret", sum)) %>%
    as.data.frame() %>% 
    purrr::reduce(cbind) %>% 
    return()
  }
  
}


## trees functions

trees <- function(df,vars, depth, date, id = F){
  
  rep(list(vars), depth) %>% 
    expand.grid() %>% 
    as.matrix() %>% 
    t() %>% 
    as.data.frame() %>% 
    as.list() -> tree_list
  
  
  if(id == T){
    tree_list %>% 
      map(~leafs(df,.x, date,id = T)) %>% 
      unlist(recursive = F) %>% 
      return()
  }else{
  
  tree_list %>% 
    map(~leafs(df,.x, date,id = F)) %>% 
    return()
  }


}






## pruning



input_prunig <- function(cov,mu,lambda){
  
  
  iota <- rep(1,length(mu))
  t_mean <- mean(mu)
  decomp <- eigen(cov)
  


  gamma <- min(sum(decomp$values > 1e-10),136)
  

  
  D <- decomp$values[1:gamma] 
  V_tilde <- decomp$vectors[,1:gamma]
  
  sig_til <- V_tilde%*%diag(sqrt(D))%*%t(V_tilde)
  
  mu_til <- V_tilde%*%diag(1/sqrt(D))%*%t(V_tilde)%*%(as.matrix(mu) + lambda[1]*t_mean*(as.matrix(iota)))
  
  x <- rbind(sig_til,diag(iota)*sqrt(lambda[2]))
  
  y <- c(mu_til,rep(0,length(mu)))
  
  list(y,x) %>% 
    return()
 
  

}







## function that estimates the weights of the optimal robust portfolio given lambda


robust_mv <- function(mu,cov,lambda){
  
  
  # First i will construct the inputs
  
  inputs <- input_prunig(cov,mu,lambda)
  
  # Calculating the weights of the portfolio
  
  weights <- glmnet(inputs[[2]],inputs[[1]], alpha = 1,lambda = lambda[3]) %>% 
    coefficients()
  
  weights[-1] -> weights
  
  (weights/sum(weights)) %>% 
    return()

}

########################################################################
# Optimization
########################################################################


## Objective function

obj_function <- function(lambda,cov,mu,df){
  
  # Optimal w given \lambda
  
  robust_mv(mu,cov,lambda) -> w
  
  # Returns
  
  df %>% 
    dplyr::select(-Date) %>% 
    as.matrix()-> ret_mat
  
  ret_mat%*%as.matrix(w) -> ret_vec
  
  
  
  (mean(ret_vec)/sd(ret_vec)) %>% 
    return()
  
}



## simulated annealing


simul_anel <- function(s_0,cov,mu,maxit,t_0,c_stop,df){
  
  # Counting variable to stop the algorithm
  count <- 0
  
  # initial lambda
  lambda <- s_0
  
  # Creating a improvement vector to see the development of the algorithm
  
  improv_vec <- c()
  
  for(i in c(1:maxit)){
  # Choosing neighborhood at random
  
  cord <- runif(1,min = 0, max = length(lambda)) %>% 
    ceiling()
  
  # mutating the string
  
  if(cord ==1){
    
  mut <- rnorm(1, sd = 1)
  
  }else{
    
  mut <- rnorm(1, sd = 0.0001)
  }
  lambda_1 <- lambda
  
  lambda_1[cord] <- (lambda[cord] + mut)
  
  # Restriction to be above 0
  
  
  lambda_1[which(lambda_1<0)] <- 0
  
  # calculating the value of the function on the strings
  
  list(lambda,lambda_1) %>% 
    map(~obj_function(.x,cov,mu,df)) %>% 
    unlist() -> c_vec
  
  c_vec <- -c_vec
  
  
  # Storing the value 
  improv_vec[i]<- -c_vec[1]
  

  
  if(any(is.na(c_vec))){
    c_vec <- c(0,1)
  }
  
  if(c_vec[2]<c_vec[1]){
    
    lambda <- lambda_1
    
    # updating the count
    count <- 0
  }else{
    
    t <- t_0/(log(i+1))
    
    p <- exp((c_vec[1]-c_vec[2])/t) 
    
    coin <- runif(1)
    
    if(coin<p){
      lambda <- lambda
      count <- count+1
    }else{
      lambda <- lambda_1
    }
      
  }
  if(count>c_stop){
    break
  }
  
  } 
  
  
  
  list(improv_vec,lambda) %>% 
    return()
  
  
}



#####################################################################################
# unconditional Sorting
#####################################################################################



unc_sort <- function(df,date){
  
  
  

  df %>% 
    filter(Date == date) %>% 
    na.omit() -> df
  
  ## Value sorted portfolios
  
  df %>% 
    split_p("Value") %>% 
    map(~split_p(.x,"Value")) %>% 
    unlist(recursive = F) -> value_port
  
  ## Size sorted
  
  df %>% 
    split_p("Size") %>% 
    map(~split_p(.x,"Size")) %>% 
    unlist(recursive = F) -> size_port
  
  ## Intersections
  
  intersec_list <- list()
  
  for(i in 1:4){
    
    size_port %>% 
      map(~dplyr::select(.x,-id)) %>% 
      map(~inner_join(.x,value_port[[i]],by = c("Stock","Date",
                                                "Value","Momentum",
                                                "ret","Size"))) -> intersec_list[[i]]
    
  }
  
  intersec_list %>% 
    unlist(recursive = F) -> intersec_list
  
  # Portfolio returns
  
  list(list(df),value_port,size_port,intersec_list) %>% 
    unlist(recursive = F) -> port_list
  
  #
  
  port_list %>% 
    map(~filter(.x,Value>0)) %>% 
    map(~mutate(.x,w = Value/unlist(summarise_at(.x,"Value",sum)))) %>% 
    map(~mutate(.x,ret = w*ret)) %>% 
    map(~summarise_at(.x,"ret", sum)) %>%
    as.data.frame() %>% 
    purrr::reduce(cbind) %>% 
    return()     
    
}



##########################################################################
# Corr function to test some of the implications
#########################################################################



corr_ts <- function(df){
  
  df$Date %>% 
    unique() -> date_vec
  
  
  
  empirical_cdf <- function(x) {
    n <- length(x)
    sorted_x <- sort(x)
    ecdf_values <- (1:n) / n
    return(ecdf_values[match(x, sorted_x)])
  }
  
  
  
  
  aux <- function(date){
    df %>% 
      filter(Date == date) %>% 
      na.omit() %>% 
      dplyr::select(-c(Date, Stock, ret)) %>% 
      mutate_all(~empirical_cdf(.)) %>% 
      as.matrix() %>% 
      cor() -> cor_mat
      
    
      c(cor_mat[2,1],cor_mat[3,1],cor_mat[3,2]) %>% 
        return()
     }

  date_vec %>% 
    future_map(~aux(.x)) %>% 
    purrr::reduce(rbind) %>% 
    as.data.frame() -> df_ret 
    
  colnames(df_ret) <-  c("s-v","s-m","v-m")
  
  df_ret %>% 
    mutate(Date = date_vec) %>% 
    return()
}


# trees modified function


trees_mod <- function(df,vars, depth, date){
  
  rep(list(vars), depth) %>% 
    expand.grid() %>% 
    as.matrix() %>% 
    t() %>% 
    as.data.frame() %>% 
    as.list() -> tree_list

  
  tree_list %>% 
    keep(~length(unique(.))==1) -> tree_list_s
  
  tree_list %>% 
    keep(~length(unique(.))!=1) -> tree_list_d
  
  tree_list_s %>% 
    map(~leafs(df,.x, date)) %>% 
    unlist() %>% 
    as.matrix() %>% 
    t()-> df_1
  
  tree_list_d %>% 
    map(~leafs(df,.x,date)) %>% 
    unlist() %>% 
    as.matrix() %>% 
    t()-> df_2
  
  cbind(df_1,df_2) %>% 
    return()
  
}


# Corr_mean

corr_mean <- function(df){
  
  df$Date %>% 
    unique() -> date_vec
  
  empirical_cdf <- function(x) {
    n <- length(x)
    sorted_x <- sort(x)
    ecdf_values <- (1:n) / n
    return(ecdf_values[match(x, sorted_x)])
  }
  
  
  aux <- function(date){
    df %>% 
      filter(Date == date) %>% 
      na.omit() %>% 
      dplyr::select(-c(Date, Stock, ret)) %>% 
      mutate_all(~empirical_cdf(.)) %>% 
      as.matrix() %>% 
      cor() %>% 
      return()
  }

  
  date_vec %>% 
    future_map(~aux(.x)) -> list_corr_mat
  
  list_corr_mat %>% 
    keep(~!any(is.na(.))) -> list_corr_mat
  
  list_corr_mat %>% 
    length() -> n
  
  list_corr_mat %>% 
    purrr::reduce(`+`) -> corr_sum
  
  (corr_sum/n) %>% 
    return()
}


# Function to estimate the number of factors (Thank you Creators of the PANICr)

minindc <- function(x) {
  
  ncols <- dim(x)[2]
  
  nrows <- dim(x)[1]
  
  pos <- matrix(0, ncols, 1)
  
  for (i in 1:ncols) {
    
    pos[i, ] <- which.min(x)
  }
  return(pos)
} 

getnfac <- function(x, kmax = 10, jj){
  
  x<-as.matrix(x)
  Tn <- dim(x)[1]
  
  N <- dim(x)[2]
  
  NT  <- N * Tn
  
  NT1 <- N + Tn
  
  CT <- matrix(0, 1, kmax)
  
  ii <- seq(1:kmax)
  
  GCT <- min(N, Tn)
  
  if (jj == 1){
    CT[1,] <- log(NT / NT1) * ii * NT1 / NT
  }
  
  if (jj == 2){
    CT[1,] <- (NT1 / NT) * log(GCT) * ii
  }
  
  if (jj == 3){
    CT[1,] <- ii * log(GCT) / GCT
  }
  
  if (jj == 4){
    CT[1,] <- 2 * ii / Tn
  }
  
  if (jj == 5){
    CT[1,] <- log(T) * ii / Tn
  }
  
  if (jj == 6){
    CT[1,] <- 2 * ii * NT1 / NT
  }
  
  if (jj == 7){
    CT[1,] <- log(NT) * ii * NT1 / NT
  }
  
  IC1   <- matrix(0, dim(CT)[1], I(kmax+1))
  
  Sigma <- matrix(0, 1, I(kmax+1))
  
  XX    <- x %*% t(x)
  
  eig   <- svd(t(XX))
  
  Fhat0 <- eig$u
  
  eigval<- as.matrix(eig$d)
  
  Fhat1 <- eig$v
  
  sumeigval <- apply(eigval, 2, cumsum) / sum(eigval)
  
  if (jj < 8){
    for ( i in kmax:1){
      
      Fhat <- Fhat0[,1:i]
      
      lambda <- crossprod(Fhat, x)
      
      chat <- Fhat %*% lambda
      
      ehat = x - chat
      
      Sigma[i] <- mean(sum(ehat * ehat / Tn))
      
      IC1[,i] <- log(Sigma[i]) + CT[,i]
    }
    
    Sigma[kmax+1] <- mean(sum(x * x / Tn))
    
    IC1[,kmax+1]  <- log(Sigma[kmax+1])
    
    ic1 <- minindc(t(IC1))
    
    ic1 <- ifelse(ic1<=kmax, ic1*1, ic1*0)
    
  }
  
  if (jj == 8){
    
    for (j in 1:I(nrow(sumeigval))){
      
      if (sumeigval[j] >= .5){
        ic1 = j
        break
      }
    }
    
  }
  
  if (ic1 == 0){
    
    Fhat=matrix(0,T,N)
    
    lambda=matrix(0,N,T)
    
    chat=matrix(0,T,N)
  }else{
    
    Fhat <- Fhat0[,1:ic1]
    
    lambda <- crossprod(x,Fhat)
    
    chat <- Fhat %*% t(lambda)
  }
  
  
  output <- list(ic = ic1, lambda = lambda, Fhat = Fhat)
  return(output)
}

# Ledoit and wolf cov


rep.row <- function(x, n){
  matrix(rep(x, each = n), nrow = n)
}

qis <- function(Y, k = -1) {
  dim.Y <- dim(Y)
  N <- dim.Y[1]
  p <- dim.Y[2]
  if (k < 0) {    # demean the data and set k = 1
    Y <- scale(Y, scale = F)
    k <- 1
  }
  n <- N - k    # effective sample size
  c <- p / n    # concentration ratio
  sample <- (t(Y) %*% Y) / n    # sample covariance matrix    
  spectral <- eigen(sample)    # spectral decompositon
  lambda <- spectral$values[p:1]    # sort eigenvalues in ascending order
  u <- spectral$vectors[,p:1]    # eigenvectors follow their eigenvalues
  h <- min(c^2, 1/c^2)^0.35 / p^0.35    # smoothing parameter
  invlambda <- 1 / lambda[max(1, p-n+1):p]    # inverse of non-null eigenvalues   
  Lj <- rep.row(invlambda, min(p, n))    # like 1 / lambda_j
  Lj.i <- Lj - t(Lj)    # like (1 / lambda_j) - (1 / lambda_i)
  theta <- rowMeans(Lj * Lj.i / (Lj.i^2 + h^2 * Lj^2))    # smoothed Stein shrinker
  Htheta <- rowMeans(Lj * (h * Lj) / (Lj.i^2 + h^2 * Lj^2)) # its conjugate
  Atheta2 <- theta^2 + Htheta^2    # its squared amplitude
  if (p <= n)    # case where sample covariance matrix is not singular
    delta <- 1 / ((1 - c)^2 * invlambda + 2 * c * (1 - c) * invlambda * theta +
                    c^2 * invlambda * Atheta2)           # optimally shrunk eigenvalues
  else {    # case where sample covariance matrix is singular
    delta0 <- 1 / ((c - 1) * mean(invlambda))     # shrinkage of null eigenvalues
    delta <- c(rep(delta0, p - n), 1 / (invlambda * Atheta2));
  }
  deltaQIS <- delta * (sum(lambda) / sum(delta))    # preserve trace
  sigmahat <- u %*% diag(deltaQIS) %*% t(u)    #reconstruct covariance matrix
}



# Auxiliary function to identify characteristics that really matters

char_aux <- function(n,list){
  
  aux_list <- list()
  
  max <- length(list)
  
  
  for(i in c(1:max)){
    
    aux_list[[i]] <- rep(list(list[[i]]), n)
  
   }
  
  aux_list %>% 
    unlist(recursive = F) %>%
    return()
}


# Function that shrinks the mean by its correlation with the PC

srin_m <- function(f,df){
  
  # Spectral decomp of the cov mat
  df %>% 
    as.matrix() %>% 
    cov() %>% 
    eigen() -> decomp
  
  # Relevant PC's
  
  decomp$vectors[,1:f] -> pc_df
  
  decomp$values[1:f] %>% 
    sqrt() %>% 
    diag() %>% 
    solve()-> D
  
  D%*%t(pc_df)%*%t(df) %>% 
    t()-> pc_assets
  
  # Function that extracts weight of a portfolio
  
  w_port <- function(asset){
    
    # Filtering the df for the interest column
    
    df %>% 
      dplyr::select(asset)-> aux
    
    # Asset
    
    targ <- aux[,1]
    
    cov_vec <- c()
    
    for(i in c(1:ncol(pc_assets))){
      
      ((cov(targ,pc_assets[,i]))) -> cov_vec[i]
      
      
    }
    
    
    
    
    
    (cov_vec[which.max(abs(cov_vec))]) %>% 
      abs() %>% 
      return()
    
  }
  # estimating the weights
  port_vec <- colnames(df)
  
  port_vec %>% 
    map(~w_port(.x)) %>% 
    unlist() %>% 
    return()
  
  
}


# Function to estimate time varying matrices


tv_matrix_c <- function(df, date, f){
  
  # Filtering the DF
  
  df %>% 
    filter(Date<date) %>% 
    dplyr::select(-Date) -> aux_1
  
  # Demeaning
  
  aux_1 %>% 
    mutate_all(~. - mean(.)) -> aux_1
  
  # Factors
  
  aux_1 %>% 
    cov() %>% 
    eigen() -> decomp
  
  V <- decomp$vectors[,1:f]
  
  D <- decomp$values[1:f] %>% 
    sqrt() %>% 
    diag() %>% 
    solve()
  
  Fact <- D%*%t(V)%*%t(aux_1) %>% 
    t()
    
  
  # Loading's
  
  Load <- V%*%solve(D)
  
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0, 0)))
  # Noise
  plan(multisession,workers = 4)
   
  (aux_1 - Fact%*%t(Load)) %>% 
    as.data.frame() %>% 
    as.list() %>% 
    future_map(~ugarchfit(spec, data = .x,solver = "hybrid")) %>% 
    future_map(~sigma(ugarchforecast(.x,n.ahead = 1))) %>% 
    unlist() %>% 
    diag()-> R
  
  # Estimating GARCH for each Factor
  
  
  Fact %>% 
    as.data.frame() %>% 
    as.list() %>%
    future_map(~ugarchfit(spec, data = .x,solver = "hybrid")) %>% 
    future_map(~sigma(ugarchforecast(.x,n.ahead = 1))) %>% 
    unlist() %>% 
    diag()-> sigma_pred
  
    sigma_pred <- sigma_pred^2
  # Estimating the Factor part of the Variance:
  
  plan(NULL)
  Fac_vol <- Load%*%sigma_pred%*%t(Load)
  
  # Residual Var
  
  
  
  (Fac_vol + R ) %>% 
    return()
  
  
  
  }



tv_matrix_d <- function(df, date, f){
  
  # Filtering the DF
  
  df %>% 
    filter(Date<date) %>% 
    dplyr::select(-Date) -> aux_1
  
  # Demeaning
  
  aux_1 %>% 
    mutate_all(~. - mean(.)) -> aux_1
  
  # Factors
  
  aux_1 %>% 
    cov() %>% 
    eigen() -> decomp
  
  V <- decomp$vectors[,1:f]
  
  D <- decomp$values[1:f] %>% 
    sqrt() %>% 
    diag() %>% 
    solve()
  
  Fact <- D%*%t(V)%*%t(aux_1) %>% 
    t()
  
  
  # Spec
  
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0, 0)))
  
  dc_spec <- dccspec(uspec = multispec(replicate(2,spec)), 
                     dccOrder = c(1,1))

  # Dynamic Loading
  
  load <- function(asset){
    
    # Filtering DF
    aux_1 %>% 
        select(asset) -> aux_2
      
    ret_ass <- aux_2$D1V11
    
    ##
    Fact %>% 
      as.data.frame() %>% 
      as.list() -> fact_list
    
    
    plan(multisession,workers = 4)
    
    fact_list %>% 
      future_map(~as.data.frame(.x)) %>% 
      future_map(~mutate(.x,r = ret_ass)) %>% 
      future_map(~dccfit(dc_spec,data = .x)) %>% 
      future_map(~dccforecast(.x,n.ahead = 1)) %>% 
      future_map(~rcov(.x)) %>% 
      future_map(~unlist(.x)) %>% 
      future_map(~.x[2]/.x[1]) %>% 
      unlist() -> opt_vec
    
    plan(NULL)
    opt_vec %>% 
      return()
  }
  
  
  aux_1 %>% 
    colnames() -> assets_vec
  
  assets_vec %>% 
    map(~load(.x)) %>% 
    purrr::reduce(rbind) -> Load

  
  # Noise
  
  
  (aux_1 - Fact%*%t(Load)) %>% 
    rowMeans() %>% 
    mean() %>% 
    rep(ncol(aux)) %>% 
    diag()-> sig
  
  
  
  # Estimating GARCH for each Factor
  
  plan(multisession,workers = 4)
  Fact %>% 
    as.data.frame() %>% 
    as.list() %>%
    future_map(~ugarchfit(spec, data = .x,solver = "hybrid")) %>% 
    future_map(~sigma(ugarchforecast(.x,n.ahead = 1))) %>% 
    unlist() %>% 
    diag()-> sigma_pred
  
  sigma_pred <- sigma_pred^2
  # Estimating the Factor part of the Variance:
  
  plan(NULL)
  Fac_vol <- Load%*%sigma_pred%*%t(Load)
  
  # Residual Var
  
  
  
  (Fac_vol + R ) %>% 
    return()
  
  
  
}
















  
