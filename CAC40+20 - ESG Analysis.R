library(quantmod)
library(tidyquant)
library(tidyverse)
library(ggplot2)
library(PerformanceAnalytics)
library(quadprog)
library(dplyr)
library(scales)
library(reshape2)
library(IntroCompFinR)
library(ggrepel)
library(kableExtra)
library(VIM)
library(timetk)
# List of companies in the pool (CAC 40 + CAC NEXT 20 - ) 
Assets <- c("AC.PA","ACA.PA","AI.PA", "AIR.PA","AKE.PA","ALO.PA",
            "ATO.PA","BIM.PA","BN.PA","BNP.PA","BVI.PA","CA.PA",
            "CAP.PA","CS.PA","DG.PA","DIM.PA","DSY.PA","EDEN.PA",
            "EDF.PA","EL.PA","EN.PA","ENGI.PA","EO.PA","ERF.PA",
            "FGR.PA","FP.PA","FR.PA","GET.PA","GFC.PA","GLE.PA",
            "HO.PA","KER.PA","LI.PA","LR.PA","MC.PA", "ML.PA",
            "MT.AS", "OR.PA","ORA.PA", "ORP.PA","PUB.PA", "RI.PA",
            "RMS.PA","RNO.PA", "SAF.PA","SAN.PA", "SCR.PA","SEV.PA",
            "SGO.PA","SOLB.BR","STLA.MI","STM.PA", "SU.PA","SW.PA",
            "TEP.PA","UBI.PA","URW.AS","VIE.PA","VIV.PA","WLN.PA" )
Market_index =  "^FCHI"
Assets_index = c(Market_index, Assets)

# 6 years period
Start_date = "2015-01-01"
Stop_date = "2021-01-01"
#PART 1 - See the assets compared to the market index in the risk return framework
Assets_ret_daily <- Assets %>%
  tq_get(get  = "stock.prices",
         from =  Start_date,to = Stop_date) %>% group_by(symbol) %>% 
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn, 
               period = "daily",type = "log",col_rename = "Assets_Return")

Assets_returns_daily <- Assets_ret_daily[-1,] %>% 
  spread(symbol, value = Assets_Return) %>% tk_xts() %>% na.omit()


#Taking the CAC40 index as market benchmark
Benchmark_return_daily <- Market_index %>%
  tq_get(get  = "stock.prices",
         from = Start_date,
         to = Stop_date) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily",type = "log",
               col_rename = "Return_MIndex")

#Annualized Assets Expected return,standard deviation

#Mean_Return <- colMeans(Assets_returns_daily)

#Annual_mean_ret =  (1 + Mean_Return)^252 - 1

Assets_cov_mat = cov(Assets_returns_daily) * 252

Assets_StdDev_an <- sqrt(diag(Assets_cov_mat))

Assets_returns_an <- tq_performance(Assets_ret_daily,Ra = Assets_Return,
                                    performance_fun = Return.annualized)

Assets_StandDev_an <- tq_performance(Assets_ret_daily,Ra = Assets_Return,
                                     performance_fun = sd.annualized)

Assets_Ret_Risk_an <- left_join(Assets_returns_an,Assets_StandDev_an, 
                                by = c("symbol" = "symbol"))

colnames(Assets_Ret_Risk_an) <- c("Asset", 'Return', 'Stand.Dev')

#Annualized Market index Expected return,standard deviation 
Index_ret_an <- tq_performance(Benchmark_return_daily,Ra = Return_MIndex,
                               performance_fun = Return.annualized)

Index_StandDev_an <- tq_performance(Benchmark_return_daily,Ra = Return_MIndex,
                                    performance_fun = sd.annualized)

Index_Ret_Risk_an <- cbind(Index_ret_an,Index_StandDev_an)

colnames(Index_Ret_Risk_an) <- c('Return', 'Stand.Dev')

#See all assets with the market index in the risk return framework

ARRA_Graph <- as.data.frame(Assets_Ret_Risk_an) %>%
  ggplot(aes(x = Stand.Dev, y = Return,
             label = Index_Ret_Risk_an$Asset)) +
  geom_point() +
  theme_classic() +
  annotate(geom = "label", x = Index_Ret_Risk_an$Stand.Dev, 
           y = Index_Ret_Risk_an$Return, colour = "red",
           label = "CAC40 Index", size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized StdDev',
       y = 'Annualized Mean.Return',
       title = "CAC40 & CAC NEXT 20 stocks with the CAC40 index") + 
  coord_cartesian(clip = "off")


# Hypothetical Risk-free rate for euro = Difference between the 10y german bond
#  and the average inflation rate in the EU for the period used for the period 
#  2015-2021. Using data from statista.com
#  https://www.statista.com/statistics/885789/average-risk-free-rate-france/


#annualized
rfr = 0.008559042 #Average daily German bond return for the period

#Trying with the CAPM Return - To have a more realistic estimation of expected return
#Estimation of market return = benchmark CAC 40

R_Assets_R_Benchmk <- left_join(Assets_ret_daily,Benchmark_return_daily, 
                                by = c("date" = "date"))

CAPM_Assets_stats <- R_Assets_R_Benchmk %>%
  tq_performance(Ra = Assets_Return, 
                 Rb = Return_MIndex,Rf = rfr,
                 performance_fun = table.CAPM)

CAPM_R_Assets <- CAPM_Assets_stats %>% select(symbol,
                                AnnualizedAlpha, Beta,Correlation)

Assets_beta <- CAPM_Assets_stats %>% select(symbol,Beta)
#Get assets expected return using CAPM
#ER(i) = Rf + beta(i)*(E(Rm) - Rf )

Assets_return_CAPM = rep(rfr,length(Assets)) + 
Assets_beta$Beta*rep((Index_ret_an$AnnualizedReturn - rfr),length(Assets))

Assets_return_CAPM = enframe(Assets_return_CAPM) %>% add_column(Asset = Assets,
                                                       .before = "value")


#Add the expected return with  CAPM to the table with historical expected return
#for comparison
Assets_hict_vs_CAPM <- left_join(Assets_Ret_Risk_an,
                                 Assets_return_CAPM, 
                                 by = c("Asset" = "Asset"))
colnames(Assets_hict_vs_CAPM) = c("Asset","H.Ret","Stand.Dev","index","CAPM_Ret")

Assets_hict_vs_CAPM_TAB  <- as.data.frame(left_join(Assets_hict_vs_CAPM,
                                                    CAPM_R_Assets, 
                                           by = c("Asset" = "symbol")))
Assets_hict_vs_CAPM_TAB %>% 
  kbl(caption = "Assets individual performance",
      escape = TRUE) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

#Put the Epected returns obtained with the CAPM in a dataframe
 Assets_Exp.return_CAPM <- (Assets_hict_vs_CAPM[,-(2:3)]) %>%
spread(Asset, value = CAPM_Ret) %>% as.data.frame()


#Assets CAPM Return
Assets_CAPM_ret_an <- Assets_hict_vs_CAPM[,-(2:4)] %>% 
  spread(Asset, value = CAPM_Ret) %>% as.data.frame()

# PART TWO 
# CONSTRUCTION OF THE EFFICIENT FRONTIER with the same procedure seen in class
# Function to get the data with tickers and number of observations

stocks_data <- function(Assets,no_obs){
  adf = data.frame(matrix(nrow = no_obs, ncol = length(Assets)))
  r = 1
  for (i in Assets) {
    adf[,r] = tail(na.omit(getSymbols(i, env = NULL,from = Start_date))[,6], no_obs)
    r = r + 1
    
  }  
  
  colnames(adf) <- Assets
  return(adf)
}

# Getting the data with the use of the constructed function
prices_df = stocks_data(Assets,no_obs = 252*6 )
# Historical data limited to just 6 years to avoid too many missing data.

# Checking if all NAs have been removed
aggr(prices_df)

#Function to turn the price dataframe to dataframe of returns

p_to_r <- function(adf){
  
  art <- data.frame(matrix(nrow = nrow(adf), ncol = ncol(adf)))
  for (i in 1:ncol(adf) ) {
    art[,i] = diff(log(adf[,i]))
  }
  
  art = art[-1,]
  colnames(art) = colnames(adf)
  return(art)
}

# Historic returns
returns = p_to_r(prices_df)
# Check what the data looks like (Useful for not too many stocks)
returns_bp = boxplot(as.matrix(returns))
#returns_stats = describe(as.matrix(returns))


#Highest_Ret
Sort_Returns <- sort(Assets_hict_vs_CAPM$CAPM_Ret,
                     decreasing = TRUE,index.return = TRUE)
Top_Returns = data.frame(matrix(ncol = 3,nrow = 3))

#Asset 1
Top_Returns[1,1] = Assets_hict_vs_CAPM[Sort_Returns$ix[1] , "Asset"]
Top_Returns[1,2] = Assets_hict_vs_CAPM[Sort_Returns$ix[1] , "CAPM_Ret"]  
Top_Returns[1,3] = Assets_hict_vs_CAPM[Sort_Returns$ix[1] , "Stand.Dev"] 
#Asset 2
Top_Returns[2,1] = Assets_hict_vs_CAPM[Sort_Returns$ix[2] , "Asset"]
Top_Returns[2,2] = Assets_hict_vs_CAPM[Sort_Returns$ix[2] , "CAPM_Ret"] 
Top_Returns[2,3] = Assets_hict_vs_CAPM[Sort_Returns$ix[2] , "Stand.Dev"] 
#Asset 3
Top_Returns[3,1] = Assets_hict_vs_CAPM[Sort_Returns$ix[3] , "Asset"]
Top_Returns[3,2] = Assets_hict_vs_CAPM[Sort_Returns$ix[3] , "CAPM_Ret"] 
Top_Returns[3,3] = Assets_hict_vs_CAPM[Sort_Returns$ix[3] , "Stand.Dev"] 

colnames(Top_Returns) = c("Asset","Exp.Return", "Std.Dev")

#Lowest_Ret
Tros_Returns <- sort(Assets_hict_vs_CAPM$Return,index.return = TRUE)

Bot_Returns = data.frame(matrix(ncol = 3,nrow = 3))

#Asset 1
Bot_Returns[1,1] = Assets_hict_vs_CAPM[Tros_Returns$ix[1] , "Asset"]
Bot_Returns[1,2] = Assets_hict_vs_CAPM[Tros_Returns$ix[1] , "CAPM_Ret"] 
Bot_Returns[1,3] = Assets_hict_vs_CAPM[Tros_Returns$ix[1] , "Stand.Dev"] 
#Asset 2
Bot_Returns[2,1] = Assets_hict_vs_CAPM[Tros_Returns$ix[2] , "Asset"]
Bot_Returns[2,2] = Assets_hict_vs_CAPM[Tros_Returns$ix[2] , "CAPM_Ret"] 
Bot_Returns[2,3] = Assets_hict_vs_CAPM[Tros_Returns$ix[2] , "Stand.Dev"] 
#Asset 3
Bot_Returns[3,1] = Assets_hict_vs_CAPM[Tros_Returns$ix[3] , "Asset"]
Bot_Returns[3,2] = Assets_hict_vs_CAPM[Tros_Returns$ix[3] , "CAPM_Ret"] 
Bot_Returns[3,3] = Assets_hict_vs_CAPM[Tros_Returns$ix[3] , "Stand.Dev"] 

colnames(Bot_Returns) = c("Asset","Exp.Return", "Std.Dev")
round(Bot_Returns$Exp.Return,4)
Top_Bot_Returns <- cbind.data.frame(Top_Returns ,Bot_Returns)

Top_Bot_Returns %>% 
  kbl(caption = "Assets with highest and lowest returns",
      escape = TRUE) %>%
  kable_classic_2(full_width = FALSE, html_font = "Cambria")

#Highest_Variance
Sort_Variance <- sort(Assets_hict_vs_CAPM$Stand.Dev,
                      decreasing = TRUE,index.return = TRUE)

Top_Variance = data.frame(matrix(ncol = 3,nrow = 3))

#Asset 1
Top_Variance[1,1] = Assets_hict_vs_CAPM[Sort_Variance$ix[1] , "Asset"]
Top_Variance[1,2] = Assets_hict_vs_CAPM[Sort_Variance$ix[1] , "CAPM_Ret"]  
Top_Variance[1,3] = Assets_hict_vs_CAPM[Sort_Variance$ix[1] , "Stand.Dev"] 
#Asset 2
Top_Variance[2,1] = Assets_hict_vs_CAPM[Sort_Variance$ix[2] , "Asset"]
Top_Variance[2,2] = Assets_hict_vs_CAPM[Sort_Variance$ix[2] , "CAPM_Ret"] 
Top_Variance[2,3] = Assets_hict_vs_CAPM[Sort_Variance$ix[2] , "Stand.Dev"] 
#Asset 3
Top_Variance[3,1] = Assets_hict_vs_CAPM[Sort_Variance$ix[3] , "Asset"]
Top_Variance[3,2] = Assets_hict_vs_CAPM[Sort_Variance$ix[3] , "CAPM_Ret"] 
Top_Variance[3,3] = Assets_hict_vs_CAPM[Sort_Variance$ix[3] , "Stand.Dev"] 

colnames(Top_Variance) = c("Asset","Exp.Return", "Std.Dev")

#Lowest_Ret
Tros_Variance <- sort(Assets_hict_vs_CAPM$Stand.Dev,index.return = TRUE)

Bot_Variance = data.frame(matrix(ncol = 3,nrow = 3))

#Asset 1
Bot_Variance[1,1] = Assets_hict_vs_CAPM[Tros_Variance$ix[1] , "Asset"]
Bot_Variance[1,2] = Assets_hict_vs_CAPM[Tros_Variance$ix[1] , "CAPM_Ret"] 
Bot_Variance[1,3] = Assets_hict_vs_CAPM[Tros_Variance$ix[1] , "Stand.Dev"] 
#Asset 2
Bot_Variance[2,1] = Assets_hict_vs_CAPM[Tros_Variance$ix[2] , "Asset"]
Bot_Variance[2,2] = Assets_hict_vs_CAPM[Tros_Variance$ix[2] , "CAPM_Ret"] 
Bot_Variance[2,3] = Assets_hict_vs_CAPM[Tros_Variance$ix[2] , "Stand.Dev"] 
#Asset 3
Bot_Variance[3,1] = Assets_hict_vs_CAPM[Tros_Variance$ix[3] , "Asset"]
Bot_Variance[3,2] = Assets_hict_vs_CAPM[Tros_Variance$ix[3] , "CAPM_Ret"] 
Bot_Variance[3,3] = Assets_hict_vs_CAPM[Tros_Variance$ix[3] , "Stand.Dev"] 

colnames(Bot_Variance) = c("Asset","Exp.Return", "Std.Dev")
Top_Bot_Variance <- cbind.data.frame(Top_Variance ,Bot_Variance)

Top_Bot_Risk_Ret <- rbind.data.frame(Top_Bot_Returns, Top_Bot_Variance)
kbl(Top_Bot_Risk_Ret, digits = 4,
    caption = "Assets with highest and lowest Risk & Returns") %>%
  kable_classic_2("striped", full_width = FALSE, html_font = "Cambria") %>%
  pack_rows("Assets with highest and lowest Expected returns", 1, 3) %>%
  pack_rows("Assets with highest and lowest Standard deviations", 4, 6)
#%>% pack_rows("Group 2", 8, 10)

# Contructing the global minimum variance portfolio (MVP)
# stocks should be less than 100 to avoid issues related to dimensional
# when inverting the co-variance matrix(which must be symmetric)

# Simulation of portfolios - MC in a dataframe
# Choose the number of simulations 
nb_simus = 50000
mc_simus_port = data.frame(matrix(nrow = nb_simus, ncol = 3))

# Get random weights that sum up to 1 as vectors(therefor as.matrix)
# Using Monte  Carlo simulation
#  - loop to generate the random portfolios
for (i in 1:nb_simus) {
  wts = as.matrix(sample(-100:10000,length(as.numeric(Assets_CAPM_ret_an))))
  wts = wts/sum(wts) # Assets weights to sum to 1
  
  mc_simus_port[i, 1] =  (t(wts) %*% as.numeric(Assets_CAPM_ret_an)) # expected return
  mc_simus_port[i, 2] =  sqrt( as.numeric(t(wts) %*% Assets_cov_mat %*% wts)) # standard deviation 
  mc_simus_port[i, 3] =  ((mc_simus_port[i, 1]) - rfr)/mc_simus_port[i,2]# sharpe ratio
  
}

colnames(mc_simus_port) = c("Exp.Return", "Std.Dev","SharpeR")

mc_simus_port

# Compute expected return(RT),standard deviation(SD) the Sharpe Ratio (RT)
# Using GGPLOT for the Scatter plot

ef = ggplot() + geom_jitter(data = mc_simus_port, 
                            aes(x = Std.Dev,y = Exp.Return, color = SharpeR)) + 
  scale_color_gradient(low = "orange",high = "green",guide = "colourbar"
  ) 
# Computing a function that calculates the global minimum portfolio MVP
# using matrix algebra                       

mvp = function(Assets_cov_mat){
  id_vet = rep(1, ncol(Assets_cov_mat))
  
  mvp_numerator = solve(Assets_cov_mat) %*% id_vet
  mvp_denominator =  as.numeric(t(id_vet) %*% solve(Assets_cov_mat) %*% id_vet)
  
  opt_asset_weigths = mvp_numerator / mvp_denominator
  
  return(opt_asset_weigths)
}
# Data frame of the MVP (Standard Deviation and Expected Return)

x_min = mvp(Assets_cov_mat)#weights of the assets in the mvp
SS_MVP_Portfolio = data.frame(matrix(ncol = 4,nrow = 1))
SS_MVP_Portfolio[1,1] = t(x_min) %*% as.numeric(Assets_CAPM_ret_an)
SS_MVP_Portfolio[1,2] = sqrt((t(x_min) %*% Assets_cov_mat %*% x_min))
SS_MVP_Portfolio[1,3] = ((SS_MVP_Portfolio[1, 1]) - rfr)/SS_MVP_Portfolio[1, 2]

#In order to calculate the portfolio beta, we need to regress the portfolio
#returns against the benchmark returns.
#Portfolio daily return
mvp_port_ret_daily <- Assets_ret_daily %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Assets_Return,
               weights = x_min,
               col_rename = 'port_ret',
               geometric = FALSE)

mvp_comb_MIret <- left_join(mvp_port_ret_daily,Benchmark_return_daily,
                            by = 'date')

#We will us the linear regression model to calculate the alpha and the beta.

mvp_rm_model <- lm(mvp_comb_MIret$port_ret ~ mvp_comb_MIret$Return_MIndex)

mvp_model_beta <- mvp_rm_model$coefficients[2]
#Since the treynor ratio measures on the systematic risk of the portfolio
#comparing with the ssharpe ratio that uses the SD ( total risk of the portfolio
#gives an idea of the diversification level of the portfoolio)
SS_MVP_Portfolio[1,4] = ((SS_MVP_Portfolio[1, 1]) - rfr)/mvp_model_beta

colnames(SS_MVP_Portfolio) = c("Exp.Return", "Std.Dev","SharpeR", "TreynorR")

# Then, a function to construct the Maximum return portfolio
maxrp = function(Assets_cov_mat, Assets_CAPM_ret_an, my_exp_ret) 
  #target-my_exp_ret #is my expected return 
{
  #  Identity vector
  id_vet = rep(1, ncol(Assets_cov_mat))
  #  Lagrange - matrix - vectors
  mat_l1 = cbind(2 * Assets_cov_mat, as.numeric(Assets_CAPM_ret_an), id_vet)
  mat_l2 = c(t(as.numeric(Assets_CAPM_ret_an)), 0, 0)
  mat_l3 = c(t(id_vet), 0, 0) 
  
  #  combine in rows to create A matrix
  A = rbind(mat_l1, mat_l2, mat_l3)
  #  To create the B vector
  B = c(rep(0, ncol(Assets_cov_mat)),my_exp_ret,1)
  
  x_opt_weight = solve(A) %*% B 
  x_opt_weight = x_opt_weight[1:ncol(Assets_cov_mat),]
  
  return(x_opt_weight)
  
}

maxrp(Assets_cov_mat, as.numeric(Assets_CAPM_ret_an), max(as.numeric(Assets_CAPM_ret_an)))
# Data frame of the Maximum return portfolio 
# (Standard Deviation and Expected Return)

#variance not equal 

# The weights 
x_max = maxrp(Assets_cov_mat,as.numeric(Assets_CAPM_ret_an),
              max(as.numeric(Assets_CAPM_ret_an)))
SS_max.ret_portfolio = data.frame(matrix(ncol = 4,nrow = 1))
SS_max.ret_portfolio[1,1] = (t(x_max) %*% as.numeric(Assets_CAPM_ret_an))
SS_max.ret_portfolio[1,2] = sqrt( t(x_max) %*% Assets_cov_mat %*% x_max)
SS_max.ret_portfolio[1,3] = (SS_max.ret_portfolio[1, 1] - rfr)/SS_max.ret_portfolio[1, 2]

maxr_port_ret_daily <- Assets_ret_daily %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Assets_Return,
               weights = x_max,
               col_rename = 'port_ret',
               geometric = FALSE)

maxr_comb_MIret <- left_join(maxr_port_ret_daily,
                             Benchmark_return_daily, by = 'date')

#We will us the linear regression model to calculate the alpha and the beta.

maxr_rm_model <- lm(maxr_comb_MIret$port_ret ~ maxr_comb_MIret$Return_MIndex)

maxr_model_beta <- maxr_rm_model$coefficients[2]
#Since the treynor ratio measures on the systematic risk of the portfolio
#comparing with the ssharpe ratio that uses the SD ( total risk of the portfolio
#gives an idea of the diversification level of the portfoolio)
SS_max.ret_portfolio[1,4] = ((SS_max.ret_portfolio[1, 1]) - rfr)/maxr_model_beta

colnames(SS_max.ret_portfolio) = c("Exp.Return", "Std.Dev","SharpeR", "TreynorR")


# Add a point that indicates the MVP on the scatterplot 
ef = ggplot() + geom_jitter(data = mc_simus_port, 
                            aes(x = Std.Dev,y = Exp.Return, color = SharpeR)) + 
  geom_point(data = SS_MVP_Portfolio, 
             aes(x = Std.Dev,y = Exp.Return), color = 'black' ) +
  geom_point(data = SS_max.ret_portfolio, 
             aes(x = Std.Dev,y = Exp.Return), color = 'black' ) +
  scale_color_gradient(low = "orange", high = "green",space = "Lab",guide = "colourbar" ) 

# Maximum risk adjusted return portfolio 

# An approximation of the max sharpe ratio from the simulations madez

# Option 3 Calculate weights for Tangent Portfolio: 

n <- ncol(Assets_cov_mat)
# risk free rate <- rfr
# Vector of ones <- rep(1, n)
mu_minus_rf <- as.numeric(Assets_CAPM_ret_an) - (rfr*rep(1, n))


top_mat <- solve(Assets_cov_mat) %*% mu_minus_rf
bot_val <- as.numeric(t(rep(1, n)) %*% top_mat)

x_Tan_Port <- top_mat[, 1] / bot_val #tangent portfolio WEIGHTS

sum(x_Tan_Port)

# Port_Return_Risk(weights = Tan_Port) -> Tan_Port

#Tangent_Portfolio calculated

SS_Tangent_Portfolio = data.frame(matrix(ncol = 4,nrow = 1))
SS_Tangent_Portfolio[1,1] = t(x_Tan_Port) %*% as.numeric(Assets_CAPM_ret_an)
SS_Tangent_Portfolio[1,2] = sqrt( t(x_Tan_Port) %*% Assets_cov_mat %*% x_Tan_Port)
SS_Tangent_Portfolio[1,3] = ((SS_Tangent_Portfolio[1, 1]) - rfr)/SS_Tangent_Portfolio[1, 2]

tan_port_ret_daily <- Assets_ret_daily %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Assets_Return,
               weights = x_Tan_Port,
               col_rename = 'port_ret',
               geometric = FALSE)
tan_comb_MIret <- left_join(tan_port_ret_daily,
                            Benchmark_return_daily, by = 'date')

#We will us the linear regression model to calculate the alpha and the beta.

tan_rm_model <- lm(tan_comb_MIret$port_ret ~ tan_comb_MIret$Return_MIndex)

tan_model_beta <- tan_rm_model$coefficients[2]
#Since the treynor ratio measures on the systematic risk of the portfolio
#comparing with the ssharpe ratio that uses the SD ( total risk of the portfolio
#gives an idea of the diversification level of the portfoolio)
SS_Tangent_Portfolio[1,4] = ((SS_Tangent_Portfolio[1, 1]) - rfr)/tan_model_beta

colnames(SS_Tangent_Portfolio) = c("Exp.Return", "Std.Dev","SharpeR", "TreynorR")

#Max sharpe ratio already annualized - mc_simus_port
# Add a point that indicates the max Sharpe ratio on the scatterplot 
ef = ggplot() + geom_jitter(data = mc_simus_port, 
                            aes(x = Std.Dev,y = Exp.Return, color = SharpeR)) + 
  geom_point(data = SS_MVP_Portfolio, 
             aes(x = Std.Dev,y = Exp.Return), color = 'black' ) +
  geom_point(data = SS_max.ret_portfolio , 
             aes(x = Std.Dev,y = Exp.Return), color = 'black' ) +
  geom_point(data = SS_Tangent_Portfolio , 
             aes(x = Std.Dev,y = Exp.Return), color = "red" ) +
  scale_color_gradient(low = "orange",
                       high = "green",space = "Lab",guide = "colourbar")


# To create the efficient frontier
# The line that gives the maximum return for every standard deviation (Risk level)
#  A vector with all returns between the max return and minimum risk portfolio
ef_returns = seq(from = SS_MVP_Portfolio$Exp.Return,
                 to = SS_max.ret_portfolio$Exp.Return, by = 0.001)
# Get the standard deviation for every return and plot
#efficient frontier with short sale
eff_frontier_ss = data.frame(matrix(ncol = 2,nrow = length(ef_returns)))

r = 1 
for (i in ef_returns) {
  #using the maxrp function to get the optimum weight for every return
  #and compute the minimum variance
  opt_weight = maxrp(Assets_cov_mat,as.numeric(Assets_CAPM_ret_an),i)
  eff_frontier_ss[r,1] = t(opt_weight) %*% as.numeric(Assets_CAPM_ret_an)
  eff_frontier_ss[r,2] = sqrt( t(opt_weight) %*% Assets_cov_mat %*% opt_weight)
  r = r + 1
  
}
colnames(eff_frontier_ss) = c("Exp.Return", "Std.Dev")

# Graph of efficient frontier
ef = ggplot() + geom_jitter(data = mc_simus_port, 
                            aes(x = Std.Dev,y = Exp.Return, color = SharpeR)) + 
  geom_point(data = SS_MVP_Portfolio, 
             aes(x = Std.Dev,y = Exp.Return), color = 'green' ) +
  geom_point(data = SS_max.ret_portfolio, 
             aes(x = Std.Dev,y = Exp.Return), color = 'red' ) +
  geom_point(data = SS_Tangent_Portfolio, 
             aes(x = Std.Dev,y = Exp.Return), color = 'orange' ) +
  
  geom_line(data = eff_frontier_ss,aes(x = Std.Dev,y = Exp.Return)) +
  scale_color_gradient(low = "lightblue",high = "blue",guide = "colourbar" )  +
  theme_classic() + ggtitle("Efficient Frontier with short sales") +
  geom_label_repel(aes( x = SS_MVP_Portfolio$Std.Dev,
                        y = SS_MVP_Portfolio$Exp.Return),
                   label = "MVP",color = 'black', color = 'white', size = 3) +
  geom_label_repel(aes( x = SS_max.ret_portfolio$Std.Dev,
                        y = SS_max.ret_portfolio$Exp.Return),
                   label = "Max Return Port",color = 'black', color = 'white',size = 3) +
  geom_label_repel(aes( x = SS_Tangent_Portfolio$Std.Dev,
                        y = SS_Tangent_Portfolio$Exp.Return), 
                   label = "Tangent Portfolio",color = 'black', color = 'white',size = 3) +
  labs(x = expression("Risk" ~ (sigma[p])),
       y = expression("Expected Return" ~ (mu[p])))

#Table of important points SS
eff_frontier_ss_Tab = rbind(SS_Tangent_Portfolio, SS_max.ret_portfolio,
                            SS_MVP_Portfolio)

row.names(eff_frontier_ss_Tab) = c("SS - Tangent Portfolio",
                                   "SS - Max Ex.Return Portfolio", 
                                   "SS - Min variance Portfolio")

eff_frontier_ss_Tab %>% 
  kbl(caption = "Portfolios on the efficient frontier with short sales",
      digits = 4,  escape = TRUE) %>%
  kable_classic_2(full_width = FALSE, html_font = "Cambria")


#PART 3
## EFFICIENT FRONTIER - portfolios with no short selling.

# Using Quadprog package, repeating the maximum return portfolio/MVP 
# including an inequality constraint (X(i)>0, (i = 1:n))

n <- ncol(Assets_cov_mat)    # n is the number of assets.
# Create initial Amat and bvec assuming only equality constraint

# Dmat = variance covariance matrix of the asset returns
# dvec = Expected returns vector

#Global Minimum Variance Portfolio with no Short Sales allowed
r.f = rfr
Er_vec = as.numeric(Assets_CAPM_ret_an)
cov_mat = Assets_cov_mat
sd_vec = sqrt(diag(cov_mat))
round(cov2cor(cov_mat),5)#covariance to correlation matrix

Dmat = 2*Assets_cov_mat
dvec = rep(0, n)
Amat <- cbind( rep(1,n), diag(n))
bvec <- c(1, rep(0,n))
t(Amat)

QP_solved_mvp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1 ) # bvec = weight

#constraints (weights sum to one and all weights are positive)
Assets_weights_mvp = QP_solved_mvp$solution

sum(Assets_weights_mvp)
names(Assets_weights_mvp) = names(Assets_CAPM_ret_an)
#Use the quadprog function solve.QP()
# to compute global minimum variance portfolio
# Expected return
ER_mvp_qp_nss = as.numeric(crossprod(Assets_weights_mvp,as.numeric(Assets_CAPM_ret_an)))
# Volatility
SD_mvp_qp_nss = sqrt(as.numeric(t(Assets_weights_mvp) %*% Assets_cov_mat %*% Assets_weights_mvp))

NSS_MVP_Portfolio = data.frame(matrix(ncol = 4,nrow = 1))
NSS_MVP_Portfolio[1,1] = ER_mvp_qp_nss 
NSS_MVP_Portfolio[1,2] = SD_mvp_qp_nss
NSS_MVP_Portfolio[1,3] = ((ER_mvp_qp_nss) - rfr)/SD_mvp_qp_nss 
#for treynor ratio mvp
NSS_mvp_port_ret_daily <- Assets_ret_daily %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Assets_Return,
               weights = Assets_weights_mvp,
               col_rename = 'port_ret',
               geometric = FALSE)

NSS_mvp_comb_MIret <- left_join( NSS_mvp_port_ret_daily,
                                 Benchmark_return_daily, by = 'date')

#We will us the linear regression model to calculate the alpha and the beta.
NSS_mvp_rm_model <- lm(  NSS_mvp_comb_MIret$port_ret ~ NSS_mvp_comb_MIret$Return_MIndex)

NSS_mvp_model_beta <-  NSS_mvp_rm_model$coefficients[2]

#Since the treynor ratio measures on the systematic risk of the portfolio
#comparing with the ssharpe ratio that uses the SD ( total risk of the portfolio
#gives an idea of the diversification level of the portfoolio)
NSS_MVP_Portfolio[1,4] = ((NSS_MVP_Portfolio[1, 1]) - rfr)/NSS_mvp_model_beta

colnames(NSS_MVP_Portfolio)  = c("Exp.Return", "Std.Dev","SharpeR", "TreynorR")


#Tangent port without short sales
NSS_Tan_port <- tangency.portfolio((as.numeric(Assets_CAPM_ret_an)),
                                   Assets_cov_mat,rfr,shorts = FALSE)
sum(NSS_Tan_port$weights)

NSS_Tangent_Portfolio = data.frame(matrix(ncol = 3,nrow = 1))
NSS_Tangent_Portfolio[1,1] = as.numeric(NSS_Tan_port$er)
NSS_Tangent_Portfolio[1,2] = as.numeric(NSS_Tan_port$sd)
NSS_Tangent_Portfolio[1,3] = (NSS_Tangent_Portfolio[1,1] - rfr)/NSS_Tangent_Portfolio[1,2]

#for treynor ratio mvp
NSS_tan_port_ret_daily <- Assets_ret_daily %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Assets_Return,
               weights = NSS_Tan_port$weights,
               col_rename = 'port_ret',
               geometric = FALSE)

NSS_tan_comb_MIret <- left_join( NSS_tan_port_ret_daily,
                                 Benchmark_return_daily, by = 'date')

#We will us the linear regression model to calculate the alpha and the beta.

NSS_tan_rm_model <- lm(NSS_tan_comb_MIret$port_ret ~ NSS_tan_comb_MIret$Return_MIndex)

NSS_tan_model_beta <-  NSS_tan_rm_model$coefficients[2]

#Since the treynor ratio measures on the systematic risk of the portfolio
#comparing with the ssharpe ratio that uses the SD ( total risk of the portfolio
#gives an idea of the diversification level of the portfoolio)
NSS_Tangent_Portfolio[1,4] = ((NSS_Tangent_Portfolio[1, 1]) - rfr)/NSS_tan_model_beta

colnames(NSS_Tangent_Portfolio)  = c("Exp.Return", "Std.Dev","SharpeR", "TreynorR")


#Max-Return portfolio without short sales
#Max return

NSS_max.ret_port <- efficient.portfolio(as.numeric(Assets_CAPM_ret_an),Assets_cov_mat,
                                        max(as.numeric(Assets_CAPM_ret_an))*0.99, shorts = FALSE)

NSS_max.ret_port_w <- NSS_max.ret_port$weights

sum(NSS_max.ret_port_w)
NSS_max.ret_portfolio = data.frame(matrix(ncol = 4,nrow = 1))
NSS_max.ret_portfolio[1,1] = NSS_max.ret_port$er
NSS_max.ret_portfolio[1,2] = NSS_max.ret_port$sd
NSS_max.ret_portfolio[1,3] = (NSS_max.ret_portfolio[1, 1] - rfr)/NSS_max.ret_portfolio[1, 2]

nss_maxr_port_ret_daily <- Assets_ret_daily %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Assets_Return,
               weights = NSS_max.ret_port_w,
               col_rename = 'port_ret',
               geometric = FALSE)

nss_maxr_comb_MIret <- left_join(nss_maxr_port_ret_daily,
                                 Benchmark_return_daily, by = 'date')

#We will us the linear regression model to calculate the alpha and the beta.
nss_maxr_rm_model <- lm(nss_maxr_comb_MIret$port_ret ~ nss_maxr_comb_MIret$Return_MIndex)

nss_maxr_model_beta <- nss_maxr_rm_model$coefficients[2]
#Since the treynor ratio measures on the systematic risk of the portfolio
#comparing with the ssharpe ratio that uses the SD ( total risk of the portfolio
#gives an idea of the diversification level of the portfoolio)
NSS_max.ret_portfolio[1,4] = ((NSS_max.ret_portfolio[1, 1]) - rfr)/nss_maxr_model_beta

colnames(NSS_max.ret_portfolio) = c("Exp.Return", "Std.Dev","SharpeR", "TreynorR")

#The argument meq = 2 specifies 2 equality constraints. It tells solve.QP()
# how to determine Aeq and Aneq from A

## #efficient frontier with no short sale
Ex.Returns_nss = seq(NSS_MVP_Portfolio$Exp.Return,
                     (NSS_max.ret_portfolio$Exp.Return),
                     length.out = 300)
w.mat = matrix(0, length(Ex.Returns_nss),n )
SDs_nss = rep(0, length(sqrt(diag(Assets_cov_mat))))
colnames(w.mat) = names(Ex.Returns_nss)
D.mat = 2*Assets_cov_mat
d.vec = rep(0, n)
A.mat = cbind(as.numeric(Assets_CAPM_ret_an), rep(1,n), diag(n))

eff_frontier_nss = data.frame(matrix(ncol = 2,
                                     nrow = length(Ex.Returns_nss)))
r = 1 
for (i in 1:length(Ex.Returns_nss)) {
  b.vec = c(Ex.Returns_nss[i],1,rep(0,n))
  qp.out = solve.QP(Dmat = D.mat, dvec = d.vec,
                    Amat = A.mat, bvec = b.vec, meq = 2)
  w.mat[i, ] = qp.out$solution
  SDs_nss[i] = sqrt(qp.out$value)
  
  weight_nss = w.mat[i, ]
  eff_frontier_nss[r,1] = t(weight_nss) %*% as.numeric(Assets_CAPM_ret_an)
  eff_frontier_nss[r,2] = sqrt( t(weight_nss) %*% Assets_cov_mat %*% weight_nss)
  r = r + 1
  
  colnames(eff_frontier_nss) = c("Exp.Return", "Std.Dev")
}
# Efficient frontier without short sale VS efficient frontier with short sales
#Table of important points NSS
eff_frontier_nss_TAB = rbind(NSS_Tangent_Portfolio, NSS_max.ret_portfolio,
                             NSS_MVP_Portfolio)

row.names(eff_frontier_nss_TAB) = c("NSS - Tangent Portfolio",
                                    "NSS - Max Ex.Return Portfolio", 
                                    "NSS - Min variance Portfolio")

eff_frontier_nss_TAB %>% 
  kbl(caption = "Portfolios on the efficient frontier without short sales",
      digits = 4,  escape = TRUE) %>%
  kable_classic_2(full_width = FALSE, html_font = "Cambria")

#Table of the points NSS & SS
Tab_ss_nss <- rbind.data.frame(eff_frontier_ss_Tab,eff_frontier_nss_TAB)

kbl(Tab_ss_nss, caption = "Maximum Expected return and minimum risk with short sale(SS)
      and withour shortsale (NSS)", digits = 4, escape = TRUE) %>%
  kable_classic_2("striped", full_width = FALSE, html_font = "Cambria") %>%
  pack_rows("Portfolios with short selling allowed", 1, 3) %>%
  pack_rows("Portfolios with short selling not allowed", 4, 6)

# Graph of efficient frontier
EF_NSS_SS = ggplot() + geom_jitter(data = mc_simus_port, 
                                   aes(x = Std.Dev,y = Exp.Return, color = SharpeR)) + 
  theme_bw() + ggtitle("Efficient Frontiers including short sales and
                       without short sales") +
  scale_color_gradient(low = "lightblue",high = "blue2",guide = "colourbar" ) +
  geom_point(data = SS_MVP_Portfolio , 
             aes(x = Std.Dev,y = Exp.Return), color = 'goldenrod' ) +
  geom_point(data = SS_max.ret_portfolio , 
             aes(x = Std.Dev,y = Exp.Return), color = 'goldenrod' ) +
  geom_point(data = NSS_MVP_Portfolio , 
             aes(x = Std.Dev,y = Exp.Return), color = "maroon" ) +
  geom_point(data = NSS_max.ret_portfolio , 
             aes(x = Std.Dev,y = Exp.Return), color = "maroon" ) +
  geom_point(data = NSS_Tangent_Portfolio , 
             aes(x = Std.Dev,y = Exp.Return), color = "maroon" ) +
  
  geom_line(data = eff_frontier_ss,aes(x = Std.Dev,y = Exp.Return),
            color = 'firebrick') +
  geom_line(data = eff_frontier_nss ,aes(x = Std.Dev,y = Exp.Return),
            color = 'slateblue4') +
  coord_cartesian(clip = "off") +
  geom_text_repel(aes( x = SS_MVP_Portfolio$Std.Dev,y = SS_MVP_Portfolio$Exp.Return),
                  label = "MVP_Port_SS",color = 'firebrick',
                  xlim = c(-Inf, Inf), ylim = c(-Inf, Inf), size = 3) +
  geom_text_repel(aes( x = SS_max.ret_portfolio$Std.Dev,y = SS_max.ret_portfolio$Exp.Return),
                  label = "Max.Ret_SS",color = 'firebrick',
                  xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),size = 3) +
  geom_text_repel(aes( x = NSS_MVP_Portfolio$Std.Dev,y = NSS_MVP_Portfolio$Exp.Return), 
                  label = "MVP_Port_NSS",color = 'slateblue4',
                  xlim = c(-Inf, Inf), ylim = c(-Inf, Inf), size = 3) +
  geom_text_repel(aes( x = NSS_max.ret_portfolio$Std.Dev, y = NSS_max.ret_portfolio$Exp.Return), 
                  label = "Max.Ret_NSS",color = 'slateblue4',
                  xlim = c(-Inf, Inf), ylim = c(-Inf, Inf), size = 3) +
  geom_text_repel(aes( x = NSS_Tangent_Portfolio$Std.Dev,
                       y =  NSS_Tangent_Portfolio$Exp.Return), 
                  label = "Tan_Port_NSS",color = 'slateblue4', 
                  xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),  size = 3) +
  labs(x = expression("Risk"~(sigma[p])),
       y = expression("Expected Return"~(mu[p]))) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  scale_x_continuous(labels = scales::label_percent()) +
 geom_point(data = SS_Tangent_Portfolio , 
             aes(x = Std.Dev,y = Exp.Return), color = 'goldenrod' ) +
  geom_text_repel(aes( x = SS_Tangent_Portfolio$Std.Dev,
                       y = SS_Tangent_Portfolio$Exp.Return), 
                  label = "Tan_Port_ss",color = 'firebrick',size = 3)

