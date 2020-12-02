#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#--------------------- Direct Forecasting Function -----------------------#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# O objetivo desta função é computar as previsões obtidas de modo direto 
# (não recursiva).

# df = (data frame) com a variável dependente na primeira coluna e as com
# as variáveis explicativas (sem lags) nas colunas restantes.
# h = (integer) horizonte de previsão
# p = (integer) defasagens das variáveis (dependente e explicativas)

# 1. Direct Forecasting Function ------------------------------------------
direct_forecast <- function(df, h = 1, p = 1){
        
        #%%%% Initial Settings %%%%#
        N <- nrow(df)
        K <- ncol(df)
        
        preds <- rep(NA,h)
        
        MDF <- embed(x = df, dimension = (p+1))
        s   <- nrow(MDF)
        
        Y_treino <- MDF[,1]
        X_treino <- MDF[,-c(1:K)]
        
        X_teste <- MDF[s,1:(K*p)]
        
        i=1
        for(i in 1:h){
                mod <- lm(Y_treino ~ X_treino)
                betas <- mod$coefficients
                preds[i] <- c(1,X_teste) %*% betas 
                
                Y_treino = Y_treino[-1]
                X_treino = X_treino[-nrow(X_treino), ]
        }
        
        OUT <- preds
        
        return(OUT)
}

# 2. Tests ----------------------------------------------------------------
#install_github(gabrielrvsc/HDeconometrics)
library(HDeconometrics)

data("BRinf")
data = BRinf[ , c(1, 12, 14)]
colnames(data) = c("INF", "IP", "U")

ytrue <- data[,1]
data  <- data[1:132,]

previsoes <- direct_forecast(df = data, h = 24, p = 3)

Metrics::rmse(actual = tail(ytrue,24), predicted = previsoes)

# 3. Final Settings -------------------------------------------------------

# https://insightr.wordpress.com/2018/01/10/direct-forecast-x-recursive-forecast/
# https://www.princeton.edu/~mwatson/papers/hstep_3.pdf




