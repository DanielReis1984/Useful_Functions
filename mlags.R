#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#------------------------ Lags Data.Frame Generator ------------------------#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Função para a geração de um data.frame com a série temporal original e lags

# df       = data.frame ou matriz com as séries temporais
# lags     = número de lags desejado
# col.date = número da coluna do df com as datas (NA ou NULL se não tiver)
# nomes    = (T) preservar os nomes das variáveis originais

mlags <- function(df, lags, col.date = 1, nomes = T){
        
        dim = lags+1
        N  <- nrow(df)
        K  <- ncol(df)
        
        if(is.numeric(col.date)){
                Date = df[,col.date]
                X    = as.matrix(df[,-col.date])
        }else{
                Date = NA
                X    = as.matrix(df)
        }
        
        out   <- embed(x = X, dimension = dim)
        N_out <- nrow(out)
        K_out <- ncol(out)
        
        aux <- expand.grid(1:ncol(X), 0:(dim-1))
        
        if(nomes == T){
                colnames(out) <- paste(colnames(X), "_", aux$Var2, sep = "")
        }else{
                colnames(out) <- paste("X", aux$Var1, "_", aux$Var2, sep = "")
        }
        
        OUT <- data.frame("Date" = tail(Date,N_out), out)
        return(OUT)
}

# 2. Tests ----------------------------------------------------------------
# set.seed(123)  
# MDF <- as.data.frame(matrix(rnorm(900), ncol = 3))
# colnames(MDF) <- c("X", "Y", "Z")
# 
# teste1 <- mlags(df = MDF, lags = 2, col.date = NA, nomes = F)
# teste2 <- mlags(df = MDF, lags = 2, col.date = NA, nomes = T)
