#Script que busca da bovespa e de uma série de índices ou ações (vetor) e salva em arquivo


stocks = c( "GSPC", "MXX", "IPSA", "MERV", "FTSE", "GDAXI", "FCHI", "N225", "HSI", "BSESN", "AXJO", "NZ50", "JKSE")

getCotacao <- function(stock){
    file = paste( "http://ichart.finance.yahoo.com/table.csv?s=^", stock, "&g=d&a=0&b=1&c=2016&&ignore=.csv", sep = "" );
    print(file);
    t = read.delim(file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE );

    t = cbind( rep(stock, dim(t)[1]), t);

    t$Date = as.Date(t$Date, format="%Y-%m-%d");
    t$Var = (t$Adj.Close - t$Open)/ t$Open * 100;

    t = cbind(1:dim(t)[1] ,  t[order(t$Date),])
    colnames(t)[1] <- c("Seq")
    colnames(t)[2] <- c("Stock")

    return (t);
}


########  Cria BVSP
bvsp = getCotacao ( "BVSP")

#bvsp$D1d = NA; bvsp$D2d = NA; bvsp$D7d = NA; bvsp$D30d = NA;
bvsp$D1c = NA; bvsp$D2c = NA; bvsp$D7c = NA; bvsp$D30c = NA;
bvsp$D1v = NA; bvsp$D2v = NA; bvsp$D7v = NA; bvsp$D30v = NA;

for( i in 1:dim(bvsp)[1]){
    if(i+1 <= dim(bvsp)[1]) bvsp$D1c[i] = bvsp$Adj.Close[i+1]; #1 du - Pregao
    if(i+2 <= dim(bvsp)[1]) bvsp$D2c[i] = bvsp$Adj.Close[i+2]; #2 du - Pregao
    if(i+5 <= dim(bvsp)[1]) bvsp$D7c[i] = bvsp$Adj.Close[i+5]; #5 du - Pregao
    if(i+21 <= dim(bvsp)[1]) bvsp$D30c[i] = bvsp$Adj.Close[i+21]; #21 du - Pregao
}

bvsp$D1v = (bvsp$D1c - bvsp$Adj.Close ) / bvsp$Adj.Close * 100
bvsp$D2v = (bvsp$D2c - bvsp$Adj.Close ) / bvsp$Adj.Close * 100
bvsp$D7v = (bvsp$D7c - bvsp$Adj.Close ) / bvsp$Adj.Close * 100
bvsp$D30v = (bvsp$D30c - bvsp$Adj.Close ) / bvsp$Adj.Close * 100

stock = stocks[1]

for(stock in stocks){
    bvsp[,stock] = NA
    tabStocks = getCotacao(stock)

    for(i in 1:dim(bvsp)[1]){
        a = tabStocks$Var[tabStocks$Date == bvsp$Date[i]];
        if(length(a) > 0 )
            bvsp[i,stock] = a
    }
}
rm(a, i, stock, tabStocks)


write.csv(file = "cotacoes.csv", x = bvsp)
