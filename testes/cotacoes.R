stocks = c( "^BVSP", "^GSPC", "^MXX", "^IPSA", "^MERV", "^FTSE", "^GDAXI", "^FCHI", "^N225", "^HSI", "^BSESN", "^AXJO", "^JKSE")

stocks = c( "^BVSP","^DJI","^GSPTSE","^SPBLPGPT","^FTSE","^IBEX","^SSMI","^N225","^HSI","^BSESN","^AXJO","^JKSE" )


cotacoes = matrix(nrow = 0, ncol = 8)
for(stock in stocks){
    file = paste( "http://ichart.finance.yahoo.com/table.csv?s=", stock, "&g=d&a=3&b=1&c=2016&&ignore=.csv", sep = "" );
    print(file);
    t = read.delim(file, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE )
    t = cbind( rep(stock, dim(t)[1]), t)
    cotacoes = rbind(cotacoes, t)
}
colnames(cotacoes) <- c("Stock", "Date", "Open", "High", "Low", "Close", "Volume", "Adj.Close")

rm(t,stock, stocks, file)
View(cotacoes)


