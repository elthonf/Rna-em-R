rm(list=ls())

nomes.treina = paste("TRE", 1:25)
nomes.valida = paste("VAL", 1:25)

#log.tab[1:100,]
#log.tab[log.tab$cenario == "TRE 1",][1:100,]

log.grafico = list()
#log.grafico$"TRE 1" = 1
#log.grafico["TRE 2"] = 2
#log.grafico$"TRE 1" = log.tab[log.tab$cenario == "TRE 1","ET"]
#log.grafico$i = log.tab[log.tab$cenario == "TRE 2","ET"]


for ( i in nomes.treina){
    log.grafico[[i]] = log.tab[log.tab$cenario == i,"ET"]
    print (i)
}
rm(i)


#plot( x = log.grafico$"TRE 5"[1:10], type = "l")

log.grafico.frame = data.frame(log.grafico)[1:1001,]

matplot(log.grafico.frame, type = c("l"),pch=1,col = 1:25) #plot


exibegrafico = 1:8 #24 horas
exibegrafico = 9:15 #48 horas
exibegrafico = 16:20 #1 semana
exibegrafico = 21:25 #1 mês

matplot(log.grafico.frame[,exibegrafico], type = c("l"),pch=1,col = exibegrafico, ylab = "") #plot
legend("topright", legend = nomes.treina[exibegrafico], col=exibegrafico, pch=1, title = "1 semana") # optional legend


matplot(log.grafico.frame[,1:8], type = c("l"),pch=1,col = 1:8, ylab = "") #plot
legend("topright", legend = nomes.treina[1:8], col=1:8, pch=1, title = "Legenda") # optional legend



log.resumo.treina = log.resumo[((1:100) %% 2 != 0 ),]
log.resumo.valida = log.resumo[((1:100) %% 2 == 0 ),]
log.resumo.final = cbind( log.resumo.treina[1:25,c("cenario", "iteracao", "ET", "acuracia")],
                          log.resumo.treina[26:50,c("iteracao", "ET", "acuracia")],
                          log.resumo.valida[1:25,c("cenario","iteracao", "ET", "acuracia")],
                          log.resumo.valida[26:50,c("iteracao", "ET", "acuracia")])
rm(log.resumo.treina, log.resumo.valida)

write.csv(x = log.resumo.final, file="dados/MLP.log.artigo.csv")
#OU:
write.csv(x = log.resumo.final, file="dados/RecExt.log.artigo.csv")

View(log.resumo.final)
