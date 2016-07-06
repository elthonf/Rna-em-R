#Plota gráfico com variação da tangente hiperbólica
#inverso seria log(sqrt( (1 + y) / (1 - y)) *5, base = exp(1))

x = seq( from= -10., to= 10, by = 0.01)
y = tanh( x/5 )
plot(x = x, y = y, type = "l", xlab = "variação", ylab = "variação normalizada")



#tanh(0.2/5)
