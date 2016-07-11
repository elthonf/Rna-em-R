#Plota gráfico com variação da tangente hiperbólica
#inverso seria log(sqrt( (1 + y) / (1 - y)) *5, base = exp(1))

x = seq( from= -5., to= 5, by = 0.01)
y = tanh( x/5 )
plot(x = x, y = y, type = "l", xlab = "variação", ylab = "variação normalizada")

x2 = log( (sqrt(1+y) / sqrt(1-y) )) *5
x - x2
#tanh(0.2/5)


x = seq( from= -3., to= 3, by = 0.01)
y = tanh( x )
plot(x = x, y = y, type = "l", xlab = "", ylab = "")


x[1]
x2[1]
