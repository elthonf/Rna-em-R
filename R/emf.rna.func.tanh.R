emf.rna.func.tanh <- function (x){
    #Tangene Hiperbolica
    #   J = SIGMOID(z) computes the sigmoid of z.

    g = ( exp(x) - exp(-x) ) / ( exp(x) + exp(-x))

    return (g)
}
