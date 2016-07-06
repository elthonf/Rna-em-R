emf.rna.func.sigmoid <- function (z){
#SIGMOID Compute sigmoid functoon
#   J = SIGMOID(z) computes the sigmoid of z.

    g = 1.0 / (1.0 + exp(-z));

    return (g)

#1 / ( 1 + exp(-(matrix(-50:49, nrow = 10, byrow = TRUE))))
#1 / ( 1 + exp(-(-7:7)))
}
