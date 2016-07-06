emf.rna.func.tanh.der <- function (x){
    #Tangene Hiperbolica
    #   J = SIGMOID(z) computes the sigmoid of z.

    #g = 1.0 - x^2.0;
    g = 1.0 - emf.rna.func.tanh(x)^2.0;

    return (g);
}
