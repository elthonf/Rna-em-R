emf.rna.func.sigmoid.der <- function (x){
    #SIGMOID Compute derivate sigmoid functoon

    fx = emf.rna.func.sigmoid(x)

    f = ( 1.0 - fx ) * fx ;

    return (f)

}
