emf.rna.runif <- function(n, min = -1, max = 1, ...){

    if(min != -1 || max != 1 ) return ( runif(n = n, min = min, max = max) );

    temp = read.csv("dados/random.csv", header = TRUE);
    colnames(temp) = NULL;
    temp = unlist(temp);

    return( temp[1:n]);
}
