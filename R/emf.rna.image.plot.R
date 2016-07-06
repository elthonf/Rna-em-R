emf.rna.image.plot <- function(img){
    if(is.list(img)) img = unlist(img);

    img.matrix = matrix(rev( img ), nrow = 20, byrow = TRUE);
    img.matrix = apply(img.matrix, 2, rev);

    image(x = img.matrix, col=gray.colors(100));
}
