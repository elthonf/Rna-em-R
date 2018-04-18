require(visNetwork)
nodes <- data.frame(id = 1:13,
                    shape = c( rep("circularImage",3) , rep("box",10) ),
                    #image = 'http://cdn0.iconfinder.com/data/icons/floral-sketchy-icons/128/17-128.png',
                    image = c( "http://image.ibb.co/kiAtcH/elthon.png",
                               "http://cdn1.player.fm/images/16429/series/7Dgkfzfca2ObQz3e/512.jpg",
                               "http://softikbox.com/uploads/images/2a/81/fe/2a81fe5d-d1d3-4cca-ba79-497949a8b957_420x300_fit.jpg",
                               "http://image.ibb.co/kiAtcH/elthon.png",
                               "http://image.ibb.co/kiAtcH/elthon.png",
                               "http://image.ibb.co/kiAtcH/elthon.png",
                               "http://image.ibb.co/kiAtcH/elthon.png",
                               "http://image.ibb.co/kiAtcH/elthon.png",
                               "http://image.ibb.co/kiAtcH/elthon.png",
                               "http://image.ibb.co/kiAtcH/elthon.png",
                               "http://image.ibb.co/kiAtcH/elthon.png",
                               "http://image.ibb.co/kiAtcH/elthon.png",
                               "http://image.ibb.co/kiAtcH/elthon.png" ),
                    color = c("lightblue", "deeppink", "black", "lightblue", "lightblue","lightblue", "lightblue", "lightblue", "lightblue", "deeppink", "deeppink", "deeppink", "deeppink"),
                    label = c("Professor", "MBA+", "R",
                              "E", "L", "T",
                              "H", "O", "N",
                              "F", "I", "A", "P"))

edges <- data.frame(
  from = c(1,1,1,1,1,1,   1, 3, 3,    2, 2, 2, 2),
  to = c(  4,5,6,7,8,9,   2, 1, 2,    10, 11, 12, 13)
)
visNetwork(nodes, edges, width = "100%") #%>%
  #visEdges(arrows = "from") %>%
  #visHierarchicalLayout(direction = "UD",sortMethod = 'directed')


