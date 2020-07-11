library(graph)
library(igraph)

###### Primeiro definimos origem e destino de uma lista conhecida

origem = "Jarinu"
destino = "Santana de Parnaíba"

###### Definindo os caminhos(edges) entre cidades de origem e destino

edges=c(
  "Cajamar", "Francisco Morato",
  "Cajamar", "Jundiaí",
  "Cajamar", "Santana de Parnaíba", 
  "Cajamar", "São Paulo",
  "Jundiaí", "Louveira", 
  "Jundiaí", "Várzea Paulista", 
  "Jundiaí", "Itatiba",
  "Louveira", "Itatiba",
  "Atibaia", "Itatiba",
  "Guarulhos", "São Paulo",
  "Itatiba", "Jarinu",
  "Jarinu", "Atibaia",
  "Mairiporã", "Atibaia",
  "Mairiporã", "Guarulhos",
  "Campo Limpo Paulista", "Várzea Paulista" ,
  "Campo Limpo Paulista", "Jarinu" ,
  "Campo Limpo Paulista", "Francisco Morato" ,
  "Franco da Rocha", "Francisco Morato" ,
  "Franco da Rocha", "Mairiporã" ,
  "Franco da Rocha", "Caieiras" ,
  "São Paulo", "Caieiras" ,
  "Barueri", "Santana de Parnaíba",
  "Barueri", "São Paulo"
)

dist = graph(edges,directed=FALSE)

###### Definindo distancias entre cidades de origem e destino
E(dist)$weight = c(
  24,
  27.5,
  18.3,
  43.5,
  15.3,
  9.9,
  27.1,
  20.8,
  37,
  20.7,
  28.2,
  26.1,
  28.3,
  23.8,
  7.7,
  17.7,
  23,
  10.1,
  19.8,
  6.8,
  38.6,
  12.7,
  32.5
  )

###### calculando distância entre o meno caminho
distancia = distances(dist,V(dist)$name==origem,V(dist)$name==destino)

###### definindo menor caminho
caminho = shortest_paths(dist,V(dist)$name==origem,V(dist)$name==destino, output=c("both"))

###### conferir de foi o menor caminho
#caminho$vpath

###### Destacando os vértices percorridos

for(i in 1:length(V(dist))){
  V(dist)$color[i] <- ifelse(i  %in% as.vector(unlist(caminho$vpath)) ,"#FFB74D","#B39DDB")
  V(dist)$frame.color[i] <- "white"
  V(dist)$label.font[i] <- ifelse(i  %in% as.vector(unlist(caminho$vpath)) ,2,2)
  V(dist)$label.cex [i] <- ifelse(i  %in% as.vector(unlist(caminho$vpath)) ,1,0.8)
  V(dist)$label.color[i] <- ifelse(i  %in% as.vector(unlist(caminho$vpath)) ,"#EF6C00", "#4527A0")
  V(dist)$label.family[i] <- "sans"
  V(dist)$size[i] <- 0
}

###### Destacando os caminhos percorridos

for(i in 1:length(E(dist))){
  E(dist)$label.font[i] <- 2
  E(dist)$label.cex [i] <- ifelse(i  %in% as.vector(unlist(caminho$vpath)) ,0.8, 0.8)
  E(dist)$lty[i] <- ifelse(i  %in% as.vector(unlist(caminho$epath)) , 1, 5)
  E(dist)$color[i] <- ifelse(i  %in% as.vector(unlist(caminho$epath)) , "#FFB74D","#B39DDB")
  E(dist)$width[i] <- ifelse(i  %in% as.vector(unlist(caminho$epath)) , 2, 1)
  E(dist)$label.color[i] <- ifelse(i  %in% as.vector(unlist(caminho$epath)) , "#EF6C00","#B39DDB")
  E(dist)$curved[i] <- ifelse(i  %in% as.vector(unlist(caminho$epath)) , 0, 0.5)
  E(dist)$family[i] <- "serif"
}

###### Montando o gráfico

plot(
  dist, 
  edge.label = E(dist)$weight, 
  edge.arrow.size=0.2, 
  layout = layout_with_graphopt, 
  frame=TRUE,
  main= paste("Menor caminho entre ",origem ," e ",destino, "(", distancia, " km)" ) ,
  axes=FALSE,ylim=c(-1,1),xlim=c(-1,1), asp = 0
)
