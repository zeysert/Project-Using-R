install.packages("igraph")
library("igraph")

setwd("C:\\Users\\zeysert\\Desktop\\Project-Using-R")


df <- read.table("sample.csv", sep=",")

df=df[-(22:255969),]

edges <- data.frame(from=df$V1,
                    to=df$V2,
                    weight = as.numeric(df$V3))  

g <- graph.data.frame(edges, directed = F)
V(g)$type <- V(g)$name %in% edges[,2]
E(g)$weight <- as.numeric(edges[,3])
X <- as_incidence_matrix(g, attr = "weight")

copy <- X
for(i in 1:11){
  for(j in 1:10){
    copy[i , j] = 0
  }
}

one_mkde <- X %*% t(X)


net = graph.adjacency(adjmatrix=one_mkde , mode="undirected" , weighted=TRUE , diag=FALSE)
test.layout <- layout_(net,with_dh(weight.edge.lengths = edge_density(net)/1000))
plot(net,layout = test.layout, edge.label=round(E(net)$weight))

for( i in 1 : 11 ){
  max = 0
  x = 0
  flag = 0
  for(j in 1:11){
    if(i != j){
      if(  one_mkde[i , j] > max) {
        max = one_mkde[i , j] 
        x = j
        flag=1
      }
    }
  }
  if(flag==1){
    for(k in 1 : 10){
      if(x!=0 & X[i , k] == 0 & X[x , k] > 0){
        copy[i , k] = 1
      }
    }
  }
} 
