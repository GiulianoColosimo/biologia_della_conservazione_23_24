n <- 20
t <- 15
i <- 1
connect_matrix <- matrix(NA,
                         nrow = t,
                         ncol = n)
gene_colors <- sample(colors(), n, replace = F)
connect_matrix[1,] <- gene_colors
while(i < t){
  connect_matrix[i+1,] <- sample(connect_matrix[i,],
                                 n, 
                                 replace = T)
  i <- i + 1
}
plot(expand.grid(1:n, 1:t)[,1],
     expand.grid(1:n, 1:t)[,2],
     asp = 1,
     frame.plot = F,
     ann = F,
     axes = F, type = "n")
points(expand.grid(1:n, 1:t)[,1],
       expand.grid(1:n, 1:t)[,2],
       pch = 21, 
       cex = 3, 
       col = "black",
       bg = t(connect_matrix))



connection_list <- vector(mode = "list", length = n)
names(connection_list) <- gene_colors
for(i in gene_colors){
  connection_list[[i]] <- which(connect_matrix == i, arr.ind = T)[
    order(which(connect_matrix == i, arr.ind = T)[,1]), ]
}
for
colnames(connection_list[[1]])

lapply(connection_list, function(x){colnames(x) <- c("y", "x")})


connection_list["gray76"]


gene_colors[1]




connection_list[i] <- which(connect_matrix == i, arr.ind = T)[
  order(which(connect_matrix == i, arr.ind = T)[,1]), ]

segment_matrix <- matrix()
for(clr in gene_colors){
  segment_matrix <- which(connect_matrix == clr, arr.ind = T)[
    order(which(connect_matrix == clr, arr.ind = T)[,1]), ]
  colnames(segment_matrix) <- c("y", "x")
  
  for(i in rev(2:max(segment_matrix[,1]))){
    segments(x0 = segment_matrix[segment_matrix[, "y"] == i, "x"],
             y0 = segment_matrix[, "y"][segment_matrix[, "y"] == i],
             x1 = sample(segment_matrix[segment_matrix[, "y"] == i-1, "x"],
                         length(segment_matrix[segment_matrix[, "y"] == i-1, "x"]),
                         replace = T), 
             y1 = sample(segment_matrix[, "y"][segment_matrix[, "y"] == i-1],
                         length(segment_matrix[, "y"][segment_matrix[, "y"] == i-1]),
                         replace = T)
    )
  }
  
  segment_matrix <- matrix()
}




segment_matrix <- which(connect_matrix == "brown2", arr.ind = T)[
  order(which(connect_matrix == "brown2", arr.ind = T)[,1]), ]
colnames(segment_matrix) <- c("y", "x")







segment_matrix <- which(connect_matrix == "lightskyblue3", arr.ind = T)[
  order(which(connect_matrix == "lightskyblue3", arr.ind = T)[,1]), ]
colnames(segment_matrix) <- c("y", "x")













rev(2:max(segment_matrix[,1]))

segment_matrix <- t(segment_matrix)













for(clr in gene_colors){
  for(g in 2:n){
    for(e in 2:t){
      segments(x0 = which(connect_matrix[g-1, ] == clr),
               y0 = e-1,
               x1 = which(connect_matrix[g, ] == clr),
               y1 = e)
    }
  }
}






head(cbind(expand.grid(1:n, 1:t), as.vector(t(connect_matrix))))


segments(x0 = 1,
         y0 = 1,
         x1 = c(1, 16, 19),
         y1 = 2)

which(connect_matrix[1,][] == "mistyrose1")
which(connect_matrix[,1][] == "mistyrose1")




segments(x0 = which(t(connect_matrix)[1,][] == "mistyrose1"),
         y0 = which(t(connect_matrix)[,1][] == "mistyrose1"),
         x1 = which(t(connect_matrix)[2,][] == "mistyrose1"),
         y1 = which(t(connect_matrix)[,2][] == "mistyrose1")
         )




t(connect_matrix)[1,]


which(t(connect_matrix[1, ] == "mistyrose1"))
which(connect_matrix[, 1] == "mistyrose1")


which(connect_matrix == "mistyrose1")



which(connect_matrix[2, ] == "mistyrose1")


t(which(connect_matrix == "mistyrose1", arr.ind = T))





for(i in gene_colors){
  points(which(connect_matrix == i, arr.ind = T)[,1],
         which(connect_matrix == i, arr.ind = T)[,2],
         pch = 21, 
         cex = 5, 
         col = "black",
         bg = i)
}

which(connect_matrix == "dodgerblue2", arr.ind = T)



for(i in gene_colors){
  print(i)
}







for(e in 1:n){
  
}







connect_matrix[which(connect_matrix == i, arr.ind = T), ]


which(connect_matrix == "pink3", arr.ind = T)[,1]

plot(x = 1:n, 
     y = rep(t, n), 
     pch = 21, 
     cex = 5, 
     col = "black",
     bg = genes_color,
     ylim = c(0, 11),
     asp = 1,
     frame.plot = F,
     ann = F,
     axes = F)
genes_color <- sample(genes_color, replace = T)
points(x = 1:n, 
       y = rep(t+1, n), 
       pch = 21, 
       cex = 5, 
       col = "black",
       bg = genes_color)






colors()



plot(x=1:10, y=rep(1,10), pch=21, cex=2.7, col = "black",
     bg = c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"),
     ylim = c(0, 11), asp = 1, frame.plot = F, ann = F, axes = F)
points(x=1:10, y=rep(2, 10), pch=21, cex=2.7, col = "black",
       bg= c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"))
points(x=1:10, y=rep(3, 10), pch=21, cex=2.7, col = "black",
       bg= c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"))
points(x=1:10, y=rep(4, 10), pch=21, cex=2.7, col = "black",
       bg= c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"))
points(x=1:10, y=rep(5, 10), pch=21, cex=2.7, col = "black",
       bg= c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"))
points(x=1:10, y=rep(6, 10), pch=21, cex=2.7, col = "black",
       bg= c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"))
points(x=1:10, y=rep(7, 10), pch=21, cex=2.7, col = "black",
       bg= c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"))
points(x=1:10, y=rep(8, 10), pch=21, cex=2.7, col = "black",
       bg= c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"))
points(x=1:10, y=rep(9, 10), pch=21, cex=2.7, col = "black",
       bg= c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"))
points(x=1:10, y=rep(10, 10), pch=21, cex=2.7, col = "black",
       bg= c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"))
points(x=1:10, y=rep(11, 10), pch=21, cex=2.7, col = "black",
       bg= c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray"))
text(x = -1.5, y = 1, labels = expression('Present (t'[i]*')'))
text(x = -1, y = 11, labels = expression('Past (t'[0]*')'))
text(x = 11.5, y = 11, labels = expression('G'[0]))
text(x = 11.5, y = 10, labels = expression('G'[1]))
text(x = 11.5, y = 9, labels = expression('G'[2]))
text(x = 11.5, y = 7, labels = ":")
text(x = 11.5, y = 6, labels = ":")
text(x = 11.5, y = 5, labels = ":")
text(x = 11.5, y = 2, labels = expression('G'[i-1]))
text(x = 11.5, y = 1, labels = expression('G'[i]))



n <- 20
t <- 15
i <- 1
connect_matrix <- matrix(NA,
                         nrow = t,
                         ncol = n)
gene_colors <- sample(colors(), n, replace = F)
connect_matrix[1,] <- gene_colors
while(i < t){
  connect_matrix[i+1,] <- sample(connect_matrix[i,],
                                 n, 
                                 replace = T)
  i <- i + 1
}
plot(expand.grid(1:n, 1:t)[,1],
     expand.grid(1:n, 1:t)[,2],
     asp = 1,
     frame.plot = F,
     ann = F,
     axes = F, type = "n")
points(expand.grid(1:n, 1:t)[,1],
       expand.grid(1:n, 1:t)[,2],
       pch = 21, 
       cex = 2, 
       col = "black",
       bg = t(connect_matrix))
text(x = -1.5, y = max(1:t), labels = expression('Present (t'[i]*')'))
text(x = -1, y = min(1:t), labels = expression('Past (t'[0]*')'))
labelingGen <- t
while(labelingGen > 0){
  text(x = max(1:n) + 1.5, y = labelingGen,
       labels = paste("Gen.", labelingGen, sep = " "))
  labelingGen <- labelingGen-1
}
