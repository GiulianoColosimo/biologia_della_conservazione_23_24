set.seed(89)
xcoords <- rnorm(15, mean = 0, sd = 1)
ycoords <- rnorm(15, mean = 0, sd = 1)


png(file = "./classes/6/population10a.png",
    width = 12, height = 8, units = "in", res = 350)
plot(xcoords, ycoords, 
     xlim = c(min(xcoords)-0.5, max(xcoords)+0.5),
     ylim = c(min(ycoords)-0.5, max(ycoords)+0.5),
     pch = 21,
     bg = "gray",
     col = "black",
     asp = 1, cex = 2, 
     frame.plot = F, ann = F, axes = F)
dev.off()


png(file = "./classes/6/population10b.png",
    width = 12, height = 8, units = "in", res = 350)
plot(xcoords, ycoords, 
     xlim = c(min(xcoords)-0.5, max(xcoords)+0.5),
     ylim = c(min(ycoords)-0.5, max(ycoords)+0.5),
     pch = 21,
     bg = sample(c("red", "yellow"), 10, replace = T),
     col = "black",
     asp = 1, cex = 2, 
     frame.plot = F, ann = F, axes = F)
legend("topleft", legend = c("A", "a"), pt.bg = c("red", "yellow"),
       pch = 21, cex = 3, bty = "n")
dev.off()

png(file = "./classes/6/population10c.png",
    width = 12, height = 8, units = "in", res = 350)
plot(xcoords, ycoords, 
     xlim = c(min(xcoords)-0.5, max(xcoords)+0.5),
     ylim = c(min(ycoords)-0.5, max(ycoords)+0.5),
     pch = 21,
     bg = sample(c("red", "yellow"), 10, replace = T),
     col = "black",
     asp = 1, cex = 2, 
     frame.plot = F, ann = F, axes = F)
points(xcoords, 
       ycoords+(sign(ycoords)*0.1),
       pch = 21,
       bg = sample(c("red", "yellow"), 10, replace = T),
       cex = 2)
legend("topleft", legend = c("A", "a"), pt.bg = c("red", "yellow"),
       pch = 21, cex = 3, bty = "n")
dev.off()




polygon((max(xcoords)+1)*sin(seq(0,2*pi,length.out=100)),
        (max(xcoords)+1)*cos(seq(0,2*pi,length.out=100)))


points(xcoords, 
       ycoords+(sign(ycoords)*0.1),
       pch = 21,
       bg = "red",
       cex = 1.5)





r <- 3*runif(30)
degs <- 360*runif(30)

# First you want to convert the degrees to radians
theta <- 2*pi*degs/360

# Plot your points by converting to cartesian
png(file = "./classes/6/genePool.png",
    width = 12, height = 8, units = "in", res = 350)
plot(r*sin(theta),
     r*cos(theta),
     main = "Gene pool",
     xlim=c(-max(r)-.2, max(r)+.2),
     ylim=c(-max(r)-.2, max(r)+.2),
     pch = 21,
     bg = c(rep("red", 19), rep("yellow", 11)),
     col = "black",
     asp = 1, cex = 2, 
     frame.plot = F, ann = T, axes = F, xlab = "", ylab = "")
# Add a circle around the points
polygon((max(r)+.1)*sin(seq(0,2*pi,length.out=100)),
        (max(r)+.1)*cos(seq(0,2*pi,length.out=100)))
dev.off()

png(file = "./classes/6/genePoolb.png",
    width = 12, height = 8, units = "in", res = 350)
plot(r*sin(theta),
     r*cos(theta),
     #main = "Gene pool",
     xlim=c(-max(r)-.2, max(r)+.2),
     ylim=c(-max(r)-.2, max(r)+.2),
     pch = 21,
     bg = c(rep("red", 19), rep("yellow", 11)),
     col = "black",
     asp = 1, cex = 2, 
     frame.plot = F, ann = T, axes = F, xlab = "", ylab = "")
# Add a circle around the points
polygon((max(r)+.1)*sin(seq(0,2*pi,length.out=100)),
        (max(r)+.1)*cos(seq(0,2*pi,length.out=100)))
dev.off()



plot(1,1,
     xlim = c(1,2),
     ylim = c(1,30),
     pch = 21,
     bg = "red",
     frame.plot = F, ann = T, axes = F, xlab = "", ylab = "")






points(r*sin(theta), 
      (r*cos(theta))+(sign((r*cos(theta)))*0.1),
       pch = 21,
       bg = "red",
       cex = 1.5)


set.seed(89)

jxcoords <- jitter(xcoords, amount = .1)
jycoords <- jitter(ycoords, amount = .1)


sign(ycoords)
-(+0.1)



dice<-c(1,2,3,4,5,6)

sample(dice, size = 1)
sim_dice<-sample(dice, size=10000,replace=T)
table(sim_dice)
barplot(table(sim_dice))






par(mai =  c(bottom, left, top, right))


# Create all vector variables that will be used in the function
x <- y <- x.new <- y.new <- x.new.p <- y.new.p <- vector()

nsteps <- 10

# Populate the vectors with values
for(i in 1:nsteps){
  # Initialize variables
  x <- rnorm(1)
  y <- rnorm(1)
  # concatenate variables 
  # to increase the vector size
  x.new <- c(x.new, x)
  y.new <- c(y.new, y)
  # sum the vector numbers
  x.new.p <- cumsum(x.new)
  y.new.p <- cumsum(y.new)  
}





zygote <- data.frame(A = 0,
                     a = 0)
barplot(as.matrix(zygote))

barplot(table(test))


test <- sample(c("A", "a"), size = 1, prob = c(p, q))


n <- 30
i <- 0



bg <- matrix(data = rep("gray", 30), nrow = 15, ncol = 2)
plot(rep(c(1,2), 15),
     sort(rep(1:15, 2)),
     xlim = c(1, 2),
     ylim = c(0,16),
     axes = F, pch = 21, asp = 1, cex = 3,
     bg = bg, ann = F)
axis(1, at = c(1,2), labels = c("Allele 1", "Allele 2"), las = 2)

#axis(2, at = 1:15, labels = 1:15)


zygote <- data.frame(A = 0,
                     a = 0)
barplot(as.matrix(zygote), ylim = c(0, 16))

p <-  0.633
q <- 1-p
n <- 30
i <- 0
bg <- matrix(data = rep("gray", 30), nrow = 15, ncol = 2)
while(i <= n){
  test <- sample(c("A", "a"), size = 1, prob = c(p, q))
  if(test == "A"){
    bg[i] <- "red" 
  } else {
    bg[i] <- "yellow"
  }
  png(file = paste("./classes/6/genePoolHWA",i,".png", sep = ""),
      width = 12, height = 8, units = "in", res = 350)
  plot(rep(c(1,2), 15),
       sort(rep(1:15, 2)),
       xlim = c(1, 2),
       ylim = c(0,16),
       axes = F, pch = 21, asp = 1, cex = 3,
       bg = bg, ann = F)
  axis(1, at = c(1,2), labels = c("Allele 1", "Allele 2"), las = 2)
  dev.off()
  i <- i+1
}

require(stats)
mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)
class(Titanic)

mosaicplot(table(c(rep("A", 13), rep("a", 2)),
      c(rep("A", 6), rep("a", 9))))

gamete1 <- c(rep("A", 13), rep("a", 2))
gamete2 <- c(rep("A", 6), rep("a", 9))
mosaicplot(table(gamete1, gamete2))

library(vcd)
tab <- table(expand.grid(c("A", "a"), c("A", "a")))
dimnames(tab)
lab.tab <- rbind(c("AA", "Aa"), c("Aa", "aa"))
dimnames(lab.tab)
dimnames(tab) <- list("Individual 1" = c("A", "a"),
                      "Individual 2" = c("A", "a"))
dimnames(lab.tab) <- list("Individual 1" = c("A", "a"),
                          "Individual 2" = c("A", "a"))
mosaicplot(tab, color = F, main = "Punnet square", 
           xlab = "Individual 1",
           ylab = "Individual 2")
text(.25, .7, expression("AA = p*p = p"^2))
text(.75, .7, expression("Aa = p*q = pq"))
text(.25, .25, expression("Aa = p*q = pq"))
text(.75, .25, expression("aa = q*q = q"^2))








```{r}
#| output-location: column-fragment
n <- 30
i <- 0
bg <- matrix(data = rep("gray", 30), 
             nrow = 15, ncol = 2)
plot(rep(c(1, 3), 15),
     sort(rep(1:15, 2)),
     xlim = c(1, 3),
     ylim = c(0,16),
     axes = F, pch = 21, 
     asp = 1, cex = 3,
     bg = bg, ann = F)
axis(1, at = c(1, 3), 
     labels = c("Allele 1", "Allele 2"), 
     las = 2)
```


```{r}
#| output-location: column-fragment
p <-  0.633 ; q <- 1-p ; allele <- 
  sample(c("A", "a"), size = 1, prob = c(p, q))
if(allele == "A"){bg[1] <- "red" } else {
  bg[1] <- "yellow"}
plot(rep(c(1, 3), 15), sort(rep(1:15, 2)),
     xlim = c(1, 3), ylim = c(0, 16),
     axes = F, pch = 21, asp = 1, cex = 3,
     bg = bg, ann = F) ; axis(1, at = c(1, 3),
                              las = 2, labels = c("Allele 1", "Allele 2"))
```


- Sampling the alleles one by one




newSample <- sample(c("A", "a"), size = 15, prob = c(p, q),
                   replace = T) 


sample(c("A","a"), size = 15, prob = c(p,q), replace = T)
newSample <- t(sapply(1:10000, function(i){table(factor(sample(c("A","a"), size = 15, prob = c(p,q), replace = T),
                                               levels = c("A","a")))}))

hist(newSample[,"A"]/15, xlim = c(0,1), xlab = "f(A)", breaks = seq(0, 1, by= 0.1),
     col = "darkgoldenrod3", border = NA, main = "10,000 samples of size 15",
     cex.axis = 2, cex.main = 2, cex.lab = 2)
abline(v = 0.637, lty = 2, lwd = 2)



newSample1 <- t(sapply(1:10000, function(i){table(factor(sample(c("A","a"), size = 30, prob = c(p,q), replace = T),
                                                        levels = c("A","a")))}))
newSample2 <- t(sapply(1:10000, function(i){table(factor(sample(c("A","a"), size = 100, prob = c(p,q), replace = T),
                                                         levels = c("A","a")))}))
newSample3 <- t(sapply(1:10000, function(i){table(factor(sample(c("A","a"), size = 1000, prob = c(p,q), replace = T),
                                                         levels = c("A","a")))}))

png(file = "./classes/6/samplingError.png",
    width = 12, height = 8, units = "in", res = 350)
par(mfrow = c(1, 3))
hist(newSample1[,"A"]/30, 
     xlim = c(0,1), xlab = "f(A)", 
     breaks = seq(0, 1, by= 0.1),
     col = "darkgoldenrod3", 
     border = NA, 
     main = "10,000 samples of size 30",
     cex.axis = 2, cex.main = 2, cex.lab = 2)
abline(v = 0.637, lty = 2, lwd = 2)
hist(newSample2[,"A"]/100, 
     xlim = c(0,1), xlab = "f(A)", 
     breaks = seq(0, 1, by= 0.01),
     col = "darkgoldenrod3", 
     border = NA, 
     main = "10,000 samples of size 100",
     cex.axis = 2, cex.main = 2, cex.lab = 2)
abline(v = 0.637, lty = 2, lwd = 2)
hist(newSample3[,"A"]/1000, 
     xlim = c(0,1), xlab = "f(A)", 
     breaks = seq(0, 1, by= 0.01),
     col = "darkgoldenrod3", 
     border = NA, 
     main = "10,000 samples of size 1000",
     cex.axis = 2, cex.main = 2, cex.lab = 2)
abline(v = 0.637, lty = 2, lwd = 2)
dev.off()