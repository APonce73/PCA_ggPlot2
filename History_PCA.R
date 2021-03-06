library(ade4)
library(ggplot2)
library(ggrepel)
library(tidyverse)

######
###################
#Small function for do PCA 
# pca1 <- dudi.hillsmith(Tabla3$Value1, scannf = F, nf = 5)
# summary(pca1)
#
#  

#Funcion con 5 componentes del PCA
# renglon = a los valores por renglón
# columnas 0 los valores por columnas
# Variables = la variable categ´órica
#Titulo = Es el titulo de la grafica
#Multiplicador =  un valor para re-escalar la gr´áfica
PCbiplot <- function(renglon, columnas, Variables, Eigen, Titulo, multiplicador){
  #pca1
  pca1a <- data.frame(renglon)
  pca1b <- data.frame(columnas)
  Variables <- Variables
  Titulo <- as.character(c(Titulo))
  PC1 <- round(100*(Eigen/sum(Eigen)),1)[1]
  PC2 <- round(100*(Eigen/sum(Eigen)),1)[2]
  
  p1 <- ggplot(pca1a,aes(x = Axis1, y = Axis2)) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "gray70") +
    geom_vline(xintercept = 0, color = "gray70") +
    geom_point(aes(color = Variables), alpha = 0.7, size = 3) +
    xlab(paste(PC1, "%", sep = " ")) +
    ylab(paste(PC2, "%", sep = " ") ) + 
    #xlim(-5, 6) + 
    ggtitle(Titulo) 
  
  p2 <- p1 +
    geom_point(data = pca1b, aes(x = multiplicador*Comp1, y = multiplicador*Comp2), alpha = 0) + 
    geom_label_repel(data = pca1b, aes(x = multiplicador*Comp1, y = multiplicador*Comp2, label = rownames(pca1b)), 
                     box.padding   = 0.7, point.padding = 0.5, size = 4, alpha = 0.7) +
    geom_segment(data =  pca1b, aes(x = 0, y = 0, xend = multiplicador*Comp1, yend = multiplicador*Comp2), arrow = arrow(length = unit(0.2,"cm")), alpha = 0.7, color = "black")
  
  p2
}

#PCbiplot(pca1$li, pca1$co, Tabla3$Var1$Grupos, pca1$eig,"PCA Grupos de K-means", 4)
#

  
  
####################

#Para Metales Stevia
head(Tabla2.1)
names(Tabla2.1$val)
datTranf <- decostand(Tabla2.1$var, "normalize", MARGIN = 2)
datTranf
apply(datTranf,2,sd)
round(apply(datTranf,2,sd),0)
Oba <- adonis(vegdist(datTranf, "euclidean")~Tabla2.1$val$Muestra, permutation = 999)
Oba
par(mfrow = c(1,1))
pca2 <- dudi.pca(datTranf[,], scannf = F, nf = 3)
scatter(pca2)
s.arrow(pca2$c1*2, boxes = T, clabel = 1, xlim = c(-4,4))
pca1$eig
round((pca2$eig/sum(pca2$eig))*100, 1)
s.class(pca2$l1, Tabla2.1$val$Muestra, cellipse = 1, cstar = 1, pch = 1,sub = "Metales Stevia", csub = 0.9, possub = "topleft", add.p = T, clabel = 1.4)


PCbiplot(pca2$li, pca2$co, Tabla2.1$val$Muestra, pca2$eig, "PCA Metales Stevia", 2)


#Para Fisicoquimicos del Suelo

head(Tabla3.1)
names(Tabla3.1$val)
datTranf <- decostand(Tabla3.1$var, "normalize", MARGIN = 2)
datTranf
apply(datTranf,2,sd)
round(apply(datTranf,2,sd),0)
Oba <- adonis(vegdist(datTranf, "euclidean")~Tabla3.1$val$Muestra, permutation = 999)
Oba
par(mfrow = c(1,1))
pca3 <- dudi.pca(datTranf[,], scannf = F, nf = 3)
scatter(pca3)
s.arrow(pca3$c1*5, boxes = T, clabel = 1)
pca3$eig
round((pca3$eig/sum(pca3$eig))*100, 1)
s.class(pca3$l1, Tabla3.1$val$Muestra, cellipse = 1, cstar = 1, pch = 1,sub = "PCA FQ del suelo", csub = 0.9, possub = "topleft", add.p = T, clabel = 1.4)

PCbiplot(pca3$li, pca3$co, Tabla3.1$val$Muestra, pca3$eig,"PCA FQ Suelo", 2)


#Tabla4.1 <- data.frame(Tabla2.1$var, Tabla3.1$var)

#Metales los valores del Suelo

head(Tabla4.1)
names(Tabla4.1)
datTranf <- decostand(Tabla4.1$var, "normalize", MARGIN=2)
datTranf
apply(datTranf,2,sd)
round(apply(datTranf,2,sd),0)
Oba <- adonis(vegdist(datTranf, "euclidean")~Tabla4.1$val$Muestra, permutation = 999)
Oba
par(mfrow = c(1,1))
pca4 <- dudi.pca(datTranf[,], scannf = F, nf = 3)
scatter(pca4)
s.arrow(pca4$c1*4, boxes = T, clabel = 1)
pca4$eig
round((pca4$eig/sum(pca1$eig))*100, 1)
s.class(pca4$l1, Tabla3.1$val$Muestra, cellipse = 1, cstar = 1, pch = 1,sub = "Metales del Suelo", csub = 0.9, possub = "topleft", add.p = T, clabel = 1.4)

PCbiplot(pca4$li, pca4$co, Tabla4.1$val$Muestra, pca4$eig, "PCA Metales Suelo", 2)


#PCA juntando los datos de la Tabla1
dim(Tabla1.1$var)
dim(Tabla2.1$var)

TablaHH <- data.frame(Tabla1.1$var,Tabla2.1$var)
pca5 <- dudi.pca(TablaHH, scannf = F, nf = 3)
PCbiplot(pca5$li, pca5$co, Tabla1.1$val$Muestra, pca5$eig, "PCA Stevia", 2)


#Coinertia Stevia FQ Vs Metales
coin1 <- coinertia(pca1,pca2, scan = FALSE, nf = 2)
coin1

summary(coin1)
plot(coin1)

coin1$eig
coin1$li[,1] > 0.05
coin1$li[,1] > -0.05

#Xuno <- data.frame(coin1$li[,1] > 0.4|coin1$li[,1] < -0.4)
#Xuno <- coin1$li[,1]
#Xuno1 <- apply(Xuno,1,sum)
#Xuno2 <- Xuno1 == 1

#Yuno <- data.frame(coin1$li[,2] > 0.4|coin1$li[,2] < -0.4)
#Yuno <- coin1$li[,2]
#Yuno1 <- apply(Yuno,1,sum)
#Yuno2 <- Yuno1 == 1
dim(coin1$li)

Porc <- (coin1$eig/sum(coin1$eig))*100
Porc

par(mfrow = c(1,1))
s.arrow(1.5*coin1$co[,], xlim = c(-7,7), clabel = 0.5, add.p = F, boxes = T, edge = F,sub = "Fig. 3", csub = 1, possub = "topleft")
s.label(coin1$li, clabel = 0.7, boxes = T, add.p = T)
#text(-0.3,-4,round(Porc[2],1))
#text(-4,0.2,round(Porc[1],1))
s.class((coin1$lY+coin1$lY)/2, Tabla1.1$val$Muestra, pch = 26, cellipse = 1, cstar = 0.3, clabel = 0.7, sub = "Coinercia", csub = 1, possub = "topleft", add.p = T, col = rep("red",9))


TablaHH1 <- data.frame(Tabla3.1$var,Tabla4.1$var)
pca6 <- dudi.pca(TablaHH1, scannf = F, nf = 3)
PCbiplot(pca6$li, pca6$co, Tabla3.1$val$Muestra, pca6$eig, "PCA Suelo", 3)


#Coinertia Suelo FQ Vs Metales
coin1 <- coinertia(pca3,pca4, scan = FALSE, nf = 2)
coin1

summary(coin1)
plot(coin1)

coin1$eig
coin1$li[,1] > 0.05
coin1$li[,1] > -0.05

#Xuno <- data.frame(coin1$li[,1] > 0.4|coin1$li[,1] < -0.4)
#Xuno <- coin1$li[,1]
#Xuno1 <- apply(Xuno,1,sum)
#Xuno2 <- Xuno1 == 1

#Yuno <- data.frame(coin1$li[,2] > 0.4|coin1$li[,2] < -0.4)
#Yuno <- coin1$li[,2]
#Yuno1 <- apply(Yuno,1,sum)
#Yuno2 <- Yuno1 == 1
dim(coin1$li)

Porc <- (coin1$eig/sum(coin1$eig))*100
Porc

par(mfrow = c(1,1))
s.arrow(1.5*coin1$co[,], xlim = c(-7,7), clabel = 0.5, add.p = F, boxes = T, edge = F,sub = "Fig. 3", csub = 1, possub = "topleft")
s.label(coin1$li, clabel = 0.7, boxes = T, add.p = T)
#text(-0.3,-4,round(Porc[2],1))
#text(-4,0.2,round(Porc[1],1))
s.class((coin1$lY+coin1$lY)/2, Tabla3.1$val$Muestra, pch = 26, cellipse = 1, cstar = 0.3, clabel = 0.7, sub = "Coinercia", csub = 1, possub = "topleft", add.p = T, col = rep("red",9))


#PCA de Stevia Sin Piloncillo, ni azúcar mascabado
Tabla1.1

head(Tabla1.1$var)
datTranf1 <- decostand(Tabla1.1$var[1:15,-4], "normalize", MARGIN = 2)
pcaH <- dudi.pca(datTranf1, scannf = F, nf=3)


#PCA de FQStevia vs FQ Suelo

TablaHH2 <- data.frame(Tabla1.1$var[1:15,-4],Tabla3.1$var)
pca7 <- dudi.pca(TablaHH2, scannf = F, nf = 3)
PCbiplot(pca7$li, pca7$co, Tabla3.1$val$Muestra, pca7$eig, "PCA FQ Suelo & Stevia", 3)


#Coinercia FQStevia Vs FQ Suelo
coin1 <- coinertia(pcaH,pca3, scan = FALSE, nf = 2)
coin1

par(mfrow = c(1,1))
s.arrow(1.5*coin1$co[,], xlim = c(-7,7), clabel = 0.5, add.p = F, boxes = T, edge = F,sub = "Fig. 3", csub = 1, possub = "topleft")
s.label(coin1$li, clabel = 0.7, boxes = T, add.p = T)
#text(-0.3,-4,round(Porc[2],1))
#text(-4,0.2,round(Porc[1],1))
s.class((coin1$lY + coin1$lY)/2, Tabla3.1$val$Muestra, pch = 26, cellipse = 1, cstar = 0.3, clabel = 0.7, sub = "Coinercia", csub = 1, possub = "topleft", add.p = T, col = rep("red",9))

#Coinercia FQStevia Vs MetalesSuelo
rm(coin1)
coin1 <- coinertia(pcaH,pca4, scan = FALSE, nf = 2)
coin1

par(mfrow = c(1,1))
s.arrow(1.5*coin1$co[,], xlim = c(-9,9), clabel = 0.5, add.p = F, boxes = T, edge = F,sub = "Fig. 3", csub = 1, possub = "topleft")
s.label(coin1$li, clabel = 0.7, boxes = T, add.p = T)
#text(-0.3,-4,round(Porc[2],1))
#text(-4,0.2,round(Porc[1],1))
s.class((coin1$lY+coin1$lY)/2, Tabla3.1$val$Muestra, pch = 26, cellipse = 1, cstar = 0.3, clabel = 0.7, sub = "Coinercia", csub = 1, possub = "topleft", add.p = T, col = rep("red",9))






