##Deep-sea origin and depth colonization associated with phenotypic innovations in scleractinian corals.
#Ana N. Campoy, Marcelo M. Rivadeneira, Cristián E. Hernández, Andrew Meade & Chris Venditti

#Reference script to obtain the figures in the main text and the Supplementary Information.
#Consider that some input files may need to be modified, for example, working with a time tree and the median tree (output of a VR model), the node numbers may not match.

#Preliminary analyses

#####

#Supplementary Fig. 2

library(ggplot2)
ascon<-read.table("as_con.txt",sep = "\t",header = T)
head(ascon)
asrep<-read.table("as_rep.txt",sep = "\t",header = T)

as<-ggplot(asrep, aes(x=depth, y=richness))+ geom_bar(stat="identity",fill="cornflowerblue",color="cornflowerblue",width = 5)+
  geom_bar(data=ascon,aes(x=depth, y=richness),stat="identity",fill="darkblue",color="darkblue",width = 5)+
  scale_x_reverse(expand = c(0, 0),limits=c(5750,0))+
  scale_y_continuous(position = "right", expand = c(0, 0), breaks = seq(0,100,20)) +
  coord_flip() +
  xlab("Depth(m)") +ylab("Richness") +ggtitle("(a) AS corals") + theme_bw() +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=11), axis.title.y = element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size=0.5), axis.line.y = element_line(size=0.5))

accon<-read.table("ac_con.txt",sep = "\t",header = T)
acrep<-read.table("ac_rep.txt",sep = "\t",header = T)

ac<-ggplot(acrep, aes(x=depth, y=richness))+ geom_bar(stat="identity",fill="cornflowerblue",colour="cornflowerblue",width = 5)+
  geom_bar(data=accon,aes(x=depth, y=richness),stat="identity",fill="darkblue",colour="darkblue",width = 5)+
  scale_x_reverse(expand = c(0, 0),limits=c(5750,0))+
  scale_y_continuous(position = "right", expand = c(0, 0), breaks = seq(0,100,10)) +
  coord_flip() +
  xlab("Depth(m)") +ylab("Richness") +ggtitle("(b) AC corals") + theme_bw() +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=11), axis.title.y = element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size=0.5), axis.line.y = element_line(size=0.5))

zscon<-read.table("zs_con.txt",sep = "\t",header = T)
zsrep<-read.table("zs_rep.txt",sep = "\t",header = T)

zs<-ggplot(zsrep, aes(x=depth, y=richness))+ geom_bar(stat="identity",fill="cornflowerblue",color="cornflowerblue",width = 5)+
  geom_bar(data=zscon,aes(x=depth, y=richness),stat="identity",fill="darkblue",colour="darkblue",width = 5)+
  scale_x_reverse(limits=c(310,0),expand = c(0,0))+
  scale_y_continuous(position = "right", expand = c(0, 0), breaks = seq(0,100,10)) +
  coord_flip() +
  xlab("Depth(m)") +ylab("Richness") +ggtitle("(c) ZS corals") + theme_bw() +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=11), axis.title.y = element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size=0.5), axis.line.y = element_line(size=0.5))

zccon<-read.table("zc_con.txt",sep = "\t",header = T)
zcrep<-read.table("zc_rep.txt",sep = "\t",header = T)

zc<-ggplot(zcrep, aes(x=depth, y=richness))+ geom_bar(stat="identity",fill="cornflowerblue",color="cornflowerblue",width = 5)+
  geom_bar(data=zccon,aes(x=depth, y=richness),stat="identity",fill="darkblue",colour="darkblue",width = 5)+
  scale_x_reverse(limits=c(310,0),expand = c(0,0))+
  scale_y_continuous(position = "right", expand = c(0, 0), breaks = seq(0,300,100)) +
  coord_flip() +
  xlab("Depth(m)") +ylab("Richness") +ggtitle("(d) ZC corals") + theme_bw() +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=11), axis.title.y = element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size=0.5), axis.line.y = element_line(size=0.5))

require(gridExtra)
library(grid)
pdf("figure.pdf",width = 13, height = 5)
grid.arrange(as,ac,zs,zc,nrow=1)
dev.off()

#####

#Depth differences in corals with different traits

#####

#Supplementary Fig.3

#inferred data - median and maximum depth
data<-read.table("phyloanova.txt",sep="\t",header=T)
data$trait<-factor(data$trait,levels=c("as","ac","zs","zc"))

library(ggplot2)
my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
                           axis.line.x = element_line(size=1),
                           axis.line.y = element_line(size=1))

#density_plot
one<-ggplot(data, aes(x=med, fill=trait,color=trait)) +geom_density(position = 'identity',alpha=0.5)+
  scale_fill_manual(values=c("cornflowerblue","cyan","maroon3","darkgoldenrod1")) +
  scale_color_manual(values=c("cornflowerblue","cyan","maroon3","darkgoldenrod1"))+
  geom_vline(xintercept=2.710, color="cornflowerblue",linetype="dashed")+
  geom_vline(xintercept=2.499, color="cyan",linetype="dashed")+
  geom_vline(xintercept=1.394, color="maroon3",linetype="dashed")+
  geom_vline(xintercept=1.350, color="darkgoldenrod1",linetype="dashed")+
  my_theme+scale_y_continuous("Density",limits=c(0,12),breaks = seq(0,12,2),expand = c(0,0))+
  scale_x_continuous(expression("Log"[10]*" Median Depth"),limits = c(1.12,3.12),breaks = seq(0,3,0.2))

two<-ggplot(data, aes(x=max, fill=trait,color=trait)) +geom_density(position = 'identity',alpha=0.5)+
  scale_fill_manual(values=c("cornflowerblue","cyan","maroon3","darkgoldenrod1")) +
  scale_color_manual(values=c("cornflowerblue","cyan","maroon3","darkgoldenrod1"))+
  geom_vline(xintercept=2.928, color="cornflowerblue",linetype="dashed")+
  geom_vline(xintercept=2.753, color="cyan",linetype="dashed")+
  geom_vline(xintercept=1.681, color="maroon3",linetype="dashed")+
  geom_vline(xintercept=1.627, color="darkgoldenrod1",linetype="dashed")+
  my_theme+scale_y_continuous("Density",limits=c(0,12),breaks = seq(0,12,2),expand = c(0,0))+
  scale_x_continuous(expression("Log"[10]*" Maximum Depth"),limits = c(1.12,3.12),breaks = seq(0,3,0.2))

require(gridExtra)
grid.arrange(one,two, ncol = 2, nrow = 1)

#####

#Evolution of depth differences in corals with different traits

#####

#Supplementary Table 5

#MAXIMUM DEPTH

#first read the second file where the input data is
data=read.delim(file="depthlat2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

###plot results Slopes&Intercepts as 0.5
#alfa,b1:as; b2,b3:ac; b4,b5:zs; b6,b7:zc.

#as - alfa, b1
a<-data[data[,"ac"]=="0",]
a<-a[a[,"zs"]=="0",]
a<-a[a[,"zc"]=="0",]

lat<- a[,"lat"]
lat=seq(min(lat),max(lat),1)

data2 <- data.frame(lat)
avcoefs <- read.delim(file="depthlat.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- a + (b1*(data2$lat[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveA + (aveB1*(data2$lat[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(lat)
max(lat)
min(preds)
max(preds)

par(mar = c(4, 5, 3, 3))
plot(tab[,1], tab[,2], type="n",xlim=c(0,78), ylim=c(0,3.9),las=1, 
     xlab="Latitude", ylab=expression("Log"[10]*" Depth"),axes=F, xaxs="i", yaxs="i", cex.lab=1.3)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("cornflowerblue", alpha = 0.05))
}

.xlim <- c(0,78)
.ylim <- c(0, 3.9)

axis(1, seq(0,80,10), cex.axis=1.3)
axis(2, seq(0,4,0.5), cex.axis=1.3)

# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="darkblue", lwd=2)

#
##
###
##
#

#ac - b2.b3
data=read.delim(file="depthlat2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))
a<-data[data[,"ac"]> 0,]

lat<- a[,"latac"]
lat=seq(min(lat),max(lat),1)

data2 <- data.frame(lat)
avcoefs <- read.delim(file="depthlat_right.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- b2 + (b3*(data2$lat[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveB2 + (aveB3*(data2$lat[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(lat)
max(lat)
min(preds)
max(preds)

#par(mar = c(4, 4, 3, 3))
#plot(tab[,1], tab[,2], type="n",xlim=c(1,72), ylim=c(1, 3.9),las=1, 
#     xlab="Lat", ylab="Depth",axes=F, xaxs="i", yaxs="i", cex.lab=1)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("cyan", alpha = 0.05))
}


#.xlim <- c(0,42)
#.ylim <- c(-1.3, 3)

#axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(1, lwd=0, lwd.ticks=1, cex.axis=0.8)
#axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=0.8)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="cyan4", lwd=2)

#
##
###
##
#

#zs - b4.b5
data=read.delim(file="depthlat2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))
a<-data[data[,"zs"]>0,]

lat<- a[,"latzs"]
lat=seq(min(lat),max(lat),1)

data2 <- data.frame(lat)
avcoefs <- read.delim(file="depthlat_right.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- b4 + (b5*(data2$lat[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveB4 + (aveB5*(data2$lat[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(lat)
max(lat)
min(preds)
max(preds)

#par(mar = c(4, 4, 3, 3))
#plot(tab[,1], tab[,2], type="n",xlim=c(5,40), ylim=c(-0.9, 1.2),las=1, 
#     xlab="Lat", ylab="Depth",axes=F, xaxs="i", yaxs="i", cex.lab=1)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("maroon3", alpha = 0.05))
}


#.xlim <- c(5,40)
#.ylim <- c(-0.9, 1.2)

#axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(1, lwd=0, lwd.ticks=1, cex.axis=0.8)
#axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=0.8)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="magenta4", lwd=2)

#
##
###
##
#

#zc - b6.b7
data=read.delim(file="depthlat2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))
a<-data[data[,"zc"]>0,]

lat<- a[,"latzc"]
lat=seq(min(lat),max(lat),1)

data2 <- data.frame(lat)
avcoefs <- read.delim(file="depthlat_right.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- b4 + (b5*(data2$lat[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveB4 + (aveB5*(data2$lat[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(lat)
max(lat)
min(preds)
max(preds)

#par(mar = c(4, 4, 3, 3))
#plot(tab[,1], tab[,2], type="n",xlim=c(10,56), ylim=c(1, 2.4),las=1, 
#xlab="Lat", ylab="Depth",axes=F, xaxs="i", yaxs="i", cex.lab=1)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("darkgoldenrod1", alpha = 0.05))
}


#.xlim <- c(5,40)
#.ylim <- c(-0.8, 1.2)

#axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(1, lwd=0, lwd.ticks=1, cex.axis=0.8)
#axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=0.8)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="darkgoldenrod4", lwd=2)

#
##
###
##
#

#MEDIAN DEPTH

#first read the second file where the input data is
data=read.delim(file="depthlat2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

###plot results Slopes&Intercepts as 0.5
#alfa,b1:as; b2,b3:ac; b4,b5:zs; b6,b7:zc.

#as - alfa, b1
a<-data[data[,"ac"]=="0",]
a<-a[a[,"zs"]=="0",]
a<-a[a[,"zc"]=="0",]

lat<- a[,"lat"]
lat=seq(min(lat),max(lat),1)

data2 <- data.frame(lat)
avcoefs <- read.delim(file="depthlat.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- a + (b1*(data2$lat[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveA + (aveB1*(data2$lat[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(lat)
max(lat)
min(preds)
max(preds)

par(mar = c(4, 5, 3, 3))
plot(tab[,1], tab[,2], type="n",xlim=c(0,45), ylim=c(0,3.8),las=1, 
     xlab="Latitude", ylab=expression("Log"[10]*" Depth"),axes=F, xaxs="i", yaxs="i", cex.lab=1.3)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("cornflowerblue", alpha = 0.05))
}

.xlim <- c(0,45)
.ylim <- c(0, 3.8)

axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1.3)
axis(1, lwd=0, lwd.ticks=1, cex.axis=1)
axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1.3)
axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=1)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="darkblue", lwd=2)

#
##
###
##
#

#ac - b2.b3
data=read.delim(file="depthlat2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))
a<-data[data[,"ac"]> 0,]

lat<- a[,"latac"]
lat=seq(min(lat),max(lat),1)

data2 <- data.frame(lat)
avcoefs <- read.delim(file="depthlat_right.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- b2 + (b3*(data2$lat[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveB2 + (aveB3*(data2$lat[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(lat)
max(lat)
min(preds)
max(preds)

#par(mar = c(4, 4, 3, 3))
#plot(tab[,1], tab[,2], type="n",xlim=c(0,42), ylim=c(-1.3, 3),las=1, 
#     xlab="Lat", ylab="Depth",axes=F, xaxs="i", yaxs="i", cex.lab=1)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("cyan", alpha = 0.05))
}


#.xlim <- c(0,42)
#.ylim <- c(-1.3, 3)

#axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(1, lwd=0, lwd.ticks=1, cex.axis=0.8)
#axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=0.8)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="cyan4", lwd=2)

#
##
###
##
#

#zc - b6.b7
data=read.delim(file="depthlat2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))
a<-data[data[,"zc"]>0,]

lat<- a[,"latzc"]
lat=seq(min(lat),max(lat),1)

data2 <- data.frame(lat)
avcoefs <- read.delim(file="depthlat_right.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- b4 + (b5*(data2$lat[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveB4 + (aveB5*(data2$lat[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(lat)
max(lat)
min(preds)
max(preds)

#par(mar = c(4, 4, 3, 3))
#plot(tab[,1], tab[,2], type="n",xlim=c(5,40), ylim=c(-0.8, 1.2),las=1, 
#xlab="Lat", ylab="Depth",axes=F, xaxs="i", yaxs="i", cex.lab=1)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("darkgoldenrod1", alpha = 0.05))
}


#.xlim <- c(5,40)
#.ylim <- c(-0.8, 1.2)

#axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(1, lwd=0, lwd.ticks=1, cex.axis=0.8)
#axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=0.8)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="darkgoldenrod4", lwd=2)

#
##
###
##
#

#zs - b4.b5
data=read.delim(file="depthlat2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))
a<-data[data[,"zs"]>0,]

lat<- a[,"latzs"]
lat=seq(min(lat),max(lat),1)

data2 <- data.frame(lat)
avcoefs <- read.delim(file="depthlat_right.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- b4 + (b5*(data2$lat[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveB4 + (aveB5*(data2$lat[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(lat)
max(lat)
min(preds)
max(preds)

#par(mar = c(4, 4, 3, 3))
#plot(tab[,1], tab[,2], type="n",xlim=c(5,40), ylim=c(-0.9, 1.2),las=1, 
#     xlab="Lat", ylab="Depth",axes=F, xaxs="i", yaxs="i", cex.lab=1)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("maroon3", alpha = 0.05))
}


#.xlim <- c(5,40)
#.ylim <- c(-0.9, 1.2)

#axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(1, lwd=0, lwd.ticks=1, cex.axis=0.8)
#axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=0.8)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="magenta4", lwd=2)

#
##
###
##
#

#Supplementary Fig. 4



#####

#####

#Supplementary Fig. 4

#Colouring the branches of the scaled tree
library(ape)
library(phytools)

#Create file with the two trees:time and rates
tree1=read.nexus("MCCtree.trees")
plot(tree1)
write.tree(tree1, "tree1.trees")

tree2=read.nexus("mediantree.trees")
plot(tree2)
write.tree(tree2, "tree2.trees")

#read file with both trees=1-time,2-max
trees=read.tree('trees.tre')
#is everything OK
test = matchNodes(trees[[1]], trees[[2]])
test

Branchesmed=log(trees[[2]]$edge.length/trees[[1]]$edge.length)

#Color branches (blue=decelerated, grey=r1, red=accelerated) [remember log1=0]
dat1=Branchesmed[which(Branchesmed>0)]
colourtab1 = data.frame(scale = seq(from = min(dat1), to = max(dat1), by = (max(dat1)-min(dat1))/15), 
                        col = colorRampPalette(c("#FFF2F2", "red", "darkred"))(16), stringsAsFactors = F)

cols1 = rep("grey", length(dat1))

for (v in 1: length(cols1)){
  val = as.numeric(dat1[v])
  # If this value is exactly in the colour table, colour match
  if(val %in% colourtab1$scale) col = colourtab1$col[colourtab1$scale == val] else       
    
    # Otherwise, find the closest value (rounding down)
    col = colourtab1$col[max(which(colourtab1$scale < val))]
  # Save this colour
  cols1[v] = col
}

BB1=rep("grey",length(Branchesmed))
BB1[which(Branchesmed<0)]="dodgerblue"
BB1[which(Branchesmed>0)]=cols1

pdf('Colorbranches.pdf',width=6,height=7,bg="transparent",useDingbats=F)

par(mar=c(1,1,1,1))
par(bg="transparent")

plot((trees[[2]]),edge.color=BB1,show.tip.label=F,edge.width=1, main="Max depth")

library(itsadug)

gradientLegend(valRange=c(min(dat1),max(dat1)),col = c("dodgerblue","grey",colorRampPalette(c("#FFF2F2", "red", "darkred"))(16)),
               border.col = F,pos=.125, side=4, inside=FALSE, pos.num=2)
dev.off()

#####

#Diversification of scleractinian corals along the depth gradient

#####

#Supplementary Table 6

#Get the path

library(ape)
tree<-read.nexus("mediantree.trees")
plot(tree, show.tip.label = F)
#take the path and the nodes number
library(adephylo)
path<-distRoot(tree,tips = "all", method ="patristic")
write.table(path,file = "path.txt")

tree<-read.nexus("MCCtree.trees")
node<-distRoot(tree,tips="all",method="nNodes")
write.table(node,file = "node.txt")

#get DR by ES
library(picante)
ES<-evol.distinct(tree,type = "equal.splits",scale = FALSE, use.branch.lengths = TRUE)
write.table(ES,file = "ES.txt")

#
##
###
##
#

#Plot the trends

#first read the second file where the input data is
data=read.delim(file="depthpath2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

###plot results Slopes&Intercepts as 0.5
#alfa,b1:as; b2,b3:ac; b4,b5:zs; b6,b7:zc.

#as - alfa, b1
a<-data[data[,"ac"]=="0",]
a<-a[a[,"zs"]=="0",]
a<-a[a[,"zc"]=="0",]

path<- a[,"path"]
lat=seq(min(path),max(path),2)

data2 <- data.frame(path)
avcoefs <- read.delim(file="depthpath.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- a + (b1*(data2$path[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveA + (aveB1*(data2$path[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(path)
max(path)
min(preds)
max(preds)

par(mar = c(4, 5, 3, 3))
plot(tab[,1], tab[,2], type="n",xlim=c(100,2955), ylim=c(0, 3),las=1, 
     xlab="Path-wise rate", ylab=expression("Log"[10]*" Depth"),axes=F, xaxs="i", yaxs="i", cex.lab=1.3)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("cornflowerblue", alpha = 0.05))
}


.xlim <- c(100,2955)
.ylim <- c(0,3)

axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1.3)
axis(1, lwd=0, lwd.ticks=1, cex.axis=1)
axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1.3)
axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=1)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="darkblue", lwd=2)

rm(list=ls())

#
##
####
##
#

#ac - b2.b3
data=read.delim(file="depthpath2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))
a<-data[data[,"ac"]>0,]

path<- a[,"acpath"]
path=seq(min(path),max(path),2)

data2 <- data.frame(path)
avcoefs <- read.delim(file="depthpath_right.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- b2 + (b3*(data2$path[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveB2 + (aveB3*(data2$path[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(path)
max(path)
min(preds)
max(preds)

#par(mar = c(4, 4, 3, 3))
#plot(tab[,1], tab[,2], type="n",xlim=c(409,2952), ylim=c(1.02, 3),las=1, 
#     xlab="Lat", ylab="Depth",axes=F, xaxs="i", yaxs="i", cex.lab=1)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("cyan", alpha = 0.05))
}


#.xlim <- c(409,2952)
#.ylim <- c(1.02, 3)

#axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(1, lwd=0, lwd.ticks=1, cex.axis=0.8)
#axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=0.8)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="cyan4", lwd=2)

rm(list=ls())

#
##
####
##
#

#zs - b4.b5
data=read.delim(file="depthpath2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))
a<-data[data[,"zs"]>0,]

path<- a[,"zspath"]
path=seq(min(path),max(path),2)

data2 <- data.frame(path)
avcoefs <- read.delim(file="depthpath_right.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- b4 + (b5*(data2$path[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveB4 + (aveB5*(data2$path[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(path)
max(path)
min(preds)
max(preds)

#par(mar = c(4, 4, 3, 3))
#plot(tab[,1], tab[,2], type="n",xlim=c(409,2268), ylim=c(0.4,2.5),las=1, 
#     xlab="Lat", ylab="Depth",axes=F, xaxs="i", yaxs="i", cex.lab=1)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("maroon3", alpha = 0.05))
}


#.xlim <- c(409,2268)
#.ylim <- c(0.4,2.5)

#axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(1, lwd=0, lwd.ticks=1, cex.axis=0.8)
#axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=0.8)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="magenta4", lwd=2)

#
##
###
##
#

#zc - b6.b7
data=read.delim(file="depthpath2.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))
a<-data[data[,"zc"]>0,]

path<- a[,"zcpath"]
path=seq(min(path),max(path),10)

data2 <- data.frame(path)
avcoefs <- read.delim(file="depthpath_right.txt", header=T,sep="\t", stringsAsFactors=F, na.strings=c(NA,"NA"))

#make the matrix for the predictions - avecoefs is my table with the posterior of coefficients
preds <- matrix(ncol=nrow(avcoefs), nrow=nrow(data2))

#Calculate the predicted y for every iteration of coefficients in a loop
for(k in 1:ncol(preds))
  
{
  a <- avcoefs$Alpha[k]
  b1 <- avcoefs$Beta_1[k]
  b2 <- avcoefs$Beta_2[k]
  b3 <- avcoefs$Beta_3[k]
  b4 <- avcoefs$Beta_4[k]
  b5 <- avcoefs$Beta_5[k]
  b6 <- avcoefs$Beta_6[k]
  b7 <- avcoefs$Beta_7[k]
  
  for(i in 1:nrow(data2))
    
  {
    #here you are multiplying each coefficient by the value in the data ie B1*X1 etc
    #First calculate y (these will be your predicted y values)
    #These are the values of the output (a,b1,b2...) and the mean of the data for each group
    y <- b4 + (b5*(data2$path[i]))
    
    #then put your predicted y value in to the correct column and row of the 'preds' table
    preds[i,k] <- y
    
  }
}

#Redo the above but using the MEAN coefficients now
#calculate the mean predicted y values
meanpreds <- matrix(ncol=1, nrow=nrow(data2))

aveA <- mean(avcoefs$Alpha)
aveB1 <- mean(avcoefs$Beta_1)
aveB2 <- mean(avcoefs$Beta_2)
aveB3 <- mean(avcoefs$Beta_3)
aveB4 <- mean(avcoefs$Beta_4)
aveB5 <- mean(avcoefs$Beta_5)
aveB6 <- mean(avcoefs$Beta_6)
aveB7 <- mean(avcoefs$Beta_7)

for(i in 1:nrow(data2))
  
{
  y <- aveB4 + (aveB5*(data2$path[i]))
  meanpreds[i,] <- y
  
}


tab <- cbind(data2, preds)
tab <- cbind(tab, meanpreds)

##PLOT
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha <- floor(255*alpha)
  newColor <- col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor <- apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
#Get a window
min(path)
max(path)
min(preds)
max(preds)

#par(mar = c(4, 4, 3, 3))
#plot(tab[,1], tab[,2], type="n",xlim=c(5,40), ylim=c(-0.8, 1.2),las=1, 
#     xlab="Lat", ylab="Depth",axes=F, xaxs="i", yaxs="i", cex.lab=1)

#loop through the orni section of the predictions and plot them (so you want to the 'preds' section of tab)
for(i in 2:(ncol(tab)-1))
{
  lines(tab[2:nrow(tab),i] ~ tab[2:nrow(tab),1], col=makeTransparent("darkgoldenrod1", alpha = 0.05))
}


#.xlim <- c(5,40)
#.ylim <- c(-0.8, 1.2)

#axis(1, at=.xlim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(1, lwd=0, lwd.ticks=1, cex.axis=0.8)
#axis(2, at=.ylim, labels=c("",""), lwd.ticks=0, cex.axis=1)
#axis(2, lwd=0, lwd.ticks=1, las=2, cex.axis=0.8)


# plot the mean predictions
lines(tab[2:nrow(tab),ncol(tab)] ~ tab[2:nrow(tab),1], col="darkgoldenrod4", lwd=2)

#
##
###
##
#


#####

#####

#Fig.2

library(phytools)
tree <- read.nexus(file="timetree.trees")
plotTree(tree)

#tips
data<-read.table("traits.txt",sep="\t",header = F)
class(data)

matrix<-as.matrix(data[,2:3])
rownames(matrix)<-data[,1]
colnames(matrix)<-NULL

#internal nodes
data2<-read.table("traits2.txt",sep="\t",header = F)
class(data2)

internal<-as.matrix(data2[,2:3])
rownames(internal)<-data2[,1]
colnames(internal)<-NULL

phylomorphospace(tree,matrix,internal,xlim=rev(range(0,415.80)),xlab="Time (MYA)", ylab=expression("Log"[10]*" Depth"),label="off")

##Tree edge - to add colors
edge<-tree$edge
write.table(edge, file = "edge.txt")

###Create the colors
#col.node: a vector of node colors.
cols1<-read.table("colnodes.txt",sep="\t",header = F)
class(cols1)
cols1a<-as.vector(cols1[,3])
names(cols1a)<-cols1[,1]
#cols1b<-list("col.node"=cols1a)

#col.edge:a vector of edge colors.
cols2<-read.table("coledge.txt",sep="\t",header = F)
class(cols2)
cols2a<-as.vector(cols2[,4])
names(cols2a)<-cols2[,2]
#cols2b<-list("col.edge"=cols2a)

cols1b2b<-list("col.node"=cols1a,"col.edge"=cols2a)

#

par(mar=c(4,4,1,1))
phylomorphospace(tree,matrix,internal,xlim=rev(range(0,415.80)),xlab="Time (MYA)", ylab=expression("Log"[10]*" Depth"),label="off",control = cols1b2b)

###

alldata<-read.table("all_data.txt",sep="\t",header=T)

alldata$Branch<-factor(alldata$Branch,levels=c("as","ac","zc","zs","trans1","trans2","unc1","unc2"))

a<-ggplot(subset(alldata,Scalar_med<1.01),aes(x=time_desc,y=med_Desc))+
  geom_pointrange(aes(ymin=med_Desc-sd_min,ymax=med_Desc+sd_max,fill=factor(Branch),color=factor(Branch)),size=0.1,shape=21)+
  geom_point(aes(fill=factor(Branch)),shape=21,color="black",size=1.7,stroke=0.3)+
  scale_fill_manual(values = c("darkblue","cyan","orange","red","orchid1","maroon1","green","darkgreen"))+
  scale_color_manual(values = c("darkblue","cyan","orange","red","orchid1","maroon1","green","darkgreen"))+
  scale_x_reverse("Time (MYA)",limits=c(430,-5),expand = c(0, 0),breaks = seq(from=0, to= 420,by=50))+
  labs(y = expression("Log"[10]*" Depth"))+
  scale_y_continuous(expand = c(0, 0),limits=c(0,4),breaks = seq(0, 4, by = 0.5))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
                    axis.line.x = element_line(size=1),
                    axis.line.y = element_line(size=1),
                    panel.background = element_rect(fill = "transparent",colour = NA))

b<-ggplot(subset(alldata,Scalar_med>1.0),aes(x=time_desc,y=med_Desc))+
  geom_pointrange(aes(ymin=med_Desc-sd_min,ymax=med_Desc+sd_max,fill=factor(Branch),color=factor(Branch)),size=0.1,shape=21)+
  geom_point(aes(fill=factor(Branch)),shape=21,color="black",size=1.7,stroke=0.3)+
  scale_fill_manual(values = c("darkblue","cyan","orange","red","orchid1","maroon1","green","darkgreen"))+
  scale_color_manual(values = c("darkblue","cyan","orange","red","orchid1","maroon1","green","darkgreen"))+
  scale_x_reverse("Time (MYA)",limits=c(430,-5),expand = c(0, 0),breaks = seq(from=0, to= 420,by=50))+
  labs(y = expression("Log"[10]*" Depth"))+
  scale_y_continuous(expand = c(0, 0),limits=c(0,4),breaks = seq(0, 4, by = 0.5))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
                    axis.line.x = element_line(size=1),
                    axis.line.y = element_line(size=1),
                    panel.background = element_rect(fill = "transparent",colour = NA))

require(gridExtra)
grid.arrange(a,b, ncol = 1, nrow = 2)


#####

#Lineage-specific evolutionary patterns in depth

#####

#Supplementary Fig.5

#Long-term trends - Background/slow colonization
diff<-read.table("dataBM.txt",sep="\t",header = T)

diff$Branch<-factor(diff$Branch,levels=c("as","zs","zc","trans1","trans2","unc2"), labels = c("AS corals","ZS corals","ZC corals","TransS","TransC","UncC"))

library(ggplot2)
ggplot(diff, aes(x=time, y=depth,color=Branch)) +geom_point(size=3, alpha=.5)+
  scale_color_manual(values = c("darkblue","red","orange","orchid1","maroon1","darkgreen"))+
  #scale_x_reverse("Time (Ma)",limits=c(420,-10),expand = c(0, 0))+ labs(y = expression("Log"[10]*" Depth"))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
                    axis.line.x = element_line(size=1),
                    axis.line.y = element_line(size=1))+
  geom_abline(intercept = 3.123, slope = -0.001, color="darkblue")+
  geom_abline(intercept = 1.799, slope = -0.00125, color="orange")+
  geom_abline(intercept = 3.206, slope = -0.00247, color="darkgreen")

#Long-term trends - Fast colonization
diff<-read.table("dataFAST.txt",sep="\t",header = T)

diff$Branch<-factor(diff$Branch,levels=c("as","ac","zc","trans1","unc2"), labels = c("AS corals","AC corals","ZC corals","TransS","UncC"))

ggplot(diff, aes(x=time, y=depth,color=Branch)) +geom_point(size=3, alpha=.5)+
  scale_color_manual(values = c("darkblue","cyan","orange","orchid1","darkgreen"))+
  scale_x_reverse("Time (Ma)",limits=c(420,-10),expand = c(0, 0))+ labs(y = expression("Log"[10]*" Depth"))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
                    axis.line.x = element_line(size=1),
                    axis.line.y = element_line(size=1))+
  geom_abline(intercept = 3.4896, slope = -0.0018, color="darkblue")+
  geom_abline(intercept = 4.2468, slope = -0.0050, color="cyan")+
  geom_abline(intercept = 1.9389, slope = -0.0016, color="orange")+
  geom_abline(intercept = 3.4040, slope = -0.0024, color="darkgreen")

#####

#Colonization rate in depth

#####

#Supplementary Fig.6

diff<-read.table("graph.txt",sep="\t",header = T)

diff$Branch<-factor(diff$Branch,levels=c("as","ac","zc"), labels = c("AS","AC","ZC"))

ggplot(diff, aes(x=scalar, y=depth,color=Branch)) +geom_point(size=3, alpha=.5)+
  scale_color_manual(values = c("darkblue","cyan","orange"))+
  scale_x_continuous("Rate of evolution",limits=c(0,25),expand = c(0, 0))+ labs(y = expression("Log"[10]*" Depth"))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
                    axis.line.x = element_line(size=1),
                    axis.line.y = element_line(size=1))+
  geom_abline(intercept = 3.11, slope = -0.04, color="darkblue")+
  geom_abline(intercept = 2.78, slope = -0.03, color="cyan")+
  geom_abline(intercept = 1.58, slope = -0.01, color="orange")

#####