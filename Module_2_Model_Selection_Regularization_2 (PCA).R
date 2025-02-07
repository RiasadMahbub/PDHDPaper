# PCA

# ------------------------------------------
# ------------------------------------------
# load data:
# ------------------------------------------
# ------------------------------------------
data(USArrests)
data = USArrests
states = row.names(data) # extract the state names
variable = names(data) # extract the variable names
head(data)

# ------------------------------------------
# ------------------------------------------
# data standardization
# ------------------------------------------
# ------------------------------------------
for (i in 1:ncol(data)){
  data[,i]=(data[,i]-mean(data[,i]))/sqrt(var(data[,i]))
}
head(data)
plot(data)

# ------------------------------------------
# ------------------------------------------
# PCA using prcomp()
# ------------------------------------------
# ------------------------------------------
pr.out = prcomp(data, scale=FALSE)
print(pr.out)
data
# show the z-matrix:
pr.out$x[,1:2]
plot(data.frame(pr.out$x))

# show the first 2 columns of the Z matrix (if q=2)
pr.out$x[,c(1:2)]

plot(data.frame(pr.out$x[,c(1:2)]))

# biplot:
biplot(pr.out,scale=0)


# ------------------------------------------
# ------------------------------------------
# PCA 3D visualization
# ------------------------------------------
# ------------------------------------------
library(rgl) # package for 3D visualization

plotPCA <- function(x, nGroup, text) {  # user-defined function
    n <- ncol(x) 
    if(!(n %in% c(2,3))) { # check if 2d or 3d
        stop("x must have either 2 or 3 columns")
    }

    fit <- hclust(dist(x), method="complete") # cluster
    groups <- cutree(fit, k=nGroup)

    if(n == 3) { # 3d plot
        plot3d(x, col=groups, type="s", size=1, axes=F)
        axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
        text3d(x,texts=text)
        grid3d("x")
        grid3d("y")
        grid3d("z")
    } else { # 2d plot
        maxes <- apply(abs(x), 2, max)
        rangeX <- c(-maxes[1], maxes[1])
        rangeY <- c(-maxes[2], maxes[2])
        plot(x, col=groups, pch=19, xlab=colnames(x)[1], ylab=colnames(x)[2], xlim=rangeX, ylim=rangeY)
        lines(c(0,0), rangeX*2)
        lines(rangeY*2, c(0,0))
    }
}

plotPCA(pr.out$x[,1:3], 4, text=rownames(data))



#-----------------------------------------------------------
#-----------------------------------------------------------
#A Simulation Study 1 ----------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------

set.seed(100)
x1 = rnorm(50,0,1)
x2 = rnorm(50,x1,2)
x3 = rnorm(50,x2,1)
x4 = rnorm(50,x3,0.5)
x5 = rnorm(50,x4,1)
x6 = rnorm(50,0,1)
x7 = rnorm(50,0,1)
x8 = rnorm(50,0,1)
x9 = rnorm(50,0,1)
x10 = rnorm(50,0,1)
data = cbind(x1, x2, x3, x4, x5,
			x6, x7, x8, x9, x10)
rownames(data) = paste("A",seq(1,50,1),sep="-")

# data standardization
for (i in 1:ncol(data)){
  data[,i]=(data[,i]-mean(data[,i]))/sqrt(var(data[,i]))
}

# PCA using prcomp()
pr.out = prcomp(data, scale=FALSE)
biplot(pr.out,scale=0,var.axes = TRUE)
pr.out

#-----------------------------------------------------------
#-----------------------------------------------------------
#A Simulation Study 2 ----------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------

set.seed(100)
x1 = rnorm(50,0,1)
x2 = rnorm(50,x1,2)
x3 = rnorm(50,x2,1)
x4 = rnorm(50,x3,0.5)
x5 = rnorm(50,x4,1)
x6 = rnorm(50,0,1)
x7 = rnorm(50,0,1)
x8 = rnorm(50,0,1)
x9 = rnorm(50,0,1)
x10 = rnorm(50,0,1)
data = cbind(x1, x2, x3, x4, x5,
			x6, x7, x8, x9, x10)
rownames(data) = paste("A",seq(1,50,1),sep="-")

data[46,] = data[45,]+rnorm(10,0,0.1)
data[47,] = data[46,]+rnorm(10,0,0.1)
data[48,] = data[47,]+rnorm(10,0,0.1)
data[49,] = data[48,]+rnorm(10,0,0.1)
data[50,] = data[49,]+rnorm(10,0,0.1)



# data standardization
for (i in 1:ncol(data)){
  data[,i]=(data[,i]-mean(data[,i]))/sqrt(var(data[,i]))
}

# PCA using prcomp()
pr.out = prcomp(data, scale=FALSE)
biplot(pr.out,scale=0,var.axes = TRUE)

plotPCA(pr.out$x[,1:3], 3, text=rownames(data))


#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#Geometric interpretation of PCA -------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------

# The code in this sub-section is out of the scope of this course
# I put the code here just in case you are interested

data(USArrests)
data = USArrests
data = data[,c(1:3)]

# data standardization
for (i in 1:ncol(data)){
  data[,i]=(data[,i]-mean(data[,i]))/sqrt(var(data[,i]))
}
pr.out = prcomp(data, scale=FALSE)


# show PC1, PC2 and PC3
library(rgl)
plot3d(data)
lines3d(x=c(-pr.out$rotation[1,1],pr.out$rotation[1,1])*2, 
		y = c(-pr.out$rotation[2,1],pr.out$rotation[2,1])*2, 
		z = c(-pr.out$rotation[3,1],pr.out$rotation[3,1])*2,col="red",lwd=2)
lines3d(x=c(-pr.out$rotation[1,2],pr.out$rotation[1,2])*2, 
		y = c(-pr.out$rotation[2,2],pr.out$rotation[2,2])*2, 
		z = c(-pr.out$rotation[3,2],pr.out$rotation[3,2])*2,col="darkgreen",lwd=2)
lines3d(x=c(-pr.out$rotation[1,3],pr.out$rotation[1,3])*2, 
		y = c(-pr.out$rotation[2,3],pr.out$rotation[2,3])*2, 
		z = c(-pr.out$rotation[3,3],pr.out$rotation[3,3])*2,col="blue",lwd=2)

# show PC1 and PC2
plot3d(data)
lines3d(x=c(-pr.out$rotation[1,1],pr.out$rotation[1,1])*2, 
		y = c(-pr.out$rotation[2,1],pr.out$rotation[2,1])*2, 
		z = c(-pr.out$rotation[3,1],pr.out$rotation[3,1])*2,col="red",lwd=2)
lines3d(x=c(-pr.out$rotation[1,2],pr.out$rotation[1,2])*2, 
		y = c(-pr.out$rotation[2,2],pr.out$rotation[2,2])*2, 
		z = c(-pr.out$rotation[3,2],pr.out$rotation[3,2])*2,col="darkgreen",lwd=2)


# show projection to PC1
d = array(0/0, dim=c(nrow(pr.out$x), 3))
for (i in 1:nrow(pr.out$x)){
  d[i,1] = pr.out$x[i,1] * pr.out$rotation[1,1]
  d[i,2] = pr.out$x[i,1] * pr.out$rotation[2,1]
  d[i,3] = pr.out$x[i,1] * pr.out$rotation[3,1]
}
plot3d(data,type="s",size=1)
lines3d(x=c(-pr.out$rotation[1,1],pr.out$rotation[1,1])*3, 
		y = c(-pr.out$rotation[2,1],pr.out$rotation[2,1])*3, 
		z = c(-pr.out$rotation[3,1],pr.out$rotation[3,1])*3,col="red",lwd=4)
points3d(d, col="red")
for (i in 1:nrow(d)){
  lines3d(x=c(d[i,1], data[i,1]), 
		y = c(d[i,2], data[i,2]), 
		z = c(d[i,3], data[i,3]),col="green",lwd=1,lty=2)
}

# show projection to PC1 and PC2
plot3d(data,type="s",size=1)
lines3d(x=c(-pr.out$rotation[1,1],pr.out$rotation[1,1])*3, 
		y = c(-pr.out$rotation[2,1],pr.out$rotation[2,1])*3, 
		z = c(-pr.out$rotation[3,1],pr.out$rotation[3,1])*3,col="red",lwd=4)
lines3d(x=c(-pr.out$rotation[1,2],pr.out$rotation[1,2])*2, 
		y = c(-pr.out$rotation[2,2],pr.out$rotation[2,2])*2, 
		z = c(-pr.out$rotation[3,2],pr.out$rotation[3,2])*2,col="darkgreen",lwd=2)
quads3d(x=c(-(pr.out$rotation[1,1]+pr.out$rotation[1,2]), (pr.out$rotation[1,1]-pr.out$rotation[1,2]), (pr.out$rotation[1,1]+pr.out$rotation[1,2]),(-pr.out$rotation[1,1]+pr.out$rotation[1,2]))*3, 
		y = c(-(pr.out$rotation[2,1]+pr.out$rotation[2,2]), (pr.out$rotation[2,1]-pr.out$rotation[2,2]), (pr.out$rotation[2,1]+pr.out$rotation[2,2]),(-pr.out$rotation[2,1]+pr.out$rotation[2,2]))*3, 
		z = c(-(pr.out$rotation[3,1]+pr.out$rotation[3,2]), (pr.out$rotation[3,1]-pr.out$rotation[3,2]), (pr.out$rotation[3,1]+pr.out$rotation[3,2]),(-pr.out$rotation[3,1]+pr.out$rotation[3,2]))*3,
		col="grey")

d = array(0/0, dim=c(nrow(pr.out$x), 3))
X = pr.out$rotation[,1:2]
P = solve(t(X)%*%X)%*%t(X)
for (i in 1:nrow(pr.out$x)){
  d[i,] = X %*% (P %*% t(data[i,]))
}
points3d(d, col="red")
for (i in 1:nrow(d)){
  lines3d(x=c(d[i,1], data[i,1]), 
		y = c(d[i,2], data[i,2]), 
		z = c(d[i,3], data[i,3]),col="green",lwd=1,lty=2)
}



#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#PVE ---------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------

data(USArrests)
data = USArrests
for (i in 1:ncol(data)){
  data[,i]=(data[,i]-mean(data[,i]))/sqrt(var(data[,i]))
}
pr.out = prcomp(data, scale=FALSE)

PVE = pr.out$sdev^2/sum(pr.out$sdev^2)
barplot(PVE, names=paste("Z", 1:4, sep=" "))




#-----------------------------------------------------------
#-----------------------------------------------------------
#PC regression on the Prostate Cancer Data -----------------
#-----------------------------------------------------------
#-----------------------------------------------------------
# file.exists("C:/Users/rbmahbub/Documents/RProjects/RlibrariesManualDownloads/lasso2_1.2-22.tar.gz")
# install.packages("C:/Users/rbmahbub/Documents/RProjects/RlibrariesManualDownloads/lasso2_1.2-22.tar.gz", 
#                  repos = NULL, 
#                  type = "source")


library(lasso2)
data(Prostate)
data = Prostate
X = data[,1:8]
X

# data standardization
for (i in 1:ncol(X)){
  X[,i]=(X[,i]-mean(X[,i]))/sqrt(var(X[,i]))
}

# PCA on "data"
pr.out = prcomp(X, scale=FALSE)
pr.out
data
# -----------------------
# -----------------------
# PVE 
# -----------------------
# -----------------------
PVE = pr.out$sdev^2/sum(pr.out$sdev^2)
barplot(PVE, names=paste("Z", 1:8, sep=" "))

PVE = round(PVE,2)
par(mar=c(4,4,4,4))
pc = barplot(PVE,
             width = 1, space = 0.2, border = NA, axes = F,
             ylim = c(0, 1.05 * max(PVE, na.rm = T)), 
             ylab = "PVE" , cex.names = 0.7, 
             names.arg = paste("PC", 1:length(PVE), sep=" "),
             main = "Pareto Chart")
## anotate left axis
axis(side = 2, at = c(0, PVE), las = 1, col.axis = "black", col = "grey62", tick = T, cex.axis = 0.8)
## frame plot
box( col = "grey62")

## Cumulative Frequency Lines 
px <- cumsum(PVE) * max(PVE, na.rm = T)
lines(pc, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
## Annotate Right Axis
axis(side = 4, at = c(0, px), labels = paste(c(0, round(cumsum(PVE)* 100)) ,"%",sep=""), 
     las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")

# -----------------------
# -----------------------
# get the new feature matrix, Z
# -----------------------
# -----------------------
Z = data.frame( pr.out$x[,1:8]) # keep the first 5 PC
data.new = Z
data.new$y = data$lpsa

# PC regression
fit.pcr = lm(y~., data.new)
summary(fit.pcr)

fit.pcr2 = lm(y~PC1+PC3, data.new)
summary(fit.pcr2)

fit.pcr3 = lm(y~PC1+PC3+PC6+PC7+PC8, data.new)
summary(fit.pcr3)


# -----------------------
# -----------------------
# Prediction
# -----------------------
# -----------------------

n = nrow(data) # sample size

mse = array()
for (i in 1:500){

train.case = sample(n, 70, replace=FALSE)
test.case = c(1:n)[-train.case]

# PC regression
Z = data.frame( pr.out$x[,c(1,3,6,7,8)]) # keep the first 3 PC
data.new = Z
data.new$y = data$lpsa

train.data = data.new[train.case,]
test.data = data.new[test.case,]

# Build the model
fit.pcr = lm(y~., train.data)
pred = predict(fit.pcr, test.data)
 
obs = test.data[,"y"] # the true lpsa values

plot(obs,pred)
mse[i] = mean( (pred-obs)^2 )

}

boxplot(mse)
summary(mse)
train.data










pr.out

