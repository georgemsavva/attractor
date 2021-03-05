
pickover <- function(prev, p){
  c(
    sin(p[1]*prev[2])+p[3]*sin(p[1]*prev[1]),
    sin(p[2]*prev[1])+p[4]*sin(p[2]*prev[2])
  )
}




lordsDreams <- list(c(-2.905148, -2.030427, 1.440550, 0.703070),
                    c(-2.951292, 1.187750, 0.517396, 1.090625),
                    c(2.668752, 1.225105, 0.709998, 0.637272),
                    c(1.380932, 2.656301, 1.157857, 1.272576),
                    c(2.733940, 1.369945, 1.471923, 0.869182),
                    c(1.008118, 2.653920, 0.599124, 0.650700))
kingsDream <- list(c(-0.966918, 2.879879, 0.765145, 0.744728))

computeAttractor <- function(params,funs,n=1e4,startX){
  X=matrix(NA, nrow=n, ncol=length(startX))
  X[1,] <- startX
  for(i in 2:n) {
    X[i,] <- funs(prev=X[i-1,], params)
  }
  X
}

findLimits <- function(params, funs, n=1e4,startX){
  att1 <- computeAttractor(params, funs, n=n, startX)
  xlim <- 1.1*c(min(att1[-c(1:1000),1]), max(att1[-c(1:1000),1]))
  ylim <- 1.1*c(min(att1[-c(1:1000),2]), max(att1[-c(1:1000),2]))
  return(list(xlim,ylim))
}


imageAttractor <- function(params, funs, n=1e4, startX, xlim=NULL, ylim=NULL, fromImage=NULL, res=NULL){
  if(is.null(fromImage)) img <- matrix(0, nrow=res, ncol=res) else {img <- fromImage; res <- dim(img)[1]}
  if(is.null(xlim)){
    att1 <- computeAttractor(params, funs, n=10000, startX)
    xlim <- 1.1*c(min(att1[-c(1:1000),1]), max(att1[-c(1:1000),1]))
    ylim <- 1.1*c(min(att1[-c(1:1000),2]), max(att1[-c(1:1000),2]))
    print(xlim)
    print(ylim)
  }
  X <- startX
  #  print(res)
  for(i in 1:n){
    X <- funs(prev=X, params)
    xd <- res/(xlim[2]-xlim[1])
    yd <- res/(ylim[2]-ylim[1])
    if(abs(X[1]<100) & abs(X[2]<100)){
      x <- floor(xd * (X[1] - xlim[1]) )
      y <- floor(yd * (X[2] - ylim[1]) )
      #      print(x)
      #      print(y)
      if((x<res) & (y<res) & (x>1) & (y>1))  img[x,y] <- img[x,y]+1
      if(i<10) print(X)
    } else (X = runif(2))
  }
  img
}

pngAttractor <- function(X,filename, 
                         width=2000,
                         height=2000, 
                         h=.6,s=0,v=1, alpha=0.1,
                         bg="white",
                         mar=rep(5,4),
                         type="p"){
  png(filename, width=width, height=height, type="cairo")
  par(bg=bg,mar=mar)
  plot(X[,1], X[,2], axes=F, ann=F,
       pch=".",  col=hsv(h=h,s=s,v=v,alpha=alpha), type=type)
  dev.off()
}

points=1e7
## Set to 1e8 for very high quality, but takes a few minutes to run


img1 <- computeAttractor(paramlist[[1]], pickover, startX = c(0.1,0.1),n = points) 
pngAttractor(img1, "king7.png", width=4000, bg="black", height=4000, h=0,s=0,v=1,alpha=0.5, mar=rep(20,4))

for(i in 1:length(lordsDreams)){
  img1 <- computeAttractor(lordsDreams[[i]], pickover, startX = c(0.1,0.1),n = points) 
  pngAttractor(img1, sprintf("lord%000d.png", i), 
               width=4000, bg="black", height=4000, h=0,s=0,v=1,alpha=0.5, mar=rep(10,4))
  
}

library(imager)
library(tictoc)
for(i in 1:length(lordsDreams)){
 print(i)
  tic()
  cimg1 <- imageAttractor(params=lordsDreams[[i]], 
                 funs=pickover, n = 1e8, startX = c(0.1, 0.1), res = 4000, 
                 xlim = (lordsDreams[[i]][3]+1)*c(-1,1), ylim = (lordsDreams[[i]][3]+1)*c(-1,1)
                 )  
  save.image(as.cimg(cimg1), file = sprintf("lord_b_%000d.png", i)) 
  toc()
}

lordsDreamsCimg <- lapply(lordsDreams[3], function(p){
  imageAttractor(params=p, 
                          funs=pickover, n = 5e7, startX = c(0.1, 0.1), res = 4000
  )  
})




library(viridis)
library(RColorBrewer)
img1 <- sqrt((lordsDreamsCimg[[1]]))
img1[img1>5]<-5

viridis10000 <- viridis(10000, option = "magma")

viridis10000 <- colorRampPalette(c("black",brewer.pal(9, name = "PuBu")[9:1]))(10000)


viridis10000R <- col2rgb(viridis10000)[1,]
viridis10000G <- col2rgb(viridis10000)[2,]
viridis10000B <- col2rgb(viridis10000)[3,]

img1R <- img1
img1G <- img1
img1B <- img1
imgINT <- floor(9999*img1/max(img1))
for(i in 1:4000){
  for(j in 1:4000){
img1R[i,j] <- viridis10000R[1+imgINT[i,j]]
img1G[i,j] <- viridis10000G[1+imgINT[i,j]]
img1B[i,j] <- viridis10000B[1+imgINT[i,j]]

  }}

library(imager)
cimg1 <- as.cimg(c(img1R, img1G, img1B), x=4000, y=4000, cc=3)
save.image(cimg1, file = sprintf("lord_v_%000d.png", 4)) 
