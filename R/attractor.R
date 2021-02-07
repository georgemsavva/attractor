
computeAttractor <- function(params,funs,n=1e4,startX){
  X=matrix(NA, nrow=n, ncol=length(startX))
  X[1,] <- startX
  for(i in 2:n) {
    X[i,] <- funs(prev=X[i-1,], params)
  }
  X
}

imageAttractor <- function(params, funs, n=1e4, startX, xlim=NULL, ylim=NULL, fromImage=NULL, res=NULL){
  if(is.null(fromImage)) img <- matrix(0, nrow=res, ncol=res) else {img <- fromImage; res <- dim(img)[1]}
  X <- startX
#  print(res)
  for(i in 1:n){
    X <- funs(prev=X, params)
    if(abs(X[1]<100) & abs(X[2]<100)){
      x <- floor(res * (X[1] - xlim[1]) / (xlim[2]-xlim[1]))
      y <- floor(res * (X[2] - ylim[1]) / (ylim[2]-ylim[1]))
#      print(x)
#      print(y)
    if((x<res) & (y<res) & (x>1) & (y>1))  img[x,y] <- img[x,y]+1
    } else (X = runif(2))
  }
  img
}

img <- imageAttractor(1.6, 
                      henon, 
                      n=1e4, startX =c(runif(1,-.55,-.45), runif(1,.05,.15)),
                      xlim=c(-0.55,-0.45), ylim=c(0.05,0.15), res=5000)
for(i in 1:10000){
  print(i)
  img <- imageAttractor(1.6, 
                        henon, 
                        n=1e5, startX =c(runif(1,-.55,-.45), runif(1,.05,.15)), 
                        xlim=c(-0.55,-0.45), ylim=c(0.05,0.15), 
                        fromImage = img)

  }

img <- img / max(img)
imgr = 1-img
imgg = (img)^.1
imgb = (img)


#plot(as.cimg(.99^img))
cimg1 <- as.cimg(c(seq(0,1,l=5000*5000), imgg, 1-imgg), x=5000, y=5000,cc=3)
cimg2<-HSVtoRGB(cimg1)



save.image(cimg1, "henon_3.png") 
shell("henon_3.png")
iterateAttractor <- function(params,funs,X){
  Y=X
  for(i in 1:(dim(X)[1])) {
    Y[i,] <- funs(prev=X[i,], params)
  }
  Y
}

clifford <- function(prev, p){
  c(
    sin(p[1]*prev[2])+p[3]*cos(p[1]*prev[1]),
    sin(p[2]*prev[1])+p[4]*cos(p[2]*prev[2])
  )
}

dejong <- function(prev, p){
  c(
    sin(p[1]*prev[2])-cos(p[3]*prev[1]),
    sin(p[3]*prev[1])-cos(p[4]*prev[2])
  )
}

henon <- function(prev, p){
  c(
    prev[1]*cos(p[1]) - (prev[2] - prev[1]^2)*sin(p[1]),
    prev[1]*sin(p[1]) + (prev[2] - prev[1]^2)*cos(p[1])
  )
}


#png("henon.png", width=1000, height=1000, type="cairo")

res = 3000
m <- matrix(1, ncol=res, nrow=res)
for(i in 1:1000){
  henon1 <- computeAttractor(1.6, henon, 1e5, runif(2))
  henon1 <- henon1[abs(henon1[,1])<1 & abs(henon1[,2])<1,]
  for(i in 1:(dim(henon1)[1])){
    x <- floor(res/2 + henon1[i,1]*res/2)
    y <- floor(res/2 + henon1[i,2]*res/2)
    m[x,y] <- m[x,y]*.97
  }
}
m <- m^3
img1 <- as.cimg(m)
plot(img1, xlim=c(0,res), ylim=c(0,res), axes=F, ann=F)
imager::save.image(img1, "henon1.png")
shell("henon1.png")
#plot(henon1, type="p", pch=".", col=hsv(0,0,0,.01))
gc()
#dev.off()
#shell("henon.png")
m[1:10,1:10]
lorenz <- function(prev, p){
  c(
    prev[1] +  p[4] * p[1] * (prev[2] - prev[1]),
    prev[2] +  p[4] * (prev[1] * (p[2] - prev[3]) - prev[2]),
    prev[3] +  p[4] * (prev[1] * prev[2] - p[3] * prev[3])
  )
}

pngAttractor <- function(X,filename, 
                         width=2000,
                         height=2000, 
                         h=.6,s=0,v=1, alpha=0.1,
                         bg="black",
                         mar=rep(5,4),
                         type="p"){
  png(filename, width=width, height=height, type="cairo")
  par(bg=bg,mar=mar)
  plot(X[,1], X[,2], axes=F, ann=F,
       pch=".",  col=hsv(h=h,s=h,v=v,alpha=alpha), type=type)
  dev.off()
}


library(viridis)
library(rgl)
library(gdata)
lor1 <- computeAttractor(c(10,28,8/3,.001), lorenz, startX = c(.1,0,0), n=1e4)
clif1 <- computeAttractor(c(1.77,-2,.45,-.667), clifford, startX = c(0,0)) 


X <- matrix(runif(500*3), ncol=3)
tail <- rep(list(X),22)

bg3d("black")
par3d(windowRect = c(20, 30, 800, 800))
rgl::plot3d(tail[[1]], col = "white", box=F, axes=F)
#um <- par3d()$userMatrix
zoom<-par3d()$zoom
um
open3d(windowRect = c(20, 30, 800, 800), zoom=zoom, bg=list("black"), userMatrix=um)

for(i in 1:1800){
 print(i)
  #rgl::plot3d(X, col = "white", xlim=c(-20,20), ylim=c(-30,30), zlim=c(0,50))
  par3d(ignoreExtent=FALSE)
  
  rgl::rgl.points(x=c(-20,20), y=c(-20,20), z=c(0,50), col="black",box=F, axes=F)
  par3d(ignoreExtent=TRUE)
  pts1 <- rgl::rgl.points(tail[[1]], 
                          col = hsv((tail[[1]][,2]/50)%%1,1,1), 
                          ignoreExtent=TRUE)
  for(j in 0:20){
    rgl::segments3d(interleave(tail[[1+j]], tail[[2+j]]), 
                    col = hsv((tail[[1]][,2]/50)%%1,.1,1),alpha=(.85^j), 
                    ignoreExtent=TRUE)
  }
  
  rgl.viewpoint(0, 360*3*i/1800,zoom = .80)
  rgl.snapshot(filename = sprintf("pngs/anim%05d.png",i))
  clear3d()

  tail <- c(list(tail[[1]]),tail[-length(tail)])
  tail[[1]] <- iterateAttractor(c(10,28,8/3,.007), lorenz, tail[[2]])
  
}


shell("ffmpeg -r 30 -y -i pngs/anim%05d.png -c:v libx264 -r 30 -pix_fmt yuv420p iter2.mp4")
shell("iter2.mp4")
makeMovie <- function(filename, X){
  bg3d("black")
  par3d(windowRect = c(20, 30, 800, 800))
  plot3d(X, col = viridis(1e4), box=F, axes=F, )
  movie3d(spin3d(rpm=6), fps=30, duration=10, dir="pngs", clean=F, convert = FALSE)
  shell(sprintf("ffmpeg -r 30 -y -i pngs/movie%03d.png -c:v libx264 -r 30 -pix_fmt yuv420p %s"), filename)
  }

