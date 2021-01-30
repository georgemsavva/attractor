library(viridis)
library(rgl)
library(gdata)

#lor1 <- computeAttractor(c(10,28,8/3,.001), lorenz, startX = c(.1,0,0), n=1e4)
#clif1 <- computeAttractor(c(1.77,-2,.45,-.667), clifford, startX = c(0,0)) 


#X <- matrix(100*runif(500*3)-50, ncol=3)
X <- matrix(rep(NA,500*3), ncol=3)
X[1,] <- .1+runif(3)/100
tail <- rep(list(X),22)

bg3d("white")
par3d(windowRect = c(20, 30, 800, 800))
rgl::plot3d(tail[[1]], col = "white", box=T, axes=F)
um <- par3d()$userMatrix
zoom<-par3d()$zoom
um
open3d(windowRect = c(20, 30, 800, 800), zoom=zoom, bg=list("white"), userMatrix=um )

for(i in 1:1800){
  print(i)
  par3d(ignoreExtent=FALSE)
  rgl::points3d(x=c(-20,20), y=c(-20,20), z=c(0,50), col="#f7f7f7",box=F, axes=F)
  par3d(ignoreExtent=TRUE)
  
  pts1 <- rgl::rgl.points(tail[[1]], 
                          col = viridis(500), 
                          ignoreExtent=TRUE,
                          alpha=min(1,(1800-i)/20)
                          )
  rgl::box3d(main="Lorentz attractor")
  for(j in 0:20){
    rgl::segments3d(interleave(tail[[1+j]], tail[[2+j]]), 
                    col = rep(viridis(500),each=2),
                    alpha=(.85^j*min(1,(1800-i)/20)), 
                    ignoreExtent=TRUE)
  }
  
  rgl.viewpoint(360*3*i/1800, 0,zoom = .9)
  rgl.snapshot(filename = sprintf("pngs/anim%05d.png",i))
  clear3d()
  
  tail <- c(list(tail[[1]]),tail[-length(tail)])
  tail[[1]] <- iterateAttractor(c(10,28,11/3,.01), lorenz, tail[[2]])
  if(i<500) tail[[1]][i+1,] <- .1+runif(3)/100
}

shell("ffmpeg -r 30 -y -i pngs/anim%05d.png -c:v libx264 -r 30 -pix_fmt yuv420p iter5.mp4")
shell("iter5.mp4")


makeMovie <- function(filename, X){
  bg3d("black")
  par3d(windowRect = c(20, 30, 800, 800))
  plot3d(X, col = viridis(1e4), box=F, axes=F, )
  movie3d(spin3d(rpm=6), fps=30, duration=10, dir="pngs", clean=F, convert = FALSE)
  shell(sprintf("ffmpeg -r 30 -y -i pngs/movie%03d.png -c:v libx264 -r 30 -pix_fmt yuv420p %s"), filename)
}

