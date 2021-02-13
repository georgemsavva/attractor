
library(rgl)
## Open the 3D device
open3d(windowRect = c(20, 30, 800, 800), bg=list("white"))

## Make the data frame for plotting
d <- expand.grid(x=seq(0,2*pi,l=100), y=seq(0,2*pi,l=100))

## Make rule 30 array
iterateRule30 <- function(x){
  rc=c(x[-1],x[1])
  lc=c(x[length(x)], x[-length(x)])
  (lc | (x | rc)) & !(lc & (x | rc))
}
rule30 = matrix(0,nrow=1000,ncol=100)
rule30[1,50] = 1
for(j in 2:dim(rule30)[1]){
rule30[j,] <- iterateRule30(rule30[j-1,])
}
rule30diag <- matrix(NA, 1000,1000)
for(i in 2:1000){
  print(i)
  diag(rule30diag[max(1,2*i-100):(2*i-1),max(1,2*i-100):(2*i-1)]) <- rule30[i,max(1,51-i):min(100,49+i)]
}

rule30d <- as.data.frame(reshape2::melt(rule30))
names(rule30d) <- c("y","x","col")


  
## Set the increment
inc = pi*2 / 100

i=1
## Iterate over 100 frames
for(i in 1:900){

  #iterate the rule 30 matrix:
    
  ## Work out the depth of each point
  d$z <- cos(d$x)*sin(d$y+i*inc) + cos(d$y+i*inc)*sin(d$x)
  
  d$col <- subset(rule30d, y>=i & y<i+100)$col
  
  ## Make sure the bounding box will be set using the subsequent points.
  par3d(ignoreExtent=FALSE)
  ## Draw 2 points as struts to make sure the main data is in shot.
  rgl::points3d(x=c(0,2*pi), y=c(0,2*pi), z=c(-1,1), col="white",box=F, axes=F)
  ## Now ignore the main data when setting the bounding box (this ensures that the view doesn't change with each frame)
  par3d(ignoreExtent=TRUE)
  ## Draw the main data
  rgl::points3d(d$x,d$y,d$z, col=hsv(1,0,.8*d$col), size=4)
  ## Set the view to top down.
  rgl.viewpoint(0,0,zoom = .9)
  ## Save the image for frame i.
  rgl.snapshot(filename = sprintf("pngs/wave%05d.png",i))
  ## Clear the points
  clear3d()
}

## Use ffmpeg to compile the stills into a video.
shell("ffmpeg -r 30 -y -i pngs/wave%05d.png -c:v libx264 -r 30 -pix_fmt yuv420p wave2.mp4")
shell("wave2.mp4")
