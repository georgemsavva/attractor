
library(rgl)
## Open the 3D device
open3d(windowRect = c(20, 30, 800, 800), zoom=zoom, bg=list("white"), userMatrix=um )

## Make the data frame for plotting
d <- expand.grid(x=seq(0,2*pi,l=100), y=seq(0,2*pi,l=100))

## Set the increment
inc = pi*2 / 100

## Iterate over 100 frames
for(i in 1:100){
  
  ## Work out the depth of each point
  d$z <- cos(d$x)*sin(d$y+i*inc) + cos(d$y+i*inc)*sin(d$x)
  
  ## Make sure the bounding box will be set using the subsequent points.
  par3d(ignoreExtent=FALSE)
  ## Draw 2 points as struts to make sure the main data is in shot.
  rgl::points3d(x=c(0,2*pi), y=c(0,2*pi), z=c(-1,1), col="white",box=F, axes=F)
  ## Now ignore the main data when setting the bounding box (this ensures that the view doesn't change with each frame)
  par3d(ignoreExtent=TRUE)
  ## Draw the main data
  rgl::points3d(d)
  ## Set the view to top down.
  rgl.viewpoint(0,0,zoom = .9)
  ## Save the image for frame i.
  rgl.snapshot(filename = sprintf("pngs/wave%05d.png",i))
  ## Clear the points
  clear3d()
}

## Use ffmpeg to compile the stills into a video.
shell("ffmpeg -r 30 -y -i pngs/wave%05d.png -c:v libx264 -r 30 -pix_fmt yuv420p wave.mp4")
shell("wave.mp4")
