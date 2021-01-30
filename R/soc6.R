
library(imager)

##  Proper art for Society6.

paramlist <- list(
  c(1.5,-1.5,1.16538,-0.81269),
  c(1.9,-1.9,-1.45794,-0.68455),
  c(-1.9,-1.9,-0.90798,-0.89101),
  c(1.15956,1.32504,1.97562,1.47191)
  
  )

img1 <- computeAttractor(c(1.5,-1.5,1.16538,-0.81269), clifford, startX = c(0,0),n = 1e6) 
pngAttractor(img1, "img1_2000.png", width=2000, height=2000, h=0,s=0,v=0,alpha=0.1, mar=rep(20,4))

for(i in 1:length(paramlist)){
  img1 <- computeAttractor(paramlist[[i]], clifford, startX = c(0,0),n = 1e7) 
  pngAttractor(img1, sprintf("img%05d_2000.png",i), 
               width=2000, height=2000, h=0,s=0,v=1,alpha=0.1, mar=rep(20,4),
               bg="black")
}


img1 <- load.image("img00002.png")
