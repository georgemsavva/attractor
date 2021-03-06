library(imager)
library(RColorBrewer)

## Going to create:

### Circle exploding into complex attractor then back again.

### Using mutation 18.

# This does 1 to 100.
# george1params <- c(runif(1,-4,4),runif(1,-4,4),runif(1,-4,4),runif(1,-2,2),runif(1,-1,1),runif(1,-1,1),runif(1,-20,20),runif(1,-20,20))

#ramp <- colorRamp(c("white","yellow","lightgreen","cyan", "darkblue","black"))


for(i in 242) {
  set.seed(i)
  
  george1params <-c(1, # freq 1
                    9, # freq 2
                    -7, # freq 3
                    1, # amp 3
                    1, # amp 2
                    0, # phase shift
                    1, # amp 1
                    0, # phase shift 2
                    0 # phase shift 3
                    ) 


  N=1000
  

    set.seed(i)
    ramp <- colorRamp(c(brewer.pal(9, name = sample(pallist,1)), "black"))
    
    george1params <-c(runif(1,-3,3), # freq 1
                      runif(1,-3,3), # expansion
                      ifelse(j>N/2,7,-4), # freq 3
                      0, # amp 3
                      0, # amp 2
                      runif(1,0,2*pi), # phase shift
                      runif(1,-5,5),# amp 1
                      2*pi*j/N,
                      -2*pi*j/N
                      )
    mutation=18
    lims <- findLimits2(george1params, mutation, startX=c(0.5,0.5))
    for(j in 1:N){    
      george1params[8] <- 2*pi*j/N
    print(c(j,george1params))
    
    
    george1 <- imageAttractorCPP(startPos = c(0.5,0.5), 
                                 n = 5e7, res=1000, 
                                 p=george1params,
                                 xlim=lims[[1]], ylim=lims[[2]],
                                 mutation=mutation)
    lg <- lg/max(lg)
    lg <- .99^george1
    cimg1 <- makeImg(lg, ramp)
    save.image(cimg1, file=sprintf("pngs/sq%d_%d-bandw_%05d.png",i,mutation,j))
    cimg1 <- makeImg(1-lg, ramp)
    save.image(cimg1, file=sprintf("pngs/sq%d_%d-bandw_%05d-INV.png",i,mutation,j))
  }
}

shell("ffmpeg -r 30 -y -i pngs/sq242_18-bandw_%05d-INV.png -c:v libx264 -r 30 -pix_fmt yuv420p blueflower.mp4")
shell("blueflower.mp4")
