library(imager)
library(RColorBrewer)

  # This does 1 to 100.
  # george1params <- c(runif(1,-4,4),runif(1,-4,4),runif(1,-4,4),runif(1,-2,2),runif(1,-1,1),runif(1,-1,1),runif(1,-20,20),runif(1,-20,20))

for(i in 110) {
  
  george1params <-round(c(1,7,-11,runif(1,-2,2),runif(1,-1,1),runif(1,0,2*pi),runif(1,0,2*pi),runif(1,-20,20)))
  #george1params <-(c(1,7,-11,runif(1,-4,4),runif(1,-4,4),runif(1,-2*pi,2*pi),runif(1,-2*pi,2*pi),runif(1,-20,20)))
  #george1params <-(c(1,11,-14,runif(1,-4,4),runif(1,-4,4),runif(1,-2*pi,2*pi),runif(1,-2*pi,2*pi),runif(1,-20,20)))
  # 1.0000000   7.0000000 -11.0000000  -0.1797678  -0.1920779   2.2072684
  # [7]   4.1739065  -1.4611410
  print(george1params)
  set.seed(2)
  #ramp <- colorRamp(c(brewer.pal(9, name = sample(pallist,1))))
  ramp <- colorRamp(c("black","purple","pink","white"))
  

  N=250
  offset <- 2*pi/N
  
  for(j in 1:25){
    set.seed(j)
    george1params <-(c(runif(1,-2,2),7,-11,runif(1,-2,2),runif(1,-1,1),runif(1,0,2*pi),runif(1,0,2*pi),runif(1,-20,20)))
    george1params[5] <- 0
    george1params[4] <- 0
    print(c(j,george1params[1]))
  }
  
  for(j in 1:800){
    set.seed(j)
    george1params <-(c(runif(1,-2,2),7,-11,runif(1,-2,2),runif(1,-1,1),runif(1,0,2*pi),runif(1,0,2*pi),runif(1,-20,20)))
    george1params[5] <- 0
    george1params[4] <- 0
    george1params[1] <- 0.5
    george1params[7] <- 5
    george1params[6] <- j/800*pi*2
    
    #X[0] = p[7]*(sin(p[1]*prev[1]+p[6])+cos(p[1]*prev[0]+p[6])) 
    #X[1] = p[7]*(triangleCPP(p[1]*prev[0]+p[6])-triangleCPPCos(p[0]*prev[1]+p[6])) 
    
    lims <- findLimits(george1params, funs = george16, startX=c(0.5,0.5))
    lims[[1]] <- c(-.6,1)*george1params[7]*2.4
    lims[[2]] <- c(-1,.6)*george1params[7]*2.4
    print(c(george1params[7],lims[[1]]/george1params[7]))
  
    mutation=17
    george1 <- imageAttractorCPP(startPos = c(0.5,0.5), 
                                 n = 1e8, res=2000, 
                                 p=george1params,
                                 xlim=lims[[1]], ylim=lims[[2]],
                                 mutation=mutation)
    lg <- ((george1)^(.5))
    
    if(j==1) {q=quantile(lg,.99)}
    lg <- pmin(lg/q,1)
    lg[,1:(2*666)] <- 1-lg[,1:(2*666)]
    cimg1 <- makeImg(lg, ramp)
    
    save.image(cimg1, file=sprintf("pngs/aa-bandw_%05d.png",j))
  }
}

shell("ffmpeg -r 30 -y -i pngs/aa-bandw_%05d.png -c:v libx264 -r 30 -pix_fmt yuv420p tiled.mp4")
shell("tiled.mp4")
