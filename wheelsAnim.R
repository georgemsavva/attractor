library(imager)
library(RColorBrewer)

  # This does 1 to 100.
  # george1params <- c(runif(1,-4,4),runif(1,-4,4),runif(1,-4,4),runif(1,-2,2),runif(1,-1,1),runif(1,-1,1),runif(1,-20,20),runif(1,-20,20))

for(i in 108) {
  set.seed(i)
  george1params <-(c(1,7,-11,runif(1,-2,2),runif(1,-1,1),runif(1,0,2*pi),runif(1,0,2*pi),runif(1,-20,20)))
  #george1params <-(c(1,7,-11,runif(1,-4,4),runif(1,-4,4),runif(1,-2*pi,2*pi),runif(1,-2*pi,2*pi),runif(1,-20,20)))
  #george1params <-(c(1,11,-14,runif(1,-4,4),runif(1,-4,4),runif(1,-2*pi,2*pi),runif(1,-2*pi,2*pi),runif(1,-20,20)))
  # 1.0000000   7.0000000 -11.0000000  -0.1797678  -0.1920779   2.2072684
  # [7]   4.1739065  -1.4611410
  print(george1params)
  ramp <- colorRamp(c(brewer.pal(9, name = sample(pallist,1))))
  lims <- findLimits(george1params, funs = george16, startX=c(0.5,0.5))
  
  N=400
  offset <- 2*pi/N
  for(j in 1:N){
    george1params[6]  <-george1params[6]+offset
    mutation=16
    george1 <- imageAttractorCPP(startPos = c(0.5,0.5), 
                                 n = 5e7, res=1000, 
                                 p=george1params,
                                 xlim=lims[[1]], ylim=lims[[2]],
                                 mutation=mutation)
    cimg1 <- makeImg(.94^sqrt(george1), ramp)
    save.image(cimg1, file=sprintf("pngs/zzpink_%d.png",j))
  }
}

shell("ffmpeg -r 30 -y -i pngs/zzpink_%d.png -c:v libx264 -r 30 -pix_fmt yuv420p pink.mp4")
shell("pink.mp4")
