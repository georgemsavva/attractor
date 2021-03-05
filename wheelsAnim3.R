library(imager)
library(RColorBrewer)

  # This does 1 to 100.
  # george1params <- c(runif(1,-4,4),runif(1,-4,4),runif(1,-4,4),runif(1,-2,2),runif(1,-1,1),runif(1,-1,1),runif(1,-20,20),runif(1,-20,20))

for(i in 110) {
  set.seed(i)
  george1params <-(c(1,7,-11,runif(1,-2,2),runif(1,-1,1),runif(1,0,2*pi),runif(1,0,2*pi),runif(1,-20,20)))
  #george1params <-(c(1,7,-11,runif(1,-4,4),runif(1,-4,4),runif(1,-2*pi,2*pi),runif(1,-2*pi,2*pi),runif(1,-20,20)))
  #george1params <-(c(1,11,-14,runif(1,-4,4),runif(1,-4,4),runif(1,-2*pi,2*pi),runif(1,-2*pi,2*pi),runif(1,-20,20)))
  # 1.0000000   7.0000000 -11.0000000  -0.1797678  -0.1920779   2.2072684
  # [7]   4.1739065  -1.4611410
  print(george1params)
  ramp <- colorRamp(c(brewer.pal(9, name = sample(pallist,1)), "black"))
  ramp <- colorRamp(c("white","pink", "purple","black"))
  lims <- findLimits(george1params, funs = george16, startX=c(0.5,0.5))

  N=250
  offset <- 2*pi/N
  for(j in 1:N){
    col1 <- hsv(j/N, s=0.6,v=.8)
    col2 <- hsv(((j+50)/N)%%1, s=0.6,v=.5)
    
    ramp <- colorRamp(c("white",col1, col2,"black"))
    george1params[6]  <-george1params[6]+2*offset
    #george1params[6]  <-george1params[6]-offset
    cp = cos(2*pi*j/250)
    sp = sin(2*pi*j/250)
    george1params[2] <- ifelse(sp>0,7,5)
    george1params[3] <- ifelse(cp>0,-11,-7)
    
    george1params[4] <-  0.4581960 * cp
    george1params[5] <-  -0.5637471 * sp
    
    mutation=16
    george1 <- imageAttractorCPP(startPos = c(0.5,0.5), 
                                 n = 2e6, res=1000, 
                                 p=george1params,
                                 xlim=lims[[1]], ylim=lims[[2]],
                                 mutation=mutation)
    lg <- (log(george1+1)^(1))
    
    if(j==1) {q=quantile(lg,.999)}
    lg <- pmin(lg,q)/q
    cimg1 <- makeImg(lg, ramp)
    
    save.image(cimg1, file=sprintf("pngs/a-bandw_%05d.png",j))
  }
}

shell("ffmpeg -r 30 -y -i pngs/a-bandw_%05d.png -c:v libx264 -r 30 -pix_fmt yuv420p red.mp4")
shell("red.mp4")
