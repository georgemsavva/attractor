# Fourier functions
circ <- function(j,s1,s2,t) exp(2*pi*1i*t*s1^j) / (s2)^j
spiral3 <- function(s1=3,s2=3,t=1,N=7,d=1e5){
  apply(sapply(0:N, circ,s1,s2,seq(0,t,1/d)) ,1,sum)
}
lines3 <- function(s1=3,s2=3,t=1,N=4){
  z=0
  for(j in 0:N) z <- c(z,z[j+1] + circ(j,s1,s2,t))
  z
}

limit <- 1.3
v = seq(4,6,l=1000)
v = seq(4,5,l=1000)

linescol <- "#ffffff"

s1=-11 # frequency
s2=5 # scale
d=5e3
shrinkFactor = 2
depth=2

s1=-7 # frequency
s2=3.2 # scale
d=2e4
shrinkFactor = 4
depth=4


sp1 <- spiral3(s1=s1,s2=s2,t=v[length(v)],d=d,N=depth)
sp2 <- spiral3(s1=s1,s2=-s2,t=v[length(v)],d=d,N=depth)

sp3a <- spiral3(s1=s1,s2=exp(1i*pi*2/3)*s2,t=v[length(v)],d=d,N=depth)
sp3b <- spiral3(s1=s1,s2=exp(1i*pi*4/3)*s2,t=v[length(v)],d=d,N=depth)

complexsegments <- function(vector, cols){
  vector <- vector[Mod(vector)>.05]
  x1 <- Re(vector)
  x2 <- c(x1[-1],NA)
  y1 <- Im(vector)
  y2 <- c(y1[-1],NA)
  cols <- darken(cols,
                 hshift=seq(0,5,l=length(vector)),
                 sshift=0.5,
                 vshift=seq(0.1,1,l=length(vector)))
  #print("got here")
  #print(vector)
  segments(x1,y1,x2,y2,col=cols, lwd=seq(0.5,2,l=length(vector)))
  #segments(x1,y1,x2,y2,col="white")
}
linesandpoints <- function(vector){
  lines(vector, col="white",lwd=3)
  points(vector, col="white", pch=19,cex=1.5)
}
darken <- function(col,hshift=0, sshift=1, vshift=1){
  cols <- as.data.frame(rgb2hsv(col2rgb(col)))
  cols
  as.vector(sapply(cols, function(x) hsv((x[1]+hshift)%%1,x[2]*sshift,x[3]*vshift^2)))
}

for(i in 1:(length(v))){
  png(file=sprintf("pngs/ggsmallspiral%05d.png",i),width=1000,height=1000, type="cairo")
  par(bg=rgb(0.05,0.05,0.05))
  
  scale = exp(-1i*pi/4)*
#          exp(2*pi*1i*v[i]) *  # rotation
          exp(shrinkFactor*seq(0,v[i],1/d)) /  # shrink
          exp(shrinkFactor*v[i]) # normalise
  scale2 = scale[length(scale)]
  
  plot(x=NA,ann=F,axes=F,xlim=c(-limit,limit), ylim=c(-limit,limit))

    lines(exp((0:6)*1i*pi/3)*scale2*(Conj(lines3(s1=s1,s2=s2,t=v[i]))[2]), col="#55ff00",lwd=2.2)                              
  
  complexsegments(scale*Conj(sp1[1:length(scale)]),cols="#ffffaa")
  complexsegments(scale*Conj(sp3a[1:length(scale)]),cols="gold")
  complexsegments(scale*Conj(sp3b[1:length(scale)]),cols="orange")
  scale = scale * exp(2i*pi/3)
  complexsegments(scale*Conj(sp1[1:length(scale)]),cols="#ddddff")
  complexsegments(scale*Conj(sp3a[1:length(scale)]),cols="#aaaadd")
  complexsegments(scale*Conj(sp3b[1:length(scale)]),cols="#4444aa")
  scale = scale * exp(2i*pi/3)
  complexsegments(scale*Conj(sp1[1:length(scale)]),cols="purple")
  complexsegments(scale*Conj(sp3a[1:length(scale)]),cols="magenta")
  complexsegments(scale*Conj(sp3b[1:length(scale)]),cols="pink")
  
  linesandpoints(scale2*Conj(lines3(s1=s1,s2=s2,t=v[i])))
  linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*2i/3)*s2,t=v[i])))
  linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*4i/3)*s2,t=v[i])))
  scale2 = scale2 * exp(2i*pi/3)
  linesandpoints(scale2*Conj(lines3(s1=s1,s2=s2,t=v[i])))
  linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*2i/3)*s2,t=v[i])))
  linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*4i/3)*s2,t=v[i])))
  scale2 = scale2 * exp(2i*pi/3)                                             
  linesandpoints(scale2*Conj(lines3(s1=s1,s2=s2,t=v[i])))
  linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*2i/3)*s2,t=v[i])))
  linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*4i/3)*s2,t=v[i])))
  
  print(i)
  dev.off() 
}
#shell("ggsmallspiral00001.png")

#shell("ffmpeg -r 30 -y -i pngs/ggsmallspiral%05d.png -c:v libx264 -r 30 -pix_fmt yuv420p spiral.mp4")
#shell("spiral.mp4")

# ## This is the imagemagick code to make the gif from the pngs.  
# ## I can't get it to work through the magick package so am just pasting this into a cmd shell.
# ## It takes ages (20 mins or something on my PC) but makes a really small gif so its worth it.
# 
# # magick convert -delay 5 smallspiral*.png -loop 0 -strip -coalesce -layers Optimize out.gif
# 
# 
# ## To make a .mp4 from the pngs using ffmpg:
# ## I can't get it to work through the magick package so am just pasting this into a cmd shell.
# ## It takes ages (20 mins or something on my PC) but makes a really small gif so its worth it.
# 
# # magick convert -delay 5 smallspiral*.png -loop 0 -strip -coalesce -layers Optimize out.gif