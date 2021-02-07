# Fourier functions
circ <- function(j,s1,s2,t) exp(2*pi*1i*t*s1^j) / (s2)^j

circ_sq <- function(j,t) exp(2*pi*1i*t*j) / j

spiral3 <- function(s1=3,s2=3,t=1,N=7,d=1e5){
  apply(sapply(0:N, circ,s1,s2,seq(0,t,1/d)) ,1,sum)
}

spiral_sq <- function(t=1,N=7,d=1e5){
  apply(sapply((1:N), circ_sq,seq(0,t,1/d)) ,1,sum)
}


lines3 <- function(s1=3,s2=3,t=1,N=4){
  z=0
  for(j in 0:N) z <- c(z,z[j+1] + circ(j,s1,s2,t))
  z
}

limit <- 1.3
v = seq(4,6,l=1000)
v = seq(0,1,l=1000)

linescol <- "#ffffff"

s1=-11 # frequency
s2=5 # scale
d=2e4
shrinkFactor = 0
depth=2

virds <- viridis(length(sp1))

complexsegments <- function(vector, cols){
  vector <- vector[(length(vector)/2):length(vector)]
  x1 <- Re(vector)
  x2 <- c(x1[-1],NA)
  y1 <- Im(vector)
  y2 <- c(y1[-1],NA)
  # cols <- darken(cols,
  #                hshift=seq(5,0,l=length(vector)),
  #                vshift=c(rep(0.1,length(vector)- length(vector)/2),
  #                   seq(0.1,1,l=length(vector)/2)))
  segments(x1,y1,x2,y2,col=cols)
}
linesandpoints <- function(vector){
  lines(vector, col="white")
   points(vector, col="white", pch=19)
}

darken <- function(col,hshift=0, sshift=1, vshift=1){
  cols <- as.data.frame(rgb2hsv(col2rgb(col)))
  cols
  as.vector(sapply(cols, function(x) hsv((x[1]+hshift)%%1,x[2]*sshift,x[3]*vshift)))
}

i = length(v)
scale = exp(-1i*pi/4)*
             exp(2*pi*1i*v[i]) *  # rotation
             exp(shrinkFactor*seq(0,v[i],1/d)) /  # shrink
             exp(shrinkFactor*v[i]) # normalise

# sp1 <- spiral3(s1=s1,s2=s2,t=v[length(v)],d=d,N=depth)
# sp2 <- spiral3(s1=s1,s2=-s2,t=v[length(v)],d=d,N=depth)
# 
# sp3a <- spiral3(s1=s1,s2=exp(1i*pi*2/3)*s2,t=v[length(v)],d=d,N=depth)
# sp3b <- spiral3(s1=s1,s2=exp(1i*pi*4/3)*s2,t=v[length(v)],d=d,N=depth)

sp <- spiral_sq(v[i], N=1000, d=1e4)
plot(sp + exp(1i*pi/2)*sp, type="l")
plot(sp, type="l")
plot(exp(1i*pi/2)*sp, type="l")

plot(x=NA,ann=F,axes=F,xlim=c(-limit,limit), ylim=c(-limit,limit))
complexsegments(scale*Conj(sp[1:length(scale)]),cols="black")

# for(i in 1:(length(v)-1)){
#   png(file=sprintf("c:/work/gsmallspiral%05d.png",i),width=800,height=800, type="cairo")
#   par(bg=rgb(0.05,0.05,0.05))
#   
#   scale = exp(-1i*pi/4)*
# #          exp(2*pi*1i*v[i]) *  # rotation
#           exp(shrinkFactor*seq(0,v[i],1/d)) /  # shrink
#           exp(shrinkFactor*v[i]) # normalise
#   scale2 = scale[length(scale)]
#   
#   plot(x=NA,ann=F,axes=F,xlim=c(-limit,limit), ylim=c(-limit,limit))
# 
#     lines(exp((0:6)*1i*pi/3)*scale2*(Conj(lines3(s1=s1,s2=s2,t=v[i]))[2]), col="green")                              
#   
#   complexsegments(scale*Conj(sp1[1:length(scale)]),cols="#ffffaa")
#   complexsegments(scale*Conj(sp3a[1:length(scale)]),cols="gold")
#   complexsegments(scale*Conj(sp3b[1:length(scale)]),cols="orange")
#   
#   scale = scale * exp(2i*pi/3)
#   complexsegments(scale*Conj(sp1[1:length(scale)]),cols="#ddddff")
#   complexsegments(scale*Conj(sp3a[1:length(scale)]),cols="#aaaadd")
#   complexsegments(scale*Conj(sp3b[1:length(scale)]),cols="#4444aa")
#   
#   scale = scale * exp(2i*pi/3)
#   complexsegments(scale*Conj(sp1[1:length(scale)]),cols="purple")
#   complexsegments(scale*Conj(sp3a[1:length(scale)]),cols="magenta")
#   complexsegments(scale*Conj(sp3b[1:length(scale)]),cols="pink")
#   
#   
#   
#   
#   linesandpoints(scale2*Conj(lines3(s1=s1,s2=s2,t=v[i])))
#   linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*2i/3)*s2,t=v[i])))
#   linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*4i/3)*s2,t=v[i])))
#   
#   scale2 = scale2 * exp(2i*pi/3)
#   linesandpoints(scale2*Conj(lines3(s1=s1,s2=s2,t=v[i])))
#   linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*2i/3)*s2,t=v[i])))
#   linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*4i/3)*s2,t=v[i])))
#   
#   scale2 = scale2 * exp(2i*pi/3)                                             
#   linesandpoints(scale2*Conj(lines3(s1=s1,s2=s2,t=v[i])))
#   linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*2i/3)*s2,t=v[i])))
#   linesandpoints(scale2*Conj(lines3(s1=s1,s2=exp(pi*4i/3)*s2,t=v[i])))
#   
#     
#   print(i)
#   dev.off() 
# }
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