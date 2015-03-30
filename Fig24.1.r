### Figure 24.1
library(ggplot2)
x = 8
y = 6
n = 10
m = 10
B = 10000
p1 = rbeta(B,x+1,n-x+1)
p2 = rbeta(B,y+1,m-y+1)
diff=p2-p1
delta = data.frame(diff)
print(mean(delta$diff))
left  = quantile(delta$diff,.025)
right = quantile(delta$diff,.975)
print(c(left,right))

prob=seq(-1,1,.2)
qplot(delta$diff, geom="histogram", binwidth=.05,
      main = "this is the chart", 
      xlab = "p",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(-1,1)
     )

gg<-ggplot(data=delta$diff, geom_histogram(aes(x=diff),binwidth=.1))

postscript("two.binom.sim.pdf",horizontal=F)
#postscript("two.binom.sim.eps",horizontal=F,onefile=F,print.it=F)
# hist(diff,xlab="",ylab="",main="",cex=2,density=5,
#    prob=T,lwd=3,xaxt="n",yaxt="n",bty="n")
# axis(1,at=c(-0.6,0,0.6),cex.axis=2)

print(gg)
dev.off()
 


