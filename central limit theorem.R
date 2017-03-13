
# central limit theorem

# histogram of real data

breaks <- 1:21

name <- "hist_test.png"

CairoPNG(filename = name,width = 1920,height = 1080,pointsize = 20)

      hist(df.cc$final,
           breaks = breaks,
           right = F,
           col = "deepskyblue1",
           border = "#ffffff",
           xlab = "Distribution",
           xlim = c(0,21),
           ylim = c(0,14),
           xaxt = "n",
           ylab = "Frequency",
           main = "")
      
      axis(side= 1, at=1.5:22.5, labels=1:22, pos=-0.5)
           # lty=, col=, las=, tck=, ...)
      
      abline(v=m+0.5,col="black",lwd=2, lty=2)

dev.off()

shell.exec(name)


# 5000 random samples from real data

x <- data.frame(0:20,seq(0,50,by=2.5))

name <- "hist_test_samp.png"

means <- NULL

m <- mean(df.cc$final)

CairoPNG(filename = name,width = 1920,height = 1080,pointsize = 20)

  par(mar=c(4.5,4.5,0.1,1))

    plot(x,
         type="n",
         xlab = "Test score",
         #yaxt = "n",
         ylab = "Sample size",
         xaxs="i",
         yaxs="i",
         # xlim=c(0,22),
         ylim=c(0, 55),
         main = "",
         bty="l"
         )
      
      for(i in 1:5000) {
        
        rn <- runif(1,3,50)
        
        s.size <-as.integer(round(rn))
        
        s1 <- sample(df.cc$final,s.size,replace = T) 
        
        p1 <- mean(s1)
        
        #y <- rn/4 - runif(1, 0, 1)
        
        means <- append(means,p1)
        
        y <- rn - runif(1, 0, 1)
        
        c <- rn/50
        
        points(x = p1,y=y,col=rgb(1-c,0,c,0.25),pch=20,cex=2)
        
      }
      
      #lines(x=c(m,m),y=c(0,12),col="#000000",lwd=3)
      
      abline(v=m,col="#000000",lwd=2,lty=2)

dev.off()

shell.exec(name)

# histogram of sample means

name <- "hist_test_samp_hist.png"

CairoPNG(filename = name,width = 1920,height = 1080,pointsize = 20)

par(mar=c(4.5,4.5,0.1,1))

      pts.dec <- seq(0,20,by=0.1)
      
      hist(means,
           breaks = pts.dec,
           right = F,
           col = "deepskyblue1",
           border = "#ffffff",
           xlab = "Distribution",
           xaxt = "n",
           ylab = "Frequency",
           main = "")
      
      axis(side= 1, at=1.05:20.05, labels=1:20)
      # lty=, col=, las=, tck=, ...)
      
      abline(v=m+0.05,col="black",lwd=2, lty=2)
      # lines(x=c(m,m),y=c(0,250),col="#000000",lwd=3)

dev.off()
