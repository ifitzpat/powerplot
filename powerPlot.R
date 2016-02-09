##Credit for this code goes to Kristoffer Magnusson: http://rpsychologist.com/creating-a-typical-textbook-illustration-of-statistical-power-using-either-ggplot-or-base-graphics
require(ggplot2)
require(grid) # need for arrow()

## Colours:
myGraphPalette = scale_color_manual("Group", values= c("HA" = "steelblue","H0" = "black"))
myFillPalette = scale_fill_manual("Area", values= c("alpha" = "#464065","beta" = "#666666","power"="steelblue")) 
myTheme = theme(panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                panel.background = element_blank(),
                plot.background = element_rect(fill="#ffffff"),
                panel.border = element_blank(),
                axis.line = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size=22))

## components dist1 dist2 alpha, beta, delta, sigma, power, crit

powerplot.z = function (delta=FALSE, sigma=1, n=0, type="mean", alpha=FALSE, beta=FALSE, power=FALSE, tails="two") {
  
    m1 <- 0  # mu H0
    if(delta){
        m2 <- m1 + delta # mu HA
    }
    ## are we plotting the sampling distribution of the test statistic or the mean?
    if (type == "mean") {
        sd1 <- sigma # sigma H0
        if(delta) {
            sd2 <- sigma
        }
    } else {
        sd1 = sigma/sqrt(n)
        if(delta) {
            sd2 = sigma/sqrt(n)
        }
    }
  
    if (alpha & tails == "two") {
        alpha = alpha/2
        ## find critical value
        z_crit <- qnorm(1-(alpha), m1, sd1)
    } else if(alpha & (tails == "lower" | tails == "upper")) {
        z_crit <- qnorm(1-(alpha), m1, sd1)
    }

  
    ## set length of tails
    min1 <- m1-sd1*4
    max1 <- m1+sd1*4
    if(delta) {
        min2 <- m2-sd2*4
        max2 <- m2+sd2*4          

        ## create x sequence
        x <- seq(min(min1,min2), max(max1, max2), .01)
        y2 <- dnorm(x, m2, sd2)
        df2 <- data.frame("x" = x, "y" = y2)
    } else{
        ## create data frames
        x = seq(min1, max1, .01)
        df2 = NA
    }
    y1 <- dnorm(x, m1, sd1)
    df1 <- data.frame("x" = x, "y" = y1)
  
  
    plotComponents(df1,df2,z_crit, alpha=alpha, beta=beta, power=power, tails=tails)
      
}

alphaPoly = function (df, crit, tails="upper") {
    y.poly <- pmin(df$y)
    poly <- data.frame(x=df$x, y=y.poly)

    if (tails=="lower") {
        poly = poly[poly$x<= -1*crit,]
        poly = rbind(c(-6,0), poly) #start at the point -4,0
                                        #        poly = rbind(c(-1 * crit,0), poly) #and ends at the point -1.96,0
        print(poly)
    } else{   
        poly <- poly[poly$x >= crit, ] 
        poly<-rbind(poly, c(crit, 0))  # add lower-left corner
    }
    poly$id = 3 # alpha, give it the highest number to make it the top layer
    poly
}

betaPoly = function (df, crit) {
    poly <- df
    poly <- poly[poly$x <= crit,] 
    poly <-rbind(poly, c(crit, 0))  # add lower-left corner
    poly$id = 2
    poly
}

powerPoly = function(df, crit) {
    poly <- df
    poly <- poly[poly$x >= crit,] 
    poly <- rbind(poly, c(crit, 0))  # add lower-left corner
    poly$id = 1
    poly
}  

plotComponents = function (df1, df2 = NA, crit=1.96,
                           alpha=FALSE, beta=FALSE, power=FALSE, annotations = TRUE, tails = "upper") {

    peak1 = max(df1$y)

    if (alpha) {
        poly = alphaPoly(df1, crit, tails = "upper")
    }
    if (alpha & (tails == "two")) {
        poly = rbind(poly, alphaPoly(df1, crit, tails="lower"))
    }
    if (beta) {
        poly = rbind(poly, betaPoly(df2, crit))
    }
    if (power) {
        poly = rbind(poly, powerPoly(df2, crit))
    }

  
    myplot = ggplot(df1, aes(x,y)) +
        geom_line(data=df1, aes(x,y,
                                color="H0",
                                group=NULL,
                                fill=NULL),
                  size=1.5, show.legend=F) +
      
        ## H_0 title
        annotate("text", label="H[0]", x=0, y=peak1+0.02, parse=T, size=8) +
        myGraphPalette +
        myFillPalette +
        myTheme

    if (alpha | beta | power) {
        mylabels = c()
        if (power) {
            mylabels = "power"
        }
        if (beta) {
            mylabels = c(mylabels,"beta")
        }
        if (alpha) {
            mylabels = c(mylabels,"alpha")
        }
   
        poly$id = factor(poly$id, labels=mylabels)
        myplot = myplot + geom_polygon(data=poly,
                                       aes(x,y, fill=id, group=id),
                                       show.legend = FALSE,
                                       alpha=I(8/10)) +
            geom_vline(xintercept = crit, size=1, linetype="dashed")
      
    }

    if (is.data.frame(df2)) {
            peak2 = max(df2$y)
            xpeak2 = which.max(df2$y)

            dx = max(df2$x)-min(df1$x)
            mean2 = sum(df2$y)/dx
            xmean2 = df2$x[which.min(abs(df2$y-mean2))]

        myplot = myplot +
            geom_line(data=df2, aes(x,y, color="HA",
                                    group=NULL, fill=NULL), size=1.5, show.legend=F) + 
            annotate("text", label="H[a]", x=df2$x[xpeak2], y=peak1+0.02, parse=T, size=8)

            if (annotations) {
                ## delta arrow
                myplot = myplot + annotate("segment", x=0, y=peak2-0.05,
                                           xend=xmean2, yend=peak2-0.05,
                                           arrow = arrow(length = unit(0.3, "cm"),
                                                         ends="both",type="closed"), size=1) +
                annotate("segment", x=0, y=peak2-0.09, xend=xmean2/2,
                         yend=peak2-0.05, arrow = arrow(length = unit(0.3, "cm")), size=1) +
                annotate("text", label="delta", x=0, y=peak2-0.1, parse=T, size=8)
          
        }
    }

    if(alpha & annotations) {
        myplot = myplot + annotate("segment", x=4,
                                   y=0.043, xend=3.4,
                                   yend=0.01,
                                   arrow = arrow(length = unit(0.3, "cm")), size=1)
        if (tails=="two") {
            myplot = myplot + annotate("text", label="frac(alpha,2)", x=4.2, y=0.05, parse=T, size=8)
        } else {
            myplot = myplot + annotate("text", label="alpha", x=4.2, y=0.05, parse=T, size=8)
        }
    }

    if(beta & annotations) {
        myplot = myplot + annotate("segment", x=0.1,
                                   y=0.045, xend=1.3,
                                   yend=0.01,
                                   arrow = arrow(length = unit(0.3, "cm")), size=1) +
            annotate("text", label="beta", x=0, y=0.05, parse=T, size=8)
    }

    if(power & annotations) {
        myplot = myplot + annotate("segment", x=6,
                                   y=0.2, xend=4.5,
                                   yend=0.15,
                                   arrow = arrow(length = unit(0.3, "cm")), size=1) +
            annotate("text", label="1-beta", x=6.1, y=0.21, parse=T, size=8)

    }

    myplot
}
