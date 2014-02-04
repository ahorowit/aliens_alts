rm(list=ls())
library(lme4)
library(plotrix)
library(ggplot2)

d1 <- read.csv("~/new compy/Stanford/Research/Frank Lab/Conferences/CogSci/CogSci 2014/aliens_alts_analyses/aliens_alts_kids_expt1b.csv")




##########################TEST TRIALS#########################
library(reshape)

d2=melt.data.frame(d1,c("Sub_ID","DOB","DOT","Age","condition","agegroup"),c("glorp","tibu","peebo", "zib", "glorp_type", "tibu_type", "peebo_type", "zib_type"))



d2=melt.data.frame(d1,c("Sub_ID","DOB","DOT","Age","condition", "agegroup"),c(8,11,14,17,6,9,12,15))
data <- d2[1:392,] 
data$contrasts <- d2$value[393:784]
names(data)[7] <- "correct"
names(data)[6] <- "alien"

names(data) <- c("Sub_ID","DOB","DOT","Age","condition", "agegroup", "alien", "correct", "contrasts")


data$contrasts[data$contrasts=="size "] <- "size"
data$contrasts <- as.factor(data$contrasts)
data$correct <- data$correct==1
data$alien <- as.factor(data$alien)
data$alien <- factor(data$alien, levels = c('glorp', 'peebo', 'tibu', 'zib'))
data$Age=as.numeric(data$Age)
as.factor(data$agegroup)

data$agegroup <- as.factor(data$agegroup)
data$agegroup <-factor(data$agegroup, levels=c('3.0 - 3.5', '3.5 - 4.0', '4.0 - 4.5', '4.5 - 5.0'))
str(data)




agg.data2 <- aggregate(data$correct, list(data$contrasts, data$agegroup), FUN=sum)
agg.data2.len <- aggregate(data$correct, list(data$contrasts, data$agegroup), FUN=length)
agg.data2$x <- agg.data2$x 
agg.data2.len$x <- agg.data2.len$x 

names(agg.data2) <- c("contrasts", "agegroup", "count")
agg.data2$total <- agg.data2.len$x
agg.data2$prop.corr <- agg.data2$count / agg.data2$total

agg.data2$q <- 1 - agg.data2$prop.corr
agg.data2$err <- sqrt((agg.data2$prop.corr * agg.data2$q) / agg.data2$total)




plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0), legend.position=c(0.30,.75),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))


dodge <- position_dodge(width=0.9) 
limits <- aes(ymax = prop.corr + err, ymin=prop.corr - err) 


rename(data$agegroup, c("3.0--3.5", "3.5--4.0", "4.0--4.5", "4.5--5.0"))

qplot(data = agg.data2,
	x = agegroup,
	y = prop.corr,
	geom="bar",
	stat="identity",
	fill=contrasts,
	#main="Inanimate 3-5 results", 
	ylab="Proportion Correct Contrast Judgement",
	xlab="Age",
	position=dodge,
	ylim=c(0,1)) + geom_abline(intercept=.5,slope=0,lty=2) + geom_errorbar(limits,position=dodge,width=0) + theme_bw() + plot.style + scale_fill_manual(values=c("orange", "red"))
