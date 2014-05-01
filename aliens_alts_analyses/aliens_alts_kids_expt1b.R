rm(list=ls())
library(lme4)
library(plotrix)
library(ggplot2)

d1 <- read.csv("~/new compy/Stanford/Research/Frank Lab/Conferences/CogSci/CogSci 2014/aliens_alts_analyses/aliens_alts_kids_expt1b.csv")


## reshape data from wide to long form
library(reshape)

## keeps subject information constant, sort trial shape information
d2=melt.data.frame(d1,c("Sub_ID","DOB","DOT","Age","condition","agegroup"),c("glorp","tibu","peebo", "zib", "glorp_type", "tibu_type", "peebo_type", "zib_type"))

## recombine so that data sorted in long form
d2=melt.data.frame(d1,c("Sub_ID","DOB","DOT","Age","condition", "agegroup"),c(8,11,14,17,6,9,12,15))
data <- d2[1:392,] 
data$contrasts <- d2$value[393:784]

##rename columns 
names(data)[7] <- "correct"
names(data)[6] <- "shape"
names(data) <- c("Sub_ID","DOB","DOT","Age","condition", "agegroup", "shape", "correct", "contrasts")

## categorize data for analyses
data$correct <- data$correct==1
data$shape <- factor(data$shape, levels = c('glorp', 'peebo', 'tibu', 'zib'))
data$Age=as.numeric(data$Age)
data$agegroup <-factor(data$agegroup, levels=c('3.0 - 3.5', '3.5 - 4.0', '4.0 - 4.5', '4.5 - 5.0'))

## aggregate correct responses by contrast type and age group
agg.data <- aggregate(data$correct, list(data$contrasts, data$agegroup), FUN=sum)
agg.data.len <- aggregate(data$correct, list(data$contrasts, data$agegroup), FUN=length)


## rename colunmns, calculate proportion correct and standard error
names(agg.data) <- c("contrasts", "agegroup", "count")
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$count / agg.data$total
agg.data$q <- 1 - agg.data$prop.corr
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)



############ Plot ###########

## ggplot aethetics 
plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0), legend.position=c(0.30,.75),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

## error bars 
dodge <- position_dodge(width=0.9) 
limits <- aes(ymax = prop.corr + err, ymin=prop.corr - err) 

## relabel age groups for prettier labels
rename(data$agegroup, c("3.0--3.5", "3.5--4.0", "4.0--4.5", "4.5--5.0"))

## plot proportion correct (y) by age group (x) for each contrast type (bar fill)
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
