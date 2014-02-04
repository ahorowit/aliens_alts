rm(list=ls())
library(lme4)
library(plotrix)
library(ggplot2)
d1 <- read.csv("~/new compy/Stanford/Research/Frank Lab/Conferences/CogSci/CogSci 2014/aliens_alts_analyses/aliens_alts_kids_expts2b&3.csv")


##########################TEST TRIALS#########################
library(reshape)

d2=melt.data.frame(d1,c("Sub_ID","experimenter","DOB","DOT","Age","condition","book_type","agegroup", "location"),c("glorp","tibu","peebo", "zib", "glorp_type", "tibu_type", "peebo_type", "zib_type"))



d2=melt.data.frame(d1,c("Sub_ID","experimenter","DOB","DOT","Age","condition","book_type","agegroup", "location"),c(10, 13, 16, 10,8,11,14,17))
data <- d2[1:332,] 
data$contrasts <- d2$value[333:664]
names(data)[11] <- "correct"
names(data)[10] <- "alien"

data$contrasts[data$contrasts=="size "] <- "size"
data$contrasts <- as.factor(data$contrasts)
data$correct <- data$correct==1
data$alien <- as.factor(data$alien)
data$alien <- factor(data$alien, levels = c('glorp', 'peebo', 'tibu', 'zib'))
data$Age=as.numeric(data$Age)
data$agegroup <- as.factor(data$agegroup)
data$older <- (data$Age>4.5)



agg.data <- aggregate(data$correct, list(data$contrasts, data$older, data$book_type), FUN=sum)
agg.data.len <- aggregate(data$correct, list(data$contrasts, data$older, data$book_type), FUN=length)
agg.data$x <- agg.data$x 
agg.data.len$x <- agg.data.len$x 

names(agg.data) <- c("contrasts", "older", "book_type", "count")
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$count / agg.data$total

agg.data$q <- 1 - agg.data$prop.corr
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)


plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0), legend.position=c(1,.75),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

dodge <- position_dodge(width=0.9) 
limits <- aes(ymax = prop.corr + err, ymin=prop.corr - err) 

agg.data$age <- ifelse(agg.data$older, "4.5-5.0", "4.0-4.5")

qplot(data = agg.data,
	x = age,
	y = prop.corr,
	geom="bar",
	stat="identity",
	fill=contrasts,	
	main="Experiment 5 results", 
	ylab="Proportion Correct Contrast Judgement",
	xlab="Age",
	position=dodge,
	ylim=c(0,1)) + facet_wrap("book_type") + geom_abline(intercept=.5,slope=0,lty=2) + geom_errorbar(limits,position=dodge,width=0) + theme_bw() + plot.style + scale_fill_manual(values=c("orange", "red"))

	#look up "point range" for error bar alternative
	#add tops and bottoms by changing width 
	#


## both expts
int.lmer <- lmer(correct ~ contrasts*book_type*older+(contrasts|Sub_ID) + (1|alien), family="binomial", data=data) 
summary(int.lmer)


## No Special only 
twob <- data[data$book_type=="Experiment 2b",]
twob$contrasts <- factor(twob$contrasts,levels=c("Size","Feature"))

int.NoSpecial <- lmer(correct ~ contrasts*older+(contrasts|Sub_ID) + (1|alien), family="binomial", data=twob) 
summary(int.NoSpecial)
aggregate(correct ~ contrasts + older + alien, data=twob, mean)

## pairs only 
int.pairs <- lmer(correct ~ contrasts*older+(contrasts|Sub_ID) + (1|alien), family="binomial", data=data[data$book_type=="Experiment 3",]) 
summary(int.pairs)



basic.lm  <- glm(correct ~ contrasts+alien+older+book_type, 	family="binomial", data=data)
summary(basic.lm)

basic.noalien.lm  <- glm(correct ~ contrasts+older+book_type, 	family="binomial", data=data)
summary(basic.noalien.lm)

basic.noalien.lmer <- lmer(correct ~ contrasts+book_type+older-1 
	+ (1|Sub_ID), 	
	family="binomial", data=data) 
summary(basic.noalien.lmer)

basic.lmer <- lmer(correct ~ contrasts+older+book_type
	+ (contrasts|Sub_ID) + (contrasts|alien), 	
	family="binomial", data=data) 	
summary(basic.lmer)
###### run random effects sub only in models
##### why not intercept? 


###****###
int.lmer <- lmer(correct ~ contrasts*book_type*older+(contrasts|Sub_ID) + (1|alien), family="binomial", data=data) 
summary(int.lmer)

int.lmer2 <- lmer(correct ~ contrasts+book_type*older+(contrasts|Sub_ID) + (1|alien), family="binomial", data=data) 
summary(int.lmer2)

int.lm1  <- glm(correct ~ contrasts * book_type, family="binomial",data=data)
summary(int.lm1)


int.lm2  <- glm(correct ~ contrasts * older * book_type, family="binomial",data=data)
summary(int.lm2)


int.lm3  <- glm(correct ~  contrasts + book_type * older, family="binomial",data=data)
summary(int.lm3)


step.lm <- step(int.lm3,direction="both")
