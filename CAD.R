library(extrafont)
library(readstata13)
library(ggplot2)
library(survey)
library(kableExtra)
library(knitr)
library(pacman)
library(tidyr)
library(plyr)
library(haven)
library(gridExtra)
library(ggalluvial)
library(stargazer)
library(nnet)
library(clusterSEs)
library(dplyr)
### library(Zelig) WTF, Gary King?

options(knitr.table.format = "html")

setwd("D:\\Dropbox\\My Projects\\CRRC\\Caucasus Barometer\\2017\\analysis\\CAD\\models")

cb2017ge <- read.dta13("CB_2017_Georgia_10.11.17.dta",
                               convert.factors = TRUE,
                               nonint.factors = TRUE,
                               generate.factors = TRUE,
                               encoding = "UTF-8")

cb2017ge[ , 1:328 ][ cb2017ge[ , 1:328 ] == "Break off" ] <- NA
cb2017ge[ , 1:328 ][ cb2017ge[ , 1:328 ] == "Legal skip" ] <- NA
cb2017ge[ , 1:328 ][ cb2017ge[ , 1:328 ] == "Interviewer error" ] <- NA

cb2017ge$psu[cb2017ge$psu==202013] <- 202014
cb2017ge$psu[cb2017ge$psu==1106004] <- 1106014
cb2017ges<-svydesign(id=~psu+id, strata=~substratum, weights=~indwt, fpc=~npsuss+nhhpsu, data=cb2017ge)

theme_plot <- theme(
  axis.text.y = element_text(colour="black", size = 16, family = "Gill Sans MT"),
  axis.text.x = element_text(colour="black", size = 16, family="Gill Sans MT"),
  axis.title.x = element_text(size=16, family = "Futura Hv BT"),
  axis.title.y = element_text(size=16, family = "Futura Hv BT"),
  strip.text  = element_text(size=16, family = "Futura Hv BT"),
  panel.border = element_rect(fill=NA, linetype = "solid", colour = "black"),
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(hjust = 0.5, colour = "Black", size=18, family = "Futura Hv BT"),
  plot.subtitle = element_text(hjust = 0.5, colour = "Black", size=14, family = "Futura Hv BT"),
  legend.title = element_text(size=12, family = "Futura Hv BT"),
  legend.text = element_text(size=14, family = "Gill Sans MT"),
  plot.caption = element_text(size=12, family = "Gill Sans MT")
)


#### P23
p23<-as.data.frame(svymean(~p23, cb2017ges, na.rm=TRUE), drop.unused.levels = TRUE)
p23$Cat <- row.names(p23)
p23$Cat <- gsub("p23", "", p23$Cat)
p23 <- subset(p23, !Cat %in% c("Break off", "Legal skip", "Interviewer error"))
kable(p23, format = "html")
row.names(p23) <- NULL

p23chart <- subset(p23, !Cat %in% c("Don't know", "Refuse to answer", "Other"))

p23chart$Strat <- c("Question")
p23chart$Strat[p23chart$Cat=="Don't know" | p23chart$Cat=="Refuse to answer" ] <- c("DK/RA")
p23chart$Cat <- factor(p23chart$Cat, levels = p23chart$Cat[order(p23chart$mean)])

p23dkra <- subset(p23, Cat %in% c("Don't know", "Refuse to answer", "Other"))
p23dkra$Cat <- factor(p23dkra$Cat, levels = c("Don't know", "Refuse to answer", "Other"))
p23dkra$Strat <- c("Question")
p23dkra$Strat[p23dkra$Cat=="Don't know" | p23dkra$Cat=="Refuse to answer" ] <- c("DK/RA")

p23chart <- rbind(p23dkra, p23chart)


ggplot(p23chart, aes(y=mean, x=Cat,group=Strat,fill=Strat))+
  geom_col()+
  coord_flip()+
  theme_plot+
  labs(x = "Option",
       
       title="What is the main reason you would not support\nGeorgia’s membership in NATO? (%)",
	   subtitle="The question was asked to the %\nwho did not support Georgia's membership in NATO")+
  geom_text(data=p23chart,
            aes(x=Cat,y=mean,label=ifelse(mean > 0.015, sprintf("%0.f", round(mean*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
 scale_fill_manual(name="კატეგორია",
  					values=c("#999999","#66c2a5"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
		theme(legend.position = "none",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
				legend.direction = "vertical")

ggsave("why_not_nato.png", height=7, width=13.5, dpi=300, device="png")
				
#### P23
p29<-as.data.frame(svymean(~p29, cb2017ges, na.rm=TRUE), 
                   drop.unused.levels = TRUE)
p29$Cat <- row.names(p29)
p29$Cat <- gsub("p29", "", p29$Cat)
p29 <- subset(p29, !Cat %in% c("Break off", "Legal skip", "Interviewer error"))
kable(p29, format = "html")
row.names(p29) <- NULL

p29chart <- subset(p29, !Cat %in% c("Don't know", "Refuse to answer", "Other"))

p29chart$Strat <- c("Question")
p29chart$Strat[p29chart$Cat=="Don't know" | p29chart$Cat=="Refuse to answer" ] <- c("DK/RA")
p29chart$Cat <- factor(p29chart$Cat, levels = p29chart$Cat[order(p29chart$mean)])

p29dkra <- subset(p29, Cat %in% c("Don't know", "Refuse to answer", "Other"))
p29dkra$Cat <- factor(p29dkra$Cat, levels = c("Don't know", "Refuse to answer", "Other"))
p29dkra$Strat <- c("Question")
p29dkra$Strat[p29dkra$Cat=="Don't know" | p29dkra$Cat=="Refuse to answer" ] <- c("DK/RA")

p29chart <- rbind(p29dkra, p29chart)


ggplot(p29chart, aes(y=mean, x=Cat,group=Strat,fill=Strat))+
  geom_col()+
  coord_flip()+
  theme_plot+
  labs(x = "Option",
       
       title="What is the main reason you would not support\nGeorgia’s membership in EEU? (%)",
	   subtitle="The question was asked to the %\nwho did not support Georgia's membership in EEU")+
  geom_text(data=p29chart,
            aes(x=Cat,y=mean,label=ifelse(mean > 0.015, sprintf("%0.f", round(mean*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
 scale_fill_manual(name="კატეგორია",
  					values=c("#999999","#66c2a5"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
		theme(legend.position = "none",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
				legend.direction = "vertical")

ggsave("why_not_odkb.png", height=7, width=13.5, dpi=300, device="png")
				
### Rand1

rand1<-as.data.frame(svymean(~rand1, cb2017ges, na.rm=TRUE), drop.unused.levels = TRUE)
rand1$Cat <- row.names(rand1)
rand1$Cat <- gsub("rand1", "", rand1$Cat)
rand1 <- subset(rand1, !Cat %in% c("Break off", "Legal skip", "Interviewer error"))
kable(rand1, format = "html")
row.names(rand1) <- NULL
rand1l <- rand1$Cat
rand1$Cat <- factor(rand1$Cat, levels=rand1l)

ggplot(rand1, aes(y=mean, x=Cat, fill=Cat))+
  geom_col()+
  coord_flip()+
  theme_plot+
  labs(x = "Option",
       
       title="Agree or disagree: tensions between Russia and the Western European countries and the US\nare detrimental to Georgia? (%)")+
  geom_text(data=rand1,
            aes(x=Cat,y=mean,label=ifelse(mean > 0.015, sprintf("%0.f", round(mean*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
 scale_fill_manual(name="კატეგორია",
  					values=c("#999999","#d6d6d6","#a6611a","#dfc27d","#80cdc1", "#018571"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
		theme(legend.position = "none",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
				legend.direction = "vertical")

ggsave("tensions_freq.png", height=7, width=13.5, dpi=300, device="png")

### Models

cb2017mod <- read_dta("CB_2017_Georgia_10.11.17.dta")
cb2017mod[, 1:373][ cb2017mod[ , 1:373 ] == -9 ] <- NA
cb2017mod[, 1:373][ cb2017mod[ , 1:373 ] == -3 ] <- NA

#### rand2 as a depvar: 1 neutral 2 block 3 neither
cb2017mod$neut <- cb2017mod$rand2
cb2017mod$neut[cb2017mod$neut==1 | cb2017mod$neut==2 ] <- 1
cb2017mod$neut[cb2017mod$neut==3 | cb2017mod$neut==4 ] <- 2
cb2017mod$neut[cb2017mod$neut==-2 | cb2017mod$neut==-1 ] <- 3

cb2017mod$neutbin <- 0
cb2017mod$neutbin[cb2017mod$rand2==1 | cb2017mod$rand2==2] <- 1

#### p22 as iv: dk/ra to the middle
cb2017mod$nato <- cb2017mod$p22
cb2017mod$nato[cb2017mod$nato==-2 | cb2017mod$nato==-1 ] <- 3

#### p25 as iv: dk/ra to the middle
cb2017mod$eu <- cb2017mod$p25
cb2017mod$eu[cb2017mod$eu==-2 | cb2017mod$eu==-1 ] <- 3

#### p25 as iv: dk/ra to the middle
cb2017mod$eeu <- cb2017mod$p28_geo
cb2017mod$eeu[cb2017mod$eeu==-2 | cb2017mod$eeu==-1 ] <- 3


### Model with covariates

cb2017mod$edu <- cb2017mod$d3
cb2017mod$edu[cb2017mod$edu>0 & cb2017mod$edu<5] <- 1
cb2017mod$edu[cb2017mod$edu==5] <- 2
cb2017mod$edu[cb2017mod$edu>5 & cb2017mod$edu<9] <- 3

# stratum

cb2017mod$ethn <- cb2017mod$d1
cb2017mod$ethn[cb2017mod$ethn>0 & cb2017mod$ethn<3 ] <- 0
cb2017mod$ethn[cb2017mod$ethn==3] <- 1
cb2017mod$ethn[cb2017mod$ethn>3] <- 0
table(cb2017mod$ethn)


# age
# rand1
cb2017mod$rand1mod <- cb2017mod$rand1
cb2017mod$rand1mod[cb2017mod$rand1mod==4] <- 5
cb2017mod$rand1mod[cb2017mod$rand1mod==3] <- 4
cb2017mod$rand1mod[cb2017mod$rand1mod==-1] <- 3
cb2017mod$rand1mod[cb2017mod$rand1mod==-2] <- 3
table(cb2017mod$rand1mod)

# Russia!

cb2017mod$ru <- 0
cb2017mod$ru[cb2017mod$p32==22] <- 1

cb2017mod$us <- 0
cb2017mod$us[cb2017mod$p31==26] <- 1

prop.table(table(cb2017mod$us))


covar <- glm(neutbin~nato+eu+eeu+rand1mod+factor(edu)+factor(stratum)+ethn+age, 
             data=cb2017mod, family="binomial")
summary(covar)

# clust.bs.p <- cluster.bs.glm(covar, cb2017mod, ~ psu, report = T)

pp <- fitted(covar)

### Settlement type

natop <- data.frame(nato = rep(c(1, 2, 3, 4, 5), 3), eu = mean(cb2017mod$eu, na.rm=TRUE), 
                    eeu = mean(cb2017mod$eeu, na.rm=TRUE), edu=1,
                    ethn = mean(cb2017mod$ethn, na.rm=TRUE), rand1mod = mean(cb2017mod$rand1mod, na.rm=TRUE), age = mean(cb2017mod$age, na.rm=TRUE),
                    stratum = rep(c(1, 2, 3), each=5))
nato.write <- cbind(natop, predict(covar, newdata = 
                                     natop, type = "response", 
                                      se.fit = TRUE))
nato.write$model <- "nato"

names(nato.write) <- c("cat", "cov1","cov2", "cov3", "cov4", "cov5", "cov6","cov7",  "fit", "sefit", "resid", "model")

eup <- data.frame(eu = rep(c(1, 2, 3, 4, 5), 3), nato = mean(cb2017mod$nato, na.rm=TRUE), 
                  eeu = mean(cb2017mod$eeu, na.rm=TRUE),  edu=1,
                  ethn = mean(cb2017mod$ethn, na.rm=TRUE), rand1mod = mean(cb2017mod$rand1mod, na.rm=TRUE), age = mean(cb2017mod$age, na.rm=TRUE),
                  stratum = rep(c(1, 2, 3), each=5))
eu.write <- cbind(eup, predict(covar, newdata = eup,
                               type = "response", se.fit = TRUE))
eu.write$model <- "eu"
names(eu.write) <- c("cat", "cov1","cov2", "cov3", "cov4", "cov5", "cov6", "cov7", "fit", "sefit", "resid", "model")

eeup <- data.frame(eeu = rep(c(1, 2, 3, 4, 5), 3), nato = mean(cb2017mod$nato, na.rm=TRUE), 
                   eu = mean(cb2017mod$eu, na.rm=TRUE),  edu=1,
                   ethn = mean(cb2017mod$ethn, na.rm=TRUE), rand1mod = mean(cb2017mod$rand1mod, na.rm=TRUE), age = mean(cb2017mod$age, na.rm=TRUE),
                   stratum = rep(c(1, 2, 3), each=5))
eeu.write <- cbind(eeup, predict(covar, newdata = eeup,
                                 type = "response", se.fit = TRUE))
eeu.write$model <- "eeu"
names(eeu.write) <- c("cat", "cov1","cov2", "cov3", "cov4", "cov5", "cov6", "cov7", "fit", "sefit", "resid", "model")


pp.write <- rbind(nato.write, eu.write, eeu.write)

pp.write$cat <- factor(pp.write$cat,
                       labels=c("Do not\nsupport at\nall", "Rather not\nsupport", "Partially support,\npartially do not",
					   "Rather\nsupport", "Fully\nsupport"))

pp.write$cov7 <- factor(pp.write$cov7,
						labels=c("Tbilisi", "Other urban", "Rural" ))


#### collect all predictions:

ggplot(pp.write, aes(cat, fit, group=model))+
  geom_point(aes(color=model), position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymax = fit+1.96*sefit,
                    ymin = fit-1.96*sefit,
                    color=model),
                width=0.1, size=0.4, position=position_dodge(width=0.4))+
  theme_plot+
  coord_cartesian(ylim = c(0, 1), expand = FALSE)+
  facet_wrap(~cov7)+
  labs(title="Agree That Georgia Should Be Neutral",
  subtitle="Predicted Probabilities",
  x="To What Extent do You Support Georgia's Membership in...")+
  theme(
    legend.position="top",
    axis.title.y = element_blank(),
    # axis.title.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
    axis.text.x = element_text(size=10),
    strip.background = element_blank(),
    panel.spacing = unit(2, "lines")
  )

ggsave("natomemb_settlement.png", height=7, width=13.5, dpi=300, device="png")


### Rand1

natop <- data.frame(rand1mod = rep(c(1, 2, 3, 4, 5), 3), eu = mean(cb2017mod$eu, na.rm=TRUE), 
                    eeu = mean(cb2017mod$eeu, na.rm=TRUE), edu=1,
                    ethn = mean(cb2017mod$ethn, na.rm=TRUE), nato = mean(cb2017mod$nato, na.rm=TRUE), age = mean(cb2017mod$age, na.rm=TRUE),
                    stratum = rep(c(1, 2, 3), each=5))

pp.write <- cbind(natop, predict(covar, newdata = 
                                     natop, type = "response", 
                                      se.fit = TRUE))
names(pp.write) <- c("cat", "cov1","cov2", "cov3", "cov4", "cov5", "cov6","cov7",  "fit", "sefit", "resid")


pp.write$cov7 <- factor(pp.write$cov7,
						labels=c("Tbilisi", "Other urban", "Rural" ))

pp.write$cat <- factor(pp.write$cat,
                       labels=c("Completely\nagree", "Rather\nagree", "In the middle",
					   "Rather\ndisagree", "Completely\ndisagree"))
					   
ggplot(pp.write, aes(cat, fit, group=cov7))+
  geom_point(aes(color=cov7),
             position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymax = fit+1.96*sefit,
                    ymin = fit-1.96*sefit,
                    color=cov7),
                width=0.1, size=0.4, 
                position=position_dodge(width=0.4))+
  theme_plot+
  coord_cartesian(ylim = c(0, 1), expand = FALSE)+
  labs(title="Agree That Georgia Should Be Neutral",
  subtitle="Predicted Probabilities",
  x="Tensions between Russia and\nthe Western European countries and\nthe US are detrimental to Georgia")+
  theme(
    legend.position="top",
    axis.title.y = element_blank(),
    # axis.title.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
    axis.text.x = element_text(size=10),
    strip.background = element_blank(),
    panel.spacing = unit(2, "lines")
  )

ggsave("tensions_settlement.png", height=7, width=13.5, dpi=300, device="png")

### Rand1

natop <- data.frame(age = rep(c(18:100), 3), rand1mod = mean(cb2017mod$rand1mod, na.rm=TRUE), eu = mean(cb2017mod$eu, na.rm=TRUE), 
                    eeu = mean(cb2017mod$eeu, na.rm=TRUE), edu=rep(c(1, 2, 3), each=1),
                    ethn = 1, nato = mean(cb2017mod$nato, na.rm=TRUE),
                    stratum = 1)
					
pp.write <- cbind(natop, predict(covar, newdata = 
                                     natop, type = "response", 
                                      se.fit = TRUE))

names(pp.write) <- c("cat", "cov1","cov2", "cov3", "cov4", "cov5", "cov6","cov7",  "fit", "sefit", "resid")


pp.write$cov5 <- factor(pp.write$cov5,
                       labels=c("Other", "Georgian"))
					   
ggplot(pp.write, aes(cat, fit, group=cov4))+
  geom_line(aes(color=cov4))+
  # geom_ribbon(aes(ymax = fit+1.96*sefit,
  #                  ymin = fit-1.96*sefit), alpha=0.2)+
  theme_plot+
  coord_cartesian(ylim = c(0, 1), expand = FALSE)+
  labs(title="Agree That Georgia Should Be Neutral",
  subtitle="Predicted Probabilities",
  x="Tensions between Russia and\nthe Western European countries and\nthe US are detrimental to Georgia")+
  theme(
    legend.position="top",
    axis.title.y = element_blank(),
    # axis.title.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
    axis.text.x = element_text(size=10),
    strip.background = element_blank(),
    panel.spacing = unit(2, "lines")
  )

ggsave("tensions_settlement.png", height=7, width=13.5, dpi=300, device="png")




### crosstabs:

rand1.stratum <- as.data.frame(svytable(~rand1+stratum, cb2017ges), drop.unused.levels = TRUE)
rand1.stratum <- subset(rand1.stratum, !rand1 %in% c("Break off", "Legal skip", "Interviewer error"))
rand1.stratum <- rand1.stratum %>%
  group_by(stratum) %>%
  mutate(prop = Freq/sum(Freq))
  
ggplot(rand1.stratum, aes(stratum, prop, fill=rand1))+
  geom_bar(stat="identity", position="stack")+
  coord_flip()

rand2 <- as.data.frame(svytable(~rand2, cb2017ges), drop.unused.levels = TRUE) %>%
  subset(., !rand2 %in% c("Break off", "Legal skip", "Interviewer error")) %>%
  mutate(prop=Freq/sum(Freq))

p22 <- as.data.frame(svytable(~p22, cb2017ges), drop.unused.levels = TRUE) %>%
  subset(., !p22 %in% c("Break off", "Legal skip", "Interviewer error")) %>%
  mutate(prop=Freq/sum(Freq))

ggplot(rand2.p22, aes(p22, prop, fill=rand2))+
  geom_bar(stat="identity", position="stack")+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  geom_text(data=rand2.p22,
            aes(x=p22,y=prop,label=ifelse(prop > 0.015, sprintf("%0.f", round(prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
  scale_fill_manual(name="კატეგორია",
                    values=c("#999999","#d6d6d6","#a6611a","#dfc27d","#80cdc1", "#018571"))+
  labs(title="Alignment with a bloc vs. neutrality \nBy support of membership in NATO",
  subtitle="Caucasus Barometer, 2017")+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text.align = 0,
    legend.title = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0.5,6,1,0.5), "cm")
  )
  
ggsave("neutral_nato_xtab.png", height=7, width=13.5, dpi=300, device="png")


rand2.rand1 <- as.data.frame(svytable(~rand2+rand1, cb2017ges), drop.unused.levels = TRUE) %>%
  subset(., !rand2 %in% c("Break off", "Legal skip", "Interviewer error") & !rand1 %in% c("Break off", "Legal skip", "Interviewer error")) %>%
  group_by(rand1) %>%
  mutate(prop=Freq/sum(Freq))

ggplot(rand2.rand1, aes(rand1, prop, fill=rand2))+
  geom_bar(stat="identity", position="stack")+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  geom_text(data=rand2.rand1,
            aes(x=rand1,y=prop,label=ifelse(prop > 0.015, sprintf("%0.f", round(prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
  scale_fill_manual(name="კატეგორია",
                    values=c("#999999","#d6d6d6","#a6611a","#dfc27d","#80cdc1", "#018571"))+
  labs(title="Alignment with a bloc vs. neutrality \nBy tensions between the West and Russia are detrimental to Georgia",
  subtitle="Caucasus Barometer, 2017")+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text.align = 0,
    legend.title = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0.5,3.5,1,0.5), "cm")
  )

# ggsave("neutral_tensions_xtab.png", height=7, width=13.5, dpi=300, device="png")

	
rand2.d6 <- as.data.frame(svytable(~rand2+d6, cb2017ges), drop.unused.levels = TRUE) %>%
  subset(., !rand2 %in% c("Break off", "Legal skip", "Interviewer error") & !d6 %in% c("Break off", "Legal skip", "Interviewer error")) %>%
  group_by(d6) %>%
  mutate(prop=Freq/sum(Freq))

ggplot(rand2.d6, aes(d6, prop, fill=rand2))+
  geom_bar(stat="identity", position="stack")+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  geom_text(data=rand2.d6,
            aes(x=d6,y=prop,label=ifelse(prop > 0.015, sprintf("%0.f", round(prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
  scale_fill_manual(name="კატეგორია",
                    values=c("#999999","#d6d6d6","#a6611a","#dfc27d","#80cdc1", "#018571"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  theme_plot+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.direction = "vertical")
	
## ggsave("neutral_flang_xtab.png", height=7, width=13.5, dpi=300, device="png")

### NATO Membership and migrants

p22.m1 <- as.data.frame(svytable(~p22+m1, cb2017ges), drop.unused.levels = TRUE) %>%
  subset(., !p22 %in% c("Break off", "Legal skip", "Interviewer error") & 
           !m1 %in% c("Refuse to answer", "Break off", "Legal skip", "Interviewer error")) %>%
  group_by(m1) %>%
  mutate(prop=Freq/sum(Freq))

ggplot(p22.m1, aes(m1, prop, fill=p22))+
  geom_bar(stat="identity", position="stack")+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  geom_text(data=p22.m1,
            aes(x=m1,y=prop,label=ifelse(prop > 0.015, 
                                        sprintf("%0.f", round(prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
  scale_fill_manual(name="კატეგორია",
                    values=c("#999999", "#444444", "#a6611a","#dfc27d","#d6d6d6", "#80cdc1", "#018571"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  labs(title="Support of Georgia's membership in NATO \nBy the attitudes towards migrants",
  subtitle="Caucasus Barometer, 2017")+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text.align = 0,
    legend.title = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0.5,3,1,0.5), "cm")
  )

ggsave("nato_migrants_xtab.png", height=7, width=13.5, dpi=300, device="png")

### EU Membership and migrants

p25.m1 <- as.data.frame(svytable(~p25+m1, cb2017ges), drop.unused.levels = TRUE) %>%
  subset(., !p25 %in% c("Break off", "Legal skip", "Interviewer error") & 
           !m1 %in% c("Refuse to answer", "Break off", "Legal skip", "Interviewer error")) %>%
  group_by(m1) %>%
  mutate(prop=Freq/sum(Freq))

ggplot(p25.m1, aes(m1, prop, fill=p25))+
  geom_bar(stat="identity", position="stack")+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  geom_text(data=p25.m1,
            aes(x=m1,y=prop,label=ifelse(prop > 0.015, 
                                        sprintf("%0.f", round(prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
  scale_fill_manual(name="კატეგორია",
                    values=c("#999999", "#444444", "#a6611a","#dfc27d","#d6d6d6", "#80cdc1", "#018571"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  labs(title="Support of Georgia's membership in the EU \nBy the attitudes towards migrants",
  subtitle="Caucasus Barometer, 2017")+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text.align = 0,
    legend.title = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0.5,3,1,0.5), "cm")
  )

ggsave("eu_migrants_xtab.png", height=7, width=13.5, dpi=300, device="png")

### EU Membership and migrants

p28_geo.p13 <- as.data.frame(svytable(~p28_geo+p13, cb2017ges), drop.unused.levels = TRUE) %>%
  subset(., !p28_geo %in% c("Break off", "Legal skip", "Interviewer error") & 
           !p13 %in% c("Break off", "Legal skip", "Interviewer error")) %>%
  group_by(p13) %>%
  mutate(prop=Freq/sum(Freq))

ggplot(p28_geo.p13, aes(p13, prop, fill=p28_geo))+
  geom_bar(stat="identity", position="stack")+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  geom_text(data=p28_geo.p13,
            aes(x=p13,y=prop,label=ifelse(prop > 0.015, 
                                        sprintf("%0.f", round(prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
  scale_fill_manual(name="კატეგორია",
                    values=c("#999999", "#444444", "#a6611a","#dfc27d","#d6d6d6", "#80cdc1", "#018571"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  labs(title="Support of Georgia's membership in the EU \nBy the attitudes towards migrants",
  subtitle="Caucasus Barometer, 2017")+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text.align = 0,
    legend.title = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0.5,3,1,0.5), "cm")
  )

ggsave("eu_govtrol_xtab.png", height=7, width=13.5, dpi=300, device="png")

### EU Membership and migrants

p28_geo.p13 <- as.data.frame(svytable(~p28_geo+p13, cb2017ges), drop.unused.levels = TRUE) %>%
  subset(., !p28_geo %in% c("Break off", "Legal skip", "Interviewer error") & 
           !p13 %in% c("Break off", "Legal skip", "Interviewer error")) %>%
  group_by(p13) %>%
  mutate(prop=Freq/sum(Freq))

ggplot(p28_geo.p13, aes(p13, prop, fill=p28_geo))+
  geom_bar(stat="identity", position="stack")+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  geom_text(data=p28_geo.p13,
            aes(x=p13,y=prop,label=ifelse(prop > 0.015, 
                                        sprintf("%0.f", round(prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
  scale_fill_manual(name="კატეგორია",
                    values=c("#999999", "#444444", "#a6611a","#dfc27d","#d6d6d6", "#80cdc1", "#018571"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  labs(title="Support of Georgia's membership in the EU \nBy the attitudes towards migrants",
  subtitle="Caucasus Barometer, 2017")+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text.align = 0,
    legend.title = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0.5,3,1,0.5), "cm")
  )

ggsave("eeu_govtrol_xtab.png", height=7, width=13.5, dpi=300, device="png")

### Russia friend
cb2017ges$variables$ru <- 0
cb2017ges$variables$ru[cb2017ges$variables$p32=="Russia"] <- 1
rand2.ru <- as.data.frame(svytable(~rand2+ru, cb2017ges), drop.unused.levels = TRUE) %>%
  subset(., !rand2 %in% c("Break off", "Legal skip", "Interviewer error") & !ru %in% c("Break off", "Legal skip", "Interviewer error")) %>%
  group_by(ru) %>%
  mutate(prop=Freq/sum(Freq))

### Membership in Nato, EU, EEU
memb <- as.data.frame(svymean(~p22+p25+p28_geo, cb2017ges, na.rm=TRUE),
                      drop.unused.levels = TRUE) %>%
  mutate(Cat = row.names(.), Cat = gsub("p22|p25|p28_geo", "", Cat)) %>%
  subset(., !Cat %in% c("Break off", "Legal skip", "Interviewer error"))

memb$Strat <- "NATO"
memb$Strat[8:14] <- "EU"
memb$Strat[15:21] <- "EEU"

memb$Cat <- factor(memb$Cat, levels=c("Refuse to answer", "Don't know", "Do not support at all",
							"Rather not support", "Partially support, partially do not support", "Rather support",
							"Fully support"))
							
ggplot(memb, aes(x=Strat, fill=Cat))+
  geom_bar(data=memb, aes(y=mean), stat="identity", position="stack")+
  labs(title="How do you support Georgia's membership in ...? (%)",
  subtitle="Caucasus Barometer, 2017")+
  scale_fill_manual(name="კატეგორია",
                    values=c("#999999", "#444444", "#a6611a","#dfc27d", "#d6d6d6","#80cdc1", "#018571"))+
  scale_y_continuous(labels=function(x)x*100)+
  guides(fill = guide_legend(reverse=T))+
  coord_flip(xlim=c(1, 3))+
  geom_text(data=memb,
            aes(x=Strat,y=mean,label=ifelse(mean > 0.015, sprintf("%0.f", round(mean*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="BPG Excelsior Caps", color="white")+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text.align = 0,
    legend.title = element_blank(),
    plot.margin = unit(c(0.5,3.5,1,0.5), "cm")
  )

ggsave("membership.png", height=7, width=13.5, dpi=300, device="png")


### Interaction term

covar <- glm(neutbin~nato+eu+eeu+rand1mod+factor(edu)+
               factor(stratum)+ethn+age+ru*us, 
             data=cb2017mod, family="binomial")

summary(covar)

c <- as.data.frame(covar$coefficients)
c$cat <- row.names(c)
clust.bs.p <- cluster.bs.glm(covar, cb2017mod, ~ psu, report = T)
