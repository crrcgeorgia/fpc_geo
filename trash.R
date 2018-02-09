natop <- data.frame(nato = rep(c(1, 2, 3, 4, 5), 45), eu = mean(cb2017mod$eu, na.rm=TRUE), 
                    eeu = mean(cb2017mod$eeu, na.rm=TRUE), edu=rep(c(1, 2, 3), 3),
                    ethn = mean(cb2017mod$ethn, na.rm=TRUE), age = mean(cb2017mod$age, na.rm=TRUE),
                    stratum = rep(c(1, 2, 3), each=3))
nato.write <- cbind(natop, predict(covar, newdata = 
                                     natop, type = "response", 
                                      se.fit = TRUE))
nato.write$model <- "nato"

names(nato.write) <- c("cat", "cov1","cov2", "cov3", "cov4", "cov5", "cov6",  "fit", "sefit", "resid", "model")

eup <- data.frame(eu = rep(c(1, 2, 3, 4, 5), 45), nato = mean(cb2017mod$eu, na.rm=TRUE), 
                  eeu = mean(cb2017mod$eeu, na.rm=TRUE),  edu=rep(c(1, 2, 3), 3),
                  ethn = mean(cb2017mod$ethn, na.rm=TRUE), age = mean(cb2017mod$age, na.rm=TRUE),
                  stratum = rep(c(1, 2, 3), each=3))
eu.write <- cbind(eup, predict(covar, newdata = eup,
                               type = "response", se.fit = TRUE))
eu.write$model <- "eu"
names(eu.write) <- c("cat", "cov1","cov2", "cov3", "cov4", "cov5", "cov6", "fit", "sefit", "resid", "model")

eeup <- data.frame(eeu = rep(c(1, 2, 3, 4, 5), 45), nato = mean(cb2017mod$eu, na.rm=TRUE), 
                   eu = mean(cb2017mod$eu, na.rm=TRUE),  edu=rep(c(1, 2, 3), 3),
                   ethn = mean(cb2017mod$ethn, na.rm=TRUE), age = mean(cb2017mod$age, na.rm=TRUE),
                   stratum = rep(c(1, 2, 3), each=3))
eeu.write <- cbind(eeup, predict(covar, newdata = eeup,
                                 type = "response", se.fit = TRUE))
eeu.write$model <- "eeu"
names(eeu.write) <- c("cat", "cov1","cov2", "cov3", "cov4", "cov5", "cov6", "fit", "sefit", "resid", "model")


pp.write <- rbind(nato.write, eu.write, eeu.write)

pp.write$cat <- factor(pp.write$cat,
                       labels=c("Do not\nsupport at\nall", "Rather not\nsupport", "Partially support,\npartially do not",
					   "Rather\nsupport", "Fully\nsupport"))

pp.write$cov6 <- factor(pp.write$cov6,
						labels=c("Tbilisi", "Other urban", "Rural" ))

pp.write$cov3 <- factor(pp.write$cov3,
                        labels=c("Secondary", "Vocational", "Higher" ))

#### collect all predictions:

ggplot(pp.write, aes(cat, fit, group=cov3))+
  geom_point(aes(color=model, group=cov3), position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymax = fit+1.96*sefit,
                    ymin = fit-1.96*sefit,
                    color=model, group=cov3),
                width=0.1, size=0.4, position=position_dodge(width=0.4))+
  theme_plot+
  coord_cartesian(ylim = c(0, 1), expand = FALSE)+
  facet_wrap(~cov6+cov3)+
  theme(
    legend.position="top",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
    axis.text.x = element_text(size=8),
    strip.background = element_blank(),
    panel.spacing = unit(2, "lines")
  )

ggsave("combination.png", height=7, width=13.5, dpi=300, device="png")