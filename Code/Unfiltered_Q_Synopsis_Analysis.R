library(data.table)
library(lme4)
library(MuMIn)
library(DHARMa)
library(ggplot2)
library(car)
library(glm2)
library(emmeans)

##############################################################################################################################
############################## LLM performance on questions across synopses ##################################################
##############################################################################################################################

synopdata <- fread(choose.files())  #synopsesdata.csv

synopdatahr <- synopdata
binarydata <- list()
for(i in 1:nrow(synopdatahr)){
  binarydata[[i]] <- data.table(Model=rep(synopdatahr$Model[i],synopdatahr$Total[i]),
                                Category=rep(synopdatahr$Category[i],synopdatahr$Total[i]),
                                Synopsis=rep(synopdatahr$Synopsis[i],synopdatahr$Total[i]),
                                Binary=c(rep(1,synopdatahr$Correct[i]),
                                rep(0,synopdatahr$Total[i]-synopdatahr$Correct[i])))
}
synopdatahrbinary <- do.call(rbind,binarydata)

mod1 <- glm(Binary ~ Model*Synopsis+Category, data = synopdatahrbinary,family="binomial")
summary(mod1)
mod1.1 <- glm(Binary ~ Model*Synopsis+Model*Category, data = synopdatahrbinary,family="binomial")
summary(mod1.1)
str(summary(mod1))

# simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
# testDispersion(simulationOutput)
# residuals(simulationOutput)
# plot(simulationOutput)

str(summary(mod1))

modsum <- data.table(summary(mod1)$coefficients)

modsum$term <- dimnames(summary(mod1)$coefficients)[1]

modsum[`Pr(>|z|)`<0.01]

#write.csv(modsum,"Modelsummary_logreg_synxmod+cat.csv")

mod2 <- glm(Binary ~ Model+Synopsis+Category, data = synopdatahrbinary,family="binomial")
summary(mod2)

modsum2 <- data.table(summary(mod2)$coefficients)

modsum2$term <- dimnames(summary(mod2)$coefficients)[1]

#write.csv(modsum2,"Modelsummary_logreg_syn+mod+cat.csv")

mod3 <- glm(Binary ~ Model+Synopsis, data = synopdatahrbinary,family="binomial")
summary(mod3)
mod4 <- glm(Binary ~ Model+Category, data = synopdatahrbinary,family="binomial")
summary(mod4)
mod5 <- glm(Binary ~ Synopsis+Category, data = synopdatahrbinary,family="binomial")
summary(mod5)
mod6 <- glm(Binary ~ Synopsis, data = synopdatahrbinary,family="binomial")
summary(mod6)
mod7 <- glm(Binary ~ Category, data = synopdatahrbinary,family="binomial")
summary(mod7)
mod8 <- glm(Binary ~ Model, data = synopdatahrbinary,family="binomial")
summary(mod8)

AICc(mod1,mod2)
AICc(mod1.1,mod2)
AICc(mod3,mod2)
AICc(mod4,mod2)
AICc(mod5,mod2)
AICc(mod6,mod2)
AICc(mod7,mod2)
AICc(mod8,mod2)

modnull <- glm(Binary ~ 1, data = synopdatahrbinary,family="binomial")
summary(modnull)
AICc(mod1.1,modnull)
AICc(mod2,modnull)
AICc(mod6,modnull)
AICc(mod7,modnull)
AICc(mod8,modnull)
anova(mod2,modnull)
anova(mod6,modnull)
anova(mod7,modnull)
anova(mod8,modnull)

Anova(mod2, type = "II")

allcombs <- data.table(CJ(Model=unique(synopdatahrbinary$Model),
                          Category=unique(synopdatahrbinary$Category),
                          Synopsis=unique(synopdatahrbinary$Synopsis)))
allcombs$est <- predict(mod2,allcombs,type="response")
allcombs$se <- predict(mod2,allcombs,type="response",se.fit = TRUE)[[2]]

critval <- 1.96 ## approx 95% CIcritval <- 1.96 ## approx 95% CI
allcombs$upr <- allcombs$est + (critval * allcombs$se)
allcombs$lwr <- allcombs$est - (critval * allcombs$se)

unique(allcombs$Synopsis)
newnames <- data.table(Synopsis=gsub("([A-Za-z]+).*", "\\1", unique(allcombs$Synopsis)))
newnames[Synopsis=="Biodiversity", Synopsis:="Mar.Art.\nStructures"]
newnames[Synopsis=="Control", Synopsis:="FW\nInvasives"]
newnames[Synopsis=="Management", Synopsis:="Captive\nAnimals"]
newnames[Synopsis=="Marine", Synopsis:=c("Marine\nFish","Marine\nFW Mammal")]
newnames[Synopsis=="Mediterranean", Synopsis:="Med.\nFarmland"]
newnames[Synopsis=="Natural", Synopsis:="Nat.Pest\nControl"]
newnames[Synopsis=="Subtidal", Synopsis:="Subtidal\nBenthic"]
newnames[Synopsis=="Sustainable", Synopsis:="Aquaculture"]
newnames[Synopsis=="Terrestrial", Synopsis:="Terr.\nMammal"]

newnamedat <- data.table(oldname=unique(allcombs$Synopsis),newname=newnames$Synopsis)

oldnames <- unique(allcombs$Synopsis)
i=1
for(i in 1:length(oldnames)){
  allcombs[Synopsis==oldnames[i],Synopsis:=newnamedat[oldname==oldnames[i],newname]]
}
unique(allcombs$Synopsis)

library(tidyverse)
ordersyn <- data.table(allcombs %>% 
  group_by(Synopsis) %>%
  summarise(mean=mean(est)))

ordersynnames <- ordersyn[rev(order(mean)),Synopsis]

allcombs$Synopsis <- factor(allcombs$Synopsis,levels=ordersynnames)

allcombs_cb_hy_or <- allcombs[Category=="closed_book"|Category=="hybrid_retrieval"|Category=="oracle"]
allcombs_cb_hy_or <- allcombs_cb_hy_or[order(Synopsis)]

nrow(allcombs_cb_hy_or)/length(unique(allcombs_cb_hy_or$Synopsis))
allcombs_cb_hy_or_first <- allcombs_cb_hy_or[1:(30*11)]
allcombs_cb_hy_or_sec <- allcombs_cb_hy_or[(1+(30*11)):nrow(allcombs_cb_hy_or)]

allcombs_cf_sp_de <- allcombs[Category=="confused"|Category=="sparse_retrieval"|Category=="dense_retrieval"]
allcombs_cf_sp_de <- allcombs_cf_sp_de[order(Synopsis)]

nrow(allcombs_cf_sp_de)/length(unique(allcombs_cf_sp_de$Synopsis))
allcombs_cf_sp_de_first <- allcombs_cf_sp_de[1:(30*11)]
allcombs_cf_sp_de_sec <- allcombs_cf_sp_de[(1+(30*11)):nrow(allcombs_cf_sp_de)]

ggplot(aes(y=est,x=Model,colour=Model),data=allcombs_cb_hy_or_first)+
  geom_point()+
  geom_errorbar(aes(ymin = lwr, ymax = upr))+
  facet_grid(Category~Synopsis)+
  scale_y_continuous(name="Probability of correct answer",limits=c(0.45,1))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),axis.text.x = element_blank())

#ggsave("cb_hy_or_1.png",height=15,width=30,units="cm")

ggplot(aes(y=est,x=Model,colour=Model),data=allcombs_cb_hy_or_sec)+
  geom_point()+
  geom_errorbar(aes(ymin = lwr, ymax = upr))+
  facet_grid(Category~Synopsis)+
  scale_y_continuous(name="Probability of correct answer",limits=c(0.45,1))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),axis.text.x = element_blank())

#ggsave("cb_hy_or_2.png",height=15,width=30,units="cm")


ggplot(aes(y=est,x=Model,colour=Model),data=allcombs_cf_sp_de_first)+
  geom_point()+
  geom_errorbar(aes(ymin = lwr, ymax = upr))+
  facet_grid(Category~Synopsis)+
  scale_y_continuous(name="Probability of correct answer",limits=c(0.45,1))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),axis.text.x = element_blank())

#ggsave("cf_sp_de_1.png",height=15,width=30,units="cm")

ggplot(aes(y=est,x=Model,colour=Model),data=allcombs_cf_sp_de_sec)+
  geom_point()+
  geom_errorbar(aes(ymin = lwr, ymax = upr))+
  facet_grid(Category~Synopsis)+
  scale_y_continuous(name="Probability of correct answer",limits=c(0.45,1))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),axis.text.x = element_blank())

#ggsave("cf_sp_de_2.png",height=15,width=30,units="cm")


EMMs_models <- emmeans(mod2,~Model,type="response")
pairs(EMMs_models)
# write.csv(pairs(EMMs_models), "Pairwisecomp_LLMs.csv")

EMMs_cat <- emmeans(mod2,~Category,type="response")
pairs(EMMs_cat)
# write.csv(pairs(EMMs_cat), "Pairwisecomp_methods.csv")

EMMs_syn <- emmeans(mod2,~Synopsis,type="response")
pairs(EMMs_syn)
# write.csv(pairs(EMMs_syn), "Pairwisecomp_synopses.csv")



str(Anova(mod2))
str(summary(mod2))
# write.csv(Anova(mod2), "Anova_mod2.csv")
# write.csv(summary(mod2)$coefficients, "summary_mod2.csv")



##############################################################################################################################
############################## LLM performance on retrieval across synopses ##################################################
##############################################################################################################################

retr_data <- fread(choose.files()) #llm_synopses_retrieval_info.csv

head(retr_data)

#create list of unique questions and their synopses
uniq.Qs <- unique(retr_data[,list(Question,`Source Synopsis`)])
retr_data$Retrieval_strategy <- retr_data$Method

#Adding in data on whether the correct source was retrieved
correcthy <- NA
for(i in 1:nrow(uniq.Qs)){
  sourcenum <- unique(retr_data[Retrieval_strategy=="Hybrid"&Question == uniq.Qs[i,1],`Source No.`])
  retrnums <- retr_data[Retrieval_strategy=="Hybrid"&Question == uniq.Qs[i,1],`Retrieved No.`]
  if(length(grep("NA",retrnums))==0&length(retrnums)>0){
    if(length(grep(sourcenum,retrnums))>0){
    correcthy[i] <- 1
    } else{
      correcthy[i] <- 0
    }
  }else{
    correcthy[i] <- NA
  }
}

correctde <- NA

for(i in 1:nrow(uniq.Qs)){
  sourcenum <- unique(retr_data[Retrieval_strategy=="Dense"&Question == uniq.Qs[i,1],`Source No.`])
  retrnums <- retr_data[Retrieval_strategy=="Dense"&Question == uniq.Qs[i,1],`Retrieved No.`]
  if(length(grep("NA",retrnums))==0&length(retrnums)>0){
    if(length(grep(sourcenum,retrnums))>0){
      correctde[i] <- 1
    } else{
      correctde[i] <- 0
    }
  }else{
    correctde[i] <- NA
  }
}


correctsp <- NA

for(i in 1:nrow(uniq.Qs)){
  sourcenum <- unique(retr_data[Retrieval_strategy=="Sparse"&Question == uniq.Qs[i,1],`Source No.`])
  retrnums <- retr_data[Retrieval_strategy=="Sparse"&Question == uniq.Qs[i,1],`Retrieved No.`]
  if(length(grep("NA",retrnums))==0&length(retrnums)>0){
    if(length(grep(sourcenum,retrnums))>0){
      correctsp[i] <- 1
    } else{
      correctsp[i] <- 0
    }
  }else{
    correctsp[i] <- NA
  }
}

retrievaldata <- data.table(Question=uniq.Qs$Question,Synopsis=uniq.Qs$`Source Synopsis`,
                            Binary=c(correctsp,correctde,correcthy),
                            Strategy=c(rep("Sparse",length(correctsp)),
                                       rep("Dense",length(correctde)),
                                       rep("Hybrid",length(correcthy))))


retr_mod1 <- glm(Binary ~ Strategy*Synopsis, data = retrievaldata,family="binomial")
summary(retr_mod1)

# simulationOutput <- simulateResiduals(fittedModel = retr_mod1, plot = F)
# testDispersion(simulationOutput)
# residuals(simulationOutput)
# plot(simulationOutput)

retr_mod2 <- glm(Binary ~ Strategy+Synopsis, data = retrievaldata,family="binomial")
summary(mod2)

retr_modsum2 <- data.table(summary(retr_mod2)$coefficients)

retr_modsum2$term <- dimnames(summary(retr_mod2)$coefficients)[1]

#write.csv(modsum2,"Modelsummary_logreg_syn+mod+cat.csv")

retr_mod3 <- glm(Binary ~ Strategy, data = retrievaldata,family="binomial")
summary(retr_mod3)

retr_mod4 <- glm(Binary ~ Synopsis, data = retrievaldata,family="binomial")
summary(retr_mod4)

AICc(mod1,mod2)
AICc(mod3,mod2)
AICc(mod4,mod2)
anova(mod1,mod2)
anova(mod3,mod2)
anova(mod4,mod2)

retr_modnull <- glm(Binary ~ 1, data = retrievaldata,family="binomial")
summary(retr_modnull)
AICc(mod1,modnull)
AICc(mod2,modnull)
anova(mod1,modnull)
anova(mod2,modnull)

Anova(mod2, type = "II")

retr_allcombs <- data.table(CJ(Strategy=unique(retrievaldata$Strategy),
                          Synopsis=unique(retrievaldata$Synopsis)))
retr_allcombs$est <- predict(retr_mod2,retr_allcombs,type="response")
retr_allcombs$se <- predict(retr_mod2,retr_allcombs,type="response",se.fit = TRUE)[[2]]

critval <- 1.96 ## approx 95% CIcritval <- 1.96 ## approx 95% CI
retr_allcombs$upr <- retr_allcombs$est + (critval * retr_allcombs$se)
retr_allcombs$lwr <- retr_allcombs$est - (critval * retr_allcombs$se)

unique(retr_allcombs$Synopsis)
newnames <- data.table(Synopsis=gsub("([A-Za-z]+).*", "\\1", unique(retr_allcombs$Synopsis)))
newnames[Synopsis=="Biodiversity", Synopsis:="Mar.Art.\nStructures"]
newnames[Synopsis=="Control", Synopsis:="FW\nInvasives"]
newnames[Synopsis=="Management", Synopsis:="Captive\nAnimals"]
newnames[Synopsis=="Marine", Synopsis:=c("Marine\nFish","Marine\nFW Mammal")]
newnames[Synopsis=="Mediterranean", Synopsis:="Med.\nFarmland"]
newnames[Synopsis=="Natural", Synopsis:="Nat.Pest\nControl"]
newnames[Synopsis=="Subtidal", Synopsis:="Subtidal\nBenthic"]
newnames[Synopsis=="Sustainable", Synopsis:="Aquaculture"]
newnames[Synopsis=="Terrestrial", Synopsis:="Terr.\nMammal"]

newnamedat <- data.table(oldname=unique(retr_allcombs$Synopsis),newname=newnames$Synopsis)

oldnames <- unique(retr_allcombs$Synopsis)
i=1
for(i in 1:length(oldnames)){
  retr_allcombs[Synopsis==oldnames[i],Synopsis:=newnamedat[oldname==oldnames[i],newname]]
}
unique(retr_allcombs$Synopsis)

library(tidyverse)
retr_ordersyn <- data.table(retr_allcombs %>% 
                         group_by(Synopsis) %>%
                         summarise(mean=mean(est)))

retr_ordersynnames <- retr_ordersyn[rev(order(mean)),Synopsis]

retr_allcombs$Synopsis <- factor(retr_allcombs$Synopsis,levels=retr_ordersynnames)
retr_allcombs <- retr_allcombs[order(Synopsis)]

retr_allcombs$Strategy <- factor(retr_allcombs$Strategy,levels=c("Hybrid", "Dense","Sparse"))
length(unique(retr_allcombs$Synopsis))
retr_allcombs_first <- retr_allcombs[1:36,]
retr_allcombs_sec <- retr_allcombs[37:72,]

retrievaldata

ggplot(aes(y=est,x=Strategy,colour=Strategy),data=retr_allcombs_first)+
  geom_point()+
  geom_errorbar(aes(ymin = lwr, ymax = upr))+
  facet_grid(.~Synopsis)+
  scale_y_continuous(name="Probability of correct answer",limits=c(0.45,1))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),legend.position="none",
        axis.text.x = element_text(angle=45,vjust=1,hjust=1))

#ggsave("retr_results1.png",height=15,width=30,units="cm")

ggplot(aes(y=est,x=Strategy,colour=Strategy),data=retr_allcombs_sec)+
  geom_point()+
  geom_errorbar(aes(ymin = lwr, ymax = upr))+
  facet_grid(.~Synopsis)+
  scale_y_continuous(name="Probability of correct answer",limits=c(0.3,1))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),legend.position="none",
        axis.text.x = element_text(angle=45,vjust=1,hjust=1))
#ggsave("retr_results2.png",height=15,width=30,units="cm")


EMMs_strat <- emmeans(retr_mod2,~Strategy,type="response")
pairs(EMMs_strat)
#write.csv(pairs(EMMs_strat), "retr_Pairwisecomp_strat.csv")

EMMs_syn <- emmeans(retr_mod2,~Synopsis,type="response")
pairs(EMMs_syn)
#write.csv(pairs(EMMs_syn), "retr_Pairwisecomp_synopses.csv")

str(Anova(retr_mod2))
str(summary(retr_mod2))
#write.csv(Anova(retr_mod2), "retr_Anova_mod2.csv")
#write.csv(summary(retr_mod2)$coefficients, "retr_summary_mod2.csv")