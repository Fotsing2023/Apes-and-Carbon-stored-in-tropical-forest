#link for Data
# https://drive.google.com/drive/folders/1jgCXPy9FiLLhc-r9L3qbTPjM7VT9LoQM?usp=drive_link

title: "ApesCarbon Analyses"
author: "Ernest Fotsing"
date: "2023-05-17"
output: html_document
---

#  Update all package

library(usdm) # for function vif
library(AICcmodavg) # for aictab
library(corrplot) #for making cool correlation visualizations/plots
library(tidyverse) #for piping and mutate function
library(coefplot)
library(GGally)
library(pgirmess)
library(influence.ME)
library(MuMIn) # for the r.squaredGLMM
library(DHARMa)
library(AICcmodavg)
library(dplyr)
theme_set(theme_bw(12))

pdata_c<-read.csv("transformed_data.csv") # we can start the script here as this data was already transformed and are ready for analysis 

 # or 

#########--------------------------Here you can start at the beging
rm(list = ls()) # clear environnement
setwd("E:/ApeProject")
temp = list.files(pattern = ".csv")
for(i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

## combine all data
pdata<-rbind(shwein.csv, troglodytes.csv, 
             verus.csv,gorilla.csv,elliot.csv)

pdata_c<-read.csv("transformed_data.csv")

#View(pdata)

pdata_c<- pdata
range(pdata_c$foot_mean)
hist(pdata_c$foot_mean)

write.csv(pdata, "alldata.csv", row.names = FALSE)
pdata_c<- pdata[!pdata$foot_mean>51,]## the values that are high than 50 were discarded(they were falling in open water and we do not expect primates to be there)

range(pdata_c$ape_abunda)
summary(pdata_c$carb_mean)## there is NA we are going to remove them
pdata_c<- pdata_c[!is.na(pdata_c$carb_mean),]
range(pdata_c$carb_mean)
hist(pdata_c$ape_abunda)

## some estimates of apes were above the realistic estimates because they come from estimates of all apes, we will exclude those that are above 10

pdata_c<- pdata_c[!pdata_c$ape_abunda>10,]
### lets do the analysis for all species

colnames(pdata_c)

#let remove unecessary dataframe in the environnement which take too much espace
rm(elliot.csv)
rm(gorilla.csv)
rm(troglodytes.csv)
rm(shwein.csv)
rm(verus.csv)


## plot predictors and response variable to see how they are distributed
## another corr test
par(mar = c(1, 1, 1, 1))

# Run correlation test

 # method 1 
corrM <- pdata_c %>% 
  dplyr::select(ape_abunda, carb_mean, foot_mean, Per_class_1,
                Per_class_2, Per_class_3, Per_class_4, Per_class_5,
                Per_class_6, Per_class_7, Per_class_8, 
                Per_class_10) %>% cor()

#  I can use the corrplot as below
windows(15, 10)
corrplot(corrM, 
         method = "number", 
         type = "lower", 
         bg = "white", 
         diag = F, 
         tl.col = "black")

#3
corrplot(corrM, method = "ellipse", # this is just another method for good visibility
         type = "upper", 
         diag = F)

## another approch of correration test 

pred=c("ape_abunda", "carb_mean", "foot_mean","Per_class_1",
       "Per_class_2", "Per_class_3", "Per_class_4", "Per_class_5",
       "Per_class_6", "Per_class_7", "Per_class_8", "Per_class_9",
       "Per_class_10")
xx<-cor(pdata_c[, pred])

write.csv(xx, "correration.csv", row.names=T)

## after the correlation test we found out that the glassland is highly 
## correlated with tree covers (-0.88) so we remove glass from predictors
## we also remove the vegetation that are ecologically are unlikely to have apes
# (lichens and moses, built up area, open water, bare land and snow). Those vegetation
## were even less represented in the data set


# human footprint and ape abundance variables are skewed to the right lets log transform it, 
#lets transform them using log function
#transformation

hist(log(pdata_c$foot_mean))
pdata_c$s.human_foot <- sqrt(pdata_c$foot_mean)

hist(pdata_c$s.human_foot)

### for ape abundance we transform it by square root 4

pdata_c$s.sape_abund<- sqrt(sqrt(pdata_c$ape_abunda))
hist(sqrt(pdata_c$s.sape_abund))
## with the proportion of vegetation cover they were all left skewed and we tried 
## every transformation but the skewness stayed and we decided to let them
# and use them no transformed

hist(log(pdata_c$Per_class_1+1))
hist((pdata_c$Per_class_1)^(1/3))
colnames(pdata_c)

## predictor variable
pred.var =c("s.sape_abund", "s.human_foot","Per_class_1",
      "Per_class_2", "Per_class_3", "Per_class_4", "Per_class_5")

#for convenience and to get comparable coefficients
#we z-transform all covariates:
xx=data.frame(scale(pdata_c[, pred.var]))
names(xx)=paste("z", names(xx), sep=".")
head(xx)

pdata_c=data.frame(pdata_c, xx)
names(pdata_c)
pdata_c<-read.csv("transformed_data.csv") # we can syart the script here

#lets write the transformed data into csv
write.csv(pdata_c, "transformed_data.csv", row.names = FALSE)
head(pdata_c)

###lets change the reference level for protection level and habitat type

pdata_c$area_categ<- as.factor(pdata_c$area_categ)

levels(pdata_c$area_categ)

pdata_c<-pdata_c %>% mutate( protection_level=
                               case_when(area_categ=="Chimpanzee Sanctuary" ~ "Medium protection",                           
                                         area_categ=="Classified Forest" ~"Medium protection",                             
                                         area_categ=="Faunal Migratory Corridor"~ "Medium protection",                      
                                         area_categ== "Faunal Reserve" ~"Medium protection",                                 
                                         area_categ=="Forest Reserve" ~"Medium protection",                                  
                                         area_categ=="Game controlled area" ~"Medium protection",                           
                                         area_categ=="Game Reserve" ~"Medium protection",                                   
                                         area_categ=="Game Sanctuary / Non-hunting Forest Reserve"~"Medium protection",     
                                         area_categ=="Hunting Area"  ~"Medium protection",                                  
                                         area_categ=="Hunting Reserve"  ~"Medium protection",                               
                                         area_categ=="Integral nature reserve" ~"High protection",                         
                                         area_categ=="National Forest Park"  ~"High protection",                           
                                         area_categ=="National Park" ~"High protection",                                   
                                         area_categ=="Natural Monument" ~"Medium protection",                               
                                         area_categ=="Natural Park" ~"High protection",                                    
                                         area_categ=="Nature reserve"  ~"Medium protection",                                
                                         area_categ=="Nature Reserve" ~"Medium protection",                                 
                                         area_categ=="No or Non - Hunting Forest Reserve" ~"Medium protection",             
                                         area_categ== "Partial Faunal Reserve" ~"Medium protection",                        
                                         area_categ=="Presidential Reserve" ~"High protection",                           
                                         area_categ=="Ramsar Site, Wetland of International Importance" ~"Medium protection",
                                         area_categ=="Resource Reserve"~"Medium protection",                                
                                         area_categ=="Special Reserve"  ~"Medium protection",                               
                                         area_categ== "Strict Nature Reserve"~"High protection",                          
                                         area_categ== "UNESCO-MAB Biosphere Reserve" ~"Medium protection",                   
                                         area_categ== "Wildlife Management Area"  ~"Medium protection",                      
                                         area_categ== "Wildlife Reserve"    ~"Medium protection",                            
                                         area_categ== "Wildlife Sanctuary" ~"Medium protection",                             
                                         area_categ== "World Heritage Site (natural or mixed)" ~"Medium protection",
                                         area_categ == "Non protected area" ~ "Low protection",
                                         area_categ== "Zone de peche reserve" ~"Medium protection"))

pdata_c$protection_level<- as.factor(pdata_c$protection_level)
levels(pdata_c$protection_level)
pdata_c$protection_level=relevel(pdata_c$protection_level, ref="Low protection")

# ordered our level in ascending order
pdata_c$protection_level <- factor(pdata_c$protection_level, levels = c("Low protection", "Medium protection", "High protection"))

levels(pdata_c$protection_level)
# we should confirm that our predictors are not colinear even after transformation

corrM <- pdata_c %>% 
  dplyr::select(s.sape_abund, s.human_foot, Per_class_1,
                Per_class_2, Per_class_3, Per_class_4, Per_class_5) %>% cor()
corrM

s.sape_abund s.human_foot Per_class_1  Per_class_2
s.sape_abund   1.00000000  -0.39523663   0.3626109 -0.165770222
s.human_foot  -0.39523663   1.00000000  -0.3119080  0.148568424
Per_class_1    0.36261090  -0.31190797   1.0000000 -0.439141261
Per_class_2   -0.16577022   0.14856842  -0.4391413  1.000000000
Per_class_3   -0.30625168   0.22968061  -0.8814961  0.295168185
Per_class_4   -0.19956683   0.21592874  -0.4701427  0.042362351
Per_class_5   -0.04231068   0.04609061  -0.1490654 -0.003893293
Per_class_3 Per_class_4  Per_class_5
s.sape_abund -0.30625168 -0.19956683 -0.042310679
s.human_foot  0.22968061  0.21592874  0.046090608
Per_class_1  -0.88149607 -0.47014267 -0.149065400
Per_class_2   0.29516818  0.04236235 -0.003893293
Per_class_3   1.00000000  0.08078825  0.033306149
Per_class_4   0.08078825  1.00000000  0.049662048
Per_class_5   0.03330615  0.04966205  1.000000000
# as a control we use different vegetation variable 

# or we use this # no correLATION GOOD ¨!

# HERE I just WANTED to see the paterrn

ggplot(data = pdata_c, 
       mapping = aes(x = s.sape_abund, 
                     y = carb_mean)) + geom_point()# this plot is not really good to see

table(pdata_c$Country)
table(pdata_c$Species)

#Chimpanzee    Gorilla 
#1416450     753814

pdata_c$Species<- as.factor(pdata_c$Species)
pdata_c$Country<- as.factor(pdata_c$Country)
pdata_c$Species=relevel(pdata_c$Species, ref="Chimpanzee")

ggplot(data = pdata_c, 
       mapping = aes(x = Country, y= carb_mean)) +
  geom_boxplot()

ggplot(data = pdata_c, 
       aes(x = Country, y= carb_mean,
           fill= factor(Species))) +
  geom_boxplot()# explore carbon in each country according to species

ggplot(data = pdata_c, 
       mapping = aes(x = Species, y= carb_mean)) +
  geom_violin()# explore another type of boxplot

ggplot(data = pdata_c, 
       mapping = aes(x = Species, y= carb_mean)) +
  geom_boxplot() + labs(x = "Apes species", y = "Mean of carbon stock (T/ha)") # proper box plot 

pdata_c<- pdata_c[!is.na(pdata_c$protection_level),]
ggplot(data = pdata_c, 
       mapping = aes(x = protection_level, y= carb_mean)) +
  geom_boxplot() + labs(x = "Status of protected area", y = "Mean of carbon (T/ha)") # proper box plot

### Perform ANOVA test
ModelProtArea <- aov(carb_mean ~ protection_level, data = pdata_c)
summary(ModelProtArea) # significant difference in carbon mean amon protected area

Df    Sum Sq  Mean Sq F value Pr(>F)    
protection_level       2 1.624e+08 81219687    7442 <2e-16 ***
  Residuals        2169770 2.368e+10    10913                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

ModelSpecies <- aov(carb_mean ~ Species, data = pdata_c)
summary(ModelSpecies) # same here is a significant difference in carbon stock according to species

Df    Sum Sq   Mean Sq F value Pr(>F)    
Species           1 2.346e+09 2.346e+09  236860 <2e-16 ***
  Residuals   2169771 2.150e+10 9.907e+03                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

P1<- pdata_c %>%
  dplyr::group_by(Species) %>%
  dplyr::mutate(Mean = round(mean(z.s.sape_abund), 1), SD = round(sd(z.s.sape_abund), 1)) %>%
  ggplot(aes(Species, z.s.sape_abund)) +
  geom_boxplot(fill=c("orange", "darkgray")) +
  geom_text(aes(label = paste("M = ", Mean, sep = ""), y = 0.5)) +
  geom_text(aes(label = paste("SD = ", SD, sep = ""), y = 0)) +
  theme_bw(base_size = 12) +
  labs(x = "Apes species") +
  labs(y = "Mean of carbon stock (T/ha)", cex = .50) +
  coord_cartesian(ylim = c(0, 20)) +
  guides(fill = "none")

 P2<- ggplot(pdata_c, aes(Species, carb_mean)) +
  geom_boxplot(aes(fill = factor(Species))) +
  scale_fill_manual(values = c("grey30", "grey70")) +
  facet_wrap(~ protection_level) +
  guides(fill = "none") +
  theme_bw() + labs(x = "Apes species", y = "Mean of carbon stock (T/ha)", cex = .50) 
 
 par(mar = c(1, 1, 1, 1))
 vip::grid.arrange(grobs = list(P1, P2), widths = c(1, 1), layout_matrix= rbind(c(1, 2)))

------------------------------
   ggplot(pdata_c, aes(Species, carb_mean)) +
   geom_boxplot(aes(fill = factor(Species))) +
   scale_fill_manual(values = c("grey30", "grey70")) +
   facet_wrap(~ protection_level) +
   guides(fill = "none") +
   theme_bw() + labs(x = "Apes species", y = "Mean of carbon stock (T/ha)", cex = .50)
------------------------------------
  
# HERE we see a huge variation of the carbon mean according to country and species which is an indication of country as a random effect but we will test it later. A clear analyse of data show that that there's no variation between gorilla and chimpanzee data which can be a reason or indication to not use country as a random

## Just for demonstration, let's fit a regular GLM and look at residuals. This is only for demonstration as model selection seem to be a good approach here for my point of view.

glm.comple.pooling <- glm(carb_mean ~ z.s.sape_abund + z.s.human_foot + Species + 
                            z.Per_class_1 + z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + 
                            protection_level, family = gaussian, data = pdata_c)
 
glm.main <- glm(carb_mean ~ z.s.sape_abund + z.s.human_foot + Species + 
                             z.Per_class_1 + z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + 
                             protection_level, family = gaussian, data = pdata_c)

# relevel to see chimp effect

 pdata_c$Species=relevel(pdata_c$Species, ref="Gorilla")

glm.main <- glm(carb_mean ~ z.s.sape_abund + z.s.human_foot + Species + 
                  z.Per_class_1 + z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + 
                  protection_level, family = gaussian, data = pdata_c)
summary(glm.main)
Call:
  glm(formula = carb_mean ~ z.s.sape_abund + z.s.human_foot + Species + 
        z.Per_class_1 + z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + 
        protection_level, family = gaussian, data = pdata_c)

Coefficients:
  Estimate Std. Error  t value Pr(>|t|)    
(Intercept)                       218.15722    0.09646 2261.593  < 2e-16 ***
  z.s.sape_abund                      4.27264    0.06163   69.324  < 2e-16 ***
  z.s.human_foot                    -25.00263    0.05764 -433.777  < 2e-16 ***
  SpeciesChimpanzee                 -27.46801    0.11891 -230.997  < 2e-16 ***
  z.Per_class_1                      55.64883    0.07006  794.254  < 2e-16 ***
  z.Per_class_2                      -3.25594    0.05846  -55.698  < 2e-16 ***
  z.Per_class_4                       7.43017    0.05953  124.819  < 2e-16 ***
  z.Per_class_5                       1.55723    0.05177   30.079  < 2e-16 ***
  protection_levelMedium protection -15.49064    0.14812 -104.579  < 2e-16 ***
  protection_levelHigh protection     1.13439    0.18741    6.053 1.42e-09 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 5634.145)

Null deviance: 2.3842e+10  on 2169772  degrees of freedom
Residual deviance: 1.2225e+10  on 2169763  degrees of freedom
AIC: 24897024

Number of Fisher Scoring iterations: 2

# remodelling the stuff 
 
 glm.inter <- glm(carb_mean ~ z.s.sape_abund*z.s.human_foot + Species + 
                             z.Per_class_1 + z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + 
                             protection_level, family = gaussian, data = pdata_c)
 
  comparison <- anova(glm.main,  glm.inter, test="Chisq") # plot showing the interaction with carbon 
  
  Analysis of Deviance Table
  
  Model 1: carb_mean ~ z.s.sape_abund + z.s.human_foot + Species + z.Per_class_1 + 
    z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + protection_level
  Model 2: carb_mean ~ z.s.sape_abund * z.s.human_foot + Species + z.Per_class_1 + 
    z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + protection_level
  Resid. Df Resid. Dev Df Deviance Pr(>Chi)
  
  1   2169763 1.2225e+10                     
  2   2169762 1.2225e+10  1   2695.8   0.4891 # not significant
  
 
  # if the interaction is sig we will ned 3D plot and 
  # if not plot showing the effect of apes abun given footprint and carbon 
  
  # abundance of apes species could have different effect  
  
  
  # plot the effect
  
 plot(pdata_c)
  
 r.squaredGLMM(glm.comple.pooling)
 
 #R2m       R2c
 #[1,] 0.4872514 0.4872514

plot(residuals(glm.comple.pooling) ~ Country, 
     data = pdata_c) 
summary(glm.comple.pooling) # we see fairly large differences in residual values across Country, even when accounting for our predictor variables. 

abline(h = 0, lty = 1, col="red")

# We can also use a coplot to get a sense of whether the slope of ape abun might also vary across our country. coplot is very handy for this.
par(mar = c(1, 1, 1, 1))
coplot(carb_mean~ z.s.sape_abund | Country, 
       data = pdata_c, 
       pch = 16, 
       panel = panel.smooth) 

# Evidence here for varying slopes and intercepts for apes abun in  different country, but it is not clear whether it would be worth accounting for this in our random terms. Also we can see that having only many samples for each random effect level makes it easy and not challenging to assess the potential for random slopes.

# load package for modeling approch and ploting model output
library(stargazer)
library(ICC) #gives us the ICCest() function estimate intraclass correlation
library(influence.ME)

# test model with gaussian familly through model selection approch 
#  this output will include AIC DeltaAIC and AICwt selection based approch

modList <- list()

modList[["FULL"]] <- glmmFull <- glm(carb_mean ~ z.s.sape_abund + z.s.human_foot + Species + 
                                       z.Per_class_1 + z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + 
                                       protection_level, family = gaussian, data = pdata_c)

modList[["Ape + human + Species + class1 + Pl"]] <- glmm1<- glm(carb_mean ~ z.s.sape_abund +    z.s.human_foot  +   Species +  z.Per_class_1 + protection_level, family = gaussian, data = pdata_c)

modList[["Ape + human + Species + class1"]] <- glmm2<- glm(carb_mean ~ z.s.sape_abund +    z.s.human_foot  +   Species +  z.Per_class_1, family = gaussian, data = pdata_c)

modList[["Ape + human + Species + class1 + INTERACTION"]] <- glmm3<- glm(carb_mean ~ z.s.sape_abund *  z.s.human_foot  + z.Per_class_1 +  Species, family = gaussian, data = pdata_c)

modList[["Ape + humanfoot + INTERACTION"]] <- glmm4<- glm(carb_mean ~ z.s.sape_abund * z.s.human_foot  , family = gaussian, data = pdata_c)

modList[["Ape + human + Allclass + Pl"]] <- glmm5 <- glm(carb_mean ~ z.s.sape_abund + z.s.human_foot  + 
                                                           z.Per_class_1 + z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + 
                                                           protection_level, family = gaussian, data = pdata_c)

modList[["Ape + human + Allclass + Species"]] <- glmm6<- glm(carb_mean ~ z.s.sape_abund + z.s.human_foot + Species + 
     z.Per_class_1 + z.Per_class_2 + z.Per_class_4 + z.Per_class_5 
   , family = gaussian, data = pdata_c)

modList[["Ape + human + Species + class1 + class2 + Pl"]] <- glmm7<- glm(carb_mean ~ z.s.sape_abund +    z.s.human_foot  +   Species +  z.Per_class_1 + z.Per_class_2 + protection_level, family = gaussian, data = pdata_c)

modList[["Ape + human + Species + class2 + Pl"]] <- glmm8<- glm(carb_mean ~ z.s.sape_abund +    z.s.human_foot  +   Species  + z.Per_class_2 + protection_level, family = gaussian, data = pdata_c)

modList[["Ape + human + Species + class1 + Pl"]] <- glmm9 <- glm(carb_mean ~ z.s.sape_abund + z.s.human_foot + Species + 
   z.Per_class_1  +  protection_level, family = gaussian, data = pdata_c)

## Compare models 

myAIC <-  aictab(modList) 

myAIC # based on AIC THE FUL MODEL IS THE BEST MODEL 

# below are the result 
Model selection based on AICc:
  
  K     AICc Delta_AICc
FULL                                         11 24897024       0.00
Ape + human + Species + class1 + class2 + Pl  9 24913149   16124.94
Ape + human + Allclass + Species              9 24914046   17022.18
Ape + human + Species + class1 + Pl           8 24920245   23221.15
Ape + human + Species + class1 + INTERACTION  7 24939720   42696.14
Ape + human + Species + class1                6 24939877   42853.11
Ape + human + Allclass + Pl                  10 24949736   52712.42
Ape + human + Species + class2 + Pl           8 25517890  620866.42
Ape + humanfoot + INTERACTION                 5 25772793  875768.50
AICcWt Cum.Wt        LL
FULL                                              1      1 -12448501
Ape + human + Species + class1 + class2 + Pl      0      1 -12456565
Ape + human + Allclass + Species                  0      1 -12457014
Ape + human + Species + class1 + Pl               0      1 -12460115
Ape + human + Species + class1 + INTERACTION      0      1 -12469853
Ape + human + Species + class1                    0      1 -12469933
Ape + human + Allclass + Pl                       0      1 -12474858
Ape + human + Species + class2 + Pl               0      1 -12758937
Ape + humanfoot + INTERACTION                     0      1 -12886391 # the best model is the full model 

bestmodel<- glmmFull 

summary(bestmodel)
Call:
 deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-243.99   -56.76    -8.30    54.78   367.54  

Coefficients:
  Estimate Std. Error  t value Pr(>|t|)
(Intercept)                       190.68922    0.07224 2639.521  < 2e-16
z.s.sape_abund                      4.27264    0.06163   69.324  < 2e-16
z.s.human_foot                    -25.00263    0.05764 -433.777  < 2e-16
SpeciesGorilla                     27.46801    0.11891  230.997  < 2e-16
z.Per_class_1                      55.64883    0.07006  794.254  < 2e-16
z.Per_class_2                      -3.25594    0.05846  -55.698  < 2e-16
z.Per_class_4                       7.43017    0.05953  124.819  < 2e-16
z.Per_class_5                       1.55723    0.05177   30.079  < 2e-16
protection_levelHigh protection     1.13439    0.18741    6.053 1.42e-09
protection_levelMedium protection -15.49064    0.14812 -104.579  < 2e-16

(Intercept)                       ***
  z.s.sape_abund                    ***
  z.s.human_foot                    ***
  SpeciesGorilla                    ***
  z.Per_class_1                     ***
  z.Per_class_2                     ***
  z.Per_class_4                     ***
  z.Per_class_5                     ***
  protection_levelHigh protection   ***
  protection_levelMedium protection ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 5634.145)

Null deviance: 2.3842e+10  on 2169772  degrees of freedom
Residual deviance: 1.2225e+10  on 2169763  degrees of freedom
(491 observations effacées parce que manquantes)
AIC: 24897024

Number of Fisher Scoring iterations: 2

r.squaredGLMM(bestmodel)# know the amount of explained variation in dataset
#0.4873157 mean 48 percent of our data could be explained which is good

## confidence interval
confint(bestmodel, method = "Wald")

#confint.merMod(bestmodel, method = "Wald")# we can also use bootstrap method

round(summary(bestmodel)$coefficients, digits = 3)

# try to get the Response curve for each predictors

bestmodel<- glmmFull

library(visreg)
# par(mfrow = c(2, 2))
visreg(bestmodel, "z.s.sape_abund",
       ylab = "Carbon stock",
       xlab = "Ape abundance",
       scale = "response") 

visreg(bestmodel, "z.s.human_foot",
       ylab = "Carbon stock",
       xlab = "human footprint",
       scale = "response")

visreg(bestmodel, "z.Per_class_1",
       ylab = "Carbon stock",
       xlab = "z.Per_class_1",
       scale = "response")

visreg(bestmodel, "z.Per_class_2",
       ylab = "Carbon stock",
       xlab = "z.Per_class_2",
       scale = "response")

# here just remplace the name of each predictors and get the same results

glmApabund <- glm(carb_mean ~ z.s.sape_abund, family = gaussian, data = pdata_c)

summary(glmApabund)
range(pdata_c$z.s.sape_abund)
xApeabund <- seq(-2.596520, 5.273753, 0.01)
yApeabund <- predict(glmApabund, list(z.s.sape_abund = xApeabund),type="response")

plot(pdata_c$z.s.sape_abund, pdata_c$carb_mean, pch = 16, xlab = "Ape abundance (inds/km2", ylab = "Carbon stock (T/ha)")

lines(xApeabund, yApeabund) # those plot are not very nice unfortunately


# I AM GOING TO SHOW THREE CODE ON HOW TO EXPORT MODEL OUTPUT IN EXCEL OR CSV FILE
## export model coeffiscients in excel sheet

df<- data.frame(round(summary(bestmodel)$coefficients, 3))
write.xlsx(df, "ApesProject//bestmodel.xlsx")

# Save model output to a CSV file
model_output <- broom::tidy(bestmodel)

# we can also use this code which include confidence interval also and # save the summary in exel 
model_summary <- tidy(bestmodel, conf.int = TRUE)
write_xlsx(model_summary, "model_summary_with_CI.xlsx")


# test collinearity in the best model

car::vif(bestmodel) # No issues with multicollinearity

# And finally the augment function is very handy, because it automatically adds columns to our original dataset that relate to the model results. This includes cook’s distance values, fitted values, residuals and others usefull for model disgnoctics

bd2 <- augment(bestmodel, data = pdata_c)

head(pdata_c)
head(bd2[, 30:46]) # you can run this top see all added colums in your original dataframe 


# save BD in exel for further view and analyse
write.csv(bd2, "bd2.csv", row.names = TRUE)


# summarise the model in table but the code above do the same job ! no need to run again 

BestModel_Summary_table<-tab_model(bestmodel, 
                                   show.se = TRUE, 
                                   show.stat = TRUE, 
                                   show.icc = FALSE, 
                                   transform = NULL, show.p = T,
                                   string.est = "Beta", 
                                   string.se = "se", use.viewer = T,
                                   show.re.var = F, show.ngroups = F,
                                   pred.labels = c("Intercept",
                                                   "z.s.sape_abund", 
                                                   "z.s.human_foot",
                                                   "protection_level",
                                                   "Species", "z.Per_class_1", "z.Per_class_2", "z.Per_class_3", 
                                                   "z.Per_class_4", "z.Per_class_5"), dv.labels = c(""))

# BestModel_Summary_table

write.csv(BestModel_Summary_table, "BestModel_Summary.csv", row.names = TRUE)
----------------------------------------------------------
  # Exploring other plotting ioption 
  
library(tidyverse) #for all data wrangling
library(cowplot) #for manuscript ready figures
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)# effect of predictors with response variable
library(sjstats) #use for r2 function

#Make a plot of the effect sizes:

sjPlot::plot_model(bestmodel)
#Let’s change the axis labels & title.

sjPlot::plot_model(bestmodel, 
                   axis.labels=c("z.s.sape_abund", 
                                 "z.s.human_foot",
                                 "protection_level",
                                 "Species", 
                                 "z.Per_class_1", "z.Per_class_2",
                                 "z.Per_class_3", "z.Per_class_4", "z.Per_class_5"
                   ),show.values=TRUE,show.p=TRUE,
                   title="Effect of covariate on carbon quantity")

# Unformatted table of model results:

sjPlot:: tab_model(bestmodel)
library(ggplot2)
theme_set(theme_bw(12))
---------------------------------------------
#tried another plotting of predictors effects
#Plot model estimates with data
#Step 1: Save the effect size estimates into a data.frame
  
  # # we can do the same code for other predictors
effects_ApesAbun <- effects::effect(term= "z.s.sape_abund", mod= bestmodel)
summary(effects_ApesAbun)

x_abun <- as.data.frame(effects_ApesAbun)

#Step 2: Use the effects value df (created above) to plot the estimates

ApesAbun_plot <- ggplot() + 

    #2
  #geom_point(data=pdata_c, aes(z.s.sape_abund , carbon_qua)) + 
  #3
  geom_point(data=x_abun, aes(x=z.s.sape_abund, y=fit), color="blue") +
  #4
  geom_line(data=x_abun, aes(x=z.s.sape_abund, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_abun, aes(x=z.s.sape_abund, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="Ape_abundance", y="Carbon quantity")

ApesAbun_plot


#################################------------------------------
# -have one boxplot for species and protection level in the same figure with diffrent shape

ggplot(data = pdata_c, 
       aes(x = protection_level, y= carb_mean,
           fill= factor(Species))) +
  geom_violin()

ggplot(data = pdata_c, 
       aes(x = protection_level, y= carb_mean,
           fill= factor(Species))) +
  geom_boxplot()+ geom_violin()

# model diagnoctic phase 1

# Plotting fitted values against residuals
par(mar = c(1, 1, 1, 1))

plot(residuals(bestmodel) ~ fitted(bestmodel), 
     xlab = "Fitted carbon", 
     ylab = "Deviance residuals", 
     pch = 16, 
     col = "darkgrey", 
     cex = 1.2)
abline(h = 0)
lines(lowess(residuals(bestmodel) ~  fitted(bestmodel)), 
      # according to this plot something seem to be not really good on the UP left 
      col = "red") ## we will probably check influentials points

# Next I'm plotting the the residuals along with our predictor variables
# here you can do same with other predictors to see the variation
plot(residuals(bestmodel) ~ pdata_c$z.s.sape_abund, 
     xlab = "Ape abundance", 
     ylab = "Deviance residuals", 
     pch = 16, 
     col = "darkgrey", 
     cex = 1.2)
abline(h = 0)
lines(lowess(residuals(bestmodel) ~  pdata_c$z.s.sape_abund), 
      col = "red")

# There seems to be no pattern here as everything converge to 0. 

# we can get an additional QQ plot for the random intercept from the sjPlot package.

sjPlot::plot_model(bestmodel, type = "diag")

# There aren't any red flags here at all, so we don't have any concerns from a diagnostics standpoint but We should also look for influentials points 

# One package that provides a tool to do this is "influence.ME". You can read more about this packages and the approach it takes here: (https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Nieuwenhuis~et~al.pdf)

# Model Diagnostics phase 2 -------------------------------------------------------
# 1.Above I write a code to include all parameters for model diagnostics directly in our main dadaframe (here pdata_c) directly.

# calculate cook's distance and plot to see what happens !
windows(15, 10)

cd <- cooks.distance(bestmodel)
plot(cd, type = "h")
abline(h = 3 * mean(cd), col = "red")

idx <- cd > 3 * mean(cd) # identified points according to our treshold
pdata_c[idx, ]

sum(idx) #  check number of influencials points sum(idx) #  check number of influencials points 
[1] 136897

windows(15, 10)
plot(cd, pch = 20, cex = 1.5, main = "Cook's Distance Plot")
abline(h = 3, col = "red") # plot cook distance

# obtain influentials points in data frame

influential_points <- which(cd > 3 * mean(cd))
sum(influential_points)

# Obtain Pearson residuals
residuals <- residuals(bestmodel, type = "pearson")
residuals

# Plot standardized residuals vs. fitted values
plot(fitted(bestmodel), residuals, pch = 20, cex = 1.5,
     main = "Standardized Residuals vs. Fitted Values")
abline(h = 0, col = "red")

# Remove influentials points in our best model 

pdata_cUpdate <- pdata_c[-influential_points, ]

# now I run the model without influentials points

# method 1 run the model with new data here call pdata_cUpdate

new_bestmodel <- glm(carb_mean ~ z.s.sape_abund + z.s.human_foot + Species + 
      z.Per_class_1 + z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + 
      protection_level, family = gaussian, data = pdata_cUpdate)

# method 2: we can just use the update function in our previous best model 
bestMcook <- update(bestmodel, data = pdata_cUpdate)
summary(bestMcook)

Call:
  glm(formula = carb_mean ~ z.s.sape_abund + z.s.human_foot + Species + 
        z.Per_class_1 + z.Per_class_2 + z.Per_class_4 + z.Per_class_5 + 
        protection_level, family = gaussian, data = pdata_cUpdate)

Deviance Residuals: 
  Min        1Q   Median       3Q      Max 
-209.440  -53.088   -6.204   50.808  227.559 

Coefficients:
  Estimate Std. Error  t value Pr(>|t|)
(Intercept)                       189.03977    0.06862 2754.920   <2e-16
z.s.sape_abund                      4.10413    0.06060   67.722   <2e-16
z.s.human_foot                    -30.21231    0.05705 -529.590   <2e-16
SpeciesGorilla                     26.86758    0.11543  232.770   <2e-16
z.Per_class_1                      56.74207    0.07076  801.870   <2e-16
z.Per_class_2                      -6.36435    0.07293  -87.267   <2e-16
z.Per_class_4                       8.06618    0.06248  129.105   <2e-16
z.Per_class_5                       1.20231    0.07541   15.944   <2e-16
protection_levelMedium protection -24.24980    0.14518 -167.035   <2e-16
protection_levelHigh protection    -1.80105    0.19415   -9.277   <2e-16

(Intercept)                       ***
  z.s.sape_abund                    ***
  z.s.human_foot                    ***
  SpeciesGorilla                    ***
  z.Per_class_1                     ***
  z.Per_class_2                     ***
  z.Per_class_4                     ***
  z.Per_class_5                     ***
  protection_levelMedium protection ***
  protection_levelHigh protection   ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 4871.428)

Null deviance: 2.1149e+10  on 2032875  degrees of freedom
Residual deviance: 9.9030e+09  on 2032866  degrees of freedom
AIC: 23030504

Number of Fisher Scoring iterations: 2

#-----------------------------------------------------------------
  # conduct model diagnostics again in our updated best model 

# Obtain Pearson residuals

residuals <- residuals(bestMcook, type = "pearson")

plot(fitted(bestMcook), residuals, pch = 20, cex = 1.5,
     main = "Standardized Residuals vs. Fitted Values")
abline(h = 0, col = "red")

# Plotting fitted values against residuals
windows(15, 10)
plot(residuals(bestMcook) ~ fitted(bestMcook), 
     xlab = "Fitted ", 
     ylab = "Deviance residuals", 
     pch = 16, 
     col = "darkgrey", 
     cex = 1.2)
abline(h = 0)
lines(lowess(residuals(bestMcook) ~  fitted(bestMcook)), 
      col = "red") # everything is ok now 

r.squaredGLMM(bestMcook) ~# 53% of explained data 

## confidence interval for the update model
confint(bestmodel, method = "Wald")

round(summary(bestMcook)$coefficients, digits = 3)

# Response curve for each predictors

library(visreg)

par(mar = c(1, 1, 1, 1))
par(mfrow = c(2, 2))
windows(15, 10)
visreg(bestMcook, "z.s.sape_abund",
       ylab = "Carbon stock (T/ha)",
       xlab = "Ape abundance (inds/km2)",
       scale = "response")

windows(15, 10)
visreg(bestMcook, "z.s.human_foot",
       ylab = "Carbon stock (T/ha)",
       xlab = "human footprint",
       scale = "response")

windows(15, 10)
visreg(bestMcook, "z.Per_class_1",
       ylab = "Carbon stock (T/ha)",
       xlab = "z.Per_class_1",
       scale = "response")

windows(15, 10)
visreg(bestMcook, "z.Per_class_2",
       ylab = "Carbon stock (T/ha)",
       xlab = "z.Per_class_2",
       scale = "response")

# save updatated best model output in excel file in our directory
setwd("E:/ApeProject")
df<- data.frame(round(summary(bestMcook)$coefficients, 3))
write.xlsx(df, "bestmodelUpdated.xlsx")

# Save model output to a CSV file
model_output <- broom::tidy(bestMcook)

# we can also use this code which include confidence interval also and # save the summary in exel 
model_summary <- tidy(bestMcook, conf.int = TRUE)
write_xlsx(model_summary, "model_summary_with_CI.xlsx")

# summarise the model in table but the code above do the same job ! no need to run again 
BestModel_Summary_table<-tab_model(bestMcook, 
                                   show.se = TRUE, 
                                   show.stat = TRUE, 
                                   show.icc = FALSE, 
                                   transform = NULL, show.p = T,
                                   string.est = "Beta", 
                                   string.se = "se", use.viewer = T,
                                   show.re.var = F, show.ngroups = F,
                                   pred.labels = c("Intercept",
                                                   "z.s.sape_abund", 
                                                   "z.s.human_foot",
                                                   "protection_level",
                                                   "Species", "z.Per_class_1", "z.Per_class_2", "z.Per_class_3", 
                                                   "z.Per_class_4", "z.Per_class_5"), dv.labels = c(""))

#Make a plot of the effect sizes:
sjPlot::plot_model(bestMcook)
#Let’s change the axis labels & title.

sjPlot::plot_model(bestMcook, 
                   axis.labels=c("z.s.sape_abund", 
                  "z.s.human_foot",
                  "protection_level",
                  "Species", 
                  "z.Per_class_1", 
                  "z.Per_class_2"),
                  show.values=TRUE,
                  show.p=TRUE,
                  title="Effect of covariate on carbon stock")

# try another predictor plotting vs response: we can do the same code for other predictors

effects_ApesAbun <- effects::effect(term= "z.s.sape_abund", mod= bestMcook)
summary(effects_ApesAbun)

x_abun <- as.data.frame(effects_ApesAbun)

ApesAbun_plot <- ggplot() + 
  
  #2
  geom_point(data=x_abun, aes(x=z.s.sape_abund, y=fit), color="blue") +
  #4
  geom_line(data=x_abun, aes(x=z.s.sape_abund, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_abun, aes(x=z.s.sape_abund, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="Ape_abundance", y="Carbon quantity")

ApesAbun_plot

# Next I'm plotting the the residuals along with our predictor variables
# here you can do same with the predictors to see the variation
plot(residuals(bestMcook) ~ pdata_c$z.s.sape_abund, 
     xlab = "Ape abundance", 
     ylab = "Deviance residuals", 
     pch = 16, 
     col = "darkgrey", 
     cex = 1.2)
abline(h = 0)
lines(lowess(residuals(bestmodel) ~  pdata_c$z.s.sape_abund), 
      col = "red")
# human foot
plot(residuals(bestMcook) ~ pdata_c$z.s.human_foot, 
     xlab = "Ape abundance", 
     ylab = "Deviance residuals", 
     pch = 16, 
     col = "darkgrey", 
     cex = 1.2)
abline(h = 0)
lines(lowess(residuals(bestmodel) ~  pdata_c$z.s.human_foot), 
      col = "red")
# class1

plot(residuals(bestMcook) ~ pdata_c$z.Per_class_2, 
     xlab = "Ape abundance", 
     ylab = "Deviance residuals", 
     pch = 16, 
     col = "darkgrey", 
     cex = 1.2)
abline(h = 0)
lines(lowess(residuals(bestMcook) ~  pdata_c$z.Per_class_2), 
      col = "red")


# There seems to be no pattern here as everything converge to 0. 

# we can get an additional QQ plot for the random intercept from the sjPlot 

windows(15, 10)
sjPlot::plot_model(bestmodel, type = "diag")
